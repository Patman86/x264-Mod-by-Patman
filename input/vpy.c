/*****************************************************************************
 * vpy.c: VapourSynth input (port from x265)
 *****************************************************************************
 * Copyright (C) 2009-2021 x264 project
 *
 * Author: Vladimir Kontserenko <djatom@beatrice-raws.org>
 * Some portions of code and ideas taken from avs.c, y4m.c files, "ffmpeg demuxer"
 * proposed at doom9 thread and from rigaya's NVEnc codebase.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02111, USA.
 *
 * This program is also available under a commercial proprietary license.
 * For more information, contact us at licensing@x264.com.
 *****************************************************************************/

#include "input.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef _MSC_VER
#include <windows.h>
#include <io.h>
typedef volatile LONG vpy_atomic_t;
static __forceinline LONG vpy_atomic_load(vpy_atomic_t *object)
{
    return InterlockedCompareExchange(object, 0, 0);
}
static __forceinline LONG vpy_atomic_fetch_add(vpy_atomic_t *object, LONG x)
{
    return InterlockedExchangeAdd(object, x);
}
static __forceinline LONG vpy_atomic_fetch_sub(vpy_atomic_t *object, LONG x)
{
    return InterlockedExchangeAdd(object, -x);
}
#else
#include <stdatomic.h>
#include <dlfcn.h>
#include <unistd.h>

typedef _Atomic int vpy_atomic_t;
static inline int vpy_atomic_load(vpy_atomic_t *object)
{
    return atomic_load(object);
}
static inline int vpy_atomic_fetch_add(vpy_atomic_t *object, int x)
{
    return atomic_fetch_add(object, x);
}
static inline int vpy_atomic_fetch_sub(vpy_atomic_t *object, int x)
{
    return atomic_fetch_sub(object, x);
}
#endif

#include "extras/VSScript4.h"
#include "extras/VSHelper4.h"

#ifdef _WIN32
typedef WCHAR libp_t;
#define vs_open(library) LoadLibraryW((LPWSTR)(library))
#define vs_close FreeLibrary
#define vs_address GetProcAddress
#define vs_sleep() Sleep(1)
#define vs_strtok strtok_s
#define vs_sscanf sscanf_s
#else
typedef char libp_t;
#define vs_open(library) dlopen((library), RTLD_GLOBAL | RTLD_NOW)
#define vs_close dlclose
#define vs_address dlsym
#define vs_sleep() usleep(1000)
#define vs_strtok strtok_r
#define vs_sscanf sscanf
#endif

#ifndef X264_MIN
#define X264_MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif

#define FAIL_IF_ERROR(cond, ...) FAIL_IF_ERR(cond, "vpy ", __VA_ARGS__)

/* Function pointer types for VSScript API loader */
typedef const VSSCRIPTAPI * (VS_CC *type_getVSScriptAPI)(int version);
typedef const char * (VS_CC *type_getVSScriptAPILastError)(void);

typedef struct VpyFrameSlot
{
    int frame_number;
    int ready;
    const VSFrame *frame;
#ifdef _WIN32
    HANDLE event;
#endif
} VpyFrameSlot;

/* Context holding all VapourSynth state and async reader state */
typedef struct VapourSynthContext
{
    /* VapourSynth handles */
    void *library;
    type_getVSScriptAPI func_getVSScriptAPI;
    type_getVSScriptAPILastError func_getVSScriptAPILastError;
    const VSSCRIPTAPI *vssapi;
    const VSAPI *vsapi;
    VSScript *script;
    VSNode *node;
    VSCore *core;

    /* asynchronous frame reader state */
    vpy_atomic_t requestedFrames;
    vpy_atomic_t completedFrames;
    vpy_atomic_t pendingFrames;
    int framesToRequest;
    int nextFrame;
    int parallelRequests;
    int async_failed_frame;
    int abort_async;

    VpyFrameSlot *slots;
    int slot_count;

    /* stream info */
    int num_frames;
    int bit_depth;
    int uc_depth;
    int vfr;
    uint64_t timebase_num;
    uint64_t timebase_den;
    int64_t current_timecode_num;
    int64_t current_timecode_den;
    int desired_bit_depth;
} VapourSynthContext;

/* ---------------------------------------------------------------------------
 * Helpers
 * ------------------------------------------------------------------------ */

static inline int vpy_slot_index(const VapourSynthContext *h, int frame_number)
{
    return frame_number % h->slot_count;
}

static inline VpyFrameSlot *vpy_get_slot(VapourSynthContext *h, int frame_number)
{
    return &h->slots[vpy_slot_index(h, frame_number)];
}

static void vpy_reset_slot(VpyFrameSlot *slot)
{
    slot->frame_number = -1;
    slot->ready = 0;
    slot->frame = NULL;
}

static void vpy_cleanup_partial(VapourSynthContext *h)
{
    if (!h)
        return;

    if (h->slots)
    {
        for (int i = 0; i < h->slot_count; i++)
        {
            if (h->slots[i].frame && h->vsapi)
                h->vsapi->freeFrame((VSFrame*)h->slots[i].frame);
#ifdef _WIN32
            if (h->slots[i].event)
                CloseHandle(h->slots[i].event);
#endif
        }
        free(h->slots);
        h->slots = NULL;
    }

    if (h->node && h->vsapi)
        h->vsapi->freeNode(h->node);
    if (h->script && h->vssapi)
        h->vssapi->freeScript(h->script);
    if (h->library)
        vs_close(h->library);

    free(h);
}

/* ---------------------------------------------------------------------------
 * Logging helpers
 * ------------------------------------------------------------------------ */

static int vs_to_x264_log_level(int msgType)
{
    switch (msgType)
    {
        case mtDebug:       return X264_LOG_DEBUG;
        case mtInformation: return X264_LOG_INFO;
        case mtWarning:     return X264_LOG_WARNING;
        case mtCritical:    return X264_LOG_WARNING;
        case mtFatal:       return X264_LOG_ERROR;
        default:            return X264_LOG_DEBUG;
    }
}

static void VS_CC log_message_handler(int msgType, const char *msg, void *userData)
{
    (void)userData;
    x264_cli_log("vpy ", vs_to_x264_log_level(msgType), "%s\n", msg ? msg : "");
}

/* ---------------------------------------------------------------------------
 * Core revision helper
 * ------------------------------------------------------------------------ */

static int get_core_revision(const char *vsVersionString)
{
    char *api_info = NULL;
    char buf[1024];

    if (!vsVersionString)
        return 0;

    snprintf(buf, sizeof(buf), "%s", vsVersionString);

    for (char *p = buf, *q = NULL, *r = NULL;
         NULL != (q = vs_strtok(p, "\n", &r)); )
    {
        if (NULL != (api_info = strstr(q, "Core")))
        {
            snprintf(buf, sizeof(buf), "%s", api_info);
            for (char *s = buf; *s; s++)
                *s = (char)tolower((unsigned char)*s);

            {
                int rev = 0;
                return (1 == vs_sscanf(buf, "core r%d", &rev)) ? rev : 0;
            }
        }
        p = NULL;
    }

    return 0;
}

/* ---------------------------------------------------------------------------
 * Async callback
 * ------------------------------------------------------------------------ */

static void VS_CC async_callback(void *user_data, const VSFrame *f, int n, VSNode *node, const char *error_msg)
{
    (void)node;
    VapourSynthContext *h = (VapourSynthContext*)user_data;
    VpyFrameSlot *slot = vpy_get_slot(h, n);

    if (!f)
    {
        if (h->async_failed_frame < 0 || n < h->async_failed_frame)
            h->async_failed_frame = n;

        x264_cli_log("vpy ", X264_LOG_ERROR,
                     "async frame request #%d failed: %s\n",
                     n, error_msg ? error_msg : "unknown error");
    }
    else
    {
        if (h->abort_async)
        {
            h->vsapi->freeFrame((VSFrame*)f);
        }
        else if (slot->frame_number != n || slot->ready || slot->frame)
        {
            h->vsapi->freeFrame((VSFrame*)f);
            if (h->async_failed_frame < 0 || n < h->async_failed_frame)
                h->async_failed_frame = n;

            x264_cli_log("vpy ", X264_LOG_ERROR,
                         "async slot collision at frame %d\n", n);
        }
        else
        {
            slot->frame = f;
            slot->ready = 1;
            vpy_atomic_fetch_add(&h->completedFrames, 1);
        }
    }

    vpy_atomic_fetch_sub(&h->pendingFrames, 1);

#ifdef _WIN32
    SetEvent(slot->event);
#endif
}

/* ---------------------------------------------------------------------------
 * Load VapourSynth script library (VSScript)
 * ------------------------------------------------------------------------ */

#ifndef _WIN32
static int try_vs_open(void **library, const char *path)
{
    if (!path || !*path)
        return -1;

    *library = vs_open(path);
    return *library ? 0 : -1;
}
#endif

static int custom_vs_load_library(VapourSynthContext *h, cli_input_opt_t *opt)
{
#ifdef _WIN32
    libp_t *library_path = L"vsscript";
    libp_t *tmp_buf = NULL;
#else
#if SYS_MACOSX
    const char *default_names[] =
    {
        "libvsscript.dylib",
        "libvapoursynth-script.dylib",
        "/opt/homebrew/lib/libvsscript.dylib",
        "/usr/local/lib/libvsscript.dylib",
        "/opt/homebrew/lib/libvapoursynth-script.dylib",
        "/usr/local/lib/libvapoursynth-script.dylib",
        NULL
    };
#else
    const char *default_names[] =
    {
        "libvsscript.so",
        "libvapoursynth-script.so",
        "/usr/lib/libvsscript.so",
        "/usr/local/lib/libvsscript.so",
        NULL
    };
#endif
    const char *env_path = getenv("VSSCRIPT_PATH");
    const char **candidate = NULL;
    const char *library_path = NULL;
#endif

    if (opt->frameserver_lib_path)
    {
#ifdef _WIN32
        int size_needed = MultiByteToWideChar(CP_UTF8, 0, opt->frameserver_lib_path, -1, NULL, 0);
        FAIL_IF_ERROR(size_needed <= 0, "invalid VapourSynth library path\n");

        tmp_buf = (libp_t*)malloc((size_t)size_needed * sizeof(libp_t));
        FAIL_IF_ERROR(!tmp_buf, "failed to allocate library path buffer\n");

        FAIL_IF_ERROR(!MultiByteToWideChar(CP_UTF8, 0, opt->frameserver_lib_path, -1, (LPWSTR)tmp_buf, size_needed),
                      "failed to convert VapourSynth library path to UTF-16\n");
        library_path = tmp_buf;
#else
        library_path = opt->frameserver_lib_path;
#endif
        x264_cli_log("vpy ", X264_LOG_INFO,
                     "using external VapourSynth library from: \"%s\"\n",
                     opt->frameserver_lib_path);
    }

#ifdef _WIN32
    h->library = vs_open(library_path);

    if (tmp_buf)
        free(tmp_buf);

    if (!h->library)
        return -1;
#else
    if (library_path)
    {
        h->library = vs_open(library_path);
        if (!h->library)
            return -1;
    }
    else
    {
        if (env_path && *env_path)
        {
            x264_cli_log("vpy ", X264_LOG_INFO,
                         "using VSSCRIPT_PATH: \"%s\"\n", env_path);

            if (!try_vs_open(&h->library, env_path))
                goto library_loaded;
        }

        for (candidate = default_names; *candidate; candidate++)
        {
            if (!try_vs_open(&h->library, *candidate))
                goto library_loaded;
        }

        return -1;
    }

library_loaded:
    ;
#endif

    h->func_getVSScriptAPI = (type_getVSScriptAPI)vs_address(h->library, "getVSScriptAPI");
    FAIL_IF_ERROR(!h->func_getVSScriptAPI,
                  "failed to load getVSScriptAPI function. Upgrade VapourSynth to R55 or newer!\n");

    h->func_getVSScriptAPILastError =
        (type_getVSScriptAPILastError)vs_address(h->library, "getVSScriptAPILastError");

    return 0;
}

/* ---------------------------------------------------------------------------
 * open_file: setup VS, query format, and start async reader
 * ------------------------------------------------------------------------ */

static int open_file(char *psz_filename, hnd_t *p_handle, video_info_t *info, cli_input_opt_t *opt)
{
    FILE *fh;
    int b_regular;
    VapourSynthContext *h = NULL;
    VSCoreInfo core_info;
    const VSVideoInfo *vi;
    const VSMap *props;
    const VSVideoFormat *fmt;
    const VSFrame *frame0;
    char errbuf[256];
    int err_sar_num, err_sar_den;
    int64_t sar_num, sar_den;
    int high_depth;

    fh = x264_fopen(psz_filename, "r");
    if (!fh)
        return -1;

    b_regular = x264_is_regular_file(fh);
    fclose(fh);
    FAIL_IF_ERROR(!b_regular, "vpy input is incompatible with non-regular file `%s'\n", psz_filename);

    h = (VapourSynthContext*)calloc(1, sizeof(VapourSynthContext));
    if (!h)
        return -1;

    if (custom_vs_load_library(h, opt))
        goto fail;

    h->vssapi = h->func_getVSScriptAPI(VSSCRIPT_API_VERSION);
    if (!h->vssapi)
    {
        const char *detail = NULL;
        if (h->func_getVSScriptAPILastError)
            detail = h->func_getVSScriptAPILastError();

        FAIL_IF_ERROR(1, "failed to initialize VSScript%s%s\n",
                      detail ? ": " : "",
                      detail ? detail : "");
    }

    h->vsapi = h->vssapi->getVSAPI(VAPOURSYNTH_API_VERSION);
    FAIL_IF_ERROR(!h->vsapi, "failed to get VapourSynth API pointer\n");

    h->core = h->vsapi->createCore(0);
    FAIL_IF_ERROR(!h->core, "failed to create VapourSynth core\n");

    h->vsapi->addLogHandler(log_message_handler, NULL, NULL, h->core);

    h->script = h->vssapi->createScript(h->core);
    FAIL_IF_ERROR(!h->script, "failed to create VapourSynth script\n");

    h->vssapi->evalSetWorkingDir(h->script, 1);
    h->vssapi->evaluateFile(h->script, (const char*)psz_filename);
    if (h->vssapi->getError(h->script))
        FAIL_IF_ERROR(1, "Can't evaluate script: %s\n", h->vssapi->getError(h->script));

    h->node = h->vssapi->getOutputNode(h->script, 0);
    FAIL_IF_ERROR(!h->node || h->vsapi->getNodeType(h->node) != mtVideo,
                  "`%s' has no video data\n", psz_filename);

    h->vsapi->getCoreInfo(h->vssapi->getCore(h->script), &core_info);
    vi = h->vsapi->getVideoInfo(h->node);
    FAIL_IF_ERROR(!vsh_isConstantVideoFormat(vi), "only constant video formats are supported\n");

    x264_cli_log("vpy ", X264_LOG_INFO,
                 "VapourSynth Video Processing Library Core R%d\n",
                 get_core_revision(core_info.versionString));

    info->width = vi->width;
    info->height = vi->height;
    info->vfr = h->vfr = 0;

    if (!opt->b_accurate_fps)
        x264_ntsc_fps(&info->fps_num, &info->fps_den);

    frame0 = h->vsapi->getFrame(0, h->node, errbuf, sizeof(errbuf));
    FAIL_IF_ERROR(!frame0, "%s occurred while getting frame 0\n", errbuf);

    props = h->vsapi->getFramePropertiesRO(frame0);
    sar_num = h->vsapi->mapGetInt(props, "_SARNum", 0, &err_sar_num);
    sar_den = h->vsapi->mapGetInt(props, "_SARDen", 0, &err_sar_den);
    info->sar_width = (!err_sar_num && sar_num > 0) ? (int)sar_num : 0;
    info->sar_height = (!err_sar_den && sar_den > 0) ? (int)sar_den : 0;

    if (vi->fpsNum == 0 && vi->fpsDen == 0)
    {
        int err_num, err_den;
        int64_t fps_num = h->vsapi->mapGetInt(props, "_DurationNum", 0, &err_num);
        int64_t fps_den = h->vsapi->mapGetInt(props, "_DurationDen", 0, &err_den);

        FAIL_IF_ERROR((err_num || err_den), "missing FPS values at frame 0\n");
        FAIL_IF_ERROR(!fps_den, "FPS denominator is zero at frame 0\n");

        info->fps_num = fps_den;
        info->fps_den = fps_num;

        info->timebase_num = h->timebase_num = 1;
        info->timebase_den = h->timebase_den = 10000000;
        h->current_timecode_num = 0;
        h->current_timecode_den = 1;
        info->vfr = h->vfr = 1;
    }
    else
    {
        info->fps_num = vi->fpsNum;
        info->fps_den = vi->fpsDen;
    }

    h->vsapi->freeFrame((VSFrame*)frame0);

    info->num_frames = h->num_frames = vi->numFrames;
    h->bit_depth = vi->format.bitsPerSample;
    h->desired_bit_depth = opt->desired_bit_depth;

    FAIL_IF_ERROR(h->bit_depth < 8 || h->bit_depth > 16,
                  "unsupported bit depth `%d'\n", h->bit_depth);
    FAIL_IF_ERROR(vi->format.sampleType == stFloat,
                  "unsupported sample type `float'\n");

    h->uc_depth = h->bit_depth & 7;

    fmt = &vi->format;
    high_depth = fmt->bitsPerSample > 8 ? X264_CSP_HIGH_DEPTH : 0;

    if (fmt->colorFamily == cfRGB)
    {
        info->csp = X264_CSP_BGR | X264_CSP_VFLIP | high_depth;
    }
    else if (fmt->colorFamily == cfYUV)
    {
        if (fmt->subSamplingW == 0 && fmt->subSamplingH == 0)
            info->csp = X264_CSP_I444 | high_depth;
        else if (fmt->subSamplingW == 1 && fmt->subSamplingH == 0)
            info->csp = X264_CSP_I422 | high_depth;
        else if (fmt->subSamplingW == 1 && fmt->subSamplingH == 1)
            info->csp = X264_CSP_I420 | high_depth;
    }
    else if (fmt->colorFamily == cfGray)
    {
        info->csp = X264_CSP_I400 | high_depth;
    }
    else
    {
        char format_name[32];
        h->vsapi->getVideoFormatName(&vi->format, format_name);
        FAIL_IF_ERROR(1, "not supported pixel type: %s\n", format_name);
    }

    if (h->bit_depth == h->desired_bit_depth)
        info->csp |= X264_CSP_SKIP_DEPTH_FILTER;

    info->thread_safe = 1;

    if (opt->vs_requests > 0)
    {
        h->parallelRequests = opt->vs_requests;
        x264_cli_log("vpy ", X264_LOG_INFO,
                     "using user-defined %d parallel VapourSynth requests\n",
                     h->parallelRequests);
    }
    else
    {
        h->parallelRequests = core_info.numThreads > 0 ? core_info.numThreads : 1;
        x264_cli_log("vpy ", X264_LOG_INFO,
                     "using %d parallel VapourSynth requests\n",
                     h->parallelRequests);
    }

    if (h->parallelRequests > h->num_frames)
        h->parallelRequests = h->num_frames;
    if (h->parallelRequests < 1)
        h->parallelRequests = 1;

    h->framesToRequest = h->num_frames;
    h->nextFrame = 0;
    h->requestedFrames = 0;
    h->completedFrames = 0;
    h->pendingFrames = 0;
    h->async_failed_frame = -1;
    h->abort_async = 0;

    h->slot_count = h->parallelRequests + 1;
    h->slots = (VpyFrameSlot*)calloc((size_t)h->slot_count, sizeof(VpyFrameSlot));
    FAIL_IF_ERROR(!h->slots, "failed to allocate async frame slots\n");

    for (int i = 0; i < h->slot_count; i++)
    {
        vpy_reset_slot(&h->slots[i]);
#ifdef _WIN32
        h->slots[i].event = CreateEvent(NULL, FALSE, FALSE, NULL);
        FAIL_IF_ERROR(!h->slots[i].event, "failed to create async event for slot %d\n", i);
#endif
    }

    {
        int initial = X264_MIN(h->parallelRequests, h->num_frames);
        for (int n = 0; n < initial; n++)
        {
            VpyFrameSlot *slot = vpy_get_slot(h, n);
            FAIL_IF_ERROR(slot->ready || slot->frame,
                          "slot collision before initial async request for frame %d\n", n);

            slot->frame_number = n;
#ifdef _WIN32
            ResetEvent(slot->event);
#endif
            h->vsapi->getFrameAsync(n, h->node, async_callback, h);
            vpy_atomic_fetch_add(&h->pendingFrames, 1);
            h->requestedFrames++;
        }
    }

    *p_handle = h;
    return 0;

fail:
    vpy_cleanup_partial(h);
    return -1;
}

/* ---------------------------------------------------------------------------
 * picture_alloc: allocate x264-owned buffers for each frame
 * ------------------------------------------------------------------------ */

static int picture_alloc(cli_pic_t *pic, hnd_t handle, int csp, int width, int height)
{
    (void)handle;
    if (x264_cli_pic_alloc(pic, csp, width, height))
        return -1;
    return 0;
}

/* ---------------------------------------------------------------------------
 * read_frame: wait for frame i, copy into x264 buffers, update VFR
 * ------------------------------------------------------------------------ */

static int read_frame(cli_pic_t *pic, hnd_t handle, int i_frame)
{
    VapourSynthContext *h = (VapourSynthContext*)handle;
    VpyFrameSlot *slot;
    const VSFrame *vs_frame;
    const VSVideoFormat *fi;
    static const int planes[3] = { 0, 1, 2 };

    if (i_frame >= h->num_frames)
        return -1;
    if (h->abort_async)
        return -1;
    if (h->async_failed_frame >= 0 && h->async_failed_frame <= i_frame)
        return -1;

    slot = vpy_get_slot(h, i_frame);

#ifdef _WIN32
    while (!slot->ready && h->async_failed_frame < 0)
        WaitForSingleObject(slot->event, 1);
#else
    while (!slot->ready && h->async_failed_frame < 0)
        vs_sleep();
#endif

    if (h->async_failed_frame >= 0 && h->async_failed_frame <= i_frame)
        return -1;

    if (slot->frame_number != i_frame || !slot->frame)
        return -1;

    vs_frame = slot->frame;

    if (!h->abort_async && h->requestedFrames < h->framesToRequest && h->async_failed_frame < 0)
    {
        int in_flight = h->requestedFrames - h->nextFrame;
        if (in_flight < h->parallelRequests)
        {
            int next = h->requestedFrames;
            VpyFrameSlot *next_slot = vpy_get_slot(h, next);

            if (next_slot->ready || next_slot->frame)
            {
                x264_cli_log("vpy ", X264_LOG_ERROR,
                             "next async slot still occupied for frame %d\n", next);
                return -1;
            }

            next_slot->frame_number = next;
#ifdef _WIN32
            ResetEvent(next_slot->event);
#endif
            h->vsapi->getFrameAsync(next, h->node, async_callback, h);
            vpy_atomic_fetch_add(&h->pendingFrames, 1);
            h->requestedFrames = next + 1;
        }
    }

    fi = h->vsapi->getVideoFrameFormat(vs_frame);

    for (int i = 0; i < pic->img.planes; i++)
    {
        int plane = planes[i];
        const uint8_t *src = h->vsapi->getReadPtr(vs_frame, plane);
        int src_stride = h->vsapi->getStride(vs_frame, plane);
        int plane_height = h->vsapi->getFrameHeight(vs_frame, plane);
        uint8_t *dst = pic->img.plane[i];
        int dst_stride = pic->img.stride[i];
        int row_bytes = X264_MIN(src_stride, dst_stride);

        for (int y = 0; y < plane_height; y++)
            memcpy(dst + (intptr_t)y * dst_stride,
                   src + (intptr_t)y * src_stride,
                   row_bytes);

        if (h->uc_depth && h->bit_depth != h->desired_bit_depth)
        {
            uint16_t *dst16 = (uint16_t*)dst;
            int row_pixels = dst_stride / fi->bytesPerSample;
            int lshift = 16 - h->bit_depth;

            for (int y = 0; y < plane_height; y++)
            {
                uint16_t *row = dst16 + (size_t)y * row_pixels;
                for (int j = 0; j < row_pixels; j++)
                    row[j] <<= lshift;
            }
        }
    }

    if (h->vfr)
    {
        const VSMap *props;
        int err_num, err_den;
        int64_t duration_num, duration_den;

        pic->pts = (h->current_timecode_num * h->timebase_den / h->current_timecode_den);
        pic->duration = 0;

        props = h->vsapi->getFramePropertiesRO(vs_frame);
        duration_num = h->vsapi->mapGetInt(props, "_DurationNum", 0, &err_num);
        duration_den = h->vsapi->mapGetInt(props, "_DurationDen", 0, &err_den);

        FAIL_IF_ERROR((err_num || err_den), "missing duration at frame %d", i_frame);
        FAIL_IF_ERROR(!duration_den, "duration denominator is zero at frame %d", i_frame);

        vsh_addRational(&h->current_timecode_num, &h->current_timecode_den,
                        duration_num, duration_den);
    }

    h->vsapi->freeFrame((VSFrame*)vs_frame);
    slot->frame = NULL;
    slot->ready = 0;
    slot->frame_number = -1;
    h->nextFrame = i_frame + 1;

    return 0;
}

/* ---------------------------------------------------------------------------
 * release_frame / picture_clean
 * ------------------------------------------------------------------------ */

static int release_frame(cli_pic_t *pic, hnd_t handle)
{
    (void)pic;
    (void)handle;
    return 0;
}

static void picture_clean(cli_pic_t *pic, hnd_t handle)
{
    (void)handle;
    memset(pic, 0, sizeof(cli_pic_t));
}

/* ---------------------------------------------------------------------------
 * close_file: wait for async completion and free all resources
 * ------------------------------------------------------------------------ */

static int close_file(hnd_t handle)
{
    VapourSynthContext *h = (VapourSynthContext*)handle;

    h->abort_async = 1;

    {
        int out;
        while ((out = vpy_atomic_load(&h->pendingFrames)) != 0)
        {
            x264_cli_log("vpy ", X264_LOG_DEBUG,
                         "waiting for %d async frame requests to complete... \r", out);
            vs_sleep();
        }
    }

    if (h->nextFrame < h->num_frames)
    {
        x264_cli_log("vpy ", X264_LOG_WARNING,
                     "only %d/%d frames delivered from VapourSynth\n",
                     h->nextFrame, h->num_frames);
    }

    if (h->slots)
    {
        for (int i = 0; i < h->slot_count; i++)
        {
            if (h->slots[i].frame)
                h->vsapi->freeFrame((VSFrame*)h->slots[i].frame);
#ifdef _WIN32
            if (h->slots[i].event)
                CloseHandle(h->slots[i].event);
#endif
        }
        free(h->slots);
    }

    if (h->node)
        h->vsapi->freeNode(h->node);
    if (h->script)
        h->vssapi->freeScript(h->script);
    if (h->library)
        vs_close(h->library);

    free(h);
    return 0;
}

/* ---------------------------------------------------------------------------
 * x264 CLI input descriptor
 * ------------------------------------------------------------------------ */

const cli_input_t vpy_input =
{
    open_file,
    picture_alloc,
    read_frame,
    release_frame,
    picture_clean,
    close_file
};
