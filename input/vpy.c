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

#ifdef _MSC_VER
#include <intrin.h>
typedef intptr_t atomic_int;
#define atomic_load(object)         _InterlockedCompareExchange64((volatile LONG64*)(object), 0, 0)
#define atomic_fetch_add(object,x)  _InterlockedExchangeAdd64((volatile LONG64*)(object), (x))
#define atomic_fetch_sub(object,x)  _InterlockedExchangeAdd64((volatile LONG64*)(object), -(x))
#else
#include <stdatomic.h>
#endif

#include "extras/VSScript4.h"
#include "extras/VSHelper4.h"

#ifdef _WIN32
#include <windows.h>
typedef WCHAR  libp_t;
#define vs_open(library)     LoadLibraryW((LPWSTR)(library))
#define vs_close             FreeLibrary
#define vs_address           GetProcAddress
#define vs_sleep()           Sleep(500)
#define vs_strtok            strtok_s
#define vs_sscanf            sscanf_s
#else
#include <dlfcn.h>
#include <unistd.h>
typedef char   libp_t;
#define vs_open(library)     dlopen((library), RTLD_GLOBAL | RTLD_LAZY | RTLD_NOW)
#define vs_close             dlclose
#define vs_address           dlsym
#define vs_sleep()           usleep(500)
#define vs_strtok            strtok_r
#define vs_sscanf            sscanf
#endif

#ifndef X264_MIN
#define X264_MIN(a,b) (( (a) < (b) ) ? (a) : (b))
#endif

#define FAIL_IF_ERROR(cond, ...) FAIL_IF_ERR(cond, "vpy ", __VA_ARGS__)

/* Function pointer type for VSScript API loader */
typedef const VSSCRIPTAPI * (VS_CC *type_getVSScriptAPI)(int version);

/* Context holding all VapourSynth state and async reader state */
typedef struct VapourSynthContext
{
    /* VapourSynth handles */
    void                *library;
    type_getVSScriptAPI  func_getVSScriptAPI;
    const VSSCRIPTAPI   *vssapi;
    const VSAPI         *vsapi;
    VSScript            *script;
    VSNode              *node;
    VSCore              *core;

    /* asynchronous frame reader state */
    atomic_int           requestedFrames;   /* number of frames requested via getFrameAsync */
    atomic_int           completedFrames;   /* number of frames delivered (callbacks) */
    atomic_int           pendingFrames;     /* currently outstanding async requests */
    int                  framesToRequest;   /* total frames to request (clip length) */
    int                  nextFrame;         /* next frame index expected to be read by CLI */
    int                  parallelRequests;  /* max in-flight async requests */
    int                  async_failed_frame;/* first failing frame index, or <0 on success */

#ifdef _WIN32
    HANDLE              *frameEvent;        /* per-frame event, size = num_frames */
#endif
    const VSFrame      **frameArray;        /* per-frame pointer, size = num_frames */

    /* stream info */
    int                  num_frames;
    int                  bit_depth;
    int                  uc_depth;
    int                  vfr;
    uint64_t             timebase_num;
    uint64_t             timebase_den;
    int64_t              current_timecode_num;
    int64_t              current_timecode_den;
    int                  desired_bit_depth;

} VapourSynthContext;


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
    x264_cli_log("vpy ", vs_to_x264_log_level(msgType), "%s\n", msg);
}


/* ---------------------------------------------------------------------------
 * Core revision helper (adapted from original vpy.c)
 * ------------------------------------------------------------------------ */

static int get_core_revision(const char *vsVersionString)
{
    char *api_info = NULL;
    char  buf[1024];
    strcpy(buf, vsVersionString);

    for (char *p = buf, *q = NULL, *r = NULL;
         NULL != (q = vs_strtok(p, "\n", &r)); )
    {
        if (NULL != (api_info = strstr(q, "Core")))
        {
            strcpy(buf, api_info);
            for (char *s = buf; *s; s++)
                *s = (char)tolower(*s);

            int rev = 0;
            return (1 == vs_sscanf(buf, "core r%d", &rev)) ? rev : 0;
        }
        p = NULL;
    }
    return 0;
}


/* ---------------------------------------------------------------------------
 * Async callback: store one VSFrame per index and signal completion
 * ------------------------------------------------------------------------ */

static void VS_CC async_callback(void *user_data, const VSFrame *f, int n, VSNode *node, const char *error_msg)
{
    (void)node;
    VapourSynthContext *h = (VapourSynthContext*)user_data;

    if (!f)
    {
        h->async_failed_frame = n;
        x264_cli_log("vpy ", X264_LOG_ERROR, "async frame request #%d failed: %s\n", n, error_msg ? error_msg : "unknown error");
    }
    else
    {
        if (!h->frameArray[n])
        {
            h->frameArray[n] = f;
            atomic_fetch_add(&h->completedFrames, 1);
        }
    }

    atomic_fetch_sub(&h->pendingFrames, 1);

#ifdef _WIN32
    if (h->frameEvent && h->frameEvent[n])
        SetEvent(h->frameEvent[n]);
#endif
}


/* ---------------------------------------------------------------------------
 * Load VapourSynth script library (VSScript)
 * ------------------------------------------------------------------------ */

static int custom_vs_load_library(VapourSynthContext *h, cli_input_opt_t *opt)
{
#ifdef _WIN32
    libp_t *library_path = L"vsscript";
    libp_t *tmp_buf      = NULL;
#else
#   if SYS_MACOSX
    libp_t *library_path = "libvapoursynth-script.dylib";
#   else
    libp_t *library_path = "libvapoursynth-script.so";
#   endif
#endif

    if (opt->frameserver_lib_path)
    {
#ifdef _WIN32
        int size_needed = MultiByteToWideChar(CP_UTF8, 0, opt->frameserver_lib_path, -1, NULL, 0);
        tmp_buf = (libp_t*)malloc(size_needed * sizeof(libp_t));
        MultiByteToWideChar(CP_UTF8, 0, opt->frameserver_lib_path, -1, (LPWSTR)tmp_buf, size_needed);
        library_path = tmp_buf;
#else
        library_path = opt->frameserver_lib_path;
#endif
        x264_cli_log("vpy ", X264_LOG_INFO, "using external VapourSynth library from %s\n", opt->frameserver_lib_path);
    }

    h->library = vs_open(library_path);

#ifdef _WIN32
    if (opt->frameserver_lib_path && tmp_buf)
        free(tmp_buf);
#endif

    if (!h->library)
        return -1;

    h->func_getVSScriptAPI = (type_getVSScriptAPI)vs_address(h->library, "getVSScriptAPI");
    FAIL_IF_ERROR(!h->func_getVSScriptAPI, "failed to load getVSScriptAPI function. Upgrade Vapoursynth to R55 or newer!\n");

    return 0;
}


/* ---------------------------------------------------------------------------
 * open_file: setup VS, query format, and start async reader
 * ------------------------------------------------------------------------ */

static int open_file(char *psz_filename, hnd_t *p_handle, video_info_t *info, cli_input_opt_t *opt)
{
    FILE *fh = x264_fopen(psz_filename, "r");
    if (!fh)
        return -1;

    int b_regular = x264_is_regular_file(fh);
    fclose(fh);
    FAIL_IF_ERROR(!b_regular, "vpy input is incompatible with non-regular file `%s'\n", psz_filename);

    VapourSynthContext *h = (VapourSynthContext*)calloc(1, sizeof(VapourSynthContext));
    if (!h)
        return -1;

    FAIL_IF_ERROR(custom_vs_load_library(h, opt), "failed to load VapourSynth\n");

    h->vssapi = h->func_getVSScriptAPI(VSSCRIPT_API_VERSION);
    FAIL_IF_ERROR(!h->vssapi, "failed to initialize VSScript\n");

    h->vsapi = h->vssapi->getVSAPI(VAPOURSYNTH_API_VERSION);
    FAIL_IF_ERROR(!h->vsapi, "failed to get VapourSynth API pointer\n");

    h->core = h->vsapi->createCore(0);
    h->vsapi->addLogHandler(log_message_handler, NULL, NULL, h->core);

    h->script = h->vssapi->createScript(h->core);
    h->vssapi->evalSetWorkingDir(h->script, 1);
    h->vssapi->evaluateFile(h->script, (const char*)psz_filename);
    if (h->vssapi->getError(h->script))
        FAIL_IF_ERROR(1, "Can't evaluate script: %s\n", h->vssapi->getError(h->script));

    h->node = h->vssapi->getOutputNode(h->script, 0);
    FAIL_IF_ERROR(!h->node || h->vsapi->getNodeType(h->node) != mtVideo, "`%s' has no video data\n", psz_filename);

    VSCoreInfo core_info;
    h->vsapi->getCoreInfo(h->vssapi->getCore(h->script), &core_info);
    const VSVideoInfo *vi = h->vsapi->getVideoInfo(h->node);
    FAIL_IF_ERROR(!vsh_isConstantVideoFormat(vi), "only constant video formats are supported\n");

    x264_cli_log("vpy ", X264_LOG_INFO, "VapourSynth Video Processing Library Core R%d\n", get_core_revision(core_info.versionString));

    info->width  = vi->width;
    info->height = vi->height;
    info->vfr    = h->vfr = 0;

    if (!opt->b_accurate_fps)
        x264_ntsc_fps(&info->fps_num, &info->fps_den);

    /* Get SAR and possibly VFR data from frame 0 */
    char errbuf[256];
    const VSFrame *frame0 = h->vsapi->getFrame(0, h->node, errbuf, sizeof(errbuf));
    FAIL_IF_ERROR(!frame0, "%s occurred while getting frame 0\n", errbuf);

    const VSMap *props = h->vsapi->getFramePropertiesRO(frame0);
    int err_sar_num, err_sar_den;
    int64_t sar_num = h->vsapi->mapGetInt(props, "_SARNum", 0, &err_sar_num);
    int64_t sar_den = h->vsapi->mapGetInt(props, "_SARDen", 0, &err_sar_den);
    info->sar_height = sar_den;
    info->sar_width  = sar_num;

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

    h->vsapi->freeFrame(frame0);

    info->num_frames = h->num_frames = vi->numFrames;
    h->bit_depth      = vi->format.bitsPerSample;
    h->desired_bit_depth = opt->desired_bit_depth;
    FAIL_IF_ERROR(h->bit_depth < 8 || h->bit_depth > 16, "unsupported bit depth `%d'\n", h->bit_depth);
    FAIL_IF_ERROR(vi->format.sampleType == stFloat, "unsupported sample type `float'\n");

    h->uc_depth = h->bit_depth & 7;

    /* CSP mapping identical to original vpy.c */
    uint32_t format_id = h->vsapi->queryVideoFormatID(vi->format.colorFamily, vi->format.sampleType, vi->format.bitsPerSample, vi->format.subSamplingW, vi->format.subSamplingH, h->core);

    if (format_id == pfRGB48)
        info->csp = X264_CSP_BGR | X264_CSP_VFLIP | X264_CSP_HIGH_DEPTH;
    else if (format_id == pfRGB24)
        info->csp = X264_CSP_BGR | X264_CSP_VFLIP;
    else if (format_id == pfYUV444P9 || format_id == pfYUV444P10 || format_id == pfYUV444P12 || format_id == pfYUV444P14 || format_id == pfYUV444P16)
        info->csp = X264_CSP_I444 | X264_CSP_HIGH_DEPTH;
    else if (format_id == pfYUV422P9 || format_id == pfYUV422P10 || format_id == pfYUV422P12 || format_id == pfYUV422P14 || format_id == pfYUV422P16)
        info->csp = X264_CSP_I422 | X264_CSP_HIGH_DEPTH;
    else if (format_id == pfYUV420P9 || format_id == pfYUV420P10 || format_id == pfYUV420P12 || format_id == pfYUV420P14 || format_id == pfYUV420P16)
        info->csp = X264_CSP_I420 | X264_CSP_HIGH_DEPTH;
    else if (format_id == pfYUV444P8)
        info->csp = X264_CSP_I444;
    else if (format_id == pfYUV422P8)
        info->csp = X264_CSP_I422;
    else if (format_id == pfYUV420P8)
        info->csp = X264_CSP_I420;
    else
    {
        char format_name[32];
        h->vsapi->getVideoFormatName(&vi->format, format_name);
        FAIL_IF_ERROR(1, "not supported pixel type: %s\n", format_name);
    }

    if (h->bit_depth == h->desired_bit_depth)
        info->csp |= X264_CSP_SKIP_DEPTH_FILTER;

    info->thread_safe = 1;

    /* Async reader initialisation (x265-style) */
    h->parallelRequests = core_info.numThreads;
    if (h->parallelRequests <= 0)
        h->parallelRequests = 1;

    h->framesToRequest    = h->num_frames;
    h->nextFrame          = 0;
    h->requestedFrames    = 0;
    h->completedFrames    = 0;
    h->pendingFrames      = 0;
    h->async_failed_frame = -1;

    h->frameArray = (const VSFrame**)malloc(h->num_frames * sizeof(const VSFrame*));
    FAIL_IF_ERROR(!h->frameArray, "failed to allocate frameArray\n");
    memset((void*)h->frameArray, 0, h->num_frames * sizeof(const VSFrame*));

#ifdef _WIN32
    h->frameEvent = (HANDLE*)malloc(h->num_frames * sizeof(HANDLE));
    FAIL_IF_ERROR(!h->frameEvent, "failed to allocate frameEvent array\n");

    for (int i = 0; i < h->num_frames; i++)
    {
        h->frameEvent[i] = CreateEvent(NULL, FALSE, FALSE, NULL);
        FAIL_IF_ERROR(!h->frameEvent[i], "failed to create async event for frame %d\n", i);
    }
#endif

    int initial = X264_MIN(h->parallelRequests, h->num_frames);
    for (int n = 0; n < initial; n++)
    {
        h->vsapi->getFrameAsync(n, h->node, async_callback, h);
        atomic_fetch_add(&h->pendingFrames, 1);
        h->requestedFrames++;
    }

    *p_handle = h;
    return 0;
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
    static const int planes[3] = { 0, 1, 2 };

    if (i_frame >= h->num_frames)
        return -1;

    if (h->async_failed_frame >= 0 && h->async_failed_frame <= i_frame)
        return -1;

    /* Wait for frame i_frame to be delivered */
#ifdef _WIN32
    WaitForSingleObject(h->frameEvent[i_frame], INFINITE);
    const VSFrame *vs_frame = h->frameArray[i_frame];
    CloseHandle(h->frameEvent[i_frame]);
    h->frameEvent[i_frame] = NULL;
#else
    while (!h->frameArray[i_frame])
        usleep(250);
    const VSFrame *vs_frame = h->frameArray[i_frame];
#endif

    if (!vs_frame)
        return -1;

    /* Prefetch more frames while keeping a bounded number in-flight */
    if (h->requestedFrames < h->framesToRequest && h->async_failed_frame < 0)
    {
        const int max_prefetch = h->parallelRequests * 4; /* soft upper bound */

        while (h->requestedFrames < h->framesToRequest && (h->requestedFrames - h->completedFrames) < max_prefetch && h->async_failed_frame < 0)
        {
            h->vsapi->getFrameAsync(h->requestedFrames, h->node, async_callback, h);
            atomic_fetch_add(&h->pendingFrames, 1);
            h->requestedFrames++;
        }
    }

    /* Copy VapourSynth frame into x264-owned buffers */
    const VSVideoFormat *fi = h->vsapi->getVideoFrameFormat(vs_frame);

    for (int p = 0; p < pic->img.planes; p++)
    {
        int plane = planes[p];

        const uint8_t *src = h->vsapi->getReadPtr(vs_frame, plane);
        int src_stride      = h->vsapi->getStride(vs_frame, plane);
        int plane_height    = h->vsapi->getFrameHeight(vs_frame, plane);

        uint8_t *dst   = pic->img.plane[p];
        int dst_stride = pic->img.stride[p];

        int row_bytes = X264_MIN(src_stride, dst_stride);
        for (int y = 0; y < plane_height; y++)
        {
            memcpy(dst + (intptr_t)y * dst_stride, src + (intptr_t)y * src_stride, row_bytes);
        }

        /* Optional up-conversion to 16-bit in-place on x264 buffers */
        if (h->uc_depth && h->bit_depth != h->desired_bit_depth)
        {
            uint16_t *dst16   = (uint16_t*)dst;
            int row_pixels    = dst_stride / fi->bytesPerSample;
            int lshift        = 16 - h->bit_depth;

            for (int y = 0; y < plane_height; y++)
            {
                uint16_t *row = dst16 + (size_t)y * row_pixels;
                for (int j = 0; j < row_pixels; j++)
                    row[j] <<= lshift;
            }
        }
    }

    /* VFR timestamps, if enabled */
    if (h->vfr)
    {
        pic->pts      = (h->current_timecode_num * h->timebase_den / h->current_timecode_den);
        pic->duration = 0;

        const VSMap *props = h->vsapi->getFramePropertiesRO(vs_frame);
        int err_num, err_den;
        int64_t duration_num = h->vsapi->mapGetInt(props, "_DurationNum", 0, &err_num);
        int64_t duration_den = h->vsapi->mapGetInt(props, "_DurationDen", 0, &err_den);
        FAIL_IF_ERROR((err_num || err_den), "missing duration at frame %d", i_frame);
        FAIL_IF_ERROR(!duration_den, "duration denominator is zero at frame %d", i_frame);
        vsh_addRational(&h->current_timecode_num, &h->current_timecode_den, duration_num, duration_den);
    }

    h->vsapi->freeFrame((VSFrame*)vs_frame);
    h->frameArray[i_frame] = NULL;
    h->nextFrame           = i_frame + 1;

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

    /* Wait for all pending async requests to complete */
    atomic_int out;
    while ((out = atomic_load(&h->pendingFrames)) != 0)
    {
        x264_cli_log("vpy ", X264_LOG_DEBUG, "waiting for %d async frame requests to complete... \r", out);
        vs_sleep();
    }

    /* Free frames that were never consumed (e.g., early stop) */
    for (int i = h->nextFrame; i < h->framesToRequest; i++)
    {
        if (h->frameArray && h->frameArray[i])
            h->vsapi->freeFrame((VSFrame*)h->frameArray[i]);
    }

#ifdef _WIN32
    if (h->frameEvent)
    {
        for (int i = 0; i < h->num_frames; i++)
            if (h->frameEvent[i])
                CloseHandle(h->frameEvent[i]);
        free(h->frameEvent);
    }
#endif

    if (h->frameArray)
        free(h->frameArray);

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

const cli_input_t vpy_input = {open_file,.picture_alloc, read_frame, release_frame, picture_clean, close_file};
