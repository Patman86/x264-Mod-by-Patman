/*****************************************************************************
 * vpy.c: VapourSynth input
 *****************************************************************************
 * Copyright (C) 2009-2018 x264 project
 *
 * Author: Vladimir Kontserenko <djatom@beatrice-raws.org>
 * Some portions of code and ideas taken for avs.c, y4m.c files, "ffmpeg demuxer" 
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
#include <stdatomic.h>
#include "extras/VSScript.h"
#include "extras/VSHelper.h"

#ifdef _M_IX86
#define VPY_X64 0
#else
#define VPY_X64 1
#endif

#ifdef __unix__
#include <dlfcn.h>
#include <unistd.h>
#include <ctype.h>
#define vs_sleep sleep
#define vs_strtok strtok_r
#define vs_sscanf sscanf
#ifdef __MACH__
#define vs_open() dlopen( "libvapoursynth-script.dylib", RTLD_NOW )
#else
#define vs_open() dlopen( "libvapoursynth-script.so", RTLD_NOW )
#endif
#define vs_close dlclose
#define vs_address dlsym
#else
#define vs_sleep Sleep
#define vs_strtok strtok_s
#define vs_sscanf sscanf_s
#define vs_open() LoadLibraryW( L"vsscript" )
#define vs_close FreeLibrary
#define vs_address GetProcAddress
#endif

#define DECLARE_VS_FUNC(name) func_##name name

#define LOAD_VS_FUNC(name, namex86)\
{\
    h->func.name = (void*)vs_address( h->library, (VPY_X64) ? #name : namex86 );\
    if( !h->func.name )\
        goto fail;\
}

#define FAIL_IF_ERROR( cond, ... ) FAIL_IF_ERR( cond, "vpy", __VA_ARGS__ )

typedef int (VS_CC *func_vsscript_init)(void);
typedef int (VS_CC *func_vsscript_finalize)(void);
typedef int (VS_CC *func_vsscript_evaluateFile)(VSScript **handle, const char *scriptFilename, int flags);
typedef void (VS_CC *func_vsscript_freeScript)(VSScript *handle);
typedef const char * (VS_CC *func_vsscript_getError)(VSScript *handle);
typedef VSNodeRef * (VS_CC *func_vsscript_getOutput)(VSScript *handle, int index);
typedef VSCore * (VS_CC *func_vsscript_getCore)(VSScript *handle);
typedef const VSAPI * (VS_CC *func_vsscript_getVSApi2)(int version);

typedef struct VapourSynthContext {
    void *library;
    const VSAPI *vsapi;
    VSScript *script;
    VSNodeRef *node;
    int curr_frame;
    int ncpu;
    atomic_int async_pending;
    int num_frames;
    int bit_depth;
    uint64_t plane_size[3];
    int uc_depth;
    struct 
    {
        DECLARE_VS_FUNC( vsscript_init );
        DECLARE_VS_FUNC( vsscript_finalize );
        DECLARE_VS_FUNC( vsscript_evaluateFile );
        DECLARE_VS_FUNC( vsscript_freeScript );
        DECLARE_VS_FUNC( vsscript_getError );
        DECLARE_VS_FUNC( vsscript_getOutput );
        DECLARE_VS_FUNC( vsscript_getCore );
        DECLARE_VS_FUNC( vsscript_getVSApi2 );
    } func;
} VapourSynthContext;

static int custom_vs_load_library( VapourSynthContext *h )
{
    h->library = vs_open();
    if( !h->library )
        return -1;
    LOAD_VS_FUNC( vsscript_init, "_vsscript_init@0" );
    LOAD_VS_FUNC( vsscript_finalize, "_vsscript_finalize@0" );
    LOAD_VS_FUNC( vsscript_evaluateFile, "_vsscript_evaluateFile@12" );
    LOAD_VS_FUNC( vsscript_freeScript, "_vsscript_freeScript@4" );
    LOAD_VS_FUNC( vsscript_getError, "_vsscript_getError@4" );
    LOAD_VS_FUNC( vsscript_getOutput, "_vsscript_getOutput@8" );
    LOAD_VS_FUNC( vsscript_getCore, "_vsscript_getCore@4" );
    LOAD_VS_FUNC( vsscript_getVSApi2, "_vsscript_getVSApi2@4" );
    return 0;
fail:
    vs_close( h->library );
    h->library = NULL;
    return -1;
}

static void VS_CC async_callback( void *user_data, const VSFrameRef *f, int n, VSNodeRef *node, const char *error_msg )
{
    VapourSynthContext *h = user_data;

    if (!f) {
        x264_cli_log( "vpy", X264_LOG_WARNING, "async frame request failed: %s\n", error_msg );
    }

    h->vsapi->freeFrame( f );
    atomic_fetch_sub( &h->async_pending, 1 );
}

/* slightly modified rigaya's VersionString parser */
int get_core_revision(const char *vsVersionString) 
{
    char *api_info = NULL;
    char buf[1024];
    strcpy( buf, vsVersionString );
    for ( char *p = buf, *q = NULL, *r = NULL; NULL != ( q = vs_strtok( p, "\n", &r ) ); ) {
        if ( NULL != ( api_info = strstr( q, "Core" ) ) ) {
            strcpy( buf, api_info );
            for ( char *s = buf; *s; s++ )
                *s = (char)tolower( *s );
            int rev = 0;
            return ( 1 == vs_sscanf( buf, "core r%d", &rev ) ) ? rev : 0;
        }
        p = NULL;
    }
    return 0;
}

static int open_file( char *psz_filename, hnd_t *p_handle, video_info_t *info, cli_input_opt_t *opt )
{
    FILE *fh = x264_fopen( psz_filename, "r" );
    if( !fh )
        return -1;
    int b_regular = x264_is_regular_file( fh );
    fclose( fh );
    FAIL_IF_ERROR( !b_regular, "VPY input is incompatible with non-regular file `%s'\n", psz_filename );

    VapourSynthContext *h = calloc( 1, sizeof(VapourSynthContext) );
    if( !h )
        return -1;
    FAIL_IF_ERROR( custom_vs_load_library( h ), "failed to load VapourSynth\n" );
    if( !h->func.vsscript_init() )
        FAIL_IF_ERROR( 1, "failed to initialize VapourSynth environment\n" );
    h->vsapi = h->func.vsscript_getVSApi2( VAPOURSYNTH_API_VERSION );
    FAIL_IF_ERROR( !h->vsapi, "failed to get vapoursynth API\n" );
    if( h->func.vsscript_evaluateFile( &h->script, (const char *)psz_filename, efSetWorkingDir ) )
        FAIL_IF_ERROR( 1, "Can't evaluate script: %s\n",  h->func.vsscript_getError( h->script ) );
    h->node = h->func.vsscript_getOutput( h->script, 0 );
    FAIL_IF_ERROR( !h->node, "`%s' has no video data\n", psz_filename );

    const VSCoreInfo *core_info = h->vsapi->getCoreInfo( h->func.vsscript_getCore( h->script ) );
    const VSVideoInfo *vi = h->vsapi->getVideoInfo( h->node );
    FAIL_IF_ERROR( !isConstantFormat(vi), "only constant video formats are supported\n" );
    x264_cli_log( "vpy", X264_LOG_INFO, "VapourSynth Core R%d\n", get_core_revision( core_info->versionString ) );
    info->width = vi->width;
    info->height = vi->height;
    info->fps_num = vi->fpsNum;
    info->fps_den = vi->fpsDen;
    h->num_frames = info->num_frames = vi->numFrames;
    h->bit_depth = vi->format->bitsPerSample;
    info->thread_safe = 1;
    h->ncpu = core_info->numThreads;
    h->uc_depth = (h->bit_depth & 7) && (vi->format->colorFamily == cmYUV || vi->format->colorFamily == cmYCoCg);

    if( vi->format->id == pfRGB48 )
        info->csp = X264_CSP_BGR | X264_CSP_VFLIP | X264_CSP_HIGH_DEPTH;
    else if( vi->format->id == pfRGB24 )
        info->csp = X264_CSP_BGR | X264_CSP_VFLIP;
    else if( vi->format->id == pfYUV444P9 || vi->format->id == pfYUV444P10 || vi->format->id == pfYUV444P12 || vi->format->id == pfYUV444P14 || vi->format->id == pfYUV444P16)
        info->csp = X264_CSP_I444 | X264_CSP_HIGH_DEPTH;
    else if( vi->format->id == pfYUV422P9 || vi->format->id == pfYUV422P10 || vi->format->id == pfYUV422P12 || vi->format->id == pfYUV422P14 || vi->format->id == pfYUV422P16)
        info->csp = X264_CSP_I422 | X264_CSP_HIGH_DEPTH;
    else if( vi->format->id == pfYUV420P9 || vi->format->id == pfYUV420P10 || vi->format->id == pfYUV420P12 || vi->format->id == pfYUV420P14 || vi->format->id == pfYUV420P16)
        info->csp = X264_CSP_I420 | X264_CSP_HIGH_DEPTH;
    else if( vi->format->id == pfYUV444P8 )
        info->csp = X264_CSP_I444;
    else if( vi->format->id == pfYUV422P8 )
        info->csp = X264_CSP_I422;
    else if( vi->format->id == pfYUV420P8 )
        info->csp = X264_CSP_I420;
    else
        FAIL_IF_ERROR( 1, "not supported pixel type: %s\n", vi->format->name );

    /* since VapourSynth supports vfr internally, it would be great to implement handling for it someday */
    info->vfr = 0;

    /* bitdepth upconversion stuff */
    if( h->uc_depth ) {
        const x264_cli_csp_t *cli_csp = x264_cli_get_csp( info->csp );
        for( int i = 0; i < cli_csp->planes; i++ ) {
            h->plane_size[i] = x264_cli_pic_plane_size( info->csp, info->width, info->height, i );
            h->plane_size[i] /= x264_cli_csp_depth_factor( info->csp );
        }
    }

    *p_handle = h;

    return 0;
}

static int picture_alloc( cli_pic_t *pic, hnd_t handle, int csp, int width, int height )
{
    if( x264_cli_pic_alloc( pic, X264_CSP_NONE, width, height ) )
        return -1;
    pic->img.csp = csp;
    const x264_cli_csp_t *cli_csp = x264_cli_get_csp( csp );
    if( cli_csp )
        pic->img.planes = cli_csp->planes;
    return 0;
}

static int read_frame( cli_pic_t *pic, hnd_t handle, int i_frame )
{
    static const int planes[3] = { 0, 1, 2 };
    char errbuf[256];
    const VSFrameRef *frm = NULL;
    VapourSynthContext *h = handle;

    if( i_frame >= h->num_frames )
        return -1;

    /* explicitly cast away the const attribute to avoid a warning */
    frm = pic->opaque = (VSFrameRef*)h->vsapi->getFrame( i_frame, h->node, errbuf, sizeof(errbuf) );
    FAIL_IF_ERROR( !frm, "%s occurred while reading frame %d\n", errbuf, i_frame );

    /* Prefetch the subsequent frames. */
    for ( int i = 0; i < h->ncpu; ++i ) {
        if ( i >= h->num_frames - i_frame )
            break;
        h->vsapi->getFrameAsync( i_frame + i, h->node, async_callback, h );
        atomic_fetch_add( &h->async_pending, 1 );
    }

    for( int i = 0; i < pic->img.planes; i++ ) {
        /* explicitly cast away the const attribute to avoid a warning */
        pic->img.plane[i] = (uint8_t*)h->vsapi->getReadPtr( frm, planes[i] );
        pic->img.stride[i] = h->vsapi->getStride( frm, planes[i] );
        if( h->uc_depth ) {
            /* upconvert non 16bit high depth planes to 16bit using the same
             * algorithm as used in the depth filter. */
            uint16_t *plane = (uint16_t*)pic->img.plane[i];
            uint64_t pixel_count = h->plane_size[i];
            int lshift = 16 - h->bit_depth;
            for( uint64_t j = 0; j < pixel_count; j++ )
                plane[j] = plane[j] << lshift;
        }
    }
    return 0;
}

static int release_frame( cli_pic_t *pic, hnd_t handle )
{
    VapourSynthContext *h = handle;
    h->vsapi->freeFrame( pic->opaque );
    return 0;
}

static void picture_clean( cli_pic_t *pic, hnd_t handle )
{
    memset( pic, 0, sizeof(cli_pic_t) );
}

static int close_file( hnd_t handle )
{
    VapourSynthContext *h = handle;

    /* Wait for any async requests to complete. */
    while ( atomic_load( &h->async_pending ) ) {
        vs_sleep( 1 );
    }

    h->vsapi->freeNode( h->node );
    h->func.vsscript_freeScript( h->script );
    h->func.vsscript_finalize();

    if( h->library )
        vs_close( h->library );

    free( h );
    return 0;
}

const cli_input_t vpy_input = { open_file, picture_alloc, read_frame, release_frame, picture_clean, close_file };