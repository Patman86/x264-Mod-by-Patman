/*****************************************************************************
 * cpu.c: cpu detection
 *****************************************************************************
 * Copyright (C) 2003-2025 x264 project
 *
 * Authors: Loren Merritt <lorenm@u.washington.edu>
 *          Laurent Aimar <fenrir@via.ecp.fr>
 *          Fiona Glaser <fiona@x264.com>
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

#include "base.h"

#if HAVE_GETAUXVAL || HAVE_ELF_AUX_INFO
#include <sys/auxv.h>
#endif
#if HAVE_SYSCONF
#include <unistd.h>
#endif
#if SYS_LINUX
#include <sched.h>
#endif
#if SYS_BEOS
#include <kernel/OS.h>
#endif
#if SYS_MACOSX || SYS_FREEBSD || SYS_NETBSD || SYS_OPENBSD
#include <sys/types.h>
#include <sys/sysctl.h>
#endif
#if SYS_OPENBSD
#include <machine/cpu.h>
#endif

const x264_cpu_name_t x264_cpu_names[] =
{
#if ARCH_X86 || ARCH_X86_64
//  {"MMX",         X264_CPU_MMX},  // we don't support asm on mmx1 cpus anymore
#define MMX2 X264_CPU_MMX|X264_CPU_MMX2
    {"MMX2",        MMX2},
    {"MMXEXT",      MMX2},
    {"SSE",         MMX2|X264_CPU_SSE},
#define SSE2 MMX2|X264_CPU_SSE|X264_CPU_SSE2
    {"SSE2Slow",    SSE2|X264_CPU_SSE2_IS_SLOW},
    {"SSE2",        SSE2},
    {"SSE2Fast",    SSE2|X264_CPU_SSE2_IS_FAST},
    {"LZCNT",       SSE2|X264_CPU_LZCNT},
    {"SSE3",        SSE2|X264_CPU_SSE3},
    {"SSSE3",       SSE2|X264_CPU_SSE3|X264_CPU_SSSE3},
    {"SSE4.1",      SSE2|X264_CPU_SSE3|X264_CPU_SSSE3|X264_CPU_SSE4},
    {"SSE4",        SSE2|X264_CPU_SSE3|X264_CPU_SSSE3|X264_CPU_SSE4},
    {"SSE4.2",      SSE2|X264_CPU_SSE3|X264_CPU_SSSE3|X264_CPU_SSE4|X264_CPU_SSE42},
#define AVX SSE2|X264_CPU_SSE3|X264_CPU_SSSE3|X264_CPU_SSE4|X264_CPU_SSE42|X264_CPU_AVX
    {"AVX",         AVX},
    {"XOP",         AVX|X264_CPU_XOP},
    {"FMA4",        AVX|X264_CPU_FMA4},
    {"FMA3",        AVX|X264_CPU_FMA3},
    {"BMI1",        AVX|X264_CPU_LZCNT|X264_CPU_BMI1},
    {"BMI2",        AVX|X264_CPU_LZCNT|X264_CPU_BMI1|X264_CPU_BMI2},
#define AVX2 AVX|X264_CPU_FMA3|X264_CPU_LZCNT|X264_CPU_BMI1|X264_CPU_BMI2|X264_CPU_AVX2
    {"AVX2",        AVX2},
    {"AVX512",      AVX2|X264_CPU_AVX512},
#undef AVX2
#undef AVX
#undef SSE2
#undef MMX2
    {"Cache32",         X264_CPU_CACHELINE_32},
    {"Cache64",         X264_CPU_CACHELINE_64},
    {"SlowAtom",        X264_CPU_SLOW_ATOM},
    {"SlowPshufb",      X264_CPU_SLOW_PSHUFB},
    {"SlowPalignr",     X264_CPU_SLOW_PALIGNR},
    {"SlowShuffle",     X264_CPU_SLOW_SHUFFLE},
    {"UnalignedStack",  X264_CPU_STACK_MOD4},
#elif ARCH_PPC
    {"Altivec",         X264_CPU_ALTIVEC},
#elif ARCH_ARM
    {"ARMv6",           X264_CPU_ARMV6},
    {"NEON",            X264_CPU_NEON},
    {"FastNeonMRC",     X264_CPU_FAST_NEON_MRC},
#elif ARCH_AARCH64
    {"ARMv8",           X264_CPU_ARMV8},
    {"NEON",            X264_CPU_NEON},
    {"DotProd",         X264_CPU_DOTPROD},
    {"I8MM",            X264_CPU_I8MM},
    {"SVE",             X264_CPU_SVE},
    {"SVE2",            X264_CPU_SVE2},
#elif ARCH_MIPS
    {"MSA",             X264_CPU_MSA},
#elif ARCH_LOONGARCH
    {"LSX",             X264_CPU_LSX},
    {"LASX",            X264_CPU_LASX},
#endif
    {"", 0},
};

static unsigned long x264_getauxval( unsigned long type )
{
#if HAVE_GETAUXVAL
    return getauxval( type );
#elif HAVE_ELF_AUX_INFO
    unsigned long aux = 0;
    elf_aux_info( type, &aux, sizeof(aux) );
    return aux;
#else
    return 0;
#endif
}

#if ((HAVE_ALTIVEC && SYS_LINUX) || (HAVE_ARMV6 && !HAVE_NEON)) && !(HAVE_GETAUXVAL || HAVE_ELF_AUX_INFO)
#include <signal.h>
#include <setjmp.h>
static sigjmp_buf jmpbuf;
static volatile sig_atomic_t canjump = 0;

static void sigill_handler( int sig )
{
    if( !canjump )
    {
        signal( sig, SIG_DFL );
        raise( sig );
    }

    canjump = 0;
    siglongjmp( jmpbuf, 1 );
}
#endif

#if HAVE_MMX
int x264_cpu_cpuid_test( void );
void x264_cpu_cpuid( uint32_t op, uint32_t *eax, uint32_t *ebx, uint32_t *ecx, uint32_t *edx );
uint64_t x264_cpu_xgetbv( int xcr );

uint32_t x264_cpu_detect( void )
{
    uint32_t cpu = 0;
    uint32_t eax, ebx, ecx, edx;
    uint32_t vendor[4] = {0};
    uint32_t max_extended_cap, max_basic_cap;

#if !ARCH_X86_64
    if( !x264_cpu_cpuid_test() )
        return 0;
#endif

    x264_cpu_cpuid( 0, &max_basic_cap, vendor+0, vendor+2, vendor+1 );
    if( max_basic_cap == 0 )
        return 0;

    x264_cpu_cpuid( 1, &eax, &ebx, &ecx, &edx );
    if( edx&0x00800000 )
        cpu |= X264_CPU_MMX;
    else
        return cpu;
    if( edx&0x02000000 )
        cpu |= X264_CPU_MMX2|X264_CPU_SSE;
    if( edx&0x04000000 )
        cpu |= X264_CPU_SSE2;
    if( ecx&0x00000001 )
        cpu |= X264_CPU_SSE3;
    if( ecx&0x00000200 )
        cpu |= X264_CPU_SSSE3|X264_CPU_SSE2_IS_FAST;
    if( ecx&0x00080000 )
        cpu |= X264_CPU_SSE4;
    if( ecx&0x00100000 )
        cpu |= X264_CPU_SSE42;

    if( ecx&0x08000000 ) /* XGETBV supported and XSAVE enabled by OS */
    {
        uint64_t xcr0 = x264_cpu_xgetbv( 0 );
        if( (xcr0&0x6) == 0x6 ) /* XMM/YMM state */
        {
            if( ecx&0x10000000 )
                cpu |= X264_CPU_AVX;
            if( ecx&0x00001000 )
                cpu |= X264_CPU_FMA3;

            if( max_basic_cap >= 7 )
            {
                x264_cpu_cpuid( 7, &eax, &ebx, &ecx, &edx );
                if( ebx&0x00000008 )
                    cpu |= X264_CPU_BMI1;
                if( ebx&0x00000100 )
                    cpu |= X264_CPU_BMI2;
                if( ebx&0x00000020 )
                    cpu |= X264_CPU_AVX2;

                if( (xcr0&0xE0) == 0xE0 ) /* OPMASK/ZMM state */
                {
                    if( (ebx&0xD0030000) == 0xD0030000 )
                        cpu |= X264_CPU_AVX512;
                }
            }
        }
    }

    x264_cpu_cpuid( 0x80000000, &eax, &ebx, &ecx, &edx );
    max_extended_cap = eax;

    if( max_extended_cap >= 0x80000001 )
    {
        x264_cpu_cpuid( 0x80000001, &eax, &ebx, &ecx, &edx );

        if( ecx&0x00000020 )
            cpu |= X264_CPU_LZCNT;             /* Supported by Intel chips starting with Haswell */
        if( ecx&0x00000040 ) /* SSE4a, AMD only */
        {
            int family = ((eax>>8)&0xf) + ((eax>>20)&0xff);
            cpu |= X264_CPU_SSE2_IS_FAST;      /* Phenom and later CPUs have fast SSE units */
            if( family == 0x14 )
            {
                cpu &= ~X264_CPU_SSE2_IS_FAST; /* SSSE3 doesn't imply fast SSE anymore... */
                cpu |= X264_CPU_SSE2_IS_SLOW;  /* Bobcat has 64-bit SIMD units */
                cpu |= X264_CPU_SLOW_PALIGNR;  /* palignr is insanely slow on Bobcat */
            }
            if( family == 0x16 )
            {
                cpu |= X264_CPU_SLOW_PSHUFB;   /* Jaguar's pshufb isn't that slow, but it's slow enough
                                                * compared to alternate instruction sequences that this
                                                * is equal or faster on almost all such functions. */
            }
        }

        if( cpu & X264_CPU_AVX )
        {
            if( ecx&0x00000800 ) /* XOP */
                cpu |= X264_CPU_XOP;
            if( ecx&0x00010000 ) /* FMA4 */
                cpu |= X264_CPU_FMA4;
        }

        if( !strcmp((char*)vendor, "AuthenticAMD") )
        {
            if( edx&0x00400000 )
                cpu |= X264_CPU_MMX2;
            if( (cpu&X264_CPU_SSE2) && !(cpu&X264_CPU_SSE2_IS_FAST) )
                cpu |= X264_CPU_SSE2_IS_SLOW; /* AMD CPUs come in two types: terrible at SSE and great at it */
        }
    }

    if( !strcmp((char*)vendor, "GenuineIntel") )
    {
        x264_cpu_cpuid( 1, &eax, &ebx, &ecx, &edx );
        int family = ((eax>>8)&0xf) + ((eax>>20)&0xff);
        int model  = ((eax>>4)&0xf) + ((eax>>12)&0xf0);
        if( family == 6 )
        {
            /* Detect Atom CPU */
            if( model == 28 )
            {
                cpu |= X264_CPU_SLOW_ATOM;
                cpu |= X264_CPU_SLOW_PSHUFB;
            }
            /* Conroe has a slow shuffle unit. Check the model number to make sure not
             * to include crippled low-end Penryns and Nehalems that don't have SSE4. */
            else if( (cpu&X264_CPU_SSSE3) && !(cpu&X264_CPU_SSE4) && model < 23 )
                cpu |= X264_CPU_SLOW_SHUFFLE;
        }
    }

    if( (!strcmp((char*)vendor, "GenuineIntel") || !strcmp((char*)vendor, "CyrixInstead")) && !(cpu&X264_CPU_SSE42))
    {
        /* cacheline size is specified in 3 places, any of which may be missing */
        x264_cpu_cpuid( 1, &eax, &ebx, &ecx, &edx );
        int cache = (ebx&0xff00)>>5; // cflush size
        if( !cache && max_extended_cap >= 0x80000006 )
        {
            x264_cpu_cpuid( 0x80000006, &eax, &ebx, &ecx, &edx );
            cache = ecx&0xff; // cacheline size
        }
        if( !cache && max_basic_cap >= 2 )
        {
            // Cache and TLB Information
            static const char cache32_ids[] = { 0x0a, 0x0c, 0x41, 0x42, 0x43, 0x44, 0x45, 0x82, 0x83, 0x84, 0x85, 0 };
            static const char cache64_ids[] = { 0x22, 0x23, 0x25, 0x29, 0x2c, 0x46, 0x47, 0x49, 0x60, 0x66, 0x67,
                                                0x68, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7c, 0x7f, 0x86, 0x87, 0 };
            uint32_t buf[4];
            int max, i = 0;
            do {
                x264_cpu_cpuid( 2, buf+0, buf+1, buf+2, buf+3 );
                max = buf[0]&0xff;
                buf[0] &= ~0xff;
                for( int j = 0; j < 4; j++ )
                    if( !(buf[j]>>31) )
                        while( buf[j] )
                        {
                            if( strchr( cache32_ids, buf[j]&0xff ) )
                                cache = 32;
                            if( strchr( cache64_ids, buf[j]&0xff ) )
                                cache = 64;
                            buf[j] >>= 8;
                        }
            } while( ++i < max );
        }

        if( cache == 32 )
            cpu |= X264_CPU_CACHELINE_32;
        else if( cache == 64 )
            cpu |= X264_CPU_CACHELINE_64;
        else
            x264_log_internal( X264_LOG_WARNING, "unable to determine cacheline size\n" );
    }

#if STACK_ALIGNMENT < 16
    cpu |= X264_CPU_STACK_MOD4;
#endif

    return cpu;
}

#elif HAVE_ALTIVEC

#if HAVE_GETAUXVAL || HAVE_ELF_AUX_INFO

#define HWCAP_PPC_ALTIVEC   (1U << 28)

uint32_t x264_cpu_detect( void )
{
    uint32_t flags = 0;

    unsigned long hwcap = x264_getauxval( AT_HWCAP );

    if ( hwcap & HWCAP_PPC_ALTIVEC )
        flags |= X264_CPU_ALTIVEC;

    return flags;
}

#elif SYS_MACOSX || SYS_FREEBSD || SYS_NETBSD || SYS_OPENBSD

uint32_t x264_cpu_detect( void )
{
    /* Thank you VLC */
    uint32_t cpu = 0;
#if SYS_OPENBSD
    int      selectors[2] = { CTL_MACHDEP, CPU_ALTIVEC };
#elif SYS_MACOSX
    int      selectors[2] = { CTL_HW, HW_VECTORUNIT };
#endif
    int      has_altivec = 0;
    size_t   length = sizeof( has_altivec );
#if SYS_MACOSX || SYS_OPENBSD
    int      error = sysctl( selectors, 2, &has_altivec, &length, NULL, 0 );
#elif SYS_NETBSD
    int      error = sysctlbyname( "machdep.altivec", &has_altivec, &length, NULL, 0 );
#else
    int      error = sysctlbyname( "hw.altivec", &has_altivec, &length, NULL, 0 );
#endif

    if( error == 0 && has_altivec != 0 )
        cpu |= X264_CPU_ALTIVEC;

    return cpu;
}

#elif SYS_LINUX

uint32_t x264_cpu_detect( void )
{
#ifdef __NO_FPRS__
    return 0;
#else
    static void (*oldsig)( int );

    oldsig = signal( SIGILL, sigill_handler );
    if( sigsetjmp( jmpbuf, 1 ) )
    {
        signal( SIGILL, oldsig );
        return 0;
    }

    canjump = 1;
    asm volatile( "mtspr 256, %0\n\t"
                  "vand 0, 0, 0\n\t"
                  :
                  : "r"(-1) );
    canjump = 0;

    signal( SIGILL, oldsig );

    return X264_CPU_ALTIVEC;
#endif
}

#else

uint32_t x264_cpu_detect( void )
{
    return 0;
}

#endif

#elif HAVE_ARMV6

void x264_cpu_neon_test( void );
int x264_cpu_fast_neon_mrc_test( void );

#define HWCAP_ARM_NEON   (1U << 12)

uint32_t x264_cpu_detect( void )
{
    uint32_t flags = 0;
    flags |= X264_CPU_ARMV6;

#if HAVE_GETAUXVAL || HAVE_ELF_AUX_INFO
    unsigned long hwcap = x264_getauxval( AT_HWCAP );

    if ( hwcap & HWCAP_ARM_NEON )
        flags |= X264_CPU_NEON;
#else
    // don't do this hack if compiled with -mfpu=neon
#if !HAVE_NEON
    static void (* oldsig)( int );
    oldsig = signal( SIGILL, sigill_handler );
    if( sigsetjmp( jmpbuf, 1 ) )
    {
        signal( SIGILL, oldsig );
        return flags;
    }

    canjump = 1;
    x264_cpu_neon_test();
    canjump = 0;
    signal( SIGILL, oldsig );
#endif

    flags |= X264_CPU_NEON;
#endif

    // fast neon -> arm (Cortex-A9) detection relies on user access to the
    // cycle counter; this assumes ARMv7 performance counters.
    // NEON requires at least ARMv7, ARMv8 may require changes here, but
    // hopefully this hacky detection method will have been replaced by then.
    // Note that there is potential for a race condition if another program or
    // x264 instance disables or reinits the counters while x264 is using them,
    // which may result in incorrect detection and the counters stuck enabled.
    // right now Apple does not seem to support performance counters for this test
#ifndef __MACH__
    flags |= x264_cpu_fast_neon_mrc_test() ? X264_CPU_FAST_NEON_MRC : 0;
#endif
    // TODO: write dual issue test? currently it's A8 (dual issue) vs. A9 (fast mrc)
    return flags;
}

#elif HAVE_AARCH64

#if defined(__linux__) || HAVE_ELF_AUX_INFO

#define HWCAP_AARCH64_ASIMDDP (1U << 20)
#define HWCAP_AARCH64_SVE     (1U << 22)
#define HWCAP2_AARCH64_SVE2   (1U << 1)
#define HWCAP2_AARCH64_I8MM   (1U << 13)

static uint32_t detect_flags( void )
{
    uint32_t flags = 0;

    unsigned long hwcap = x264_getauxval( AT_HWCAP );
    unsigned long hwcap2 = x264_getauxval( AT_HWCAP2 );

    if ( hwcap & HWCAP_AARCH64_ASIMDDP )
        flags |= X264_CPU_DOTPROD;
    if ( hwcap2 & HWCAP2_AARCH64_I8MM )
        flags |= X264_CPU_I8MM;
    if ( hwcap & HWCAP_AARCH64_SVE )
        flags |= X264_CPU_SVE;
    if ( hwcap2 & HWCAP2_AARCH64_SVE2 )
        flags |= X264_CPU_SVE2;

    return flags;
}

#elif defined(__APPLE__)
#include <sys/sysctl.h>

static int have_feature( const char *feature )
{
    int supported = 0;
    size_t size = sizeof(supported);
    if ( sysctlbyname( feature, &supported, &size, NULL, 0 ) )
        return 0;
    return supported;
}

static uint32_t detect_flags( void )
{
    uint32_t flags = 0;

    if ( have_feature( "hw.optional.arm.FEAT_DotProd" ) )
        flags |= X264_CPU_DOTPROD;
    if ( have_feature( "hw.optional.arm.FEAT_I8MM" ) )
        flags |= X264_CPU_I8MM;
    /* No SVE and SVE2 feature detection available on Apple platforms. */
    return flags;
}

#elif defined(_WIN32)
#include <windows.h>

static uint32_t detect_flags( void )
{
    uint32_t flags = 0;

#ifdef PF_ARM_V82_DP_INSTRUCTIONS_AVAILABLE
    if ( IsProcessorFeaturePresent( PF_ARM_V82_DP_INSTRUCTIONS_AVAILABLE ) )
        flags |= X264_CPU_DOTPROD;
#endif
#ifdef PF_ARM_SVE_INSTRUCTIONS_AVAILABLE
    if ( IsProcessorFeaturePresent( PF_ARM_SVE_INSTRUCTIONS_AVAILABLE ) )
        flags |= X264_CPU_SVE;
#endif
#ifdef PF_ARM_SVE2_INSTRUCTIONS_AVAILABLE
    if ( IsProcessorFeaturePresent( PF_ARM_SVE2_INSTRUCTIONS_AVAILABLE ) )
        flags |= X264_CPU_SVE2;
#endif
#ifdef PF_ARM_SVE_I8MM_INSTRUCTIONS_AVAILABLE
    /* There's no PF_* flag that indicates whether plain I8MM is available
     * or not. But if SVE_I8MM is available, that also implies that
     * regular I8MM is available. */
    if ( IsProcessorFeaturePresent( PF_ARM_SVE_I8MM_INSTRUCTIONS_AVAILABLE ) )
        flags |= X264_CPU_I8MM;
#endif
    return flags;
}

#endif

uint32_t x264_cpu_detect( void )
{
    uint32_t flags = X264_CPU_ARMV8;
#if HAVE_NEON
    flags |= X264_CPU_NEON;
#endif

    // If these features are enabled unconditionally in the compiler, we can
    // assume that they are available.
#ifdef __ARM_FEATURE_DOTPROD
    flags |= X264_CPU_DOTPROD;
#endif
#ifdef __ARM_FEATURE_MATMUL_INT8
    flags |= X264_CPU_I8MM;
#endif
#ifdef __ARM_FEATURE_SVE
    flags |= X264_CPU_SVE;
#endif
#ifdef __ARM_FEATURE_SVE2
    flags |= X264_CPU_SVE2;
#endif

    // Where possible, try to do runtime detection as well.
#if defined(__linux__) || HAVE_ELF_AUX_INFO || \
    defined(__APPLE__) || defined(_WIN32)
    flags |= detect_flags();
#endif

    return flags;
}

#elif HAVE_MSA

uint32_t x264_cpu_detect( void )
{
    uint32_t flags = 0;

# ifdef __linux__
    FILE* fp = fopen("/proc/cpuinfo", "r");
    if (!fp)
        return flags;

    char buf[200];
    memset(buf, 0, sizeof(buf));
    while (fgets(buf, sizeof(buf), fp)) {
        if(!strncmp(buf, "ASEs implemented", strlen("ASEs implemented"))) {
            if (strstr(buf, "msa")) {
                flags |= X264_CPU_MSA;
            }
            break;
        }
    }
    fclose(fp);
# else
    flags |= X264_CPU_MSA;
# endif

    return flags;
}

#elif HAVE_LSX

#define LA_HWCAP_LSX    ( 1U << 4 )
#define LA_HWCAP_LASX   ( 1U << 5 )

uint32_t x264_cpu_detect( void )
{
    uint32_t flags = 0;
    uint32_t hwcap = (uint32_t)x264_getauxval( AT_HWCAP );

    if( hwcap & LA_HWCAP_LSX )
        flags |= X264_CPU_LSX;
    if( hwcap & LA_HWCAP_LASX )
        flags |= X264_CPU_LASX;

    return flags;
}

#else

uint32_t x264_cpu_detect( void )
{
    return 0;
}

#endif

int x264_cpu_num_processors( void )
{
#if !HAVE_THREAD
    return 1;

#elif SYS_WINDOWS
    return x264_pthread_num_processors_np();

#elif SYS_LINUX
    cpu_set_t p_aff;
    memset( &p_aff, 0, sizeof(p_aff) );
    if( sched_getaffinity( 0, sizeof(p_aff), &p_aff ) )
        return 1;
#if HAVE_CPU_COUNT
    return CPU_COUNT(&p_aff);
#else
    int np = 0;
    for( size_t bit = 0; bit < 8 * sizeof(p_aff); bit++ )
        np += (((uint8_t *)&p_aff)[bit / 8] >> (bit % 8)) & 1;
    return np;
#endif

#elif SYS_BEOS
    system_info info;
    get_system_info( &info );
    return info.cpu_count;

#elif SYS_MACOSX
    int ncpu;
    size_t length = sizeof( ncpu );
    if( sysctlbyname("hw.logicalcpu", &ncpu, &length, NULL, 0) )
    {
        ncpu = 1;
    }
    return ncpu;

#elif defined(_SC_NPROCESSORS_ONLN)
    return sysconf( _SC_NPROCESSORS_ONLN );

#elif defined(_SC_NPROCESSORS_CONF)
    return sysconf( _SC_NPROCESSORS_CONF );

#else
    return 1;
#endif
}
