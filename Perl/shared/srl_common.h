#ifndef SRL_COMMON_H_
#define SRL_COMMON_H_

#include "srl_inline.h"
#include <string.h>

#define SRL_UNKNOWN_OPTION_POLICY_WARN   0
#define SRL_UNKNOWN_OPTION_POLICY_DIE    1
#define SRL_UNKNOWN_OPTION_POLICY_IGNORE 2

SRL_STATIC_INLINE U8
srl_parse_unknown_option_policy(pTHX_ const char *policy, STRLEN len,
                                const char *source)
{
    if (len == sizeof("warn") - 1 && memEQ(policy, "warn", len))
        return SRL_UNKNOWN_OPTION_POLICY_WARN;
    if (len == sizeof("die") - 1 && memEQ(policy, "die", len))
        return SRL_UNKNOWN_OPTION_POLICY_DIE;
    if (len == sizeof("ignore") - 1 && memEQ(policy, "ignore", len))
        return SRL_UNKNOWN_OPTION_POLICY_IGNORE;
    croak("Invalid %s; expected 'warn', 'die', or 'ignore'", source);
    return SRL_UNKNOWN_OPTION_POLICY_WARN; /* not reached */
}

SRL_STATIC_INLINE U8
srl_get_unknown_option_policy(pTHX_ SV *option_value, const char *module_policy_var)
{
    SV *value = option_value;
    STRLEN len;
    const char *policy;

    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        policy = SvPVbyte(value, len);
        return srl_parse_unknown_option_policy(aTHX_ policy, len, "unknown_option_policy");
    }

    value = get_sv(module_policy_var, 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        policy = SvPVbyte(value, len);
        return srl_parse_unknown_option_policy(aTHX_ policy, len, module_policy_var);
    }

    value = get_sv("Sereal::UnknownOptionPolicy", 0);
    if (value && SvGMAGICAL(value))
        SvGETMAGIC(value);
    if (value && SvOK(value)) {
        policy = SvPVbyte(value, len);
        return srl_parse_unknown_option_policy(aTHX_ policy, len, "$Sereal::UnknownOptionPolicy");
    }

    return SRL_UNKNOWN_OPTION_POLICY_WARN;
}

/* Constructors collect unknown names during their option loop, then resolve
 * the policy once all supplied options (including the policy itself) are read. */
SRL_STATIC_INLINE void
srl_handle_unknown_option_keys(pTHX_ AV *keys, SV *policy_value,
                                const char *module, const char *module_policy_var)
{
    U8 policy = srl_get_unknown_option_policy(aTHX_ policy_value, module_policy_var);
    if (keys && av_len(keys) >= 0 && policy != SRL_UNKNOWN_OPTION_POLICY_IGNORE) {
        SV *key_list = sv_2mortal(newSVpvs(""));
        SSize_t i;
        for (i = 0; i <= av_len(keys); i++) {
            SV **key = av_fetch(keys, i, 0);
            if (i)
                sv_catpvs(key_list, ", ");
            sv_catpvs(key_list, "'");
            sv_catsv(key_list, *key);
            sv_catpvs(key_list, "'");
        }
        if (policy == SRL_UNKNOWN_OPTION_POLICY_DIE)
            croak("Unknown option keys passed to %s: %s", module, SvPV_nolen(key_list));
        warn("Unknown option keys passed to %s: %s", module, SvPV_nolen(key_list));
    }
}

/* inspired by JSON::XS code */
#if __GNUC__ >= 3
# define expect(expr,value) __builtin_expect((expr), (value))
#else
# define expect(expr,value) (expr)
#endif

#define expect_false(expr) expect((expr) != 0, 0)
#define expect_true(expr)  expect((expr) != 0, 1)

/* these defines are somewhat borrowed from miniz.c */
#ifndef HAS_LONGDOUBLE
#define SRL_EXTENDED_PRECISION_LONG_DOUBLE 0
#endif

#ifdef HAS_QUADMATH
#define SRL_EXTENDED_PRECISION_LONG_DOUBLE 0
#endif

#define HAS_LONG_FLOAT (sizeof(NV) > sizeof(double))

#if defined(_M_IX86) || defined(_M_X64) || defined(__i386__) || defined(__i386) || defined(__i486__) || defined(__i486) || defined(i386) || defined(__ia64__) || defined(__x86_64__) || defined(__x86_64)
/* SRL_X86_OR_X64_CPU is only used to help set the below macros. */
#define SRL_X86_OR_X64_CPU
#ifndef SRL_EXTENDED_PRECISION_LONG_DOUBLE
#define SRL_EXTENDED_PRECISION_LONG_DOUBLE 1
#endif
#endif

#ifndef SRL_EXTENDED_PRECISION_LONG_DOUBLE
#define SRL_EXTENDED_PRECISION_LONG_DOUBLE 0
#endif

#ifndef SRL_USE_ALIGNED_LOADS_AND_STORES

#ifdef __hpux
/* HP-UX runs on Itanium but has strict alignment so we check it first. */
#define SRL_USE_ALIGNED_LOADS_AND_STORES 1
#elif defined(SRL_X86_OR_X64_CPU)
/* Set SRL_USE_ALIGNED_LOADS_AND_STORES to 0 on CPU's that permit efficient integer loads and stores from unaligned addresses. */
#define SRL_USE_ALIGNED_LOADS_AND_STORES 0
#else
/* When in doubt use aligned loads and stores */
#define SRL_USE_ALIGNED_LOADS_AND_STORES 1
#endif

#endif


/* In x86 one can try to enforce strict alignment in runtime.
 *
 * Setting the CPU flag bit 18 (called "AC", aligment check) in
 * the "EFLAGS" (user-settable) causes unaligned access traps but
 * only iff the system register CR0 (only system-settable, usually done
 * (or not) during kernel boot) has the same bit set (there called "AM",
 * alignment mask).  If both flags are not set, the strict alignment
 * traps (silently) do not happen.
 *
 * The Linux kernel and the Solarix x86 set the "AM".  The Windows and
 * OS X do not.  The *BSD behavior is unknown, though suspecting they do.
 *
 * http://en.wikipedia.org/wiki/Control_register
 * http://en.wikipedia.org/wiki/FLAGS_register_(computing)
 */
#ifdef SRL_X86_OR_X64_CPU
#  if defined(__x86_64__) || defined(__x86_64)
#    define SRL_TRY_ENABLE_STRICT_ALIGN() asm("pushf\norl $0x40000, (%rsp)\npopf")
#  elif defined(__i386__) || defined(__i386)
#    define SRL_TRY_ENABLE_STRICT_ALIGN() asm("pushf\norl $0x40000, (%esp)\npopf")
#  endif
#else
#  define SRL_TRY_ENABLE_STRICT_ALIGN() (void)0
#endif

/* define constant for other code to use in preallocations or buffer space
 * assertions */
#define SRL_MAX_VARINT_LENGTH 11


/* perl 5.25 op_sibling renaming related compat macros. Should probably
 * live in ppport or so. */

#ifndef OpSIBLING
# define OpSIBLING(op) ((op)->op_sibling)
#endif

#ifndef OpHAS_SIBLING
# define OpHAS_SIBLING(op) ((op)->op_sibling != NULL)
#endif

/* This is completely opting out, sigh */
#ifndef op_parent
# undef OpLASTSIB_set
# undef OpMORESIB_set
# define op_parent(op) NULL
# define OpMORESIB_set(op, sib) ((op)->op_sibling = (sib))
# define OpLASTSIB_set(op, parent) ((op)->op_sibling = NULL)
#endif

#endif
