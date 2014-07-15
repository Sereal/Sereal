#ifndef SRL_COMMON_H_
#define SRL_COMMON_H_

#include "srl_inline.h"

/* inspired by JSON::XS code */
#if __GNUC__ >= 3
# define expect(expr,value) __builtin_expect((expr), (value))
#else
# define expect(expr,value) (expr)
#endif

#define expect_false(expr) expect((expr) != 0, 0)
#define expect_true(expr)  expect((expr) != 0, 1)

/* these defines are somewhat borrowed from miniz.c */

#if defined(_M_IX86) || defined(_M_X64) || defined(__i386__) || defined(__i386) || defined(__i486__) || defined(__i486) || defined(i386) || defined(__ia64__) || defined(__x86_64__)
// SRL_X86_OR_X64_CPU is only used to help set the below macros.
#define SRL_X86_OR_X64_CPU 1
#endif

#if SRL_X86_OR_X64_CPU && !defined(SRL_USE_ALIGNED_LOADS_AND_STORES)
// Set SRL_USE_ALIGNED_LOADS_AND_STORES to 0 on CPU's that permit efficient integer loads and stores from unaligned addresses.
#define SRL_USE_ALIGNED_LOADS_AND_STORES 0
#endif

/* HP-UX runs on Itanium but has strict alignment. */
#ifdef __hpux
#undef SRL_USE_ALIGNED_LOADS_AND_STORES
#define SRL_USE_ALIGNED_LOADS_AND_STORES 1
#endif

#endif
