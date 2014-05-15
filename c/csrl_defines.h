#ifndef CSRL_DEFINES_H_
#define CSRL_DEFINES_H_

/* Defines required in public CSereal API such as types. */

/* TODO obviously, these types would normally sanely be detected and
 * defined via a configure script of sorts. */

#include <limits.h>
#include <stdint.h>

#ifndef U8
# define U8 unsigned char
#endif
#ifndef I8
# define I8 char
#endif
#ifndef I32
# define I32 int32_t
#endif
#ifndef U32
# define U32 uint32_t
#endif
#ifndef I64
# define I64 int64_t
#endif
#ifndef U64
# define U64 uint64_t
#endif

#ifndef IV
# define IV intptr_t
#endif
#ifndef UV
# define UV uintptr_t
#endif

#ifndef INT2PTR
#  define INT2PTR(any,d)	(any)(intptr_t)(d)
#endif
#define PTR2IV(p)	INT2PTR(IV,p)
#define PTR2UV(p)	INT2PTR(UV,p)

#ifndef INLINE
# ifdef NOINLINE
#   define INLINE
# elif defined(_MSC_VER)
#   define INLINE __inline
# else
#   define INLINE inline
# endif
#endif


/*
 * STMT_START { statements; } STMT_END;
 * can be used as a single statement, as in
 * if (x) STMT_START { ... } STMT_END; else ...
 */
#if !(defined(STMT_START) && defined(STMT_END))
# ifdef USE_GCC_BRACE_GROUPS
#   define STMT_START	(void)(	/* gcc supports "({ STATEMENTS; })" */
#   define STMT_END	)
# else
#   define STMT_START	do
#   define STMT_END	while (0)
# endif
#endif

#endif
