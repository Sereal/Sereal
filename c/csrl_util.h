#ifndef CSRL_UTIL_H_
#define CSRL_UTIL_H_

/* Private utilities / macros */
#include <csrl_defines.h>

#ifndef UNUSED_VAR
# define UNUSED_VAR(x) ((void)x)
#endif

#ifndef FALSE
# define FALSE 0
#endif
#ifndef TRUE
# define TRUE (!FALSE)
#endif

#if __GNUC__ >= 3
# define expect(expr,value) __builtin_expect((expr), (value))
#else
# define expect(expr,value) (expr)
#endif

#define expect_false(expr) expect((expr) != 0, 0)
#define expect_true(expr)  expect((expr) != 0, 1)

#ifndef STATIC_INLINE
# ifdef NOINLINE
#   define STATIC_INLINE static
# elif defined(_MSC_VER)
#   define STATIC_INLINE static __inline
# else
#   define STATIC_INLINE static inline
# endif
#endif

#endif
