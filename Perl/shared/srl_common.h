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

#endif
