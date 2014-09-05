#ifndef SRL_PARSER_STACK_H_
#define SRL_PARSER_STACK_H_

#include <stdlib.h>

#include "assert.h"
#include "srl_inline.h"
#include "srl_common.h"

#ifndef NDEBUG
#define DEBUG_ASSERT_STACK_SANE(stack) STMT_START { \
    assert(stack != NULL);                          \
    assert(stack->idx >= -1);                       \
    assert(stack->idx < stack->cap);               \
} STMT_END
#else
#define DEBUG_ASSERT_STACK_SANE(stack)
#endif

#define DEBUG_ASSERT_STACK_VALUE(stack) assert(stack->arr[stack->idx] >= 0)

//#define SRL_PARSER_STACK_TRACE(msg, args...) warn(msg, args)
#define SRL_PARSER_STACK_TRACE(msg, args...)

typedef struct {
    int64_t *arr;
    int idx, cap;
} srl_stack_t;

/* Allocate new arrfer (but not the parser stack struct */
SRL_STATIC_INLINE int
srl_stack_init(srl_stack_t * stack, size_t cap)
{
    assert(stack != NULL);
    stack->arr = NULL;

    Newx(stack->arr, cap, int64_t);
    if (expect_false(stack->arr == NULL))
        return 1;

    stack->cap = (int) cap;
    stack->idx = -1;
    return 0;
}

/* Free stack arrfer (not not the parser stack struct */
SRL_STATIC_INLINE void
srl_stack_destroy(pTHX_ srl_stack_t *stack)
{
    if (stack == NULL) return;
    Safefree(stack->arr);
}

SRL_STATIC_INLINE int
srl_stack_empty(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    return stack->idx < 0;
}

SRL_STATIC_INLINE void
srl_stack_clear(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    stack->idx = -1;
}

SRL_STATIC_INLINE void
srl_stack_incr_value_nocheck(pTHX_ srl_stack_t *stack, int idx, int val)
{
    stack->arr[idx] += val;
    DEBUG_ASSERT_STACK_VALUE(stack);
}

SRL_STATIC_INLINE void
srl_stack_incr_value(pTHX_ srl_stack_t *stack, int idx, int val)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (expect_false(idx < 0 || idx > stack->idx))
        croak("srl_stack_incr passed wrong idx %d (stack->idx: %d)",
              idx, stack->idx);

    srl_stack_incr_value_nocheck(stack, idx, val);
}

SRL_STATIC_INLINE int64_t
srl_stack_peek(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (expect_false(srl_stack_empty(stack)))
        croak("srl_stack_peek on empty stack");

    DEBUG_ASSERT_STACK_VALUE(stack);
    return stack->arr[stack->idx];
}

SRL_STATIC_INLINE void
srl_stack_push(pTHX_ srl_stack_t *stack, int64_t cnt)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (expect_false(stack->idx >= (int) stack->cap - 1)) {
        stack->cap *= 2;
        assert(stack->cap <= 1024 * 1024); // make some sanity

        Renew(stack->arr, stack->cap, int64_t);
        if (stack->arr == NULL)
            croak("Out of memory");
    }

    stack->arr[++stack->idx] = cnt;
    DEBUG_ASSERT_STACK_SANE(stack);
    DEBUG_ASSERT_STACK_VALUE(stack);
    SRL_PARSER_STACK_TRACE("pushed %d on parser stack, current idx %d", cnt, stack->idx);
}

SRL_STATIC_INLINE void
srl_stack_pop(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (expect_false(srl_stack_empty(stack)))
        croak("Pop empty stack");

    stack->idx--;
    DEBUG_ASSERT_STACK_SANE(stack);
    SRL_PARSER_STACK_TRACE("poped parser stack, current idx %d", stack->idx);
}

SRL_STATIC_INLINE int
srl_stack_idx(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    return stack->idx;
}

SRL_STATIC_INLINE int
__compare_int64_t(const void *a, const void *b)
{
    return (*(int64_t*) b - *(int64_t*) a);
}

SRL_STATIC_INLINE void
srl_stack_rsort(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (srl_stack_empty(stack)) return;
    qsort((void *) stack->arr, (size_t) stack->idx + 1, sizeof(int64_t), __compare_int64_t);
}

SRL_STATIC_INLINE void
srl_stack_dedupe(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (srl_stack_empty(stack)) return;

    int i = 0, j = 0;
    for (; i <= stack->idx; i++) {
        if (stack->arr[j] != stack->arr[i]) {
            stack->arr[++j] = stack->arr[i];
        }
    }

    stack->idx = j;
    DEBUG_ASSERT_STACK_SANE(stack);
}
#endif
