#ifndef SRL_STACK_H_
#define SRL_STACK_H_

#ifndef srl_stack_type_t
#error define srl_stack_type_t before including srl_stack.h
#endif

#include <stdlib.h>
#include <assert.h>
#include "srl_inline.h"
#include "srl_common.h"

#define DEBUG_ASSERT_STACK_PTR(stack, ptr)                                    \
    assert((ptr) >= (stack)->begin && (ptr) <= (stack)->end)

#define DEBUG_ASSERT_STACK_SANE(stack) STMT_START {                           \
    assert((stack) != NULL);                                                  \
    assert((stack)->begin != NULL);                                           \
    assert((stack)->end != NULL);                                             \
    assert((stack)->begin <= (stack)->end);                                   \
    assert((stack)->ptr == NULL ||                                            \
           ((stack)->ptr >= (stack)->begin && (stack)->ptr <= (stack)->end)); \
} STMT_END

#ifdef TRACE_READER
#   define SRL_STACK_TRACE(mrg, args...)                                      \
        fprintf(stderr, "%s:%d:%s(): " msg, __FILE__, __LINE__, __func__, args)
#else
#   define SRL_STACK_TRACE(mrg, args...)
#endif

#define SRL_STACK_SIZE(stack)  (((stack)->end - (stack)->begin) + 1)
#define SRL_STACK_POS(stack)   ((stack)->ptr ? (stack)->ptr - (stack)->begin : -1)

typedef struct srl_stack srl_stack_t;
struct srl_stack {
    srl_stack_type_t *begin, *end, *ptr;
};

/* Allocate new arrfer (but not the stack struct */
SRL_STATIC_INLINE int
srl_stack_init(pTHX_ srl_stack_t * stack, size_t size)
{
    assert(size > 0);
    assert(stack != NULL);

    stack->begin = NULL;
    Newx(stack->begin, size, srl_stack_type_t);
    if (expect_false(stack->begin == NULL))
        return 1;

    stack->end = stack->begin + size - 1;
    stack->ptr = NULL;

    assert(SRL_STACK_SIZE(stack) == (int) size);
    return 0;
}

SRL_STATIC_INLINE void
srl_stack_grow(pTHX_ srl_stack_t *stack)
{
    ptrdiff_t pos   = SRL_STACK_POS(stack);
    size_t new_size = SRL_STACK_SIZE(stack) * 2;
    assert(new_size <= 1024 * 1024);

    Renew(stack->begin, new_size, srl_stack_type_t);
    if (stack->begin == NULL)
        croak("Out of memory");

    stack->end = stack->begin + new_size - 1;
    stack->ptr = stack->begin + pos;

    DEBUG_ASSERT_STACK_SANE(stack);
    assert(SRL_STACK_SIZE(stack) == (int) new_size);
    SRL_STACK_TRACE("grew stack to size %zu", new_size);
}

/* Free stack arrfer (not not the stack struct */
SRL_STATIC_INLINE void
srl_stack_destroy(pTHX_ srl_stack_t *stack)
{
    if (stack == NULL) return;
    Safefree(stack->begin);
}

SRL_STATIC_INLINE void
srl_stack_clear(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    stack->ptr = NULL;
}

#define srl_stack_ptr(stack)   ((stack)->ptr)
#define srl_stack_empty(stack) ((stack)->ptr == NULL)
#define srl_stack_full(stack)  ((stack)->ptr == (stack)->end)

#define srl_stack_push_ptr(stack, val_ptr) STMT_START {               \
    DEBUG_ASSERT_STACK_SANE(stack);                                   \
                                                                      \
    if (srl_stack_empty(stack)) {                                     \
        (stack)->ptr = (stack)->begin;                                \
        (val_ptr) = (stack)->begin;                                   \
    } else {                                                          \
        if (expect_false(srl_stack_full(stack)))                      \
            srl_stack_grow((stack));                                  \
                                                                      \
        (val_ptr) = ++(stack)->ptr;                                   \
    }                                                                 \
                                                                      \
    DEBUG_ASSERT_STACK_SANE(stack);                                   \
    SRL_STACK_TRACE("pushed value on stack, current idx %d",          \
                    (int) SRL_STACK_POS(stack));                      \
} STMT_END

#define srl_stack_push_val(stack, val) STMT_START {                   \
    DEBUG_ASSERT_STACK_SANE(stack);                                   \
                                                                      \
    if (srl_stack_empty(stack)) {                                     \
        (stack)->ptr = (stack)->begin;                                \
    } else {                                                          \
        if (expect_false(srl_stack_full(stack)))                      \
            srl_stack_grow((stack));                                  \
                                                                      \
        (stack)->ptr++;                                               \
    }                                                                 \
                                                                      \
    (stack)->ptr = val;                                               \
                                                                      \
    DEBUG_ASSERT_STACK_SANE(stack);                                   \
    SRL_STACK_TRACE("pushed value on stack, current idx %d",          \
                    (int) SRL_STACK_POS(stack));                      \
} STMT_END


#define srl_stack_pop_nocheck(stack) STMT_START {                     \
    DEBUG_ASSERT_STACK_SANE(stack);                                   \
                                                                      \
    if (expect_false((stack)->ptr == (stack)->begin)) {               \
        (stack)->ptr = NULL;                                          \
    } else {                                                          \
        (stack)->ptr--;                                               \
    }                                                                 \
                                                                      \
    DEBUG_ASSERT_STACK_SANE(stack);                                   \
    SRL_STACK_TRACE("poped stack, current idx %d",                    \
                    (int) SRL_STACK_POS(stack));                      \
} STMT_END

SRL_STATIC_INLINE void
srl_stack_pop(pTHX_ srl_stack_t *stack)
{
    if (expect_false(srl_stack_empty(stack)))
        croak("Pop empty stack");

    srl_stack_pop_nocheck(stack);
}

SRL_STATIC_INLINE srl_stack_type_t
srl_stack_peek_nocheck(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    return *stack->ptr;
}

SRL_STATIC_INLINE srl_stack_type_t
srl_stack_peek(pTHX_ srl_stack_t *stack)
{
    DEBUG_ASSERT_STACK_SANE(stack);
    if (expect_false(srl_stack_empty(stack)))
        croak("srl_stack_peek on empty stack");

    return srl_stack_peek_nocheck(aTHX_ stack);
}

#endif
