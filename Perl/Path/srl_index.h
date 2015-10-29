#ifndef SRL_INDEX_H_
#define SRL_INDEX_H_

#include <stdlib.h>
#include <assert.h>
#include "srl_inline.h"
#include "srl_common.h"

#define DEBUG_ASSERT_INDEX_PTR(index, ptr)                                    \
    assert((ptr) >= (index)->begin && (ptr) <= (index)->end)

#define DEBUG_ASSERT_INDEX_SANE(index) STMT_START {                           \
    assert((index) != NULL);                                                  \
    assert((index)->begin != NULL);                                           \
    assert((index)->end != NULL);                                             \
    assert((index)->ptr != NULL);                                             \
    assert((index)->begin <= (index)->end);                                   \
    assert((index)->ptr >= (index)->begin && (index)->ptr <= (index)->end);   \
} STMT_END

#ifdef TRACE_INDEX
#   define SRL_INDEX_TRACE(msg, args...)                                      \
        fprintf(stderr, "%s:%d:%s(): "msg"\n", __FILE__, __LINE__, __func__, args)
#else
#   define SRL_INDEX_TRACE(msg, args...)
#endif

#define SRL_INDEX_SIZE(index)  (((index)->end - (index)->begin) + 1)
#define SRL_INDEX_SPACE(index) (((index)->ptr - (index)->begin) + 1)

#define SRL_INDEX_TYPE_MASK         (0xFF000000)
#define SRL_INDEX_SIZE_MASK         (0x00FFFFFF)

#define SRL_INDEX_TYPE_SCALAR       (0x00000000) // offset points to tag
#define SRL_INDEX_TYPE_ARRAY_NODE   (0x01000000) // offset points to child
#define SRL_INDEX_TYPE_ARRAY_LEAF   (0x02000000) // offset points to tag
#define SRL_INDEX_TYPE_HASH_NODE    (0x03000000)
#define SRL_INDEX_TYPE_HASH_LEAF    (0x04000000)

// [ 1, 2, 3, 4 ] => srl_indexed_container{ type=ARRAY, size=4, [ offsets to tags ] }
// [ [1], [2] ]   => srl_indexed_container{ type=ARRAY, size=2, [ ptr to elements ] }

typedef struct srl_index srl_index_t;
typedef struct srl_index *srl_index_ptr;
typedef struct srl_indexed_hash srl_indexed_hash_t;
typedef struct srl_indexed_array srl_indexed_array_t;
typedef struct srl_indexed_element srl_indexed_elemet_t;
typedef struct srl_indexed_element *srl_indexed_element_ptr;
typedef struct srl_indexed_array_element srl_indexed_array_element_t;
typedef struct srl_indexed_hash_element srl_indexed_hash_element_t;

struct srl_index {
    char *ptr, *begin, *end;
};

struct srl_indexed_element {
    uint32_t flags;
    uint32_t offset;
};

struct srl_indexed_array_element {
    uint32_t flags;
    uint32_t offset;
};

struct srl_indexed_array {
    uint32_t flags;
    uint32_t offset;
    srl_indexed_array_element_t dataset[];
};

union srl_indexed_hash_element_key {
    char str[8]; 
    struct {
        uint32_t hash;
        uint32_t str;
    } h;
};

struct srl_indexed_hash_element {
    uint32_t flags;
    uint32_t offset;
    union srl_indexed_hash_element_key key;
};

struct srl_indexed_hash {
    uint32_t flags;
    uint32_t offset;
    srl_indexed_hash_element_t dataset[];
};

// array -> { struct, [ UVs ] }
// array -> { struct, [ array elements ] }


/* Allocate new array (but not the index struct) */
SRL_STATIC_INLINE int
srl_index_init(pTHX_ srl_index_t * index, size_t size)
{
    assert(size > 0);
    assert(index != NULL);

    index->begin = NULL;
    Newx(index->begin, size, char);
    if (expect_false(index->begin == NULL))
        return 1;

    index->end = index->begin + size - 1;
    index->ptr = NULL;

    assert(SRL_INDEX_SIZE(index) == (int) size);
    return 0;
}

/* Free index array (not not the index struct) */
SRL_STATIC_INLINE void
srl_index_destroy(pTHX_ srl_index_t *index)
{
    if (index == NULL) return;
    Safefree(index->begin);
}

#define srl_index_ptr(index)   ((index)->ptr)
#define srl_index_full(index)  ((index)->ptr >= (index)->end)
#define srl_index_clear(index) STMT_START {                           \
    (index)->ptr = (index)->begin;                                    \
    DEBUG_ASSERT_INDEX_SANE(index);                                   \
} STMT_END

SRL_STATIC_INLINE char *
srl_index_allocate(pTHX_ srl_index_t *index, size_t size)
{
    SRL_INDEX_TRACE("new allocation request of size %zu", size);
    if (expect_false(SRL_INDEX_SPACE(index) < size)) {
        SRL_INDEX_TRACE("not enough space");
        return NULL;
    }

    index->ptr += size;
    DEBUG_ASSERT_INDEX_SANE(index);
    return index->ptr;
}

SRL_STATIC_INLINE srl_indexed_element_ptr
srl_allocate_array(pTHX_ srl_index_t *index, size_t length, uint32_t type, uint32_t offset)
{
    size_t size;
    srl_indexed_array_t *ptr;
    SRL_INDEX_TRACE("index array of length %zu", length);
    if (expect_false(length > SRL_INDEX_SIZE_MASK)) return NULL;

    size = sizeof(srl_indexed_array_t) + length * sizeof(srl_indexed_array_element_t);
    ptr = (srl_indexed_array_t*) srl_index_allocate(aTHX_ index, size);
    if (expect_false(ptr == NULL)) return NULL;

    ptr->offset = offset;
    ptr->flags = type | length;
    return (srl_indexed_element_ptr) ptr;
}

SRL_STATIC_INLINE srl_indexed_element_ptr
srl_allocate_hash(pTHX_ srl_index_t *index, size_t length, uint32_t type, uint32_t offset)
{
    size_t size;
    srl_indexed_hash_t *ptr;
    SRL_INDEX_TRACE("index hash of length %zu", length);
    if (expect_false(length > SRL_INDEX_SIZE_MASK)) return NULL;

    size = sizeof(srl_indexed_hash_t) + length * sizeof(srl_indexed_hash_element_t);
    ptr = (srl_indexed_hash_t*) srl_index_allocate(aTHX_ index, size);
    if (expect_false(ptr == NULL)) return NULL;

    ptr->offset = offset;
    ptr->flags = type | length;
    return (srl_indexed_element_ptr) ptr;
}

#endif
