#ifndef SRL_INDEX_H_
#define SRL_INDEX_H_

#include "EXTERN.h"
#include "perl.h"

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_iterator.h"

#define SRL_INDEX_HASH_KEY_SMALL_LENGTH (2*sizeof(uint32_t))

#define DEBUG_ASSERT_INDEX_PTR(index, ptr)                                    \
    assert((ptr) >= (index)->beg && (ptr) < (index)->end)

#define DEBUG_ASSERT_INDEX_SANE(index) STMT_START {                           \
    assert((index) != NULL);                                                  \
    assert((index)->beg != NULL);                                             \
    assert((index)->end != NULL);                                             \
    assert((index)->ptr != NULL);                                             \
    assert((index)->begin < (index)->end);                                    \
    assert((index)->ptr >= (index)->beg && (index)->ptr < (index)->end);      \
} STMT_END

// [ 1, 2, 3, 4 ] => srl_indexed_container{ type=ARRAY, size=4, [ offsets to tags ] }
// [ [1], [2] ]   => srl_indexed_container{ type=ARRAY, size=2, [ ptr to elements ] }

typedef struct srl_index srl_index_t;
typedef struct srl_indexed_element srl_indexed_element_t;
typedef struct srl_indexed_array_element srl_indexed_array_element_t;
typedef struct srl_indexed_array srl_indexed_array_t;
typedef struct srl_indexed_hash_element srl_indexed_hash_element_t;
typedef union  srl_indexed_hash_element_key srl_indexed_hash_element_key_t;
typedef struct srl_indexed_hash srl_indexed_hash_t;

struct srl_index {
    srl_iterator_t* iter;
    char *ptr, *beg, *end;
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
    char str[SRL_INDEX_HASH_KEY_SMALL_LENGTH];
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

srl_index_t* srl_create_index(pTHX_ srl_iterator_t* iter);

#endif
