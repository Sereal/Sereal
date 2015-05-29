#ifndef _STRTABLE_H_
#define _STRTABLE_H_

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "ppport.h"
#include "srl_inline.h"
#include "srl_buffer_types.h"

struct STRTABLE_ENTRY {
    U32 hash;
    U32 length;
    UV key_offset;
    UV tag_offset;
};

struct STRTABLE {
    struct STRTABLE_ENTRY *entries;
    const srl_buffer_t *buf;
    U32 keys;
    U32 capacity; // has to be power of 2
};

typedef struct STRTABLE STRTABLE_t;
typedef struct STRTABLE_ENTRY STRTABLE_ENTRY_t;
typedef struct STRTABLE_ENTRY * STRTABLE_ENTRY_ptr;

#define STRTABLE_HASH(hash, str, len) PERL_HASH((hash), (str), (len))
#define STRTABLE_MAX_STR_SIZE 0xFFFFFFFF
#define STRTABLE_ENTRY_TAG(tbl, ent) ((tbl)->buf->body_pos + (ent)->tag_offset)
#define STRTABLE_ENTRY_KEY(tbl, ent) ((tbl)->buf->body_pos + (ent)->key_offset)

#define STRTABLE_ASSERT_ENTRY(tbl, ent) STMT_START {                      \
    assert((tbl)->buf->body_pos <= STRTABLE_ENTRY_TAG((tbl), (ent)));     \
    assert((tbl)->buf->end      >= STRTABLE_ENTRY_TAG((tbl), (ent)));     \
} STMT_END

#define STRTABLE_ASSERT_ENTRY_KEY(tbl, ent, key, len) STMT_START {        \
    U32 hash;                                                             \
    STRTABLE_HASH(hash, (key), (len));                                    \
    assert((ent)->length == (len));                                       \
    assert((ent)->hash == hash);                                          \
    assert((tbl)->buf->body_pos <= STRTABLE_ENTRY_KEY((tbl), (ent)));     \
    assert((tbl)->buf->end      >= STRTABLE_ENTRY_KEY((tbl), (ent)));     \
    assert(memcmp(STRTABLE_ENTRY_KEY((tbl), (ent)), (key), (len)) == 0);  \
} STMT_END

SRL_STATIC_INLINE void
STRTABLE_init(STRTABLE_t *tbl, const srl_buffer_t *buf, U32 capacity)
{
    assert((capacity & (capacity - 1)) == 0); // capacity must be power of 2

    tbl->buf = buf;
    tbl->keys = 0;
    tbl->entries = NULL;
    tbl->capacity = capacity;
    Newxz(tbl->entries, capacity, STRTABLE_ENTRY_t);
    if (!tbl->entries) croak("Out of memory");
}

SRL_STATIC_INLINE STRTABLE_t *
STRTABLE_new(const srl_buffer_t *buf)
{
    STRTABLE_t *tbl = NULL;
    Newx(tbl, 1, STRTABLE_t);
    if (tbl) STRTABLE_init(tbl, buf, 8);
    return tbl;
}

SRL_STATIC_INLINE void
STRTABLE_clear(STRTABLE_t *tbl)
{
    tbl->keys = 0;
    Zero(tbl->entries, tbl->capacity, STRTABLE_ENTRY_t);
}

SRL_STATIC_INLINE void
STRTABLE_free(STRTABLE_t *tbl)
{
    if (tbl) Safefree(tbl->entries);
    Safefree(tbl);
}

SRL_STATIC_INLINE void
STRTABLE_grow(STRTABLE_t *tbl)
{
    U32 half_capacity = tbl->capacity;
    U32 capacity = tbl->capacity * 2;
    U32 i, slot, mask = capacity - 1;
    STRTABLE_ENTRY_t *entries, *n_entries = NULL;
    if (half_capacity >= 0x80000000) croak("Max capacity reached");

    entries = tbl->entries;
    Newxz(n_entries, capacity, STRTABLE_ENTRY_t);
    if (!n_entries) croak("Out of memory");

    for (i = 0; i < half_capacity; ++i) {
        if (entries[i].key_offset == 0) continue;
        slot = entries[i].hash & mask;

        while (1) {
            if (n_entries[slot].key_offset == 0) {
                n_entries[slot] = entries[i];
                break;
            }
            slot = (slot + 1) & mask;
        }
    }

    Safefree(tbl->entries);
    tbl->entries = n_entries;
    tbl->capacity = capacity;
}

/* caller have to fill fields in retured structer if found = 1 */
SRL_STATIC_INLINE STRTABLE_ENTRY_ptr
STRTABLE_insert(STRTABLE_t *tbl, const void *key, U32 len, int *found)
{
    U32 slot, hash, mask;
    STRTABLE_ENTRY_t *entries;
    *found = 0;

    // need to grow buffer before inserting as we return pointer
    if ((tbl->keys + 1) >= (tbl->capacity >> 1)) // tbl->capacity / 2
        STRTABLE_grow(tbl);

    STRTABLE_HASH(hash, (const char *)key, len);
    entries = tbl->entries;
    mask = tbl->capacity - 1;
    slot = hash & mask;

    while (1) {
        if (entries[slot].key_offset == 0) {
            entries[slot].hash = hash;
            entries[slot].length = len;
            tbl->keys++;
            return entries + slot;
        }

        if (   entries[slot].hash == hash
            && entries[slot].length == len
            && memcmp(STRTABLE_ENTRY_KEY(tbl, &entries[slot]), key, len) == 0
        ) {
            *found = 1;
            return entries + slot;
        }

        slot = (slot + 1) & mask;
    }
}

#endif
