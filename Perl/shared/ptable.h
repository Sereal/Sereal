#ifndef _PTABLE_H_
#define _PTABLE_H_

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "ppport.h"
#include "srl_inline.h"

#if PTRSIZE == 8
    /*
     * This is one of Thomas Wang's hash functions for 64-bit integers from:
     * http://www.concentric.net/~Ttwang/tech/inthash.htm
     */
    SRL_STATIC_INLINE U32 ptr_hash(PTRV u) {
        u = (~u) + (u << 18);
        u = u ^ (u >> 31);
        u = u * 21;
        u = u ^ (u >> 11);
        u = u + (u << 6);
        u = u ^ (u >> 22);
        return (U32)u;
    }
#else
    /*
     * This is one of Bob Jenkins' hash functions for 32-bit integers
     * from: http://burtleburtle.net/bob/hash/integer.html
     */
    SRL_STATIC_INLINE U32 ptr_hash(PTRV u) {
        u = (u + 0x7ed55d16) + (u << 12);
        u = (u ^ 0xc761c23c) ^ (u >> 19);
        u = (u + 0x165667b1) + (u << 5);
        u = (u + 0xd3a2646c) ^ (u << 9);
        u = (u + 0xfd7046c5) + (u << 3);
        u = (u ^ 0xb55a4f09) ^ (u >> 16);
        return u;
    }
#endif

#define PTABLE_HASH(ptr) ptr_hash(PTR2nat(ptr))
#define PTABLE_SKIP_EMPTY(entry) if ((entry).key == NULL) { continue; }

struct PTABLE_ENTRY {
    void *key;
    void *value;
};

struct PTABLE {
    struct PTABLE_ENTRY *entries;
    U32 keys;
    U32 capacity; // has to be power of 2
};

typedef struct PTABLE PTABLE_t;
typedef struct PTABLE_ENTRY PTABLE_ENTRY_t;
typedef struct PTABLE_ENTRY *PTABLE_ENTRY_ptr;

SRL_STATIC_INLINE void
PTABLE_init(PTABLE_t *tbl, U32 capacity)
{
    assert((capacity & (capacity - 1)) == 0); // capacity must be power of 2

    tbl->keys = 0;
    tbl->entries = NULL;
    tbl->capacity = capacity;
    Newxz(tbl->entries, capacity, PTABLE_ENTRY_t);
    if (!tbl->entries) croak("Out of memory");
}

SRL_STATIC_INLINE PTABLE_t *
PTABLE_new()
{
    PTABLE_t *tbl = NULL;
    Newx(tbl, 1, PTABLE_t);
    if (tbl) PTABLE_init(tbl, 8);
    return tbl;
}

SRL_STATIC_INLINE void
PTABLE_clear(PTABLE_t *tbl)
{
    tbl->keys = 0;
    Zero(tbl->entries, tbl->capacity, PTABLE_ENTRY_t);
}

SRL_STATIC_INLINE void
PTABLE_clear_dec(PTABLE_t *tbl)
{
    U32 i;
    for (i = 0; i < tbl->capacity; ++i) {
        PTABLE_SKIP_EMPTY(tbl->entries[i]);
        if (tbl->entries[i].value) SvREFCNT_dec((SV*) tbl->entries[i].value);
    }

    PTABLE_clear(tbl);
}

SRL_STATIC_INLINE void
PTABLE_free(PTABLE_t *tbl)
{
    if (tbl) Safefree(tbl->entries);
    Safefree(tbl);
}

SRL_STATIC_INLINE void
PTABLE_grow(PTABLE_t *tbl)
{
    U32 half_capacity = tbl->capacity;
    U32 capacity = tbl->capacity * 2;
    U32 i, hash, slot, mask = capacity - 1;
    PTABLE_ENTRY_t *entries, *n_entries = NULL;
    if (half_capacity >= 0x80000000) croak("Max capacity reached");

    entries = tbl->entries;
    Newxz(n_entries, capacity, PTABLE_ENTRY_t);
    if (!n_entries) croak("Out of memory");

    for (i = 0; i < half_capacity; ++i) {
        PTABLE_SKIP_EMPTY(entries[i]);
        hash = PTABLE_HASH(entries[i].key);
        slot = hash & mask;

        while (1) {
            if (n_entries[slot].key == NULL) {
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

SRL_STATIC_INLINE PTABLE_ENTRY_ptr
PTABLE_insert(PTABLE_t *tbl, void *key, int *found)
{
    U32 slot, mask;
    PTABLE_ENTRY_t *entries;
    *found = 0;

    // need to grow buffer before inserting as we return pointer
    if ((tbl->keys + 1) >= (tbl->capacity >> 1)) // tbl->capacity / 2
        PTABLE_grow(tbl);

    entries = tbl->entries;
    mask = tbl->capacity - 1;
    slot = PTABLE_HASH(key) & mask;

    while (1) {
        if (entries[slot].key == NULL) {
            entries[slot].key = key;
            tbl->keys++;
            return entries + slot;
        }

        if (entries[slot].key == key) {
            *found = 1;
            return entries + slot;
        }

        slot = (slot + 1) & mask;
    }
}

SRL_STATIC_INLINE void
PTABLE_store(PTABLE_t *tbl, void *key, void *value)
{
    U32 mask = tbl->capacity - 1;
    U32 slot = PTABLE_HASH(key) & mask;
    PTABLE_ENTRY_t *entries = tbl->entries;

    while (1) {
        if (entries[slot].key == NULL) {
            entries[slot].key = key;
            entries[slot].value = value;
            tbl->keys++;

            if (tbl->keys >= (tbl->capacity >> 1)) // tbl->capacity / 2
                PTABLE_grow(tbl);

            return;
        }

        if (entries[slot].key == key) {
            entries[slot].value = value;
            return;
        }

        slot = (slot + 1) & mask;
    }
}

SRL_STATIC_INLINE void *
PTABLE_fetch(PTABLE_t *tbl, void *key)
{
    U32 mask = tbl->capacity - 1;
    U32 slot = PTABLE_HASH(key) & mask;
    PTABLE_ENTRY_t *entries = tbl->entries;

    while (1) {
        if (entries[slot].key == NULL) return NULL;
        if (entries[slot].key == key)  return entries[slot].value;
        slot = (slot + 1) & mask;
    }
}

SRL_STATIC_INLINE void
PTABLE_delete(PTABLE_t *tbl, void *key)
{
    /*
     * 1. Find and remove the desired element
     * 2. Go to the next bucket
     * 3. If the bucket is empty, quit
     * 4. If the bucket is full, delete the element in that bucket and re-add
     *    it to the hash table using the normal means. The item must be removed
     *    before re-adding, because it is likely that the item could be added back
     *    into its original spot.
     * 5. Repeat step 2.
     */

    U32 mask = tbl->capacity - 1;
    U32 slot = PTABLE_HASH(key) & mask;
    PTABLE_ENTRY_t *entries = tbl->entries;

    while (1) {
        if (entries[slot].key == NULL) return;
        if (entries[slot].key == key) {
            entries[slot].key = NULL;
            tbl->keys--;

            for (slot = (slot + 1) & mask; entries[slot].key; slot = (slot + 1) & mask) {
                tbl->keys--;
                entries[slot].key = NULL;
                PTABLE_store(tbl, key, entries[slot].value);
            }

            return;
        }

        slot = (slot + 1) & mask;
    }
}

SRL_STATIC_INLINE void
PTABLE_debug_dump(PTABLE_t *tbl, void (*func)(PTABLE_ENTRY_ptr e))
{
    U32 i;
    for (i = 0; i < tbl->capacity; ++i) {
        PTABLE_SKIP_EMPTY(tbl->entries[i]);
        func(tbl->entries + i);
    }
}

#endif
