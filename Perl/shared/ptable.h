/* Taken from Chocolateboy's autobox module. License same as perl's and
 * this same as this module's license.
 */

/*
 * This is a customized version of the pointer table implementation in sv.c
 */

#ifndef PTABLE_H_
#define PTABLE_H_

#include <assert.h>
#include <limits.h>
#include "ppport.h"

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

struct PTABLE_entry {
    struct PTABLE_entry     *next;
    void                    *key;
    void                    *value;
};

struct PTABLE {
    struct PTABLE_entry     **tbl_ary;
    UV                      tbl_max;
    UV                      tbl_items;
};

struct PTABLE_iter {
    struct PTABLE           *table;
    UV                      bucket_num;
    struct PTABLE_entry     *cur_entry;
};

typedef struct PTABLE_entry PTABLE_ENTRY_t;
typedef struct PTABLE       PTABLE_t;
typedef struct PTABLE_iter  PTABLE_ITER_t;


STATIC PTABLE_t * PTABLE_new(void);
STATIC PTABLE_t * PTABLE_new_size(const U8 size_base2_exponent);
STATIC PTABLE_ENTRY_t * PTABLE_find(PTABLE_t *tbl, const void *key);
STATIC void * PTABLE_fetch(PTABLE_t *tbl, const void *key);
STATIC void PTABLE_store(PTABLE_t *tbl, void *key, void *value);
STATIC void PTABLE_delete(PTABLE_t *tbl, void *key);
STATIC void PTABLE_grow(PTABLE_t *tbl);
STATIC void PTABLE_clear(PTABLE_t *tbl);
STATIC void PTABLE_free(PTABLE_t *tbl);

STATIC PTABLE_ITER_t * PTABLE_iter_new(PTABLE_t *tbl);
STATIC PTABLE_ENTRY_t * PTABLE_iter_next(PTABLE_ITER_t *iter);
STATIC void PTABLE_iter_free(PTABLE_ITER_t *iter);

/* create a new pointer => pointer table */
SRL_STATIC_INLINE PTABLE_t *
PTABLE_new(void)
{
    return PTABLE_new_size(9);
}

STATIC PTABLE_t *
PTABLE_new_size(const U8 size_base2_exponent)
{
    PTABLE_t *tbl;
    Newxz(tbl, 1, PTABLE_t);
    tbl->tbl_max = (1 << size_base2_exponent) - 1;
    tbl->tbl_items = 0;
    Newxz(tbl->tbl_ary, tbl->tbl_max + 1, PTABLE_ENTRY_t*);
    return tbl;
}

/* map an existing pointer using a table */
STATIC PTABLE_ENTRY_t *
PTABLE_find(PTABLE_t *tbl, const void *key) {
    PTABLE_ENTRY_t *tblent;
    const UV hash = PTABLE_HASH(key);
    tblent = tbl->tbl_ary[hash & tbl->tbl_max];
    for (; tblent; tblent = tblent->next) {
        if (tblent->key == key)
            return tblent;
    }
    return NULL;
}

SRL_STATIC_INLINE void *
PTABLE_fetch(PTABLE_t *tbl, const void *key)
{
    PTABLE_ENTRY_t const *const tblent = PTABLE_find(tbl, key);
    return tblent ? tblent->value : NULL;
}

/* add a new entry to a pointer => pointer table */

STATIC void
PTABLE_store(PTABLE_t *tbl, void *key, void *value)
{
    PTABLE_ENTRY_t *tblent = PTABLE_find(tbl, key);

    if (tblent) {
        tblent->value = value;
    } else {
        const UV entry = PTABLE_HASH(key) & tbl->tbl_max;
        Newx(tblent, 1, PTABLE_ENTRY_t);

        tblent->key = key;
        tblent->value = value;
        tblent->next = tbl->tbl_ary[entry];
        tbl->tbl_ary[entry] = tblent;
        tbl->tbl_items++;
        if (tblent->next && (tbl->tbl_items > tbl->tbl_max))
            PTABLE_grow(tbl);
    }

}

/* double the hash bucket size of an existing ptr table */

STATIC void
PTABLE_grow(PTABLE_t *tbl)
{
    PTABLE_ENTRY_t **ary = tbl->tbl_ary;
    const UV oldsize = tbl->tbl_max + 1;
    UV newsize = oldsize * 2;
    UV i;

    Renew(ary, newsize, PTABLE_ENTRY_t*);
    Zero(&ary[oldsize], newsize - oldsize, PTABLE_ENTRY_t*);
    tbl->tbl_max = --newsize;
    tbl->tbl_ary = ary;

    for (i = 0; i < oldsize; i++, ary++) {
        PTABLE_ENTRY_t **curentp, **entp, *ent;
        if (!*ary)
            continue;
        curentp = ary + oldsize;
        for (entp = ary, ent = *ary; ent; ent = *entp) {
            if ((newsize & PTABLE_HASH(ent->key)) != i) {
                *entp = ent->next;
                ent->next = *curentp;
                *curentp = ent;
                continue;
            } else {
                entp = &ent->next;
            }
        }
    }
}

/* remove all the entries from a ptr table */

STATIC void
PTABLE_clear(PTABLE_t *tbl)
{
    if (tbl && tbl->tbl_items) {
        register PTABLE_ENTRY_t * * const array = tbl->tbl_ary;
        UV riter = tbl->tbl_max;

        do {
            PTABLE_ENTRY_t *entry = array[riter];

            while (entry) {
                PTABLE_ENTRY_t * const oentry = entry;
                entry = entry->next;
                Safefree(oentry);
            }

            /* chocolateboy 2008-01-08
             *
             * make sure we clear the array entry, so that subsequent probes fail
             */

            array[riter] = NULL;
        } while (riter--);

        tbl->tbl_items = 0;
    }
}

/* remove one entry from a ptr table */

STATIC void
PTABLE_delete(PTABLE_t *tbl, void *key)
{
    PTABLE_ENTRY_t *tblent;
    PTABLE_ENTRY_t *tblent_prev;

    if (!tbl || !tbl->tbl_items) {
        return;
    } else {
        const UV hash = PTABLE_HASH(key);
        tblent_prev = NULL;
        tblent = tbl->tbl_ary[hash & tbl->tbl_max];

        for (; tblent; tblent_prev = tblent, tblent = tblent->next) {
            if (tblent->key == key) {
                if (tblent_prev != NULL) {
                    tblent_prev->next = tblent->next;
                }
                else {
                    /* First entry in chain */
                    tbl->tbl_ary[hash & tbl->tbl_max] = tblent->next;
                }
                Safefree(tblent);
                break;
            }
        }
    }
}

/* clear and free a ptr table */

STATIC void
PTABLE_free(PTABLE_t *tbl)
{
    if (!tbl) {
        return;
    }
    PTABLE_clear(tbl);
    Safefree(tbl->tbl_ary);
    Safefree(tbl);
}


#define PTABLE_ITER_NEXT_ELEM(iter, tbl)                                    \
    STMT_START {                                                            \
        if ((iter)->cur_entry && (iter)->cur_entry->next) {                 \
            (iter)->cur_entry = (iter)->cur_entry->next;                    \
        }                                                                   \
        else {                                                              \
            do {                                                            \
                if ((iter)->bucket_num > (tbl)->tbl_max) {                  \
                    (iter)->cur_entry = NULL;                               \
                    break;                                                  \
                }                                                           \
                (iter)->cur_entry = (tbl)->tbl_ary[(iter)->bucket_num++];   \
            } while ((iter)->cur_entry == NULL);                            \
        }                                                                   \
    } STMT_END

/* Create new iterator object */
STATIC PTABLE_ITER_t *
PTABLE_iter_new(PTABLE_t *tbl)
{
    PTABLE_ITER_t *iter;
    Newx(iter, 1, PTABLE_ITER_t);
    iter->table = tbl;
    iter->bucket_num = 0;
    iter->cur_entry = NULL;
    if (tbl->tbl_items == 0) {
        /* Prevent hash bucket scanning.
         * This can be a significant optimization on large, empty hashes. */
        iter->bucket_num = INT_MAX;
        return iter;
    }
    PTABLE_ITER_NEXT_ELEM(iter, tbl);
    assert(iter->cur_entry != NULL);
    return iter;
}

/* Return next item from hash, NULL if at end */
STATIC PTABLE_ENTRY_t *
PTABLE_iter_next(PTABLE_ITER_t *iter)
{
    PTABLE_ENTRY_t *retval = iter->cur_entry;
    PTABLE_t *tbl = iter->table;
    PTABLE_ITER_NEXT_ELEM(iter, tbl);
    return retval;
}

/* Free iterator object */
STATIC void
PTABLE_iter_free(PTABLE_ITER_t *iter)
{
    Safefree(iter);
}

STATIC void
PTABLE_debug_dump(PTABLE_t *tbl, void (*func)(PTABLE_ENTRY_t *e))
{
    PTABLE_ENTRY_t *e;
    PTABLE_ITER_t *iter = PTABLE_iter_new(tbl);
    while (NULL != (e = PTABLE_iter_next(iter))) {
        func(e);
    }
    PTABLE_iter_free(iter);
}

#endif
