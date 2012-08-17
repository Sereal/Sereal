/* Taken from Chocolateboy's autobox module. License same as perl's and
 * this same as this module's license.
 */

/*
 * This is a customized version of the pointer table implementation in sv.c
 */

#include "ppport.h"

#if PTRSIZE == 8
    /*
     * This is one of Thomas Wang's hash functions for 64-bit integers from:
     * http://www.concentric.net/~Ttwang/tech/inthash.htm
     */
    U32 ptr_hash(PTRV u) {
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
    U32 ptr_hash(PTRV u) {
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

typedef struct PTABLE_entry PTABLE_ENTRY_t;
typedef struct PTABLE            PTABLE_t;

static PTABLE_t * PTABLE_new(void);
static PTABLE_ENTRY_t * PTABLE_find(PTABLE_t *tbl, const void *key);
/* commented since unused */
/* static void * PTABLE_fetch(PTABLE_t *tbl, const void *key); */
static void PTABLE_store(PTABLE_t *tbl, void *key, void *value);
static void PTABLE_delete(PTABLE_t *tbl, void *key);
static void PTABLE_grow(PTABLE_t *tbl);
static void PTABLE_clear(PTABLE_t *tbl);
static void PTABLE_free(PTABLE_t *tbl);

/* create a new pointer => pointer table */

static PTABLE_t *
PTABLE_new(void)
{
    PTABLE_t *tbl;
    Newxz(tbl, 1, PTABLE_t);
    tbl->tbl_max = 511;
    tbl->tbl_items = 0;
    Newxz(tbl->tbl_ary, tbl->tbl_max + 1, PTABLE_ENTRY_t*);
    return tbl;
}

/* map an existing pointer using a table */

static PTABLE_ENTRY_t *
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

/* commented since unused */
static inline void *
PTABLE_fetch(PTABLE_t *tbl, const void *key)
{
    PTABLE_ENTRY_t const *const tblent = PTABLE_find(tbl, key);
    return tblent ? tblent->value : NULL;
}

/* add a new entry to a pointer => pointer table */

static void
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

static void
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

static void
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

static void
PTABLE_delete(PTABLE_t *tbl, void *key)
{
    PTABLE_ENTRY_t *tblent;
    PTABLE_ENTRY_t *tblent_prev;

    if (!tbl || !tbl->tbl_items)
        return;

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

/* clear and free a ptr table */

static void
PTABLE_free(PTABLE_t *tbl)
{
    if (!tbl) {
        return;
    }
    PTABLE_clear(tbl);
    Safefree(tbl->tbl_ary);
    Safefree(tbl);
}
