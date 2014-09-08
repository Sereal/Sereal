/* Taken from Chocolateboy's autobox module. License same as perl's and
 * this same as this module's license.
 */

/*
 * This is a customized version of the pointer table implementation in sv.c
 */

#ifndef STRTABLE_H_
#define STRTABLE_H_

#include <assert.h>
#include <limits.h>
#include <string.h>
#include "ppport.h"
#include "../Encoder/srl_buffer_types.h"

#define STRTABLE_HASH(str, len) S_perl_hash_murmur3(PERL_HASH_SEED, (U8*) str, len)
#define STRTABLE_HASH_FROM_ENTRY(tbl, ent) STRTABLE_HASH(STRTABLE_ENTRY_STR((tbl), (ent)), (ent)->len)
#define STRTABLE_ENTRY_TAG(tbl, ent) ((tbl)->buf->body_pos + (ent)->tag_offset)
#define STRTABLE_ENTRY_STR(tbl, ent) ((tbl)->buf->body_pos + (ent)->str_offset)

#define STRTABLE_ASSERT_ENTRY(tbl, ent) STMT_START {                             \
    assert((ent) != NULL);                                                       \
    assert((tbl)->buf->body_pos <= STRTABLE_ENTRY_TAG((tbl), (ent)));            \
    assert((tbl)->buf->end      >= STRTABLE_ENTRY_TAG((tbl), (ent)));            \
} STMT_END

#define STRTABLE_ASSERT_ENTRY_STRICT(tbl, ent) STMT_START {                      \
    STRTABLE_ASSERT_ENTRY((tbl), (ent));                                         \
    assert((tbl)->buf->body_pos <= STRTABLE_ENTRY_STR((tbl), (ent)));            \
    assert((tbl)->buf->end      >= STRTABLE_ENTRY_STR((tbl), (ent)));            \
    assert(STRTABLE_ENTRY_STR((tbl), (ent)) > STRTABLE_ENTRY_TAG((tbl), (ent))); \
} STMT_END

#define STRTABLE_ASSERT_ENTRY_STR(tbl, ent, str) STMT_START {                    \
    assert(strncmp(STRTABLE_ENTRY_STR((tbl), (ent)), (str), (ent)->len) == 0);   \
    assert((ent)->hash == STRTABLE_HASH((str), (ent)->len));                     \
} STMT_END

#define STRTABLE_UPDATE_ENTRY(tbl, ent, offset) STMT_START { \
    (ent)->str_offset = (offset);                            \
    STRTABLE_ASSERT_ENTRY_STRICT(tbl, ent);                  \
} STMT_END

typedef struct STRTABLE         STRTABLE_t;
typedef struct STRTABLE *       strtable_ptr;
typedef struct STRTABLE_entry   STRTABLE_ENTRY_t;
typedef struct STRTABLE_entry * strtable_entry_ptr;

struct STRTABLE_entry {
    struct STRTABLE_entry   *next;
    UV                      hash;

    /* Following two fields represent a key.
     * But in order to avoid copying and storing strings
     * inside STRTABLE_entry offset inside STRTABLE->buf
     * is stored */

    STRLEN                  len;
    UV                      str_offset;

    /* Value of a key is offset inside STRTABLE->buf
     * where tag (STR_UTF8|BINARY|SHORT_BINARY) is located */

    UV                      tag_offset;
};

struct STRTABLE_arena {
    struct STRTABLE_arena   *next;
    struct STRTABLE_entry   array[1023/5]; /* as STRTABLE_entry has 1 pointer and 4 intergers */
};

struct STRTABLE {
    struct STRTABLE_entry   **tbl_ary;
    UV                      tbl_max;
    UV                      tbl_items;
    struct STRTABLE_arena   *tbl_arena;
    struct STRTABLE_entry   *tbl_arena_next;
    struct STRTABLE_entry   *tbl_arena_end;
    const srl_buffer_t      *buf;
};

STATIC STRTABLE_t * STRTABLE_new(const srl_buffer_t *buf);
STATIC STRTABLE_t * STRTABLE_new_size(const srl_buffer_t *buf, const U8 size_base2_exponent);
inline STATIC STRTABLE_ENTRY_t * STRTABLE_insert(STRTABLE_t *tbl, const char *str, STRLEN len, UV offset, int *ok);

STATIC void STRTABLE_grow(STRTABLE_t *tbl);
STATIC void STRTABLE_clear(STRTABLE_t *tbl);
STATIC void STRTABLE_free(STRTABLE_t *tbl);

/* create a new pointer => pointer table */
SRL_STATIC_INLINE STRTABLE_t *
STRTABLE_new(const srl_buffer_t *buf)
{
    return STRTABLE_new_size(buf, 9);
}

STATIC STRTABLE_t *
STRTABLE_new_size(const srl_buffer_t *buf, const U8 size_base2_exponent)
{
    STRTABLE_t *tbl;
    Newxz(tbl, 1, STRTABLE_t);

    tbl->buf = buf;
    tbl->tbl_max = (1 << size_base2_exponent) - 1;
    tbl->tbl_items      = 0;
    tbl->tbl_arena      = NULL;
    tbl->tbl_arena_next = NULL;
    tbl->tbl_arena_end  = NULL;

    Newxz(tbl->tbl_ary, tbl->tbl_max + 1, STRTABLE_ENTRY_t*);
    return tbl;
}

/* lookup key, return if found, otherwise store */
inline STATIC STRTABLE_ENTRY_t *
STRTABLE_insert(STRTABLE_t *tbl, const char *str, STRLEN len, UV offset, int *ok)
{
    STRTABLE_ENTRY_t *tblent;
    const UV hash = STRTABLE_HASH(str, len);
    *ok = 0;

    tblent = tbl->tbl_ary[hash & tbl->tbl_max];
    for (; tblent; tblent = tblent->next) {
        STRTABLE_ASSERT_ENTRY_STRICT(tbl, tblent);

        if (   tblent->hash == hash
            && tblent->len == len
            && strncmp(STRTABLE_ENTRY_STR(tbl, tblent), str, len) == 0
        ) {
            *ok = 1;
            return tblent;
        }
    }

    // didn't found record, tblent == NULL
    assert(tblent == NULL);

    if (tbl->tbl_arena_next == tbl->tbl_arena_end) {
       struct STRTABLE_arena *new_arena;
       Newx(new_arena, 1, struct STRTABLE_arena);
       new_arena->next = tbl->tbl_arena;

       tbl->tbl_arena = new_arena;
       tbl->tbl_arena_next = new_arena->array;
       tbl->tbl_arena_end = new_arena->array + sizeof(new_arena->array) / sizeof(new_arena->array[0]);
    }

    const UV entry = hash & tbl->tbl_max;
    tblent = tbl->tbl_arena_next++;

    tblent->len = len;
    tblent->hash = hash;
    tblent->tag_offset = offset;
    tblent->next = tbl->tbl_ary[entry];

    /* since actual location of the string inside tbl->buf (output buffer)
     * is not known yet, let str_offset be zero allowing the calee
     * to fill it in later */
    tblent->str_offset = 0;

    tbl->tbl_ary[entry] = tblent;
    tbl->tbl_items++;

    if (tblent->next && (tbl->tbl_items > tbl->tbl_max))
        STRTABLE_grow(tbl);

    STRTABLE_ASSERT_ENTRY(tbl, tblent);
    return tblent;
}

/* double the hash bucket size of an existing ptr table */

STATIC void
STRTABLE_grow(STRTABLE_t *tbl)
{
    STRTABLE_ENTRY_t **ary = tbl->tbl_ary;
    const UV oldsize = tbl->tbl_max + 1;
    UV newsize = oldsize * 2;
    UV i;

    Renew(ary, newsize, STRTABLE_ENTRY_t*);
    Zero(&ary[oldsize], newsize - oldsize, STRTABLE_ENTRY_t*);
    tbl->tbl_max = --newsize;
    tbl->tbl_ary = ary;

    for (i = 0; i < oldsize; i++, ary++) {
        STRTABLE_ENTRY_t **curentp, **entp, *ent;
        if (!*ary)
            continue;
        curentp = ary + oldsize;
        for (entp = ary, ent = *ary; ent; ent = *entp) {
            if ((newsize & STRTABLE_HASH_FROM_ENTRY(tbl, ent)) != i) {
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
STRTABLE_clear(STRTABLE_t *tbl)
{
    if (tbl && tbl->tbl_items) {
        struct STRTABLE_arena *arena = tbl->tbl_arena;

        Zero(tbl->tbl_ary, tbl->tbl_max + 1, struct STRTABLE_arena **);

        while (arena) {
            struct STRTABLE_arena *next = arena->next;

            Safefree(arena);
            arena = next;
        };

        tbl->tbl_items = 0;
        tbl->tbl_arena = NULL;
        tbl->tbl_arena_next = NULL;
        tbl->tbl_arena_end = NULL;
    }
}

/* clear and free a ptr table */

STATIC void
STRTABLE_free(STRTABLE_t *tbl)
{
    struct STRTABLE_arena *arena;
    if (!tbl) return;

    arena = tbl->tbl_arena;

    while (arena) {
        struct STRTABLE_arena *next = arena->next;
        Safefree(arena);
        arena = next;
    }

    Safefree(tbl->tbl_ary);
    Safefree(tbl);
}

#endif
