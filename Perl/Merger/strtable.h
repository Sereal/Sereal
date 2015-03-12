/* Taken from Chocolateboy's autobox module. License same as perl's and
 * this same as this module's license.
 */

/*
 * This is a customized version of the pointer table implementation in sv.c
 * The hash functions are taken from hv_func.h
 */

#ifndef STRTABLE_H_
#define STRTABLE_H_

#include <assert.h>
#include <limits.h>
#include <string.h>
#include "ppport.h"
#include "srl_inline.h"
#include "srl_buffer_types.h"

#ifndef PERL_HASH_FUNC_MURMUR_HASH_64A
/* This code is from Austin Appleby and is in the public domain.
   Altered by Yves Orton to match Perl's hash interface, and to
   return a 32 bit hash.

   Note uses unaligned 64 bit loads - will NOT work on machines with
   strict alginment requirements.

   Also this code may not be suitable for big-endian machines.
*/

/* a 64 bit hash where we only use the low 32 bits */
PERL_STATIC_INLINE U32
S_perl_hash_murmur_hash_64a (const unsigned char * const seed, const unsigned char *str, const STRLEN len)
{
        const U64TYPE m = 0xc6a4a7935bd1e995;
        const int r = 47;
        U64TYPE h = *((U64TYPE*)seed) ^ len;
        const U64TYPE * data = (const U64TYPE *)str;
        const U64TYPE * end = data + (len/8);
        const unsigned char * data2;

        while(data != end)
        {
            U64TYPE k = *data++;

            k *= m;
            k ^= k >> r;
            k *= m;

            h ^= k;
            h *= m;
        }

        data2 = (const unsigned char *)data;

        switch(len & 7)
        {
            case 7: h ^= (U64TYPE)(data2[6]) << 48; /* fallthrough */
            case 6: h ^= (U64TYPE)(data2[5]) << 40; /* fallthrough */
            case 5: h ^= (U64TYPE)(data2[4]) << 32; /* fallthrough */
            case 4: h ^= (U64TYPE)(data2[3]) << 24; /* fallthrough */
            case 3: h ^= (U64TYPE)(data2[2]) << 16; /* fallthrough */
            case 2: h ^= (U64TYPE)(data2[1]) << 8;  /* fallthrough */
            case 1: h ^= (U64TYPE)(data2[0]);       /* fallthrough */
                    h *= m;
        };

        h ^= h >> r;
        h *= m;
        h ^= h >> r;

        /* was: return h; */
        return h & 0xFFFFFFFF;
}
#endif

#ifndef PERL_HASH_FUNC_MURMUR_HASH_64B
/* This code is from Austin Appleby and is in the public domain.
   Altered by Yves Orton to match Perl's hash interface and return
   a 32 bit value

   Note uses unaligned 32 bit loads - will NOT work on machines with
   strict alginment requirements.

   Also this code may not be suitable for big-endian machines.
*/

/* a 64-bit hash for 32-bit platforms where we only use the low 32 bits */
PERL_STATIC_INLINE U32
S_perl_hash_murmur_hash_64b (const unsigned char * const seed, const unsigned char *str, STRLEN len)
{
        const U32 m = 0x5bd1e995;
        const int r = 24;

        U32 h1 = ((U32 *)seed)[0] ^ len;
        U32 h2 = ((U32 *)seed)[1];

        const U32 * data = (const U32 *)str;

        while(len >= 8)
        {
            U32 k1, k2;
            k1 = *data++;
            k1 *= m; k1 ^= k1 >> r; k1 *= m;
            h1 *= m; h1 ^= k1;
            len -= 4;

            k2 = *data++;
            k2 *= m; k2 ^= k2 >> r; k2 *= m;
            h2 *= m; h2 ^= k2;
            len -= 4;
        }

        if(len >= 4)
        {
            U32 k1 = *data++;
            k1 *= m; k1 ^= k1 >> r; k1 *= m;
            h1 *= m; h1 ^= k1;
            len -= 4;
        }

        switch(len)
        {
            case 3: h2 ^= ((unsigned char*)data)[2] << 16;  /* fallthrough */
            case 2: h2 ^= ((unsigned char*)data)[1] << 8;   /* fallthrough */
            case 1: h2 ^= ((unsigned char*)data)[0];        /* fallthrough */
                    h2 *= m;
        };

        h1 ^= h2 >> 18; h1 *= m;
        h2 ^= h1 >> 22; h2 *= m;
        /*
        The following code has been removed as it is unused
        when only the low 32 bits are used. -- Yves

        h1 ^= h2 >> 17; h1 *= m;

        U64TYPE h = h1;

        h = (h << 32) | h2;
        */

        return h2;
}
#endif

/* hash function has to return 4 bytes long value i.e. U32 */

#if PTRSIZE == 8
#   define PERL_HASH_SEED_BYTES 8
#   define STRTABLE_HASH(str, len) S_perl_hash_murmur_hash_64a(PERL_HASH_SEED, (U8*) (str), (len))
#else
#   define PERL_HASH_SEED_BYTES 8
#   define STRTABLE_HASH(str, len) S_perl_hash_murmur_hash_64b(PERL_HASH_SEED, (U8*) (str), (len))
#endif

#define STRTABLE_MAX_STR_SIZE 0xFFFFFFFF
#define STRTABLE_ENTRY_STR(tbl, ent) ((tbl)->buf->body_pos + (ent)->offset)

#define STRTABLE_ASSERT_ENTRY(tbl, ent) STMT_START {                      \
    assert((ent) != NULL);                                                \
    assert((tbl)->buf->body_pos <= STRTABLE_ENTRY_STR((tbl), (ent)));     \
    assert((tbl)->buf->end      >= STRTABLE_ENTRY_STR((tbl), (ent)));     \
} STMT_END

#define STRTABLE_ASSERT_ENTRY_STR(tbl, ent, str, len) STMT_START {                         \
    assert((ent)->length == (len));                                                        \
    assert((ent)->hash == STRTABLE_HASH((str), (len)));                                    \
    assert(strncmp((char *) STRTABLE_ENTRY_STR((tbl), (ent)), (char*) (str), (len)) == 0); \
} STMT_END

typedef struct STRTABLE         STRTABLE_t;
typedef struct STRTABLE       * strtable_ptr;
typedef struct STRTABLE_entry   STRTABLE_ENTRY_t;
typedef struct STRTABLE_entry * strtable_entry_ptr;

struct STRTABLE_entry {
    struct STRTABLE_entry   *next;

    U32                     hash;

    /* length of string at offset inside tbl->buf.
     * Limit to 4 bytes to get more compact struct */
    U32                     length;

    /* offset inside STRTABLE->buf
     * where tag (STR_UTF8|BINARY|SHORT_BINARY) is located */
    UV                      offset;
};

struct STRTABLE_arena {
    struct STRTABLE_arena   *next;
#if PTRSIZE == 8
    struct STRTABLE_entry   array[1023/3]; /* as STRTABLE_entry has 8B + 2*4B + 8B = 3*8B,
                                            * this magic math makes sure that STRTABLE_arena
                                            * fits inside two pages (8192 bytes) */
#else
    struct STRTABLE_entry   array[1023/5]; /* as STRTABLE_entry has 4B + 2*4B + 8B = 5*4B
                                            * this magic math make sure that STRTABLE_arena
                                            * fits inside one page (4096 bytes) */
#endif
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

SRL_STATIC_INLINE STRTABLE_t * STRTABLE_new(const srl_buffer_t *buf);
SRL_STATIC_INLINE STRTABLE_t * STRTABLE_new_size(const srl_buffer_t *buf, const U8 size_base2_exponent);

/* Caller has to fill offset field in returned STRTABLE_ENTRY_t.
 * Such approach shows better performance, BODY_POS_OFS() seems to be quite expensive
 * to calculate it on every call of STRTABLE_insert */
SRL_STATIC_INLINE STRTABLE_ENTRY_t * STRTABLE_insert(STRTABLE_t *tbl, const unsigned char *str, U32 len, int *ok);

SRL_STATIC_INLINE void STRTABLE_grow(STRTABLE_t *tbl);
SRL_STATIC_INLINE void STRTABLE_clear(STRTABLE_t *tbl);
SRL_STATIC_INLINE void STRTABLE_free(STRTABLE_t *tbl);

/* create a new pointer => pointer table */
SRL_STATIC_INLINE STRTABLE_t *
STRTABLE_new(const srl_buffer_t *buf)
{
    return STRTABLE_new_size(buf, 9);
}

SRL_STATIC_INLINE STRTABLE_t *
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
SRL_STATIC_INLINE STRTABLE_ENTRY_t *
STRTABLE_insert(STRTABLE_t *tbl, const unsigned char *str, U32 len, int *ok)
{
    UV entry;
    STRTABLE_ENTRY_t *tblent;
    const U32 hash = STRTABLE_HASH(str, len);

    assert(len <= STRTABLE_MAX_STR_SIZE);
    *ok = 0;

    tblent = tbl->tbl_ary[hash & tbl->tbl_max];
    for (; tblent; tblent = tblent->next) {
        STRTABLE_ASSERT_ENTRY(tbl, tblent);

        if (   tblent->hash == hash
            && tblent->length == len
            && strncmp((char*) STRTABLE_ENTRY_STR(tbl, tblent), (char*) str, len) == 0
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

    entry = hash & tbl->tbl_max;
    tblent = tbl->tbl_arena_next++;

    /* tblent->offset has to be set by caller,
     * but assign tag_offset and str_offset to invalid value
     * in order to suppress valgrind warnings about uninitalized memory */
    tblent->offset = (UV) -1;
    tblent->hash = hash;
    tblent->length = len;
    tblent->next = tbl->tbl_ary[entry];

    tbl->tbl_ary[entry] = tblent;
    tbl->tbl_items++;

    if (tblent->next && (tbl->tbl_items > tbl->tbl_max))
        STRTABLE_grow(tbl);

    return tblent;
}

/* double the hash bucket size of an existing ptr table */

SRL_STATIC_INLINE void
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
            if ((newsize & ent->hash) != i) {
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

SRL_STATIC_INLINE void
STRTABLE_clear(STRTABLE_t *tbl)
{
    if (tbl && tbl->tbl_items) {
        struct STRTABLE_arena *arena = tbl->tbl_arena;

        Zero(tbl->tbl_ary, tbl->tbl_max + 1, struct STRTABLE_entry **);

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

SRL_STATIC_INLINE void
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

SRL_STATIC_INLINE void
STRTABLE_purge(STRTABLE_t *tbl, UV offset)
{
    struct STRTABLE_arena *arena;
    struct STRTABLE_entry *tblent;
    struct STRTABLE_entry *tblent_prev;
    struct STRTABLE_entry *next, *entry, *arena_start;
    size_t arena_size = sizeof(arena->array) / sizeof(arena->array[0]);

    if (!tbl || !tbl->tbl_items)
        return;

    assert(tbl->tbl_arena_next >= tbl->tbl_arena->array);
    assert(tbl->tbl_arena_next <= tbl->tbl_arena->array + arena_size);
    assert(tbl->tbl_arena_end  == tbl->tbl_arena->array + arena_size);

    arena = tbl->tbl_arena;
    arena_start = &arena->array[0];

    next = tbl->tbl_arena_next;     // pointer to next available for usage element
    entry = next - 1;               // pointer to currently inspected element

    while (entry->offset >= offset) {
        //warn("entry->offset: %d", entry->offset);

        /* start of remove entry from hash buckets.
         * Hash buckets are single-linked lists,
         * so I need to scan the list in order to delete it.
         * But, due to the fact that I'm scanning arena in backward order
         * and elements are added always to head of the list, it's very
         * likely that the needed elemnt will be head of linked list */

        assert(tbl->tbl_items > 0);
        tbl->tbl_items--;

        tblent_prev = NULL;
        tblent = tbl->tbl_ary[entry->hash & tbl->tbl_max];

        for (; tblent; tblent_prev = tblent, tblent = tblent->next) {
            if (tblent == entry) {
                if (tblent_prev != NULL) {
                    tblent_prev->next = tblent->next;
                } else {
                    /* First entry in chain */
                    tbl->tbl_ary[entry->hash & tbl->tbl_max] = tblent->next;
                }
                break;
            }
        }
        /* end of remove entry from hash buckets */

        if (entry == arena_start) {
            //warn("entry == arena_start");

            // entry ptr reach start of arena.
            // If there is no arenas - our table is empty,
            // so, make first element in arena to be available
            // for further allocations
            struct STRTABLE_arena * next_arena = arena->next;
            if (!next_arena) {
                //warn("!next_arena");
                next = entry;
                break;
            }

            // Otherwise, move entry pointer to last item in next arena
            // Next iteration would free this arena and
            // adjust next pointer accordingly.
            // See comments for next if statemnt.
            entry = next_arena->array + arena_size - 1;
            --next;
        } else if (next == arena_start) {
            //warn("next == arena_start");
 
            // next pointer reached start of arena
            // meaning this arena is not used any more.
            // Free arena and move next to last element in next arena.
            // It's not possible and arena->next is NULL here,
            // because if so, we would already exit.
            // See entry == arena_start
            struct STRTABLE_arena *next_arena = arena->next;
            Safefree(arena);

            arena = next_arena;
            arena_start = &arena->array[0];

            next = arena->array + arena_size - 1;
            --entry;

            assert(entry == next - 1);
        } else {
            --entry;
            --next;

            assert(entry == next - 1);
        }
    }

    tbl->tbl_arena = arena;
    tbl->tbl_arena_next = next;
    tbl->tbl_arena_end = arena->array + arena_size;

    assert(tbl->tbl_arena_next >= tbl->tbl_arena->array);
    assert(tbl->tbl_arena_next <= tbl->tbl_arena->array + arena_size);
    assert(tbl->tbl_arena_end  == tbl->tbl_arena->array + arena_size);
}

#endif
