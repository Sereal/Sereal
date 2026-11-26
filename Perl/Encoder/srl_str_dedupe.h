#ifndef SRL_STR_DEDUPE_H_
#define SRL_STR_DEDUPE_H_

/*
 * Single open-addressed C hash table for Sereal::Encoder string deduplication
 * (dedupe_strings and aliased_dedupe_strings).
 *
 * Design notes:
 * - Stores buffer offsets relative to enc->buf.body_pos rather than copying
 *   string bytes or interning into Perl's PL_strtab via HV/HEK.
 * - Because offsets are resolved dynamically as (buf->body_pos + ofs), output
 *   buffer reallocations (Renew) never invalidate table entries.
 * - Every occupied slot has copy_ofs != 0 (V1 body offsets are >= 6 after the
 *   header; V2+ body and user-header offsets are >= 1 because body_pos is set
 *   to start_of_body - 1). Thus copy_ofs == 0 is a sound empty-slot sentinel.
 * - Identical raw bytes with differing SvUTF8 flags are distinguished in the
 *   mixed hash, in slot->flags, and by the wire tag (SRL_HDR_STR_UTF8 vs
 *   SRL_HDR_SHORT_BINARY / SRL_HDR_BINARY), so binary and UTF-8 strings never
 *   collide.
 */

#include <string.h>
#include "assert.h"
#include "srl_inline.h"
#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_buffer_types.h"

#ifndef IS_SRL_HDR_SHORT_BINARY
#define IS_SRL_HDR_SHORT_BINARY(tag) \
    (((tag) >= SRL_HDR_SHORT_BINARY_LOW) && ((tag) <= SRL_HDR_SHORT_BINARY_HIGH))
#endif

#define SRL_STR_DEDUPE_INIT_CAP     128U
#define SRL_STR_DEDUPE_SHRINK_CAP   4096U
#define SRL_STR_DEDUPE_FLAG_UTF8    1U

typedef struct {
    UV  copy_ofs;   /* 0 = empty slot; >= 1 = offset of literal string in body */
    UV  alias_ofs;  /* 0 = no alias target yet (e.g. first seen as hash key) */
    U32 hash;       /* PERL_HASH mixed with UTF-8 flag */
    U32 flags;      /* bit 0: SRL_STR_DEDUPE_FLAG_UTF8 */
} srl_str_dedupe_slot_t;

typedef struct {
    srl_str_dedupe_slot_t *slots;
    UV cap;         /* always a power of 2 when allocated */
    UV mask;        /* cap - 1 */
    UV items;       /* number of occupied slots */
} srl_str_dedupe_tbl_t;

SRL_STATIC_INLINE U32
srl_str_dedupe_hash(const char *str, STRLEN len, U32 utf8_flag)
{
    U32 h;
    PERL_HASH(h, str ? str : "", len);
    return h ^ (utf8_flag ? 0x9e3779b9U : 0U);
}

/* Decode an unsigned LEB128 length varint previously written into enc->buf,
 * bounded by [p, end) and guarded against STRLEN shift overflow on both
 * 32-bit and 64-bit platforms. Returns 1 on success, 0 on malformed input. */
SRL_STATIC_INLINE int
srl_str_dedupe_read_varint(const srl_buffer_char *p,
                           const srl_buffer_char *end,
                           STRLEN *out_len,
                           const srl_buffer_char **out_next)
{
    STRLEN val = 0;
    unsigned int shift = 0;
    const unsigned int max_shift = (unsigned int)(sizeof(STRLEN) * 8);

    while (p < end) {
        U8 b = (U8)*p++;
        U8 low = (U8)(b & 0x7FU);
        if (expect_false(shift >= max_shift))
            return 0;
        if (expect_false(shift + 7 > max_shift
                         && (low >> (max_shift - shift)) != 0))
            return 0;
        val |= ((STRLEN)low << shift);
        if (expect_true(!(b & 0x80U))) {
            *out_len = val;
            *out_next = p;
            return 1;
        }
        shift += 7;
    }
    return 0;
}

/* Compare candidate (ptr, len, hash, utf8_flag) against the literal string
 * stored in buf at (buf->body_pos + slot->copy_ofs). */
SRL_STATIC_INLINE int
srl_str_dedupe_slot_matches(const srl_buffer_t *buf,
                            const srl_str_dedupe_slot_t *slot,
                            const char *ptr, STRLEN len,
                            U32 hash, U32 utf8_flag)
{
    const srl_buffer_char *entry;
    const srl_buffer_char *end;
    const srl_buffer_char *stored_ptr;
    STRLEN stored_len;
    U8 tag;

    if (slot->hash != hash || slot->flags != utf8_flag)
        return 0;

    if (expect_false(buf->pos <= buf->body_pos
                     || slot->copy_ofs >= (UV)(buf->pos - buf->body_pos)))
        return 0;

    entry = buf->body_pos + slot->copy_ofs;
    end   = buf->pos;
    tag   = (U8)(*(const U8 *)entry) & (U8)~SRL_HDR_TRACK_FLAG;

    if (utf8_flag) {
        if (expect_false(tag != SRL_HDR_STR_UTF8))
            return 0;
        if (expect_false(!srl_str_dedupe_read_varint(entry + 1, end,
                                                     &stored_len, &stored_ptr)))
            return 0;
    } else if (IS_SRL_HDR_SHORT_BINARY(tag)) {
        stored_len = (STRLEN)SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        stored_ptr = entry + 1;
    } else if (expect_true(tag == SRL_HDR_BINARY)) {
        if (expect_false(!srl_str_dedupe_read_varint(entry + 1, end,
                                                     &stored_len, &stored_ptr)))
            return 0;
    } else {
        return 0;
    }

    if (stored_len != len)
        return 0;
    if (expect_false((size_t)(end - stored_ptr) < (size_t)len))
        return 0;

    return (len == 0) || (memcmp(stored_ptr, ptr, len) == 0);
}

SRL_STATIC_INLINE void
srl_str_dedupe_init(pTHX_ srl_str_dedupe_tbl_t *tbl, UV cap)
{
    assert(cap >= 4 && (cap & (cap - 1)) == 0);
    if (expect_false(cap > (~(size_t)0) / sizeof(srl_str_dedupe_slot_t)))
        croak("String deduplication table size overflow");
    Newxz(tbl->slots, cap, srl_str_dedupe_slot_t);
    if (expect_false(tbl->slots == NULL))
        croak("Out of memory");
    tbl->cap = cap;
    tbl->mask = cap - 1;
    tbl->items = 0;
}

SRL_STATIC_INLINE void
srl_str_dedupe_free(pTHX_ srl_str_dedupe_tbl_t *tbl)
{
    PERL_UNUSED_CONTEXT;
    if (tbl->slots != NULL) {
        Safefree(tbl->slots);
        tbl->slots = NULL;
    }
    tbl->cap = 0;
    tbl->mask = 0;
    tbl->items = 0;
}

/* Reset the table between encodes (or between header data and body).
 * If a reusable encoder previously grew past SRL_STR_DEDUPE_SHRINK_CAP and
 * the most recent encode used < 12.5% of capacity, release the oversized
 * array without allocating (safe during SAVEDESTRUCTOR_X cleanup); the next
 * lookup will lazily allocate SRL_STR_DEDUPE_INIT_CAP slots on demand. */
SRL_STATIC_INLINE void
srl_str_dedupe_clear(pTHX_ srl_str_dedupe_tbl_t *tbl)
{
    PERL_UNUSED_CONTEXT;
    if (tbl->slots == NULL)
        return;

    if (tbl->cap > SRL_STR_DEDUPE_SHRINK_CAP
        && tbl->items < (tbl->cap >> 3))
    {
        srl_str_dedupe_free(aTHX_ tbl);
        return;
    }

    if (tbl->items > 0) {
        Zero(tbl->slots, tbl->cap, srl_str_dedupe_slot_t);
        tbl->items = 0;
    }
}

SRL_STATIC_INLINE void
srl_str_dedupe_grow(pTHX_ srl_str_dedupe_tbl_t *tbl)
{
    UV old_cap = tbl->cap;
    srl_str_dedupe_slot_t *old_slots = tbl->slots;
    srl_str_dedupe_slot_t *new_slots;
    UV new_cap;
    UV new_mask;
    UV i;

    if (expect_false(old_cap > ((~(UV)0) >> 1)))
        croak("String deduplication table size overflow");
    new_cap = old_cap << 1;
    if (expect_false(new_cap > (~(size_t)0) / sizeof(srl_str_dedupe_slot_t)))
        croak("String deduplication table size overflow");

    new_mask = new_cap - 1;
    Newxz(new_slots, new_cap, srl_str_dedupe_slot_t);
    if (expect_false(new_slots == NULL))
        croak("Out of memory");

    for (i = 0; i < old_cap; i++) {
        if (old_slots[i].copy_ofs != 0) {
            UV idx = (UV)old_slots[i].hash & new_mask;
            while (new_slots[idx].copy_ofs != 0)
                idx = (idx + 1) & new_mask;
            new_slots[idx] = old_slots[i];
        }
    }

    Safefree(old_slots);
    tbl->slots = new_slots;
    tbl->cap = new_cap;
    tbl->mask = new_mask;
}

/* Look up (str, len, is_utf8) in tbl, inserting a new slot with
 * copy_ofs = current_ofs if absent. Sets *found = 1 on hit, 0 on insert. */
SRL_STATIC_INLINE srl_str_dedupe_slot_t *
srl_str_dedupe_lookup_or_insert(pTHX_ srl_str_dedupe_tbl_t *tbl,
                                const srl_buffer_t *buf,
                                const char *str, STRLEN len, U32 is_utf8,
                                UV current_ofs, int *found)
{
    U32 utf8_flag = is_utf8 ? SRL_STR_DEDUPE_FLAG_UTF8 : 0U;
    U32 hash = srl_str_dedupe_hash(str, len, utf8_flag);
    UV mask;
    UV idx;
    srl_str_dedupe_slot_t *slot;

    assert(current_ofs != 0);

    if (expect_false(tbl->slots == NULL))
        srl_str_dedupe_init(aTHX_ tbl, SRL_STR_DEDUPE_INIT_CAP);

    mask = tbl->mask;
    idx = (UV)hash & mask;
    for (;;) {
        slot = &tbl->slots[idx];
        if (slot->copy_ofs == 0)
            break;
        if (srl_str_dedupe_slot_matches(buf, slot, str, len, hash, utf8_flag)) {
            *found = 1;
            return slot;
        }
        idx = (idx + 1) & mask;
    }

    /* Keep load factor <= 75% (items < cap * 3 / 4) without integer overflow. */
    if (expect_false(tbl->items >= tbl->cap - (tbl->cap >> 2))) {
        srl_str_dedupe_grow(aTHX_ tbl);
        mask = tbl->mask;
        idx = (UV)hash & mask;
        while (tbl->slots[idx].copy_ofs != 0)
            idx = (idx + 1) & mask;
        slot = &tbl->slots[idx];
    }

    slot->copy_ofs  = current_ofs;
    slot->alias_ofs = 0;
    slot->hash      = hash;
    slot->flags     = utf8_flag;
    tbl->items++;
    *found = 0;
    return slot;
}

#endif
