#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

#include "assert.h"

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_buffer_types.h"

/* The static's below plus the ifndef sort of make this header only
 * usable in one place per compilation unit. Drop "static" when necessary.
 * For now, potentially smaller code wins. */

/* buffer operations */
#define BUF_POS_OFS(buf) (((buf)->pos) - ((buf)->start))
#define BUF_SPACE(buf) (((buf)->end) - ((buf)->pos))
#define BUF_SIZE(buf) (((buf)->end) - ((buf)->start))
#define BUF_NEED_GROW(buf, minlen) ((size_t)BUF_SPACE(buf) <= minlen)
#define BUF_NEED_GROW_TOTAL(buf, minlen) ((size_t)BUF_SIZE(buf) <= minlen)
#define BUF_NOT_DONE(buf) ((buf)->pos < (buf)->end)
#define BUF_DONE(buf) ((buf)->pos >= (buf)->end)

/* body-position/size related operations */
#define BODY_POS_OFS(buf) (((buf)->pos) - ((buf)->body_pos))

/* these are mostly for right between (de)serializing the header and the body */
#define SRL_SET_BODY_POS(buf, pos_ptr) ((buf)->body_pos = pos_ptr)
#define SRL_UPDATE_BODY_POS(buf, protocol_version)                          \
    STMT_START {                                                            \
        if (expect_false((protocol_version) == 1)) {                        \
            SRL_SET_BODY_POS((buf), (buf)->start);                          \
        } else {                                                            \
            SRL_SET_BODY_POS((buf), (buf)->pos-1);                          \
        }                                                                   \
    } STMT_END

/* Internal debugging macros, used only in DEBUG mode */
#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SPACE(buf, len) STMT_START {                       \
    if((BUF_SPACE(buf) < (ptrdiff_t)(len))) {                               \
        warn("failed assertion check - pos: %ld [%p %p %p] %ld < %ld",      \
                (long)BUF_POS_OFS(buf), (buf)->start,                       \
                (buf)->pos, (buf)->end,                                     \
                (long)BUF_SPACE(buf),(long)(len));                          \
    }                                                                       \
    assert(BUF_SPACE(buf) >= (ptrdiff_t)(len));                             \
} STMT_END
#else
#define DEBUG_ASSERT_BUF_SPACE(buf, len) ((void)0)
#endif

#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SANE(buf) STMT_START {                             \
    if(!(((buf)->start <= (buf)->pos) && ((buf)->pos <= (buf)->end))){      \
        warn("failed sanity assertion check - pos: %ld [%p %p %p] %ld",     \
                (long)BUF_POS_OFS(buf), (buf)->start,                       \
                (buf)->pos, (buf)->end, (long)BUF_SPACE(buf));              \
    }                                                                       \
    assert(((buf)->start <= (buf)->pos) && ((buf)->pos <= (buf)->end));     \
} STMT_END
#else
#define DEBUG_ASSERT_BUF_SANE(buf)                                          \
    assert(((buf)->start <= (buf)->pos) && ((buf)->pos <= (buf)->end))
#endif

/* Allocate a virgin buffer (but not the buffer struct) */
SRL_STATIC_INLINE int
srl_buf_init_buffer(pTHX_ srl_buffer_t *buf, const STRLEN init_size)
{
    Newx(buf->start, init_size, srl_buffer_char);
    if (expect_false( buf->start == NULL ))
        return 1;
    buf->end = buf->start + init_size - 1;
    buf->pos = buf->start;
    buf->body_pos = buf->start; /* SRL_SET_BODY_POS(enc, enc->buf.start) equiv */
    return 0;
}

/* Free a buffer (but not the buffer struct) */
SRL_STATIC_INLINE void
srl_buf_free_buffer(pTHX_ srl_buffer_t *buf)
{
    Safefree(buf->start);
}

/* Copy one buffer to another (shallowly!) */
SRL_STATIC_INLINE void
srl_buf_copy_buffer(pTHX_ srl_buffer_t *src, srl_buffer_t *dest)
{
    Copy(src, dest, 1, srl_buffer_t);
}

/* Swap two buffers */
SRL_STATIC_INLINE void
srl_buf_swap_buffer(pTHX_ srl_buffer_t *buf1, srl_buffer_t *buf2)
{
    srl_buffer_t tmp;
    Copy(buf1, &tmp, 1, srl_buffer_t);
    Copy(buf2, buf1, 1, srl_buffer_t);
    Copy(&tmp, buf2, 1, srl_buffer_t);
}

/* old_size + (new_size / 4) */
#define OVERALLOC_FUNC(cur_size,req_size) (cur_size + (req_size >> 2))

SRL_STATIC_INLINE void
srl_buf_grow_nocheck(pTHX_ srl_buffer_t *buf, const size_t minlen)
{
    const size_t pos_ofs= BUF_POS_OFS(buf); /* have to store the offset of pos */
    const size_t body_ofs= buf->body_pos - buf->start; /* have to store the offset of the body */
#ifdef MEMDEBUG
    const size_t new_size = minlen;
#else
    const size_t cur_size = BUF_SIZE(buf);
    const size_t tmp_size = OVERALLOC_FUNC(cur_size, minlen);
    const size_t new_size = (minlen > tmp_size) ? minlen : tmp_size;
#endif

    DEBUG_ASSERT_BUF_SANE(buf);
    /* assert that Renew means GROWING the buffer */
    assert(buf->start + new_size > buf->end);

    Renew(buf->start, new_size, srl_buffer_char);
    if (buf->start == NULL)
        croak("Out of memory!");
    if (0) warn("Renew(%ld)", new_size);

    buf->end = (srl_buffer_char*) (buf->start + new_size);
    buf->pos = buf->start + pos_ofs;
    SRL_SET_BODY_POS(buf, buf->start + body_ofs);

    DEBUG_ASSERT_BUF_SANE(buf);
    assert(buf->end - buf->start > (ptrdiff_t)0);
    assert(buf->pos - buf->start >= (ptrdiff_t)0);
    /* The following is checking against -1 because SRL_UPDATE_BODY_POS
     * will actually set the body_pos to pos-1, where pos can be 0.
     * This works out fine in the end, but is admittedly a bit shady.
     * FIXME */
    assert(buf->body_pos - buf->start >= (ptrdiff_t)-1);
}

#define BUF_SIZE_ASSERT(buf, minlen)                                    \
  STMT_START {                                                          \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
    if (BUF_NEED_GROW(buf, minlen))                                     \
      srl_buf_grow_nocheck(aTHX_ (buf), (BUF_SIZE(buf) + minlen));      \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
  } STMT_END

#define BUF_SIZE_ASSERT_TOTAL(buf, minlen)                              \
  STMT_START {                                                          \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
    if (BUF_NEED_GROW_TOTAL(buf, minlen))                               \
      srl_buf_grow_nocheck(aTHX_ (buf), (minlen));                      \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
  } STMT_END

SRL_STATIC_INLINE void
srl_buf_cat_str_int(pTHX_ srl_buffer_t *buf, const char *str, size_t len)
{
    BUF_SIZE_ASSERT(buf, len);
    Copy(str, buf->pos, len, char);
    buf->pos += len;
    DEBUG_ASSERT_BUF_SANE(buf);
}
#define srl_buf_cat_str(buf, str, len) srl_buf_cat_str_int(aTHX_ buf, str, len)
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s(buf, str) srl_buf_cat_str(buf, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_str_nocheck_int(pTHX_ srl_buffer_t *buf, const char *str, size_t len)
{
    DEBUG_ASSERT_BUF_SANE(buf);
    DEBUG_ASSERT_BUF_SPACE(buf, len);
    Copy(str, buf->pos, len, char);
    buf->pos += len;
    DEBUG_ASSERT_BUF_SANE(buf);
}
#define srl_buf_cat_str_nocheck(buf, str, len) srl_buf_cat_str_nocheck_int(aTHX_ buf, str, len)
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s_nocheck(buf, str) srl_buf_cat_str_nocheck(buf, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_char_int(pTHX_ srl_buffer_t *buf, const char c)
{
    DEBUG_ASSERT_BUF_SANE(buf);
    BUF_SIZE_ASSERT(buf, 1);
    DEBUG_ASSERT_BUF_SPACE(buf, 1);
    *buf->pos++ = c;
    DEBUG_ASSERT_BUF_SANE(buf);
}
#define srl_buf_cat_char(buf, c) srl_buf_cat_char_int(aTHX_ buf, c)

SRL_STATIC_INLINE void
srl_buf_cat_char_nocheck_int(pTHX_ srl_buffer_t *buf, const char c)
{
    DEBUG_ASSERT_BUF_SANE(buf);
    DEBUG_ASSERT_BUF_SPACE(buf, 1);
    *buf->pos++ = c;
    DEBUG_ASSERT_BUF_SANE(buf);
}
#define srl_buf_cat_char_nocheck(buf, c) srl_buf_cat_char_nocheck_int(aTHX_ buf, c)

/*
 * This implements "varint" and "zigzag varint" types as used in protobufs, etc.
 *
 * varint is a variable length encoding of unsigned integers, where the low
 * 7 bits of the input value are encoded into each byte of output, with the high bit
 * used as a flag to indicate there is another byte worth of bits to be read.
 *
 * zigzag is a way of encoding signed integers as an unsigned integer in such a way
 * that positive and negative numbers are interleaved, so that z0=0, z1=-1, z2=1,
 * z3=-2, z4=2, etc. When the zigzag form is represented as a varint, the result is
 * that both negative and positive number take space proportional to their distance
 * from zero.
 *
 * see: https://developers.google.com/protocol-buffers/docs/encoding#types
 *
 */
#define srl_varint_size(x) (    \
    z <= (1UL << 7)  ? 1 :    \
    z <= (1UL << 14) ? 2 :    \
    z <= (1UL << 21) ? 3 :    \
    z <= (1UL << 28) ? 4 :    \
    z <= (1UL << 35) ? 5 :    \
    z <= (1UL << 42) ? 6 :    \
    z <= (1UL << 49) ? 7 :    \
    z <= (1UL << 56) ? 8 :    \
    z <= (1UL << 63) ? 9 :    \
                     10 )


SRL_STATIC_INLINE void
srl_buf_cat_varint_raw_nocheck(pTHX_ srl_buffer_t *buf, UV value) {
    DEBUG_ASSERT_BUF_SANE(buf);
    DEBUG_ASSERT_BUF_SPACE(buf, SRL_MAX_VARINT_LENGTH);
    while (value >= 0x80) {                     /* while we are larger than 7 bits long */
        *buf->pos++ = (value & 0x7f) | 0x80;    /* write out the least significant 7 bits, set the high bit */
        value >>= 7;                            /* shift off the 7 least significant bits */
    }
    *buf->pos++ = (U8)value;                    /* encode the last 7 bits without the high bit being set */
    DEBUG_ASSERT_BUF_SANE(buf);
}

SRL_STATIC_INLINE UV
srl_zigzag_iv(IV value) {
    return (UV)((value << 1) ^ (value >> (sizeof(IV) * 8 - 1)));
}

SRL_STATIC_INLINE void
srl_buf_cat_zigzag_raw_nocheck(pTHX_ srl_buffer_t *buf, const IV value) {
    srl_buf_cat_varint_raw_nocheck(aTHX_ buf, srl_zigzag_iv(value));
}

SRL_STATIC_INLINE void
srl_buf_cat_varint_nocheck(pTHX_ srl_buffer_t *buf, const char tag, UV value) {
    DEBUG_ASSERT_BUF_SPACE(buf, 1);
    if (expect_true( tag ))
        *buf->pos++ = tag;
    srl_buf_cat_varint_raw_nocheck(aTHX_ buf, value);
}

SRL_STATIC_INLINE void
srl_buf_cat_zigzag_nocheck(pTHX_ srl_buffer_t *buf, const char tag, const IV value) {
    srl_buf_cat_varint_nocheck(aTHX_ buf, tag, srl_zigzag_iv(value));
}

SRL_STATIC_INLINE void
srl_buf_cat_varint(pTHX_ srl_buffer_t *buf, const char tag, const UV value) {
    /* this implements "varint" from google protocol buffers */
    BUF_SIZE_ASSERT(buf, SRL_MAX_VARINT_LENGTH + 1); /* always allocate space for the tag, overalloc is harmless */
    srl_buf_cat_varint_nocheck(aTHX_ buf, tag, value);
}

SRL_STATIC_INLINE void
srl_buf_cat_zigzag(pTHX_ srl_buffer_t *buf, const char tag, const IV value) {
    srl_buf_cat_varint(aTHX_ buf, tag, srl_zigzag_iv(value));
}

#endif
