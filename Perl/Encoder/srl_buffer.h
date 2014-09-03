#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

#include "assert.h"

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_encoder.h"

#include "srl_buffer_types.h"

#ifdef MEMDEBUG
#   define BUFFER_GROWTH_FACTOR 1
#else
#   define BUFFER_GROWTH_FACTOR 2
#endif

/* The static's below plus the ifndef sort of make this header only
 * usable in one place per compilation unit. Drop "static" when necessary.
 * For now, potentially smaller code wins. */

/* buffer operations */
#define BUF_POS_OFS(buf) (((buf).pos) - ((buf).start))
#define BUF_SPACE(buf) (((buf).end) - ((buf).pos))
#define BUF_SIZE(buf) (((buf).end) - ((buf).start))
#define BUF_NEED_GROW(buf, minlen) ((size_t)BUF_SPACE(buf) <= minlen)
#define BUF_NEED_GROW_TOTAL(buf, minlen) ((size_t)BUF_SIZE(buf) <= minlen)
#define BUF_NOT_DONE(buf) ((buf).pos < (buf).end)

/* body-position/size related operations */
#define BODY_POS_OFS(buf) (((buf).pos) - ((buf).body_pos))

/* these are mostly for right between (de)serializing the header and the body */
#define SRL_SET_BODY_POS(enc, pos_ptr) ((enc)->buf.body_pos = pos_ptr)
#define SRL_UPDATE_BODY_POS(enc)                                            \
    STMT_START {                                                            \
        if (expect_false((enc)->protocol_version == 1)) {                   \
            SRL_SET_BODY_POS(enc, (enc)->buf.start);                        \
        } else {                                                            \
            SRL_SET_BODY_POS(enc, (enc)->buf.pos-1);                        \
        }                                                                   \
    } STMT_END

/* Internal debugging macros, used only in DEBUG mode */
#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SPACE(buf, len) STMT_START {                       \
    if((BUF_SPACE(buf) < (ptrdiff_t)(len))) {                               \
        warn("failed assertion check - pos: %ld [%p %p %p] %ld < %ld",      \
                (long)BUF_POS_OFS(buf), (buf).start,                        \
                (buf).pos, (buf).end,                                       \
                (long)BUF_SPACE(buf),(long)(len));                          \
    }                                                                       \
    assert(BUF_SPACE(buf) >= (ptrdiff_t)(len));                             \
} STMT_END
#else
#define DEBUG_ASSERT_BUF_SPACE(buf, len) ((void)0)
#endif

#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SANE(buf) STMT_START {                             \
    if(!(((buf).start <= (buf).pos) && ((buf).pos <= (buf).end))){          \
        warn("failed sanity assertion check - pos: %ld [%p %p %p] %ld",     \
                (long)BUF_POS_OFS(buf), (buf).start,                        \
                (buf).pos, (buf).end, (long)BUF_SPACE(buf));                \
    }                                                                       \
    assert(((buf).start <= (buf).pos) && ((buf).pos <= (buf).end));         \
} STMT_END
#else
#define DEBUG_ASSERT_BUF_SANE(buf)                                          \
    assert(((buf).start <= (buf).pos) && ((buf).pos <= (buf).end))
#endif

/* Allocate a virgin buffer (but not the buffer struct) */
SRL_STATIC_INLINE int
srl_buf_init_buffer(pTHX_ srl_buffer_t *buf, const STRLEN init_size)
{
    Newx(buf->start, init_size, char);
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

SRL_STATIC_INLINE void
srl_buf_grow_nocheck(pTHX_ srl_buffer_t *buf, size_t minlen)
{
    const size_t pos_ofs= BUF_POS_OFS(*buf); /* have to store the offset of pos */
    const size_t body_ofs= buf->body_pos - buf->start; /* have to store the offset of the body */
#ifdef MEMDEBUG
    const size_t new_size = minlen;
#else
    const size_t cur_size = BUF_SIZE(*buf);
    const size_t grown_len = (size_t)(cur_size * BUFFER_GROWTH_FACTOR);
    const size_t new_size = 100 + (minlen > grown_len ? minlen : grown_len);
#endif

    DEBUG_ASSERT_BUF_SANE(*buf);
    /* assert that Renew means GROWING the buffer */
    assert(buf->start + new_size > buf->end);

    Renew(buf->start, new_size, char);
    if (buf->start == NULL)
        croak("Out of memory!");

    buf->end = (char *)(buf->start + new_size);
    buf->pos = buf->start + pos_ofs;
    buf->body_pos = buf->start + body_ofs; /* SRL_SET_BODY_POS(enc, enc->buf.start + body_ofs); equiv */

    DEBUG_ASSERT_BUF_SANE(*buf);
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
      srl_buf_grow_nocheck(aTHX_ &(buf), (BUF_SIZE(buf) + minlen));     \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
  } STMT_END

#define BUF_SIZE_ASSERT_TOTAL(buf, minlen)                              \
  STMT_START {                                                          \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
    if (BUF_NEED_GROW_TOTAL(buf, minlen))                               \
      srl_buf_grow_nocheck(aTHX_ &(buf), (minlen));                     \
    DEBUG_ASSERT_BUF_SANE(buf);                                         \
  } STMT_END

SRL_STATIC_INLINE void
srl_buf_cat_str_int(pTHX_ srl_encoder_t *enc, const char *str, size_t len)
{
    BUF_SIZE_ASSERT(enc->buf, len);
    Copy(str, enc->buf.pos, len, char);
    enc->buf.pos += len;
    DEBUG_ASSERT_BUF_SANE(enc->buf);
}
#define srl_buf_cat_str(enc, str, len) srl_buf_cat_str_int(aTHX_ enc, str, len)
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s(enc, str) srl_buf_cat_str(enc, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_str_nocheck_int(pTHX_ srl_encoder_t *enc, const char *str, size_t len)
{
    DEBUG_ASSERT_BUF_SANE(enc->buf);
    DEBUG_ASSERT_BUF_SPACE(enc->buf, len);
    Copy(str, enc->buf.pos, len, char);
    enc->buf.pos += len;
    DEBUG_ASSERT_BUF_SANE(enc->buf);
}
#define srl_buf_cat_str_nocheck(enc, str, len) srl_buf_cat_str_nocheck_int(aTHX_ enc, str, len)
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s_nocheck(enc, str) srl_buf_cat_str_nocheck(enc, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_char_int(pTHX_ srl_encoder_t *enc, const char c)
{
    DEBUG_ASSERT_BUF_SANE(enc->buf);
    BUF_SIZE_ASSERT(enc->buf, 1);
    DEBUG_ASSERT_BUF_SPACE(enc->buf, 1);
    *enc->buf.pos++ = c;
    DEBUG_ASSERT_BUF_SANE(enc->buf);
}
#define srl_buf_cat_char(enc, c) srl_buf_cat_char_int(aTHX_ enc, c)

SRL_STATIC_INLINE void
srl_buf_cat_char_nocheck_int(pTHX_ srl_encoder_t *enc, const char c)
{
    DEBUG_ASSERT_BUF_SANE(enc->buf);
    DEBUG_ASSERT_BUF_SPACE(enc->buf, 1);
    *enc->buf.pos++ = c;
    DEBUG_ASSERT_BUF_SANE(enc->buf);
}
#define srl_buf_cat_char_nocheck(enc, c) srl_buf_cat_char_nocheck_int(aTHX_ enc, c)

/* define constant for other code to use in preallocations */
#define SRL_MAX_VARINT_LENGTH 11

SRL_STATIC_INLINE void
srl_buf_cat_varint_nocheck(pTHX_ srl_encoder_t *enc, const char tag, UV n) {
    DEBUG_ASSERT_BUF_SANE(enc->buf);
    DEBUG_ASSERT_BUF_SPACE(enc->buf, (tag==0 ? 0 : 1) + SRL_MAX_VARINT_LENGTH);
    if (expect_true( tag ))
        *enc->buf.pos++ = tag;
    while (n >= 0x80) {                      /* while we are larger than 7 bits long */
        *enc->buf.pos++ = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        n = n >> 7;                          /* shift off the 7 least significant bits */
    }
    *enc->buf.pos++ = n;                     /* encode the last 7 bits without the high bit being set */
    DEBUG_ASSERT_BUF_SANE(enc->buf);
}

SRL_STATIC_INLINE void
srl_buf_cat_varint(pTHX_ srl_encoder_t *enc, const char tag, const UV n) {
    /* this implements "varint" from google protocol buffers */
    DEBUG_ASSERT_BUF_SANE(enc->buf);
    BUF_SIZE_ASSERT(enc->buf, SRL_MAX_VARINT_LENGTH + 1); /* always allocate space for the tag, overalloc is harmless */
    srl_buf_cat_varint_nocheck(aTHX_ enc, tag, n);
}

SRL_STATIC_INLINE void
srl_buf_cat_zigzag_nocheck(pTHX_ srl_encoder_t *enc, const char tag, const IV n) {
    const UV z= (n << 1) ^ (n >> (sizeof(IV) * 8 - 1));
    srl_buf_cat_varint_nocheck(aTHX_ enc, tag, z);
}

SRL_STATIC_INLINE void
srl_buf_cat_zigzag(pTHX_ srl_encoder_t *enc, const char tag, const IV n) {
    /*
     * This implements googles "zigzag varints" which effectively interleave negative
     * and positive numbers.
     *
     * see: https://developers.google.com/protocol-buffers/docs/encoding#types
     *
     * Note: maybe for negative numbers we should just invert and then treat as a positive?
     *
     */
    const UV z= (n << 1) ^ (n >> (sizeof(IV) * 8 - 1));
    srl_buf_cat_varint(aTHX_ enc, tag, z);
}

#endif
