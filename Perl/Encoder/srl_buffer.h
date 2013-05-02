#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

#include "assert.h"

#include "srl_inline.h"
#include "srl_encoder.h"

#ifdef MEMDEBUG
#   define BUFFER_GROWTH_FACTOR 1
#else
#   define BUFFER_GROWTH_FACTOR 2
#endif

/* The static's below plus the ifndef sort of make this header only
 * usable in one place per compilation unit. Drop "static" when necessary.
 * For now, potentially smaller code wins. */


/* buffer operations */
#define BUF_POS_OFS(enc) ((enc)->pos - (enc)->buf_start)
#define BUF_SPACE(enc) ((enc)->buf_end - (enc)->pos)
#define BUF_SIZE(enc) ((enc)->buf_end - (enc)->buf_start)
#define BUF_NEED_GROW(enc, minlen) ((size_t)BUF_SPACE(enc) <= minlen)
#define BUF_NEED_GROW_TOTAL(enc, minlen) ((size_t)BUF_SIZE(enc) <= minlen)


/* body-position/size related operations */
#define BODY_POS_OFS(enc) ((enc)->pos - (enc)->body_pos)

/* these are mostly for right between (de)serializing the header and the body */
#define SRL_SET_BODY_POS(enc, pos_ptr) ((enc)->body_pos = pos_ptr)
#define SRL_UPDATE_BODY_POS(enc)                                            \
    STMT_START {                                                            \
        if (expect_false(SRL_ENC_HAVE_OPTION((enc), SRL_F_USE_PROTO_V1))) { \
            SRL_SET_BODY_POS(enc, (enc)->buf_start);                        \
        } else {                                                            \
            SRL_SET_BODY_POS(enc, (enc)->pos);                              \
        }                                                                   \
    } STMT_END


/* Internal debugging macros, used only in DEBUG mode */
#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SPACE(enc, len) STMT_START { \
    if((BUF_SPACE(enc) < (ptrdiff_t)(len))) { \
        warn("failed assertion check - pos: %ld [%p %p %p] %ld < %ld",  \
                (long)BUF_POS_OFS(enc), (enc)->buf_start, (enc)->pos, (enc)->buf_end, (long)BUF_SPACE(enc),(long)(len)); \
    } \
    assert(BUF_SPACE(enc) >= (ptrdiff_t)(len)); \
} STMT_END
#else
#define DEBUG_ASSERT_BUF_SPACE(enc, len) ((void)0)
#endif

#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SANE(enc) STMT_START { \
    if(!(((enc)->buf_start <= (enc)->pos) && ((enc)->pos <= (enc)->buf_end))){\
        warn("failed sanity assertion check - pos: %ld [%p %p %p] %ld",  \
                (long)BUF_POS_OFS(enc), (enc)->buf_start, (enc)->pos, (enc)->buf_end, (long)BUF_SPACE(enc)); \
    } \
    assert(((enc)->buf_start <= (enc)->pos) && ((enc)->pos <= (enc)->buf_end));\
} STMT_END
#else
#define DEBUG_ASSERT_BUF_SANE(enc) assert(((enc)->buf_start <= (enc)->pos) && ((enc)->pos <= (enc)->buf_end))
#endif

SRL_STATIC_INLINE void
srl_buf_grow_nocheck(pTHX_ srl_encoder_t *enc, size_t minlen)
{
    const size_t pos_ofs= BUF_POS_OFS(enc); /* have to store the offset of pos */
    const size_t body_ofs= enc->body_pos - enc->buf_start; /* have to store the offset of the body */
#ifdef MEMDEBUG
    const size_t new_size = minlen;
#else
    const size_t cur_size = BUF_SIZE(enc);
    const size_t grown_len = (size_t)(cur_size * BUFFER_GROWTH_FACTOR);
    const size_t new_size = 100 + (minlen > grown_len ? minlen : grown_len);
#endif

    DEBUG_ASSERT_BUF_SANE(enc);
    /* assert that Renew means GROWING the buffer */
    assert(enc->buf_start + new_size > enc->buf_end);

    Renew(enc->buf_start, new_size, char);
    if (enc->buf_start == NULL)
        croak("Out of memory!");
    enc->buf_end = (char *)(enc->buf_start + new_size);
    enc->pos= enc->buf_start + pos_ofs;
    SRL_SET_BODY_POS(enc, enc->buf_start + body_ofs);

    DEBUG_ASSERT_BUF_SANE(enc);
    assert(enc->buf_end - enc->buf_start > (ptrdiff_t)0);
    assert(enc->pos - enc->buf_start >= (ptrdiff_t)0);
    assert(enc->body_pos - enc->buf_start >= (ptrdiff_t)0);
}

#define BUF_SIZE_ASSERT(enc, minlen) \
  STMT_START { \
    DEBUG_ASSERT_BUF_SANE(enc); \
    if (BUF_NEED_GROW(enc, minlen)) \
      srl_buf_grow_nocheck(aTHX_ (enc), (BUF_SIZE(enc) + minlen)); \
    DEBUG_ASSERT_BUF_SANE(enc); \
  } STMT_END

#define BUF_SIZE_ASSERT_TOTAL(enc, minlen) \
  STMT_START { \
    DEBUG_ASSERT_BUF_SANE(enc); \
    if (BUF_NEED_GROW_TOTAL(enc, minlen)) \
      srl_buf_grow_nocheck(aTHX_ (enc), (minlen)); \
    DEBUG_ASSERT_BUF_SANE(enc); \
  } STMT_END

SRL_STATIC_INLINE void
srl_buf_cat_str_int(pTHX_ srl_encoder_t *enc, const char *str, size_t len)
{
    BUF_SIZE_ASSERT(enc, len);
    Copy(str, enc->pos, len, char);
    enc->pos += len;
    DEBUG_ASSERT_BUF_SANE(enc);
}
#define srl_buf_cat_str(enc, str, len) srl_buf_cat_str_int(aTHX_ enc, str, len)
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s(enc, str) srl_buf_cat_str(enc, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_str_nocheck_int(pTHX_ srl_encoder_t *enc, const char *str, size_t len)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    DEBUG_ASSERT_BUF_SPACE(enc, len);
    Copy(str, enc->pos, len, char);
    enc->pos += len;
    DEBUG_ASSERT_BUF_SANE(enc);
}
#define srl_buf_cat_str_nocheck(enc, str, len) srl_buf_cat_str_nocheck_int(aTHX_ enc, str, len)
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s_nocheck(enc, str) srl_buf_cat_str_nocheck(enc, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_char_int(pTHX_ srl_encoder_t *enc, const char c)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    BUF_SIZE_ASSERT(enc, 1);
    DEBUG_ASSERT_BUF_SPACE(enc, 1);
    *enc->pos++ = c;
    DEBUG_ASSERT_BUF_SANE(enc);
}
#define srl_buf_cat_char(enc, c) srl_buf_cat_char_int(aTHX_ enc, c)

SRL_STATIC_INLINE void
srl_buf_cat_char_nocheck_int(pTHX_ srl_encoder_t *enc, const char c)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    DEBUG_ASSERT_BUF_SPACE(enc, 1);
    *enc->pos++ = c;
    DEBUG_ASSERT_BUF_SANE(enc);
}
#define srl_buf_cat_char_nocheck(enc, c) srl_buf_cat_char_nocheck_int(aTHX_ enc, c)

/* define constant for other code to use in preallocations */
#define SRL_MAX_VARINT_LENGTH 11

SRL_STATIC_INLINE void
srl_buf_cat_varint_nocheck(pTHX_ srl_encoder_t *enc, const char tag, UV n) {
    DEBUG_ASSERT_BUF_SANE(enc);
    DEBUG_ASSERT_BUF_SPACE(enc, (tag==0 ? 0 : 1) + SRL_MAX_VARINT_LENGTH);
    if (tag)
        *enc->pos++ = tag;
    while (n >= 0x80) {                  /* while we are larger than 7 bits long */
        *enc->pos++ = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        n = n >> 7;                      /* shift off the 7 least significant bits */
    }
    *enc->pos++ = n;                     /* encode the last 7 bits without the high bit being set */
    DEBUG_ASSERT_BUF_SANE(enc);
}

SRL_STATIC_INLINE void
srl_buf_cat_varint(pTHX_ srl_encoder_t *enc, const char tag, const UV n) {
    /* this implements "varint" from google protocol buffers */
    DEBUG_ASSERT_BUF_SANE(enc);
    BUF_SIZE_ASSERT(enc, SRL_MAX_VARINT_LENGTH + 1); /* always allocate space for the tag, overalloc is harmless */
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
