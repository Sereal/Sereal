#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

#include "srl_encoder.h"

#define BUFFER_GROWTH_FACTOR 1.5

/* buffer operations */
#define BUF_POS_OFS(enc) ((enc)->pos - (enc)->buf_start)
#define BUF_SPACE(enc) ((enc)->buf_end - (enc)->pos)
#define BUF_SIZE(enc) ((enc)->buf_end - (enc)->buf_start)
#define BUF_NEED_GROW(enc, minlen) ((size_t)BUF_SPACE(enc) <= minlen)
#define BUF_NEED_GROW_TOTAL(enc, minlen) ((size_t)BUF_SIZE(enc) <= minlen)

inline void
srl_buf_grow_nocheck(pTHX_ srl_encoder_t *enc, size_t minlen)
{
  const size_t cur_size = BUF_SIZE(enc);
  const size_t new_size = 100 + MAX(minlen, (size_t)(cur_size * BUFFER_GROWTH_FACTOR));
  Renew(enc->buf_start, new_size, char);
  enc->buf_end = (char *)(enc->buf_start + new_size);
}

#define BUF_SIZE_ASSERT(enc, minlen) \
  STMT_START { \
    if (BUF_NEED_GROW(enc, minlen)) \
      srl_buf_grow_nocheck(aTHX_ (enc), (BUF_SIZE(enc) + minlen)); \
  } STMT_END

#define BUF_SIZE_ASSERT_TOTAL(enc, minlen) \
  STMT_START { \
    if (BUF_NEED_GROW_TOTAL(enc, minlen)) \
      srl_buf_grow_nocheck(aTHX_ (enc), (minlen)); \
  } STMT_END

inline void
srl_buf_cat_str_int(pTHX_ srl_encoder_t *enc, const char *str, size_t len)
{
  BUF_SIZE_ASSERT(enc, len);
  Copy(str, enc->pos, len, char);
  enc->pos += len;
}
#define srl_buf_cat_str(enc, str, len) srl_buf_cat_str_int(aTHX_ enc, str, len)
#define srl_buf_cat_str_s(enc, str) srl_buf_cat_str(enc, ("" str), strlen("" str))

inline void
srl_buf_cat_str_nocheck_int(pTHX_ srl_encoder_t *enc, const char *str, size_t len)
{
  Copy(str, enc->pos, len, char);
  enc->pos += len;
}
#define srl_buf_cat_str_nocheck(enc, str, len) srl_buf_cat_str_nocheck_int(aTHX_ enc, str, len)
#define srl_buf_cat_str_s_nocheck(enc, str) srl_buf_cat_str_nocheck(enc, ("" str), strlen("" str))

inline void
srl_buf_cat_char_int(pTHX_ srl_encoder_t *enc, const char c)
{
  BUF_SIZE_ASSERT(enc, 1);
  *enc->pos++ = c;
}
#define srl_buf_cat_char(enc, c) srl_buf_cat_char_int(aTHX_ enc, c)

inline void
srl_buf_cat_char_nocheck_int(pTHX_ srl_encoder_t *enc, const char c)
{
  *enc->pos++ = c;
}
#define srl_buf_cat_char_nocheck(enc, c) srl_buf_cat_char_nocheck_int(aTHX_ enc, c)

/* define constant for other code to use in preallocations */
#define SRL_MAX_VARINT_LENGTH 11

static inline void
srl_buf_cat_varint_nocheck(pTHX_ srl_encoder_t *enc, const char tag, UV n) {
    if (tag)
        *enc->pos++ = tag;
    while (n > 0x80) {                   /* while we are larger than 7 bits long */
        *enc->pos++ = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        n = n >> 7;                      /* shift off the 7 least significant bits */
    }
    *enc->pos++ = n;                     /* encode the last 7 bits without the high bit being set */
}

static inline void
srl_buf_cat_varint(pTHX_ srl_encoder_t *enc, const char tag, const UV n) {
    /* this implements googles "varint" from google protocol buffers */
    BUF_SIZE_ASSERT(enc, SRL_MAX_VARINT_LENGTH);
    srl_buf_cat_varint_nocheck(aTHX_ enc, tag, n);
}

static inline void
srl_buf_cat_zigzag_nocheck(pTHX_ srl_encoder_t *enc, const char tag, const IV n) {
    const UV z= (n << 1) ^ (n >> (sizeof(IV) * 8 - 1));
    srl_buf_cat_varint_nocheck(aTHX_ enc, tag, z);
}

static inline void
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
