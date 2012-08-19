#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

#include "assert.h"
#include "srl_decoder.h"

#define BUFFER_GROWTH_FACTOR 1.5

/* The static's below plus the ifndef sort of make this header only
 * usable in one place per compilation unit. Drop "static" when necessary.
 * For now, potentially smaller code wins. */


/* buffer operations */
#define BUF_POS(enc) ((enc)->pos)
#define BUF_SPACE(enc) ((enc)->buf_end - (enc)->pos)
#define BUF_POS_OFS(enc) ((enc)->pos - (enc)->buf_start)
#define BUF_SIZE(enc) ((enc)->buf_end - (enc)->buf_start)
#define BUF_NOT_DONE(enc) ((enc)->pos < enc->buf_end)








/* define constant for other code to use in preallocations */
#define SRL_MAX_VARINT_LENGTH 11

static inline void
srl_buf_cat_varint_nocheck(pTHX_ srl_decoder_t *enc, const char tag, UV n) {
    DEBUG_ASSERT_BUF_SANE(enc);
    DEBUG_ASSERT_BUF_SPACE(enc, (tag==0 ? 0 : 1) + SRL_MAX_VARINT_LENGTH);
    if (tag)
        *enc->pos++ = tag;
    while (n > 0x80) {                   /* while we are larger than 7 bits long */
        *enc->pos++ = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        n = n >> 7;                      /* shift off the 7 least significant bits */
    }
    *enc->pos++ = n;                     /* decode the last 7 bits without the high bit being set */
    DEBUG_ASSERT_BUF_SANE(enc);
}

static inline void
srl_buf_cat_varint(pTHX_ srl_decoder_t *enc, const char tag, const UV n) {
    /* this implements googles "varint" from google protocol buffers */
    BUF_SIZE_ASSERT(enc, SRL_MAX_VARINT_LENGTH + 1); /* always allocate space for the tag, overalloc is harmless */
    srl_buf_cat_varint_nocheck(aTHX_ enc, tag, n);
}

static inline void
srl_buf_cat_zigzag_nocheck(pTHX_ srl_decoder_t *enc, const char tag, const IV n) {
    const UV z= (n << 1) ^ (n >> (sizeof(IV) * 8 - 1));
    srl_buf_cat_varint(aTHX_ enc, tag, z);
}

static inline void
srl_buf_cat_zigzag(pTHX_ srl_decoder_t *enc, const char tag, const IV n) {
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
