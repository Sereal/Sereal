#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

/*
  Adapted from Perl/Encoder/srl_buffer.h
 */

#include <assert.h>
#include <stddef.h>

#include "srl_inline.h"
#include "srl_encoder.h"

#define UV unsigned long
#define IV long

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

/* Internal debugging macros, used only in DEBUG mode */
#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SPACE(enc, len)  {   \
        if((BUF_SPACE(enc) < (ptrdiff_t)(len))) {                       \
            fprintf(stderr,"failed assertion check - pos: %ld [%p %p %p] %ld < %ld", \
                 (long)BUF_POS_OFS(enc), (enc)->buf_start, (enc)->pos, (enc)->buf_end, (long)BUF_SPACE(enc),(long)(len)); \
        }                                                               \
    assert(BUF_SPACE(enc) >= (ptrdiff_t)(len)); \
}
#else
#define DEBUG_ASSERT_BUF_SPACE(enc, len) ((void)0)
#endif

#ifndef NDEBUG
#define DEBUG_ASSERT_BUF_SANE(enc) {                                    \
        if(!(((enc)->buf_start <= (enc)->pos) && ((enc)->pos <= (enc)->buf_end))){ \
            fprintf(stderr, "failed sanity assertion check - pos: %ld [%p %p %p] %ld", \
                    (long)BUF_POS_OFS(enc), (enc)->buf_start, (enc)->pos, (enc)->buf_end, (long)BUF_SPACE(enc)); \
        }                                                               \
        assert(((enc)->buf_start <= (enc)->pos) && ((enc)->pos <= (enc)->buf_end)); \
    } 
#else
#define DEBUG_ASSERT_BUF_SANE(enc) assert(((enc)->buf_start <= (enc)->pos) && ((enc)->pos <= (enc)->buf_end))
#endif

SRL_STATIC_INLINE int
srl_buf_grow_nocheck(srl_encoder_t *enc, size_t minlen)
{
    const size_t pos_ofs= BUF_POS_OFS(enc); /* have to store the offset of pos */
#ifdef MEMDEBUG
    const size_t new_size = minlen;
#else
    const size_t cur_size = BUF_SIZE(enc);
    const size_t grown_len = (size_t)(cur_size * BUFFER_GROWTH_FACTOR);
    const size_t new_size = 100 + (minlen > grown_len ? minlen : grown_len);
#endif
    char *p;
    DEBUG_ASSERT_BUF_SANE(enc);
    /* assert that PyMem_Resize means GROWING the buffer */
    assert(enc->buf_start + new_size > enc->buf_end);
    p = enc->buf_start;
    PyMem_Resize(enc->buf_start, char, new_size);
    if (enc->buf_start == NULL) {
        PyMem_Free(p);
        PyErr_NoMemory();
        return -1;
    }
    enc->buf_end = (char *)(enc->buf_start + new_size);
    enc->pos= enc->buf_start + pos_ofs;
    assert(enc->buf_end - enc->buf_start > (ptrdiff_t)0);

    return 0;
}

SRL_STATIC_INLINE int
BUF_SIZE_ASSERT(srl_encoder_t *enc, size_t minlen)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    if (BUF_NEED_GROW(enc, minlen))
        if (-1 == srl_buf_grow_nocheck((enc), (BUF_SIZE(enc) + minlen)))
            return -1; /*OOM*/
    DEBUG_ASSERT_BUF_SANE(enc);
    return 0;
} 

SRL_STATIC_INLINE int
BUF_SIZE_ASSERT_TOTAL(srl_encoder_t *enc, size_t minlen)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    if (BUF_NEED_GROW_TOTAL(enc, minlen))
        if (-1 == srl_buf_grow_nocheck(enc, minlen))
            return -1; /*OOM*/
    DEBUG_ASSERT_BUF_SANE(enc);
    return 0;
}

SRL_STATIC_INLINE int
srl_buf_cat_str(srl_encoder_t *enc, const char *str, size_t len)
{
    if (-1 == BUF_SIZE_ASSERT(enc, len))
        return -1; /*OOM*/
    memcpy(enc->pos, str, len);
    enc->pos += len;
    DEBUG_ASSERT_BUF_SANE(enc);
    return 0;
}
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s(enc, str) srl_buf_cat_str(enc, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE void
srl_buf_cat_str_nocheck(srl_encoder_t *enc, const char *str, size_t len)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    DEBUG_ASSERT_BUF_SPACE(enc, len);
    memcpy(enc->pos, str, len);
    enc->pos += len;
    DEBUG_ASSERT_BUF_SANE(enc);
}
/* see perl.git:handy.h STR_WITH_LEN macro for explanation of the below code */
#define srl_buf_cat_str_s_nocheck(enc, str) srl_buf_cat_str_nocheck(enc, ("" str ""), sizeof(str)-1)

SRL_STATIC_INLINE int
srl_buf_cat_char(srl_encoder_t *enc, const char c)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    if (-1 == BUF_SIZE_ASSERT(enc, 1))
        return -1; /*OOM*/
    DEBUG_ASSERT_BUF_SPACE(enc, 1);
    *enc->pos++ = c;
    DEBUG_ASSERT_BUF_SANE(enc);
    return 0;
}

SRL_STATIC_INLINE void
srl_buf_cat_char_nocheck(srl_encoder_t *enc, const char c)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    DEBUG_ASSERT_BUF_SPACE(enc, 1);
    *enc->pos++ = c;
    DEBUG_ASSERT_BUF_SANE(enc);
}

/* define constant for other code to use in preallocations */
#define SRL_MAX_VARINT_LENGTH 11

SRL_STATIC_INLINE void
srl_buf_cat_varint_nocheck(srl_encoder_t *enc, const char tag, UV n) {
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

SRL_STATIC_INLINE int
srl_buf_cat_varint(srl_encoder_t *enc, const char tag, const UV n) {
    /* this implements "varint" from google protocol buffers */
    DEBUG_ASSERT_BUF_SANE(enc);
    /* always allocate space for the tag, overalloc is harmless */
    if (-1 == BUF_SIZE_ASSERT(enc, SRL_MAX_VARINT_LENGTH + 1))
        return -1; /*OOM*/
    srl_buf_cat_varint_nocheck(enc, tag, n);
    return 0;
}

SRL_STATIC_INLINE int
srl_buf_cat_double(srl_encoder_t *enc, const char tag, double d)
{
    DEBUG_ASSERT_BUF_SANE(enc);
    /* heuristic: header + string + simple value */
    if (-1 == BUF_SIZE_ASSERT(enc, 1 + sizeof(d)))
        return -1; /* OOM */
    srl_buf_cat_char_nocheck(enc,tag);
    memcpy(enc->pos, (char *)&d, sizeof(d));
    enc->pos += sizeof(d);
    DEBUG_ASSERT_BUF_SANE(enc);
    return 0;
}

SRL_STATIC_INLINE void
srl_buf_cat_zigzag_nocheck(srl_encoder_t *enc, const char tag, const IV n) {
    const UV z= (n << 1) ^ (n >> (sizeof(IV) * 8 - 1));
    srl_buf_cat_varint_nocheck(enc, tag, z);
}

SRL_STATIC_INLINE int
srl_buf_cat_zigzag(srl_encoder_t *enc, const char tag, const IV n) {
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
    return srl_buf_cat_varint(enc, tag, z);
}

#undef UV
#undef IV

#endif
