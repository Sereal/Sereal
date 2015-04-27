#ifndef SRL_READER_VARINT_H_
#define SRL_READER_VARINT_H_

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader.h"
#include "srl_reader_error.h"

SRL_STATIC_INLINE void
srl_skip_varint(pTHX_ srl_reader_buffer_t *buf)
{
    U8 max_varint_len = sizeof(UV) == sizeof(U32) ? 5 : 10;
    while (SRL_RDR_NOT_DONE(buf) && *buf->pos & 0x80) {
        buf->pos++;
        if (!max_varint_len--)
            SRL_RDR_ERROR(buf, "varint too long");
    }

    if (expect_false( SRL_RDR_DONE(buf) ))
        SRL_RDR_ERROR(buf, "end of packet reached before varint parsed");

    buf->pos++;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_safe(pTHX_ srl_reader_buffer_t *buf)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (SRL_RDR_NOT_DONE(buf) && *buf->pos & 0x80) {
        uv |= ((UV)(*buf->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            SRL_RDR_ERROR(buf, "varint too big");
    }

    if (expect_true( SRL_RDR_NOT_DONE(buf) )) {
        uv |= ((UV)*buf->pos++ << lshift);
    } else {
        SRL_RDR_ERROR(buf, "end of packet reached before varint parsed");
    }

    return uv;
}

#define SET_UV_FROM_VARINT(buf, uv, ptr) STMT_START {                      \
    U32 b;                                                                  \
                                                                            \
    /* Splitting into 32-bit pieces gives better performance on 32-bit      \
       processors. */                                                       \
    U32 part0 = 0, part1 = 0, part2 = 0;                                    \
    do {                                                                    \
                                                                            \
    b = *(ptr++); part0  = b      ; if (!(b & 0x80)) break;                 \
    part0 -= 0x80;                                                          \
    b = *(ptr++); part0 += b <<  7; if (!(b & 0x80)) break;                 \
    part0 -= 0x80 << 7;                                                     \
    b = *(ptr++); part0 += b << 14; if (!(b & 0x80)) break;                 \
    part0 -= 0x80 << 14;                                                    \
    b = *(ptr++); part0 += b << 21; if (!(b & 0x80)) break;                 \
    part0 -= 0x80 << 21;                                                    \
                                                                            \
    b = *(ptr++); part1  = b      ; if (!(b & 0x80)) break;                 \
    part1 -= 0x80;                                                          \
    b = *(ptr++); part1 += b <<  7; if (!(b & 0x80)) break;                 \
    part1 -= 0x80 << 7;                                                     \
    b = *(ptr++); part1 += b << 14; if (!(b & 0x80)) break;                 \
    part1 -= 0x80 << 14;                                                    \
    b = *(ptr++); part1 += b << 21; if (!(b & 0x80)) break;                 \
    part1 -= 0x80 << 21;                                                    \
                                                                            \
    b = *(ptr++); part2  = b      ; if (!(b & 0x80)) break;                 \
    part2 -= 0x80;                                                          \
    b = *(ptr++); part2 += b <<  7; if (!(b & 0x80)) break;                 \
    /* "part2 -= 0x80 << 7" is irrelevant because (0x80 << 7) << 56 is 0. */\
                                                                            \
    /* We have overrun the maximum size of a varint (10 bytes).  The data   \
        must be corrupt. */                                                 \
    SRL_RDR_ERROR(buf, "varint not terminated in time, corrupt packet");   \
                                                                            \
    } while (0);                                                            \
                                                                            \
    uv= (((UV)part0)      ) |                                               \
        (((UV)part1) << 28) |                                               \
        (((UV)part2) << 56);                                                \
                                                                            \
} STMT_END

SRL_STATIC_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_reader_buffer_t *buf)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*buf->pos & 0x80) {
        uv |= ((UV)(*buf->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (expect_false( lshift > (sizeof(UV) * 8) ))
            SRL_RDR_ERROR(buf, "varint too big");
    }

    uv |= ((UV)(*buf->pos++) << lshift);
    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_u32_nocheck(pTHX_ srl_reader_buffer_t *buf)
{
    const U8* ptr = buf->pos;
    U32 b;
    U32 part0 = 0;

    b = *(ptr++); part0  = b      ; if (!(b & 0x80)) goto done;
    part0 -= 0x80;
    b = *(ptr++); part0 += b <<  7; if (!(b & 0x80)) goto done;
    part0 -= 0x80 << 7;
    b = *(ptr++); part0 += b << 14; if (!(b & 0x80)) goto done;
    part0 -= 0x80 << 14;
    b = *(ptr++); part0 += b << 21; if (!(b & 0x80)) goto done;
    part0 -= 0x80 << 21;
    b = *(ptr++); part0 += b << 28; if (b < 16) goto done;

    SRL_RDR_ERROR(buf, "varint overflows U32, cannot restore packet");

   done:
    buf->pos= (U8*)ptr;

    return part0;
}

SRL_STATIC_INLINE UV
srl_read_varint_u64_nocheck(pTHX_ srl_reader_buffer_t *buf)
{
    UV uv;
    const U8* ptr = buf->pos;

    SET_UV_FROM_VARINT(buf, uv, ptr);

    buf->pos= (U8*)ptr;

    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv(pTHX_ srl_reader_buffer_t *buf)
{
    // TODO check expect_true logic
    if (expect_true( buf->end - buf->pos >= 10 ) || (buf->end[-1] & 0x80)) {
        if (sizeof(UV) == sizeof(U32)) {
            return srl_read_varint_u32_nocheck(aTHX_ buf);
        } else {
            return srl_read_varint_u64_nocheck(aTHX_ buf);
        }
    } else {
        return srl_read_varint_uv_safe(aTHX_ buf);
    }
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_reader_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    if (buf->body_pos + len >= buf->pos) {
        SRL_RDR_ERRORf4(buf, "Corrupted packet%s. Offset %"UVuf" points past current position %"UVuf" in packet with length of %"UVuf" bytes long",
                         errstr, len, (UV)SRL_RDR_POS_OFS(buf), (UV)SRL_RDR_SIZE(buf));
    }
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_length(pTHX_ srl_reader_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    SRL_RDR_ASSERT_SPACE(buf, len, errstr);
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_count(pTHX_ srl_reader_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    if (len > I32_MAX) {
        SRL_RDR_ERRORf3(buf, "Corrupted packet%s. Count %"UVuf" exceeds I32_MAX (%i), which is impossible.",
                         errstr, len, I32_MAX);
    }
    return len;
}

#endif
