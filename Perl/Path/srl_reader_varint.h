#ifndef SRL_RDRER_VARINT_H_
#define SRL_RDRER_VARINT_H_

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader.h"
#include "srl_reader_error.h"

SRL_STATIC_INLINE void
srl_skip_varint(pTHX_ srl_reader_t *rdr)
{
    U8 max_varint_len = sizeof(UV) == sizeof(U32) ? 5 : 10;
    while (SRL_RB_NOT_DONE(rdr) && *rdr->rb_pos++ & 0x80) {
        if (!max_varint_len--)
            SRL_RDR_ERROR(rdr, "varint too long");
    }

    if (expect_false( SRL_RB_DONE(rdr) ))
        SRL_RDR_ERROR(rdr, "end of packet reached before varint parsed");

    rdr->rb_pos++;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_safe(pTHX_ srl_reader_t *rdr)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (SRL_RB_NOT_DONE(rdr) && *rdr->rb_pos & 0x80) {
        uv |= ((UV)(*rdr->rb_pos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            SRL_RDR_ERROR(rdr, "varint too big");
    }

    if (expect_true( SRL_RB_NOT_DONE(rdr) )) {
        uv |= ((UV)*rdr->rb_pos++ << lshift);
    } else {
        SRL_RDR_ERROR(rdr, "end of packet reached before varint parsed");
    }

    return uv;
}

#define SET_UV_FROM_VARINT(rdr, uv, ptr) STMT_START {                      \
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
    SRL_RDR_ERROR(rdr, "varint not terminated in time, corrupt packet");   \
                                                                            \
    } while (0);                                                            \
                                                                            \
    uv= (((UV)part0)      ) |                                               \
        (((UV)part1) << 28) |                                               \
        (((UV)part2) << 56);                                                \
                                                                            \
} STMT_END

SRL_STATIC_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_reader_t *rdr)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*rdr->rb_pos & 0x80) {
        uv |= ((UV)(*rdr->rb_pos++ & 0x7F) << lshift);
        lshift += 7;
        if (expect_false( lshift > (sizeof(UV) * 8) ))
            SRL_RDR_ERROR(rdr, "varint too big");
    }

    uv |= ((UV)(*rdr->rb_pos++) << lshift);
    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_u32_nocheck(pTHX_ srl_reader_t *rdr)
{
    const U8* ptr = rdr->rb_pos;
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

    SRL_RDR_ERROR(rdr, "varint overflows U32, cannot restore packet");

   done:
    rdr->rb_pos= (U8*)ptr;

    return part0;
}

SRL_STATIC_INLINE UV
srl_read_varint_u64_nocheck(pTHX_ srl_reader_t *rdr)
{
    UV uv;
    const U8* ptr = rdr->rb_pos;

    SET_UV_FROM_VARINT(rdr, uv, ptr);

    rdr->rb_pos= (U8*)ptr;

    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv(pTHX_ srl_reader_t *rdr)
{
    // TODO check expect_true logic
    if (expect_true( rdr->rb_end - rdr->rb_pos >= 10 ) || (rdr->rb_end[-1] & 0x80)) {
        if (sizeof(UV) == sizeof(U32)) {
            return srl_read_varint_u32_nocheck(aTHX_ rdr);
        } else {
            return srl_read_varint_u64_nocheck(aTHX_ rdr);
        }
    } else {
        return srl_read_varint_uv_safe(aTHX_ rdr);
    }
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_reader_t *rdr, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ rdr);
    if (rdr->rb_body_pos + len >= rdr->rb_pos) {
        SRL_RDR_ERRORf4(rdr, "Corrupted packet%s. Offset %"UVuf" points past current position %"UVuf" in packet with length of %"UVuf" bytes long",
                         errstr, len, (UV)SRL_RB_POS_OFS(rdr), (UV)SRL_RB_SIZE(rdr));
    }
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_length(pTHX_ srl_reader_t *rdr, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ rdr);
    SRL_RB_ASSERT_SPACE(rdr, len, errstr);
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_count(pTHX_ srl_reader_t *rdr, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ rdr);
    if (len > I32_MAX) {
        SRL_RDR_ERRORf3(rdr, "Corrupted packet%s. Count %"UVuf" exceeds I32_MAX (%i), which is impossible.",
                         errstr, len, I32_MAX);
    }
    return len;
}

#endif
