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

    if (expect_false( !max_varint_len ))
        SRL_RDR_ERROR(buf, "varint too long");

    /* The final byte of a maximum-length ULEB128 carries only the bits
     * remaining in UV. Reject values that would overflow when shifted. */
    if (expect_false( max_varint_len == 1
            && (sizeof(UV) == sizeof(U32) ? *buf->pos > 0x0F : *buf->pos > 0x01) ))
        SRL_RDR_ERROR(buf, "varint too big");

    buf->pos++;
}

SRL_STATIC_INLINE UV
srl_varint_length(pTHX_ UV value)
{
    UV length = 0;
    while (value >= 0x80) {
        length++;
        value >>= 7;
    }

    return ++length;
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
        const U8 terminal = *buf->pos;
        if (expect_false( lshift >= (sizeof(UV) * 8)
                || (UV)terminal > (UV_MAX >> lshift) ))
            SRL_RDR_ERROR(buf, "varint too big");
        uv |= ((UV)terminal << lshift);
        buf->pos++;
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
    b = *(ptr++); part2 += b <<  7;                                        \
    if (!(b & 0x80)) {                                                     \
        if (expect_false( b > 1 ))                                         \
            SRL_RDR_ERROR(buf, "varint too big");                         \
        break;                                                             \
    }                                                                      \
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

    {
        const U8 terminal = *buf->pos;
        if (expect_false( lshift >= (sizeof(UV) * 8)
                || (UV)terminal > (UV_MAX >> lshift) ))
            SRL_RDR_ERROR(buf, "varint too big");
        uv |= ((UV)terminal << lshift);
        buf->pos++;
    }
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
    /* We can use the unrolled-loop version of varint decoding
     * IFF there is certainly enough space in the input buffer
     * even for the longest possible varint (11 bytes, see
     * SRL_MAX_VARINT_LENGTH). Otherwise, use the safe byte-by-byte
     * reader which bounds-checks each byte against buf->end. */
    if (expect_true( buf->end - buf->pos >= SRL_MAX_VARINT_LENGTH )) {
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
    const UV offset= srl_read_varint_uv(aTHX_ buf);
    /* V2+ body offsets are one-based: zero points immediately before the
     * body, into uninitialized header space after decompression. In V1,
     * zero points at the document's magic, which is not a value either. */
    if (expect_false(offset == 0))
        SRL_RDR_ERRORf1(buf,
            "Corrupted packet%s. Offset 0 does not point into the document body",
            errstr);
    /* A "copy" reference can only refer to things that precede it
     * in the same body. Compare the offset to the number of bytes already
     * decoded instead of adding an uncontrolled UV to body_pos as
     * that pointer addition can wrap below body_pos for a large offset. */
    if (expect_false( offset >= (UV)(buf->pos - buf->body_pos) )) {
        SRL_RDR_ERRORf4(buf, "Corrupted packet%s. Offset %"UVuf" points past current position %"UVuf" in packet with length of %"UVuf" bytes long",
                         errstr, offset, (UV)SRL_RDR_POS_OFS(buf), (UV)SRL_RDR_SIZE(buf));
    }
    return offset;
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
    if (expect_false( len > I32_MAX )) {
        SRL_RDR_ERRORf3(buf, "Corrupted packet%s. Count %"UVuf" exceeds I32_MAX (%i), which is impossible.",
                         errstr, len, I32_MAX);
    }
    return len;
}

#endif
