#ifndef SRL_READER_SKIP_H_
#define SRL_READER_SKIP_H_

/* Stepping over self-contained values: tags that carry their whole
 * payload inline, with no nested values and no back-reference. Used by
 * the tools that walk a Sereal body without decoding it (Sereal::Merger,
 * Sereal::Splitter, Sereal::Path::Iterator), so that each of them agrees
 * with the decoder on payload sizes and on which protocol version
 * introduced a tag. */

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_protocol.h"

/* Payload sizes, as the decoder reads them. LONG_DOUBLE is
 * platform-dependent, like the decoder's sizeof(long double) read. */
#define SRL_FLOAT_SIZE          4
#define SRL_DOUBLE_SIZE         8
#define SRL_LONG_DOUBLE_SIZE    sizeof(long double)
#define SRL_FLOAT_128_SIZE      16

#define SRL_RDR_ASSERT_PROTOCOL(buf, protocol_version, min_version, tag_name) STMT_START {  \
    if (expect_false((protocol_version) < (min_version)))                                    \
        SRL_RDR_ERRORf1((buf), tag_name " tag requires protocol version %u or later",        \
                        (unsigned int)(min_version));                                        \
} STMT_END

/* Read the magnitude of a NEG_VARINT (buf->pos just past the tag) and
 * validate it like the decoder does. */
SRL_STATIC_INLINE UV
srl_read_neg_varint_magnitude(pTHX_ srl_reader_buffer_t *buf)
{
    UV magnitude = srl_read_varint_uv(aTHX_ buf);
    if (expect_false(magnitude == 0 || magnitude > (UV)IV_MAX + 1))
        SRL_RDR_ERRORf1(buf, "Invalid NEG_VARINT magnitude %"UVuf, magnitude);
    return magnitude;
}

/* Read an EXTEND tag's extended-tag ID and the length of its payload
 * (buf->pos just past the EXTEND tag), leaving buf->pos at the payload.
 * UNDEF_DEADLY has no payload and is an ordinary undef when reading.
 * Only v-strings have a payload; unknown IDs are rejected. */
SRL_STATIC_INLINE UV
srl_read_extend_header(pTHX_ srl_reader_buffer_t *buf, U8 protocol_version, U8 *ext_tag_out)
{
    U8 ext_tag;
    SRL_RDR_ASSERT_PROTOCOL(buf, protocol_version, 6, "EXTEND");
    SRL_RDR_ASSERT_SPACE(buf, 1, " while reading extended tag ID");
    ext_tag = *buf->pos++;
    if (ext_tag == SRL_EXT_UNDEF_DEADLY) {
        if (ext_tag_out)
            *ext_tag_out = ext_tag;
        return 0;
    }
    if (ext_tag != SRL_EXT_VSTRING_BINARY && ext_tag != SRL_EXT_VSTRING_UTF8)
        SRL_RDR_ERRORf1(buf, "Unknown extended tag ID %u", (unsigned int)ext_tag);
    if (ext_tag_out)
        *ext_tag_out = ext_tag;
    return srl_read_varint_uv_length(aTHX_ buf, " while reading extended VSTRING");
}

/* buf->pos points just past tag, which has its track flag removed. If
 * tag is a self-contained value, step over its payload and return 1;
 * otherwise return 0 and leave buf untouched. Croaks on truncated
 * payloads and on tags that protocol_version does not allow. Rewriters set
 * forbid_undef_deadly because compliant encoders must not emit that tag. */
SRL_STATIC_INLINE int
srl_reader_skip_scalar(pTHX_ srl_reader_buffer_t *buf, U8 tag, U8 protocol_version, int forbid_undef_deadly)
{
    UV len;

    if (tag <= SRL_HDR_NEG_HIGH)
        return 1;

    if (tag >= SRL_HDR_SHORT_BINARY_LOW) {
        len = tag & SRL_MASK_SHORT_BINARY_LEN;
        SRL_RDR_ASSERT_SPACE(buf, len, " while reading SHORT_BINARY");
        buf->pos += len;
        return 1;
    }

    switch (tag) {
        case SRL_HDR_VARINT:
        case SRL_HDR_ZIGZAG:
            srl_skip_varint(aTHX_ buf);
            return 1;

        case SRL_HDR_NEG_VARINT:
            SRL_RDR_ASSERT_PROTOCOL(buf, protocol_version, 6, "NEG_VARINT");
            (void)srl_read_neg_varint_magnitude(aTHX_ buf);
            return 1;

        case SRL_HDR_FLOAT:       len = SRL_FLOAT_SIZE;       break;
        case SRL_HDR_DOUBLE:      len = SRL_DOUBLE_SIZE;      break;
        case SRL_HDR_LONG_DOUBLE: len = SRL_LONG_DOUBLE_SIZE; break;
        case SRL_HDR_FLOAT_128:
            SRL_RDR_ASSERT_PROTOCOL(buf, protocol_version, 5, "FLOAT_128");
            len = SRL_FLOAT_128_SIZE;
            break;

        case SRL_HDR_NO:
        case SRL_HDR_YES:
            SRL_RDR_ASSERT_PROTOCOL(buf, protocol_version, 5, "NO/YES");
            return 1;

        case SRL_HDR_CANONICAL_UNDEF:
            SRL_RDR_ASSERT_PROTOCOL(buf, protocol_version, 3, "CANONICAL_UNDEF");
            return 1;

        case SRL_HDR_TRUE:
        case SRL_HDR_FALSE:
        case SRL_HDR_UNDEF:
            return 1;

        case SRL_HDR_BINARY:
        case SRL_HDR_STR_UTF8:
            len = srl_read_varint_uv_length(aTHX_ buf, " while reading BINARY or STR_UTF8");
            buf->pos += len;
            return 1;

        case SRL_HDR_EXTEND: {
            U8 ext_tag;
            len = srl_read_extend_header(aTHX_ buf, protocol_version, &ext_tag);
            if (forbid_undef_deadly && ext_tag == SRL_EXT_UNDEF_DEADLY)
                SRL_RDR_ERROR(buf, "Cannot emit UNDEF_DEADLY extended tag");
            buf->pos += len;
            return 1;
        }

        default:
            return 0;
    }

    SRL_RDR_ASSERT_SPACE(buf, len, " while reading a floating point value");
    buf->pos += len;
    return 1;
}

#endif
