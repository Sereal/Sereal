#ifndef SRL_READER_HEADER_H_
#define SRL_READER_HEADER_H_

/* Opening a Sereal document for the tools that walk a body without
 * decoding it (Sereal::Merger, Sereal::Splitter, Sereal::Path::Iterator):
 * header validation, the v6 checksum envelope, the header suffix and
 * decompression, with the same rules as Sereal::Decoder. */

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_reader_misc.h"
#include "srl_reader_decompress.h"
#include "srl_reader_checksum.h"
#include "srl_protocol.h"

/* buf->start, buf->pos and buf->end must span the whole document. On
 * return buf->pos is at the first body tag, buf->body_pos is set for the
 * document's protocol version and buf->end is the end of the body (for a
 * checksummed document, the end of the protected payload). When the body
 * is decompressed, the reader is switched to a new buffer owned by a
 * mortal SV, which is stored in *buf_owner if buf_owner is not NULL; it
 * is set to NULL otherwise. Returns the document's protocol version. */
SRL_STATIC_INLINE U8
srl_reader_open_document(pTHX_ srl_reader_buffer_t *buf, const srl_checksum_config_t *checksum, SV **buf_owner)
{
    UV header_len;
    U8 version, encoding;
    int has_checksum;
    const IV version_and_encoding = srl_validate_header_version(aTHX_ buf->pos, SRL_RDR_SPACE_LEFT(buf));

    if (buf_owner)
        *buf_owner = NULL;

    if (expect_false(version_and_encoding < 1)) {
        if (version_and_encoding == 0)
            SRL_RDR_ERROR(buf, "Bad Sereal header: It seems your document was accidentally UTF-8 encoded");
        else
            SRL_RDR_ERROR(buf, "Bad Sereal header: Not a valid Sereal document.");
    }

    version = (U8)(version_and_encoding & SRL_PROTOCOL_VERSION_MASK);
    encoding = (U8)(version_and_encoding & SRL_PROTOCOL_ENCODING_BASE_MASK);
    has_checksum = (version_and_encoding & SRL_PROTOCOL_ENCODING_CHECKSUM) != 0;
    buf->pos += SRL_MAGIC_STRLEN + 1;

    if (expect_false(version > SRL_PROTOCOL_VERSION))
        SRL_RDR_ERRORf1(buf, "Unsupported Sereal protocol version %u", (unsigned int)version);
    if (expect_false(has_checksum && version < 6))
        SRL_RDR_ERROR(buf, "Checksummed Sereal documents require protocol version 6 or later");
    if (expect_false(!has_checksum && checksum->policy == SRL_CHECKSUM_POLICY_REQUIRE))
        SRL_RDR_ERROR(buf, "checksum_policy 'require_key' requires a checksummed packet");

    switch (encoding) {
        case SRL_PROTOCOL_ENCODING_RAW:
        case SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL:
            break;
        case SRL_PROTOCOL_ENCODING_SNAPPY:
            if (expect_false(version >= 2 && !has_checksum))
                SRL_RDR_ERROR(buf, "Non-incremental Snappy compression requires protocol version 1");
            break;
        case SRL_PROTOCOL_ENCODING_ZLIB:
            if (expect_false(version < 3))
                SRL_RDR_ERROR(buf, "ZLIB compression requires protocol version 3 or later");
            break;
        case SRL_PROTOCOL_ENCODING_ZSTD:
            if (expect_false(version < 4))
                SRL_RDR_ERROR(buf, "Zstd compression requires protocol version 4 or later");
            break;
        default:
            SRL_RDR_ERRORf1(buf, "Sereal document encoded in an unknown format '%d'",
                            encoding >> SRL_PROTOCOL_VERSION_BITS);
    }

    /* the tools have no use for header user data, so skip the suffix */
    header_len = srl_read_varint_uv_length(aTHX_ buf, " while reading header");
    buf->pos += header_len;

    if (has_checksum) {
        srl_checksum_envelope_t envelope;
        srl_read_checksum_envelope(aTHX_ buf, &envelope);
        srl_checksum_apply_policy(aTHX_ buf, &envelope, checksum);
        buf->pos = envelope.payload;
        buf->end = envelope.payload + envelope.payload_len;
    }

    switch (encoding) {
        case SRL_PROTOCOL_ENCODING_SNAPPY:
        case SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL:
            srl_decompress_body_snappy(aTHX_ buf, encoding, buf_owner, 0);
            break;
        case SRL_PROTOCOL_ENCODING_ZLIB:
            srl_decompress_body_zlib(aTHX_ buf, buf_owner, 0);
            break;
        case SRL_PROTOCOL_ENCODING_ZSTD:
            srl_decompress_body_zstd(aTHX_ buf, buf_owner, 0);
            break;
    }

    /* this *MUST* come after srl_decompress_body_*() */
    SRL_RDR_UPDATE_BODY_POS(buf, version);
    DEBUG_ASSERT_RDR_SANE(buf);
    return version;
}

#endif
