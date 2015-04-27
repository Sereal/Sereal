#ifndef SRL_READER_DECOMPRESS_H_
#define SRL_READER_DECOMPRESS_H_

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_protocol.h"

#if defined(HAVE_CSNAPPY)
    #include <csnappy.h>
#else
/* PLEASE READ!!!
 * Since the decompression code was migrated to this header file
 * and it's not a good practise to include C files into headers (actually, doing so
 * cases 'duplicate symbol' errors in Sereal::Path, expected huh?) and I didn't
 * found right way how to compile snappy's implementation files and link them
 * (main problem is that snappy files in a subdirectory) I'm only including
 * header file here and a dev should include C file in his code.
 *
 * It should be something like this:
 * #if !defined(HAVE_CSNAPPY)
 * # include "snappy/csnappy_decompress.c"
 * #endif
 */
    #include "snappy/csnappy.h"
#endif

#if defined(HAVE_MINIZ)
    #include <miniz.h>
#else
    #include "miniz.h"
#endif

/* Creates a new buffer of size header_len + body_len + 1 and swaps it into place
 * of the current reader's buffer. Sets reader position to right after the
 * header and makes the reader state internally consistent. The buffer is
 * owned by a mortal SV which is returned. */
/* TODO reuse the buffer */

SRL_STATIC_INLINE SV *
srl_realloc_empty_buffer(pTHX_ srl_reader_buffer_t *buf,
                         const STRLEN header_len,
                         const STRLEN body_len)
{
    SV *b_sv;
    srl_reader_char_ptr b;

    /* Let perl clean this up. Yes, it's not the most efficient thing
     * ever, but it's just one mortal per full decompression, so not
     * a bottle-neck. */
    b_sv = sv_2mortal( newSV(header_len + body_len + 1 ));
    b = (srl_reader_char_ptr) SvPVX(b_sv);

    buf->start = b;
    buf->pos = b + header_len;
    buf->end = buf->pos + body_len;

    SRL_RDR_UPDATE_BODY_POS(buf);
    return b_sv;
}

/* Decompress a Snappy-compressed document body and put the resulting document
 * body back in the place of the old compressed blob. The function internaly
 * creates temporary buffer which is owned by mortal SV. If the caller is
 * interested in keeping the buffer around for longer time, it should pass
 * buf_owner parameter and unmortalize it. */

SRL_STATIC_INLINE UV
srl_decompress_body_snappy(pTHX_ srl_reader_buffer_t *buf, SV** buf_owner)
{
    SV *buf_sv;
    int header_len;
    int decompress_ok;
    uint32_t dest_len;
    UV bytes_consumed;

    srl_reader_char_ptr old_pos;
    const STRLEN sereal_header_len = (STRLEN) SRL_RDR_POS_OFS(buf);
    const STRLEN compressed_packet_len =
        buf->encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL
        ? (STRLEN) srl_read_varint_uv_length(aTHX_ buf, " while reading compressed packet size")
        : (STRLEN) SRL_RDR_SPACE_LEFT(buf);

    /* All bufl's above here, or we break C89 compilers */
    old_pos = buf->pos;
    bytes_consumed = compressed_packet_len + SRL_RDR_POS_OFS(buf);
    header_len = csnappy_get_uncompressed_length((char *)buf->pos,
                                                 compressed_packet_len,
                                                 &dest_len);

    if (header_len == CSNAPPY_E_HEADER_BAD)
        SRL_RDR_ERROR(buf, "Invalid Snappy header in Snappy-compressed Sereal packet");

    /* Allocate output buffer and swap it into place within the bufoder. */
    buf_sv = srl_realloc_empty_buffer(aTHX_ buf, sereal_header_len, dest_len);
    if (buf_owner) *buf_owner = buf_sv;

    decompress_ok = csnappy_decompress_noheader((char *)(old_pos + header_len),
                                                compressed_packet_len - header_len,
                                                (char *)buf->pos,
                                                &dest_len);

    if (expect_false( decompress_ok != 0 )) {
        SRL_RDR_ERRORf1(buf, "Snappy decompression of Sereal packet payload failed with error %i!",
                        decompress_ok);
    }

    return bytes_consumed;
}

/* Decompress a zlib-compressed document body and put the resulting
 * document body back in the place of the old compressed blob. The function
 * internaly creates temporary buffer which is owned by mortal SV. If the
 * caller is interested in keeping the buffer around for longer time, it should
 * pass buf_owner parameter and unmortalize it. */

SRL_STATIC_INLINE UV
srl_decompress_body_zlib(pTHX_ srl_reader_buffer_t *buf, SV** buf_owner)
{
    SV *buf_sv;
    mz_ulong tmp;
    int decompress_ok;
    UV bytes_consumed;
    srl_reader_char_ptr old_pos;
    const STRLEN sereal_header_len = (STRLEN)SRL_RDR_POS_OFS(buf);
    const STRLEN uncompressed_packet_len = (STRLEN)srl_read_varint_uv(aTHX_ buf);
    const STRLEN compressed_packet_len =
        (STRLEN)srl_read_varint_uv_length(aTHX_ buf, " while reading compressed packet size");

    /* All decl's above here, or we break C89 compilers */
    old_pos = buf->pos;
    bytes_consumed = compressed_packet_len + SRL_RDR_POS_OFS(buf);

    /* Allocate output buffer and swap it into place within the decoder. */
    buf_sv = srl_realloc_empty_buffer(aTHX_ buf, sereal_header_len, uncompressed_packet_len);
    if (buf_owner) *buf_owner = buf_sv;

    tmp = uncompressed_packet_len;
    decompress_ok = mz_uncompress((unsigned char *)buf->pos,
                                  &tmp, old_pos, compressed_packet_len);

    if (expect_false( decompress_ok != Z_OK )) {
        SRL_RDR_ERRORf1(buf, "ZLIB decompression of Sereal packet payload failed with error %i!", decompress_ok);
    }

    return bytes_consumed;
}

#endif
