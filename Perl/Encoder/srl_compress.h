#ifndef SRL_COMPRESS_H_
#define SRL_COMPRESS_H_

#include "srl_buffer.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "srl_buffer_types.h"

/* WARNING: This is different from the protocol bit SRL_PROTOCOL_ENCODING_SNAPPY
 *          and SRL_PROTOCOL_ENCODING_ZLIB in that it's a flag indicating that
 *          we want to use Snappy or Zlib.
 *
 * DO NOT CHANGE THIS WITHOUT REVIEWING THE BITS IN srl_encoder.h and etc.
 */

#define SRL_F_COMPRESS_SNAPPY                   0x00040UL
#define SRL_F_COMPRESS_SNAPPY_INCREMENTAL       0x00080UL
#define SRL_F_COMPRESS_ZLIB                     0x00100UL
#define SRL_F_COMPRESS_ZSTD                     0x40000UL
/* WARNING: IF ADDING NEW COMPRESSION MAKE SURE THAT NEW CONSTANT DOES NOT
 *          COLLIDE WITH CONSTANTS IN srl_encoder.h!
 */

#define SRL_F_COMPRESS_FLAGS_MASK               (SRL_F_COMPRESS_SNAPPY | \
                                                 SRL_F_COMPRESS_SNAPPY_INCREMENTAL | \
                                                 SRL_F_COMPRESS_ZLIB | \
                                                 SRL_F_COMPRESS_ZSTD)

#if defined(HAVE_CSNAPPY)
#include <csnappy.h>
#else
#include "snappy/csnappy_compress.c"
#endif

#if defined(HAVE_MINIZ)
#include <miniz.h>
#else
#include "miniz.h"
#endif

#if defined(HAVE_ZSTD)
#include <zstd.h>
#else
#include "zstd/zstd.h"
#endif

/* Update a varint anywhere in the output stream with defined start and end
 * positions. This can produce non-canonical varints and is useful for filling
 * pre-allocated varints. */
SRL_STATIC_INLINE void
srl_update_varint_from_to(pTHX_ unsigned char *varint_start, unsigned char *varint_end, UV number)
{
    while (number >= 0x80) {                      /* while we are larger than 7 bits long */
        *varint_start++ = (number & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        number = number >> 7;                     /* shift off the 7 least significant bits */
    }

    /* if it is the same size we can use a canonical varint */
    if ( varint_start == varint_end ) {
        *varint_start = number;                   /* encode the last 7 bits without the high bit being set */
    } else {
        /* if not we produce a non-canonical varint, basically we stuff
         * 0 bits (via 0x80) into the "tail" of the varint, until we can
         * stick in a null to terminate the sequence. This means that the
         * varint is effectively "self-padding", and we only need special
         * logic in the encoder - a decoder will happily process a non-canonical
         * varint with no problem */
        *varint_start++ = (number & 0x7f) | 0x80;
        while ( varint_start < varint_end )
            *varint_start++ = 0x80;
        *varint_start= 0;
    }
}

/* Lazy working buffer alloc */
SRL_STATIC_INLINE void
srl_init_snappy_workmem(pTHX_ void **workmem)
{
    /* Lazy working buffer alloc */
    if (expect_false(*workmem == NULL)) {
        /* Cleaned up automatically by the cleanup handler */
        Newx(*workmem, CSNAPPY_WORKMEM_BYTES, char);
        if (*workmem == NULL)
            croak("Out of memory!");
    }
}

/* Destroy working buffer */
SRL_STATIC_INLINE void
srl_destroy_snappy_workmem(pTHX_ void *workmem)
{
    Safefree(workmem);
}

SRL_STATIC_INLINE U8
srl_get_compression_header_flag(const U32 compress_flags)
{
    if (compress_flags & SRL_F_COMPRESS_SNAPPY) {
        return SRL_PROTOCOL_ENCODING_SNAPPY;
    } else if (compress_flags & SRL_F_COMPRESS_SNAPPY_INCREMENTAL) {
        return SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL;
    } else if (compress_flags & SRL_F_COMPRESS_ZLIB) {
        return SRL_PROTOCOL_ENCODING_ZLIB;
    } else if (compress_flags & SRL_F_COMPRESS_ZSTD) {
        return SRL_PROTOCOL_ENCODING_ZSTD;
    } else {
        return SRL_PROTOCOL_ENCODING_RAW;
    }
}

/* Sets the compression header flag */
SRL_STATIC_INLINE void
srl_set_compression_header_flag(srl_buffer_t *buf, const U32 compress_flags)
{
    /* sizeof(const char *) includes a count of \0 */
    srl_buffer_char *flags_and_version_byte = buf->start + sizeof(SRL_MAGIC_STRING) - 1;
    *flags_and_version_byte |= srl_get_compression_header_flag(compress_flags);
}

/* Resets the compression header flag to OFF.
 * Obviously requires that a Sereal header was already written to the
 * encoder's output buffer. */
SRL_STATIC_INLINE void
srl_reset_compression_header_flag(srl_buffer_t *buf)
{
    /* sizeof(const char *) includes a count of \0 */
    srl_buffer_char *flags_and_version_byte = buf->start + sizeof(SRL_MAGIC_STRING) - 1;

    /* disable snappy flag in header */
    *flags_and_version_byte = SRL_PROTOCOL_ENCODING_RAW |
                              (*flags_and_version_byte & SRL_PROTOCOL_VERSION_MASK);
}

/* Compress body with one of available compressors (zlib, snappy).
 * The function sets/resets compression bits at version byte.
 * The caller has to adjust buf->body_pos by calling SRL_UPDATE_BODY_POS
 * right after exiting from srl_compress_body.
 */

SRL_STATIC_INLINE void
srl_compress_body(pTHX_ srl_buffer_t *buf, STRLEN sereal_header_length,
                  const U32 compress_flags, const int compress_level, void **workmem)
{
    const int is_traditional_snappy = compress_flags & SRL_F_COMPRESS_SNAPPY;
    const int is_incremental_snappy = compress_flags & SRL_F_COMPRESS_SNAPPY_INCREMENTAL;
    const int is_zstd = compress_flags & SRL_F_COMPRESS_ZSTD;
    const int is_zlib = !is_traditional_snappy && !is_incremental_snappy && !is_zstd;

    size_t uncompressed_body_length = BUF_POS_OFS(buf) - sereal_header_length;
    size_t compressed_body_length;
    srl_buffer_char *varint_start = NULL;
    srl_buffer_char *varint_end = NULL;
    srl_buffer_t old_buf;

    DEBUG_ASSERT_BUF_SANE(buf);

    /* Get estimated compressed payload length */
    if (is_incremental_snappy) {
        compressed_body_length = (size_t) csnappy_max_compressed_length(uncompressed_body_length);
        compressed_body_length += SRL_MAX_VARINT_LENGTH; /* will have to embed compressed packet length as varint */
    } else if (is_traditional_snappy) {
        compressed_body_length = (size_t) csnappy_max_compressed_length(uncompressed_body_length);
    } else if (is_zstd) {
        compressed_body_length = ZSTD_compressBound(uncompressed_body_length);
        compressed_body_length += SRL_MAX_VARINT_LENGTH; /* will have to embed compressed packet length as varint */
    } else {
        compressed_body_length = (size_t) mz_compressBound(uncompressed_body_length);
        compressed_body_length += SRL_MAX_VARINT_LENGTH; /* will have to embed uncommpressed packet length as varint */
        compressed_body_length += SRL_MAX_VARINT_LENGTH; /* will have to embed compressed packet length as varint */
    }

    /* Back up old buffer and allocate new one with correct size */
    srl_buf_copy_buffer(aTHX_ buf, &old_buf);
    srl_buf_init_buffer(aTHX_ buf, sereal_header_length + compressed_body_length + 1);

    /* Copy Sereal header */
    Copy(old_buf.start, buf->pos, sereal_header_length, char);
    buf->pos += sereal_header_length;

    /* Embed uncompressed packet length if Zlib */
    if (is_zlib) srl_buf_cat_varint_nocheck(aTHX_ buf, 0, uncompressed_body_length);

    /* Embed compressed packet length if incr. Snappy, Zlib or Zstd*/
    if (is_incremental_snappy || is_zlib || is_zstd) {
        varint_start = buf->pos;
        srl_buf_cat_varint_nocheck(aTHX_ buf, 0, compressed_body_length);
        varint_end = buf->pos - 1;
    }

    if (is_incremental_snappy || is_traditional_snappy) {
        uint32_t len = (uint32_t) compressed_body_length;
        srl_init_snappy_workmem(aTHX_ workmem);

        csnappy_compress((char*) old_buf.start + sereal_header_length, (uint32_t) uncompressed_body_length,
                         (char*) buf->pos, &len, *workmem, CSNAPPY_WORKMEM_BYTES_POWER_OF_TWO);

        compressed_body_length = (size_t) len;
    } else if (is_zstd) {
        size_t code = ZSTD_compress((void*) buf->pos, compressed_body_length,
                                    (void*) old_buf.start + sereal_header_length, uncompressed_body_length,
                                    compress_level);

        assert(ZSTD_isError(code) == 0);
        compressed_body_length = code;
    } else if (is_zlib) {
        mz_ulong dl = (mz_ulong) compressed_body_length;
        int status = mz_compress2(
            buf->pos,
            &dl,
            old_buf.start + sereal_header_length,
            (mz_ulong) uncompressed_body_length,
            compress_level
        );

        (void)status;
        assert(status == Z_OK);
        compressed_body_length = (size_t) dl;
    }

    assert(compressed_body_length != 0);

    /* If compression didn't help, swap back to old, uncompressed buffer */
    if (compressed_body_length >= uncompressed_body_length) {
        /* swap in old, uncompressed buffer */
        srl_buf_swap_buffer(aTHX_ buf, &old_buf);
        /* disable compression flag */
        srl_reset_compression_header_flag(buf);
    } else { /* go ahead with Snappy and do final fixups */
        /* overwrite the max size varint with the real size of the compressed data */
        if (varint_start)
            srl_update_varint_from_to(aTHX_ varint_start, varint_end, compressed_body_length);

        buf->pos += compressed_body_length;

        /* enable compression flag */
        srl_set_compression_header_flag(buf, compress_flags);
    }

    srl_buf_free_buffer(aTHX_ &old_buf);
    DEBUG_ASSERT_BUF_SANE(buf);
}

#endif
