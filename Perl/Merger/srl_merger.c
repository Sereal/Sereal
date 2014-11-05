/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"
#ifdef __cplusplus
}
#endif

#include <stdlib.h>

#ifndef PERL_VERSION
#    include <patchlevel.h>
#    if !(defined(PERL_VERSION) || (PERL_SUBVERSION > 0 && defined(PATCHLEVEL)))
#        include <could_not_find_Perl_patchlevel.h>
#    endif
#    define PERL_REVISION       5
#    define PERL_VERSION        PATCHLEVEL
#    define PERL_SUBVERSION     PERL_SUBVERSION
#endif
#if PERL_VERSION < 8
#   define PERL_MAGIC_qr                  'r' /* precompiled qr// regex */
#   define BFD_Svs_SMG_OR_RMG SVs_RMG
#elif ((PERL_VERSION==8) && (PERL_SUBVERSION >= 1) || (PERL_VERSION>8))
#   define BFD_Svs_SMG_OR_RMG SVs_SMG
#   define MY_PLACEHOLDER PL_sv_placeholder
#else
#   define BFD_Svs_SMG_OR_RMG SVs_RMG
#   define MY_PLACEHOLDER PL_sv_undef
#endif
#if (((PERL_VERSION == 9) && (PERL_SUBVERSION >= 4)) || (PERL_VERSION > 9))
#   define NEW_REGEX_ENGINE 1
#endif
#if (((PERL_VERSION == 8) && (PERL_SUBVERSION >= 1)) || (PERL_VERSION > 8))
#define MY_CAN_FIND_PLACEHOLDERS
#define HAS_SV2OBJ
#endif

#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)
#define SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_ARRAYREF_COUNT)
#define SRL_HDR_HASHREF_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_HASHREF_COUNT)

#define SRL_GET_STRING_DEDUPER_TBL(mrg)  (expect_false((mrg)->string_deduper_tbl == NULL)  \
                                         ? srl_init_string_deduper_tbl(aTHX_ mrg)          \
                                         : (mrg)->string_deduper_tbl)

#define SRL_GET_CLASSNAME_DEDUPER_TBL(mrg) (expect_false((mrg)->classname_deduper_tbl == NULL)  \
                                           ? srl_init_classname_deduper_tbl(aTHX_ mrg)          \
                                           : (mrg)->classname_deduper_tbl)

#define SRL_GET_TRACKED_OFFSETS_TBL(mrg) (expect_false((mrg)->tracked_offsets_tbl == NULL) \
                                         ? srl_init_tracked_offsets_tbl(aTHX_ mrg)         \
                                         : (mrg)->tracked_offsets_tbl)

#define SRL_GET_TRACKED_OFFSETS(mrg)     (expect_false((mrg)->tracked_offsets == NULL)    \
                                         ? srl_init_tracked_offsets(aTHX_ mrg)            \
                                         : (mrg)->tracked_offsets)

//#define SRL_MERGER_TRACE(msg, args...) warn((msg), args)
#define SRL_MERGER_TRACE(msg, args...)

#define SRL_REPORT_CURRENT_TAG(mrg, tag) STMT_START {                \
    SRL_MERGER_TRACE(                                                \
        "%s: tag %d (0x%X) at abs %d, rel %d (obuf abs %d, rel %d)", \
        __FUNCTION__, (tag), (tag),                                  \
        (int) BUF_POS_OFS(mrg->ibuf),                                \
        (int) BODY_POS_OFS(mrg->ibuf),                               \
        (int) BUF_POS_OFS(mrg->obuf),                                \
        (int) BODY_POS_OFS(mrg->obuf)                                \
    );                                                               \
} STMT_END

#define ASSERT_BUF_SPACE(buf, len, msg) STMT_START {         \
    if (expect_false((UV) BUF_SPACE((buf)) < (UV) (len))) {  \
        SRL_ERRORf3(                                         \
            (buf),                                           \
            "Unexpected termination of packet%s, "           \
            "want %lu bytes, only have %lu available",       \
            (msg), (UV) (len), (UV) BUF_SPACE((buf))         \
        );                                                   \
    }                                                        \
} STMT_END

#define GROW_BUF(buf, minlen) STMT_START {                            \
    DEBUG_ASSERT_BUF_SANE(buf);                                       \
    if (expect_false(BUF_NEED_GROW(buf, minlen))) {                   \
        srl_buf_grow_nocheck(aTHX_ &(buf), (BUF_SIZE(buf) + minlen)); \
        DEBUG_ASSERT_BUF_SANE(buf);                                   \
    }                                                                 \
} STMT_END

#define SRL_UPDATE_BUF_BODY_POS(buf, version) STMT_START { \
    if (expect_false((version) == 1)) {                    \
        SRL_SET_BODY_POS((buf), (buf).start);              \
    } else {                                               \
        SRL_SET_BODY_POS((buf), (buf).pos - 1);            \
    }                                                      \
} STMT_END

#define srl_buf_cat_tag_nocheck(mrg, tag) STMT_START { \
    GROW_BUF((mrg)->obuf, 1);                          \
                                                       \
    *(mrg)->obuf.pos++ = (tag);                        \
    (mrg)->ibuf.pos++;                                 \
                                                       \
    DEBUG_ASSERT_BUF_SANE((mrg)->ibuf);                \
    DEBUG_ASSERT_BUF_SANE((mrg)->obuf);                \
} STMT_END

/* srl_buffer.h has set of functions (srl_buf_cat_*) which I need in merger,
 * but, for performance reason (avoid another level of inderection),
 * the functions want srl_encoder_t* as first parameter to access the buffer.
 * Hopefully, buffer is first field inside srl_merget_t structure, so if I
 * put buffer at same position it's semi-safely to cast srl_merget_t* to
 * srl_encoder_t*
 */
#define MRG2ENC(mrg) ((srl_encoder_t *) (mrg))
#define SRL_MRG_SET_OPTION(mrg, flag_num) ((mrg)->flags |= (flag_num))
#define SRL_MRG_HAVE_OPTION(mrg, flag_num) ((mrg)->flags & (flag_num))

#define SRL_MAX_VARINT_LENGTH_U32 5

#include "srl_merger.h"
#include "srl_common.h"
#include "ptable.h"
#include "strtable.h"
#include "srl_protocol.h"
#include "srl_inline.h"
#include "srl_mrg_error.h"
#include "../Encoder/srl_buffer.h"
#include "snappy/csnappy_decompress.c"
#include "snappy/csnappy_compress.c"

typedef PTABLE_ENTRY_t *ptable_entry_ptr;

/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE UV srl_read_varint_uv_safe(pTHX_ srl_buffer_t *buf);
SRL_STATIC_INLINE UV srl_read_varint_uv_nocheck(pTHX_ srl_buffer_t *buf);
SRL_STATIC_INLINE UV srl_read_varint_uv(pTHX_ srl_buffer_t *buf);
SRL_STATIC_INLINE UV srl_read_varint_uv_offset(pTHX_ srl_buffer_t *buf, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_length(pTHX_ srl_buffer_t *buf, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_count(pTHX_ srl_buffer_t *buf, const char * const errstr);
SRL_STATIC_INLINE IV srl_validate_header_version_pv_len(pTHX_ char *strdata, STRLEN len);
SRL_STATIC_INLINE UV srl_decompress_body_snappy(pTHX_ srl_buffer_t *buf, int incremental);
SRL_STATIC_INLINE void srl_update_varint_from_to(pTHX_ char *varint_start, char *varint_end, UV number);
SRL_STATIC_INLINE void srl_init_snappy_workmem(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE void srl_buf_copy_content_nocheck(pTHX_ srl_merger_t *mrg, size_t len);
SRL_STATIC_INLINE void srl_copy_varint(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE ptable_ptr   srl_init_tracked_offsets_tbl(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE strtable_ptr srl_init_string_deduper_tbl(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE strtable_ptr srl_init_classname_deduper_tbl(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE srl_stack_t * srl_init_tracked_offsets(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE srl_merger_t * srl_empty_merger_struct(pTHX);                         /* allocate an empty merger struct - flags still to be set up */
SRL_STATIC_INLINE void srl_set_input_buffer(pTHX_ srl_merger_t *mrg, SV *src);        /* reset input buffer (ibuf) */
SRL_STATIC_INLINE void srl_build_track_table(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE void srl_merge_single_value(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE void srl_merge_stringish(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE void srl_merge_hash(pTHX_ srl_merger_t *mrg, const U8 tag, UV length);
SRL_STATIC_INLINE void srl_merge_array(pTHX_ srl_merger_t *mrg, const U8 tag, UV length);
SRL_STATIC_INLINE void srl_merge_binary_utf8(pTHX_ srl_merger_t *mrg, ptable_entry_ptr ptable_entry);
SRL_STATIC_INLINE void srl_merge_short_binary(pTHX_ srl_merger_t *mrg, const U8 tag, ptable_entry_ptr ptable_entry);
SRL_STATIC_INLINE void srl_merge_object(pTHX_ srl_merger_t *mrg, const U8 objtag);

SRL_STATIC_INLINE ptable_entry_ptr srl_store_tracked_offset(pTHX_ srl_merger_t *mrg, UV from, UV to);
SRL_STATIC_INLINE UV srl_lookup_tracked_offset(pTHX_ srl_merger_t *mrg, UV offset);
SRL_STATIC_INLINE strtable_entry_ptr srl_lookup_string(pTHX_ srl_merger_t *mrg, const char *src, STRLEN len, int *ok);
SRL_STATIC_INLINE strtable_entry_ptr srl_lookup_classname(pTHX_ srl_merger_t *mrg, const char *src, STRLEN len, int *ok);

SRL_STATIC_INLINE ptable_ptr
srl_init_tracked_offsets_tbl(pTHX_ srl_merger_t *mrg)
{
    mrg->tracked_offsets_tbl = PTABLE_new();
    return mrg->tracked_offsets_tbl;
}

SRL_STATIC_INLINE strtable_ptr
srl_init_string_deduper_tbl(pTHX_ srl_merger_t *mrg)
{
    mrg->string_deduper_tbl = STRTABLE_new(&mrg->obuf);
    return mrg->string_deduper_tbl;
}

SRL_STATIC_INLINE strtable_ptr
srl_init_classname_deduper_tbl(pTHX_ srl_merger_t *mrg)
{
    mrg->classname_deduper_tbl = STRTABLE_new(&mrg->obuf);
    return mrg->classname_deduper_tbl;
}

SRL_STATIC_INLINE srl_stack_t *
srl_init_tracked_offsets(pTHX_ srl_merger_t *mrg)
{
    mrg->tracked_offsets = NULL;
    Newx(mrg->tracked_offsets, 1, srl_stack_t);

    if (expect_false(mrg->tracked_offsets == NULL))
        croak("Out of memory");

    if (expect_false(srl_stack_init(aTHX_ mrg->tracked_offsets, 16) != 0)) {
        Safefree(mrg->tracked_offsets);
        mrg->tracked_offsets = NULL;
        croak("Out of memory");
    }

    return mrg->tracked_offsets;
}

srl_merger_t *
srl_build_merger_struct(pTHX_ HV *opt)
{
    srl_merger_t *mrg;
    U8 flags;
    SV **svp;
    int i;

    mrg = srl_empty_merger_struct(aTHX);

    /* load options */
    if (opt != NULL) {
        /* Needs to be before the snappy options */
        /* mrg->protocol_version defaults to SRL_PROTOCOL_VERSION. */
        svp = hv_fetchs(opt, "protocol_version", 0);
        if (svp && SvOK(*svp)) {
            mrg->protocol_version = SvUV(*svp);
            if (mrg->protocol_version < 1 || mrg->protocol_version > SRL_PROTOCOL_VERSION) {
                croak("Specified Sereal protocol version ('%lu') is invalid",
                      (unsigned long) mrg->protocol_version);
            }
        }

        svp = hv_fetchs(opt, "top_level_element", 0);
        if (svp && SvOK(*svp)) {
            switch (SvUV(*svp)) {
                case 0: /* SCALAR */
                    SRL_MRG_SET_OPTION(mrg, SRL_F_TOPLEVEL_KEY_SCALAR);
                    break;

                case 1: /* ARRAYREF */
                    SRL_MRG_SET_OPTION(mrg, SRL_F_TOPLEVEL_KEY_ARRAY);
                    break;

                case 2: /* HASHREF */
                    SRL_MRG_SET_OPTION(mrg, SRL_F_TOPLEVEL_KEY_HASH);
                    break;

                default:
                    croak("Invalid Sereal::Merger top level element");
            }
        }

        svp = hv_fetchs(opt, "dedupe_strings", 0);
        if (svp && SvTRUE(*svp))
            SRL_MRG_SET_OPTION(mrg, SRL_F_DEDUPE_STRINGS);

        svp = hv_fetchs(opt, "compress", 0);
        if (svp && SvOK(*svp)) {
            switch (SvIV(*svp)) {
                case 0: /* uncompressed */
                    break;

                case 1: /* snappy incremental */
                    SRL_MRG_SET_OPTION(mrg, SRL_F_COMPRESS_SNAPPY_INCREMENTAL);
                    break;

                default:
                    croak("Invalid Sereal compression format");
            }
        }
    }

    /* 4 byte magic string + proto version
     * + potentially uncompressed size varint
     * +  1 byte varint that indicates zero-length header
     * if not SRL_F_TOPLEVEL_KEY_SCALAR
     * +  1 byte SRL_HDR_REFN
     * +  1 byte SRL_HDR_ARRAY|HASH
     * +  SRL_MAX_VARINT_LENGTH_U32 bytes for padding varint */
    GROW_BUF(mrg->obuf, sizeof(SRL_MAGIC_STRING) + 1 + 1 +
             SRL_MRG_HAVE_OPTION(mrg, SRL_F_TOPLEVEL_KEY_SCALAR) ? 0 : 1 + 1 + SRL_MAX_VARINT_LENGTH_U32);

    if (expect_true(mrg->protocol_version > 2)) {
        srl_buf_cat_str_s_nocheck(MRG2ENC(mrg), SRL_MAGIC_STRING_HIGHBIT);
    } else {
        srl_buf_cat_str_s_nocheck(MRG2ENC(mrg), SRL_MAGIC_STRING);
    }

    flags = SRL_MRG_HAVE_OPTION(mrg, SRL_F_COMPRESS_SNAPPY_INCREMENTAL) ? SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL : 0;

    srl_buf_cat_char_nocheck(MRG2ENC(mrg), (U8) mrg->protocol_version | flags);
    srl_buf_cat_char_nocheck(MRG2ENC(mrg), '\0');

    SRL_UPDATE_BUF_BODY_POS(mrg->obuf, mrg->protocol_version);

    if (!SRL_MRG_HAVE_OPTION(mrg, SRL_F_TOPLEVEL_KEY_SCALAR)) {
        srl_buf_cat_char_nocheck(MRG2ENC(mrg), SRL_HDR_REFN);
        srl_buf_cat_char_nocheck(MRG2ENC(mrg), SRL_MRG_HAVE_OPTION(mrg, SRL_F_TOPLEVEL_KEY_HASH) ? SRL_HDR_HASH : SRL_HDR_ARRAY);

        mrg->obuf_padding_bytes_offset = BUF_POS_OFS(mrg->obuf);
        for (i = 0; i < SRL_MAX_VARINT_LENGTH_U32; ++i) {
            srl_buf_cat_char_nocheck(MRG2ENC(mrg), SRL_HDR_PAD);
        }
    }

    return mrg;
}

void
srl_destroy_merger(pTHX_ srl_merger_t *mrg)
{
    srl_buf_free_buffer(aTHX_ &mrg->obuf);

    Safefree(mrg->snappy_workmem);

    if (mrg->tracked_offsets) {
        srl_stack_destroy(aTHX_ mrg->tracked_offsets);
        Safefree(mrg->tracked_offsets);
        mrg->tracked_offsets = NULL;
    }

    if (mrg->tracked_offsets_tbl) {
        PTABLE_free(mrg->tracked_offsets_tbl);
        mrg->tracked_offsets_tbl = NULL;
    }

    if (mrg->string_deduper_tbl) {
        STRTABLE_free(mrg->string_deduper_tbl);
        mrg->string_deduper_tbl = NULL;
    }

    if (mrg->classname_deduper_tbl) {
        STRTABLE_free(mrg->classname_deduper_tbl);
        mrg->classname_deduper_tbl = NULL;
    }

    Safefree(mrg);
}

void
srl_merger_append(pTHX_ srl_merger_t *mrg, SV *src)
{
    assert(mrg != NULL);

    srl_set_input_buffer(aTHX_ mrg, src);
    srl_build_track_table(aTHX_ mrg);

    /* preallocate space in obuf,
     * but this is still not enough because due to
     * varint we might need more space in obug then size of ibuf */
    GROW_BUF(mrg->obuf, (size_t) BUF_SIZE(mrg->ibuf));

    mrg->ibuf.pos = mrg->ibuf.body_pos + 1;
    srl_merge_single_value(aTHX_ mrg);

    mrg->cnt_of_merged_elements++;
}

void
srl_merger_append_all(pTHX_ srl_merger_t *mrg, AV *src)
{
    assert(mrg != NULL);

    SSize_t i;
    SV **svptr;
    SSize_t tidx = av_top_index(src);

    STRLEN size = 0;
    for (i = 0; i <= tidx; ++i) {
        svptr = av_fetch(src, i, 0);
        if (expect_false(svptr == NULL))
            croak("av_fetch returned NULL");

        size += SvLEN(*svptr);
    }

    /* preallocate space in obuf in one go,
     * of course this's is very rough estimation */
    GROW_BUF(mrg->obuf, size);

    for (i = 0; i <= tidx; ++i) {
        srl_set_input_buffer(aTHX_ mrg, *av_fetch(src, i, 0));
        srl_build_track_table(aTHX_ mrg);

        mrg->ibuf.pos = mrg->ibuf.body_pos + 1;
        srl_merge_single_value(aTHX_ mrg);

        mrg->cnt_of_merged_elements++;
    }
}

SV *
srl_merger_finish(pTHX_ srl_merger_t *mrg)
{
    assert(mrg != NULL);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    if (!SRL_MRG_HAVE_OPTION(mrg, SRL_F_TOPLEVEL_KEY_SCALAR)) {
        char* oldpos = mrg->obuf.pos;
        mrg->obuf.pos = mrg->obuf.start + mrg->obuf_padding_bytes_offset;
        DEBUG_ASSERT_BUF_SANE(mrg->obuf);

        srl_buf_cat_varint_nocheck(aTHX_ MRG2ENC(mrg), 0, mrg->cnt_of_merged_elements);
        DEBUG_ASSERT_BUF_SANE(mrg->obuf);

        mrg->obuf.pos = oldpos;
        DEBUG_ASSERT_BUF_SANE(mrg->obuf);
    }

    if (SRL_MRG_HAVE_OPTION(mrg, SRL_F_COMPRESS_SNAPPY_INCREMENTAL)) {
        // there is no support of user's Sereal header,
        // so body's offset is fixed and always 5 (i.e. =srl + 1 byte for version + 1 byte for header)
        // TODO compress_threshold
        // TODO max body length < 4GB

        srl_buffer_t old_buf;
        size_t compressed_body_len;
        size_t sereal_header_len = 6;
        char *varint_start, *varint_end;
        uint32_t uncompressed_body_len = BUF_POS_OFS(mrg->obuf) - sereal_header_len;

        assert(BUF_POS_OFS(mrg->obuf) > sereal_header_len);

        compressed_body_len = csnappy_max_compressed_length(uncompressed_body_len);
        compressed_body_len += SRL_MAX_VARINT_LENGTH;
        srl_init_snappy_workmem(aTHX_ mrg);

        /* Back up old buffer and allocate new one with correct size */
        srl_buf_copy_buffer(aTHX_ &mrg->obuf, &old_buf);
        srl_buf_init_buffer(aTHX_ &mrg->obuf, compressed_body_len);

        /* Copy Sereal header */
        Copy(old_buf.start, mrg->obuf.pos, sereal_header_len, char);
        mrg->obuf.pos += sereal_header_len;
        SRL_UPDATE_BUF_BODY_POS(mrg->obuf, mrg->protocol_version);

        /* Embed compressed packet length if incr. Snappy or Zlib*/
        varint_start = mrg->obuf.pos;
        srl_buf_cat_varint_nocheck(aTHX_ MRG2ENC(mrg), 0, compressed_body_len);
        varint_end = mrg->obuf.pos - 1;

        /* Compress */
        uint32_t len = (uint32_t) compressed_body_len;
        csnappy_compress(old_buf.start + sereal_header_len, uncompressed_body_len, mrg->obuf.pos,
                         &len, mrg->snappy_workmem, CSNAPPY_WORKMEM_BYTES_POWER_OF_TWO);

        compressed_body_len = (size_t) len;

        assert(compressed_body_len != 0);

        // TODO If compression didn't help, swap back to old, uncompressed buffer

        if (varint_start)
            srl_update_varint_from_to(aTHX_ varint_start, varint_end, compressed_body_len);

        mrg->obuf.pos += compressed_body_len;

        srl_buf_free_buffer(aTHX_ &old_buf);
        assert(mrg->obuf.pos <= mrg->obuf.end);
    }

    return newSVpvn(mrg->obuf.start, BUF_POS_OFS(mrg->obuf));
}

SRL_STATIC_INLINE srl_merger_t *
srl_empty_merger_struct(pTHX)
{
    srl_merger_t *mrg = NULL;
    Newx(mrg, 1, srl_merger_t);
    if (mrg == NULL)
        croak("Out of memory");

    /* Init buffer struct */
    if (expect_false(srl_buf_init_buffer(aTHX_ &mrg->obuf, INITIALIZATION_SIZE) != 0)) {
        Safefree(mrg);
        croak("Out of memory");
    }

    /* Zero fields */
    mrg->cnt_of_merged_elements = 0;
    mrg->obuf_padding_bytes_offset = 0;
    mrg->protocol_version = SRL_PROTOCOL_VERSION;
    mrg->classname_deduper_tbl = NULL;
    mrg->string_deduper_tbl = NULL;
    mrg->tracked_offsets_tbl = NULL;
    mrg->tracked_offsets = NULL;
    mrg->snappy_workmem = NULL;
    mrg->flags = 0;
    return mrg;
}

SRL_STATIC_INLINE void
srl_set_input_buffer(pTHX_ srl_merger_t *mrg, SV *src)
{
    char *tmp;
    STRLEN len;
    UV header_len;
    U8 encoding_flags;
    U8 protocol_version;

    tmp = (char*) SvPV(src, len);
    mrg->ibuf.start = mrg->ibuf.pos = tmp;
    mrg->ibuf.end = mrg->ibuf.start + len;

    IV proto_version_and_encoding_flags_int = srl_validate_header_version_pv_len(aTHX_ mrg->ibuf.start, len);

    if (proto_version_and_encoding_flags_int < 1) {
        if (proto_version_and_encoding_flags_int == 0)
            SRL_ERROR(mrg->ibuf, "Bad Sereal header: It seems your document was accidentally UTF-8 encoded");
        else
            SRL_ERROR(mrg->ibuf, "Bad Sereal header: Not a valid Sereal document.");
    }

    mrg->ibuf.pos += 5;
    encoding_flags = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_ENCODING_MASK);
    protocol_version = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_VERSION_MASK);

    if (expect_false(protocol_version > 3 || protocol_version < 1)) {
        SRL_ERRORf1(mrg->ibuf, "Unsupported Sereal protocol version %u", mrg->protocol_version);
    }

    // skip header in any case
    header_len = srl_read_varint_uv_length(aTHX_ &mrg->ibuf, " while reading header");
    mrg->ibuf.pos += header_len;

    if (encoding_flags == SRL_PROTOCOL_ENCODING_RAW) {
        /* no op */
    } else if (   encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY
               || encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL)
    {
        srl_decompress_body_snappy(aTHX_ &mrg->ibuf, encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL);
    } else if (encoding_flags == SRL_PROTOCOL_ENCODING_ZLIB) {
        SRL_ERROR(mrg->ibuf, "zlib compression is not implemented");
    } else {
        SRL_ERROR(mrg->ibuf, "Sereal document encoded in an unknown format");
    }

    SRL_UPDATE_BUF_BODY_POS(mrg->ibuf, protocol_version);
    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
}

SRL_STATIC_INLINE void
srl_build_track_table(pTHX_ srl_merger_t *mrg)
{
    U8 tag;
    UV offset, length;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);

    if (mrg->tracked_offsets)
        srl_stack_clear(aTHX_ mrg->tracked_offsets);

    while (expect_true(BUF_NOT_DONE(mrg->ibuf))) {
        /* since we're doing full pass, it's not necessary to
         * add items into tracked_offsets here. They will be added
         * by corresponding REFP/ALIAS/COPY and other tags */

        tag = *mrg->ibuf.pos & ~SRL_HDR_TRACK_FLAG;
        SRL_REPORT_CURRENT_TAG(mrg, tag);
        mrg->ibuf.pos++;

        if (tag >= SRL_HDR_SHORT_BINARY_LOW) {
            mrg->ibuf.pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        } else if (tag > SRL_HDR_NEG_HIGH && tag < SRL_HDR_ARRAYREF_LOW) {
            switch (tag) {
                case SRL_HDR_VARINT:
                case SRL_HDR_ZIGZAG:
                    srl_read_varint_uv(aTHX_ &mrg->ibuf); // TODO test/implement srl_skip_varint()
                    break;

                case SRL_HDR_FLOAT:         mrg->ibuf.pos += 4;     break;
                case SRL_HDR_DOUBLE:        mrg->ibuf.pos += 8;     break;
                case SRL_HDR_LONG_DOUBLE:   mrg->ibuf.pos += 16;    break;

                case SRL_HDR_BINARY:
                case SRL_HDR_STR_UTF8:
                    length = srl_read_varint_uv_length(aTHX_ &mrg->ibuf, " while reading BINARY or STR_UTF8");
                    mrg->ibuf.pos += length;
                    break;

                case SRL_HDR_HASH:
                case SRL_HDR_ARRAY:
                    srl_read_varint_uv_count(aTHX_ &mrg->ibuf, " while reading ARRAY or HASH");
                    break;

                case SRL_HDR_TRUE:
                case SRL_HDR_FALSE:
                case SRL_HDR_UNDEF:
                case SRL_HDR_CANONICAL_UNDEF:
                    // noop
                    break;

                default:
                    switch (tag) {
                        case SRL_HDR_COPY:
                        case SRL_HDR_REFP:
                        case SRL_HDR_ALIAS:
                        case SRL_HDR_OBJECTV:
                        case SRL_HDR_OBJECTV_FREEZE:
                            offset = srl_read_varint_uv_offset(aTHX_ &mrg->ibuf, " while reading COPY, OBJECTV or OBJECTV_FREEZE");
                            srl_stack_push(SRL_GET_TRACKED_OFFSETS(mrg), offset);
                            break;

                        case SRL_HDR_PAD:
                        case SRL_HDR_REFN:
                        case SRL_HDR_WEAKEN:
                        case SRL_HDR_EXTEND:
                        case SRL_HDR_REGEXP:
                        case SRL_HDR_OBJECT:
                        case SRL_HDR_OBJECT_FREEZE:
                            // noop
                            break;

                        default:
                            SRL_ERROR_UNIMPLEMENTED(mrg->ibuf, tag, "");
                            break;
                    }
            }
        }
    }

    if (mrg->tracked_offsets && !srl_stack_empty(mrg->tracked_offsets)) {
        srl_stack_rsort(aTHX_ mrg->tracked_offsets);
        srl_stack_dedupe(aTHX_ mrg->tracked_offsets);

        //int i = 0;
        //SRL_STACK_TYPE *ptr = mrg->tracked_offsets->begin;
        //while (ptr <= mrg->tracked_offsets->ptr) {
        //    warn("tracked_offsets: offset dedups idx %d offset %d\n", i, (int) *ptr);
        //    i++; ptr++;
        //}
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
}

SRL_STATIC_INLINE void
srl_merge_single_value(pTHX_ srl_merger_t *mrg)
{
    U8 tag;
    UV length, offset;
    ptable_entry_ptr ptable_entry;

read_again:
    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    ptable_entry = NULL;
    if (expect_false(BUF_DONE(mrg->ibuf)))
        SRL_ERROR(mrg->ibuf, "Unexpected termination of input buffer");

    tag = *mrg->ibuf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_REPORT_CURRENT_TAG(mrg, tag);

    if (mrg->tracked_offsets && !srl_stack_empty(mrg->tracked_offsets)) {
        UV itag_offset = BODY_POS_OFS(mrg->ibuf);
        if (expect_false(itag_offset == srl_stack_peek_nocheck(aTHX_ mrg->tracked_offsets))) {
            // trackme case
            srl_stack_pop_nocheck(mrg->tracked_offsets);
            ptable_entry = srl_store_tracked_offset(aTHX_ mrg, itag_offset, BODY_POS_OFS(mrg->obuf));
        }
    }

    if (tag <= SRL_HDR_NEG_HIGH) {
        srl_buf_cat_tag_nocheck(mrg, tag);
    } else if (tag >= SRL_HDR_ARRAYREF_LOW && tag <= SRL_HDR_ARRAYREF_HIGH) {
        srl_merge_array(aTHX_ mrg, tag, SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag));
    } else if (tag >= SRL_HDR_HASHREF_LOW && tag <= SRL_HDR_HASHREF_HIGH) {
        srl_merge_hash(aTHX_ mrg, tag, SRL_HDR_HASHREF_LEN_FROM_TAG(tag));
    } else if (tag >= SRL_HDR_SHORT_BINARY_LOW) {
        srl_merge_short_binary(aTHX_ mrg, tag, ptable_entry);
    } else {
        switch (tag) {
            case SRL_HDR_VARINT:
            case SRL_HDR_ZIGZAG:
                srl_buf_cat_tag_nocheck(mrg, tag);
                srl_copy_varint(aTHX_ mrg);
                break;

            case SRL_HDR_FLOAT:         srl_buf_copy_content_nocheck(aTHX_ mrg, 5);  break;
            case SRL_HDR_DOUBLE:        srl_buf_copy_content_nocheck(aTHX_ mrg, 9);  break;
            case SRL_HDR_LONG_DOUBLE:   srl_buf_copy_content_nocheck(aTHX_ mrg, 17); break;

            case SRL_HDR_TRUE:
            case SRL_HDR_FALSE:
            case SRL_HDR_UNDEF:
            case SRL_HDR_CANONICAL_UNDEF:
                srl_buf_cat_tag_nocheck(mrg, tag);
                break;

            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:
                srl_merge_binary_utf8(aTHX_ mrg, ptable_entry);
                break;

            case SRL_HDR_HASH:
                mrg->ibuf.pos++; // skip tag in input buffer
                length = srl_read_varint_uv_count(aTHX_ &mrg->ibuf, " while reading ARRAY or HASH");
                srl_merge_hash(aTHX_ mrg, tag, length);
                break;

            case SRL_HDR_ARRAY:
                mrg->ibuf.pos++; // skip tag in input buffer
                length = srl_read_varint_uv_count(aTHX_ &mrg->ibuf, " while reading ARRAY or HASH");
                srl_merge_array(aTHX_ mrg, tag, length);
                break;

            default:
                switch (tag) {
                    case SRL_HDR_COPY:
                    case SRL_HDR_REFP:
                    case SRL_HDR_ALIAS:
                        mrg->ibuf.pos++; // skip tag in input buffer
                        offset = srl_read_varint_uv_offset(aTHX_ &mrg->ibuf, " while reading COPY/ALIAS/REFP");
                        offset = srl_lookup_tracked_offset(aTHX_ mrg, offset); // convert ibuf offset to obuf offset
                        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), tag, offset);

                        if (tag == SRL_HDR_REFP || tag == SRL_HDR_ALIAS) {
                            SRL_SET_TRACK_FLAG(*(mrg->obuf.body_pos + offset));
                        }

                        break;

                    case SRL_HDR_REFN:
                    case SRL_HDR_WEAKEN:
                    case SRL_HDR_EXTEND:
                        srl_buf_cat_tag_nocheck(mrg, tag);
                        goto read_again;

                    case SRL_HDR_OBJECT:
                    case SRL_HDR_OBJECT_FREEZE:
                        srl_merge_object(aTHX_ mrg, tag);
                        break;

                    case SRL_HDR_REGEXP:
                        srl_buf_cat_tag_nocheck(mrg, tag);
                        srl_merge_stringish(aTHX_ mrg);

                        tag = *mrg->ibuf.pos;
                        if (expect_false(tag < SRL_HDR_SHORT_BINARY_LOW))
                            SRL_ERROR_UNEXPECTED(mrg->ibuf, tag, "SRL_HDR_SHORT_BINARY");

                        srl_buf_copy_content_nocheck(aTHX_ mrg, SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) + 1);
                        break;

                    case SRL_HDR_OBJECTV:
                    case SRL_HDR_OBJECTV_FREEZE:
                        mrg->ibuf.pos++; // skip tag in input buffer
                        offset = srl_read_varint_uv_offset(aTHX_ &mrg->ibuf, " while reading OBJECTV/OBJECTV_FREEZE");
                        offset = srl_lookup_tracked_offset(aTHX_ mrg, offset); // convert ibuf offset to obuf offset
                        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), tag, offset);
                        goto read_again;

                    case SRL_HDR_PAD:
                        while (BUF_NOT_DONE(mrg->ibuf) && *mrg->ibuf.pos == SRL_HDR_PAD) {
                            srl_buf_cat_tag_nocheck(mrg, SRL_HDR_PAD);
                        }

                        goto read_again;

                     default:
                        SRL_ERROR_UNIMPLEMENTED(mrg->ibuf, tag, "");
                        break;
                }
        }
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_merge_array(pTHX_ srl_merger_t *mrg, const U8 tag, UV length)
{
    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    if (tag == SRL_HDR_ARRAY) {
        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), tag, length);
    } else {
        srl_buf_cat_tag_nocheck(mrg, tag);
    }

    unsigned int i;
    for (i = 0; i < length; ++i) {
        srl_merge_single_value(aTHX_ mrg);
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_merge_hash(pTHX_ srl_merger_t *mrg, const U8 tag, UV length)
{
    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    if (tag == SRL_HDR_HASH) {
        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), tag, length);
    } else {
        srl_buf_cat_tag_nocheck(mrg, tag);
    }

    unsigned int i;
    for (i = 0; i < length; ++i) {
        srl_merge_stringish(aTHX_ mrg);
        srl_merge_single_value(aTHX_ mrg);
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_merge_binary_utf8(pTHX_ srl_merger_t *mrg, ptable_entry_ptr ptable_entry)
{
    int ok;
    UV length, total_length;
    strtable_entry_ptr strtable_entry;
    const char *tag_ptr = mrg->ibuf.pos;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    mrg->ibuf.pos++; // skip tag in input buffer
    length = srl_read_varint_uv_length(aTHX_ &mrg->ibuf, " while reading BINARY or STR_UTF8");

    assert((mrg->ibuf.pos - tag_ptr) > 0);
    assert((mrg->ibuf.pos - tag_ptr) <= SRL_MAX_VARINT_LENGTH);
    total_length = length + (mrg->ibuf.pos - tag_ptr);

    strtable_entry = srl_lookup_string(aTHX_ mrg, tag_ptr, total_length, &ok);

    if (ok) {
        // issue COPY tag
        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), SRL_HDR_COPY, strtable_entry->offset);
        mrg->ibuf.pos += length;

        if (expect_false(ptable_entry)) {
            // update value in ptable entry
            // This is needed because if any of following tags will reffer to
            // this one as COPY we need to point them to original string.
            // By Sereal spec a COPY tag cannot reffer to another COPY tag.
            ptable_entry->value = INT2PTR(void *, strtable_entry->offset);
        }
    } else if (strtable_entry) {
        mrg->ibuf.pos = (char*) tag_ptr;
        strtable_entry->offset = BODY_POS_OFS(mrg->obuf);
        srl_buf_copy_content_nocheck(aTHX_ mrg, total_length);

        STRTABLE_ASSERT_ENTRY(mrg->string_deduper_tbl, strtable_entry);
        STRTABLE_ASSERT_ENTRY_STR(mrg->string_deduper_tbl, strtable_entry,
                                  mrg->ibuf.pos - total_length, total_length);
    } else {
        mrg->ibuf.pos = (char*) tag_ptr;
        srl_buf_copy_content_nocheck(aTHX_ mrg, total_length);
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_merge_short_binary(pTHX_ srl_merger_t *mrg, const U8 tag, ptable_entry_ptr ptable_entry)
{
    int ok;
    strtable_entry_ptr strtable_entry;
    UV length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) + 1; // + 1 for tag

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    // +1 because need to respect tag
    // no need to do ASSERT_BUF_SPACE because srl_build_track_table has asserted ibuf
    strtable_entry = srl_lookup_string(aTHX_ mrg, mrg->ibuf.pos, length, &ok);

    if (ok) {
        // issue COPY tag
        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), SRL_HDR_COPY, strtable_entry->offset);
        mrg->ibuf.pos += length;

        if (expect_false(ptable_entry)) {
            // update value in ptable entry
            // This is needed because if any of following tags will reffer to
            // this one as COPY we need to point them to original string.
            // By Sereal spec a COPY tag cannot reffer to another COPY tag
            ptable_entry->value = INT2PTR(void *, strtable_entry->offset);
        }
    } else if (strtable_entry) {
        strtable_entry->offset = BODY_POS_OFS(mrg->obuf);
        srl_buf_copy_content_nocheck(aTHX_ mrg, length);

        STRTABLE_ASSERT_ENTRY(mrg->string_deduper_tbl, strtable_entry);
        STRTABLE_ASSERT_ENTRY_STR(mrg->string_deduper_tbl, strtable_entry, mrg->ibuf.pos - length, length);
    } else {
        srl_buf_copy_content_nocheck(aTHX_ mrg, length);
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_merge_stringish(pTHX_ srl_merger_t *mrg)
{
    U8 tag;
    UV offset = 0;
    ptable_entry_ptr ptable_entry = NULL;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    if (expect_false(BUF_DONE(mrg->ibuf)))
        SRL_ERROR(mrg->ibuf, "Unexpected termination of input buffer");

    tag = *mrg->ibuf.pos;
    tag = tag & ~SRL_HDR_TRACK_FLAG;
    SRL_REPORT_CURRENT_TAG(mrg, tag);

    if (mrg->tracked_offsets && !srl_stack_empty(mrg->tracked_offsets)) {
        UV itag_offset = BODY_POS_OFS(mrg->ibuf);
        if (expect_false(itag_offset == srl_stack_peek_nocheck(aTHX_ mrg->tracked_offsets))) {
            // trackme case
            srl_stack_pop_nocheck(mrg->tracked_offsets);
            ptable_entry = srl_store_tracked_offset(aTHX_ mrg, itag_offset, BODY_POS_OFS(mrg->obuf));
        }
    }

    if (tag >= SRL_HDR_SHORT_BINARY_LOW) {
        srl_merge_short_binary(aTHX_ mrg, tag, ptable_entry);
    } else if (tag == SRL_HDR_BINARY || tag == SRL_HDR_STR_UTF8) {
        srl_merge_binary_utf8(aTHX_ mrg, ptable_entry);
    } else if (tag == SRL_HDR_COPY) {
        mrg->ibuf.pos++; // skip tag in input buffer
        offset = srl_read_varint_uv_offset(aTHX_ &mrg->ibuf, " while reading COPY");
        offset = srl_lookup_tracked_offset(aTHX_ mrg, offset); // convert ibuf offset to obuf offset

        U8 newtag = *(mrg->obuf.body_pos + offset);
        if (expect_false(newtag != SRL_HDR_BINARY && newtag != SRL_HDR_STR_UTF8 && newtag < SRL_HDR_SHORT_BINARY_LOW)) {
            SRL_ERROR_BAD_COPY(mrg->ibuf, newtag);
        }

        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), tag, offset);
        //otag_offset = offset;
    } else {
        SRL_ERROR_UNEXPECTED(mrg->ibuf, tag, "stringish");
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_merge_object(pTHX_ srl_merger_t *mrg, const U8 objtag)
{
    int ok;
    U8 strtag;
    const char *strtag_ptr = NULL;
    ptable_entry_ptr ptable_entry = NULL;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    mrg->ibuf.pos++; // skip object tag

    strtag = *mrg->ibuf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_REPORT_CURRENT_TAG(mrg, strtag);

    if (mrg->tracked_offsets && !srl_stack_empty(mrg->tracked_offsets)) {
        UV itag_offset = BODY_POS_OFS(mrg->ibuf);
        if (expect_false(itag_offset == srl_stack_peek_nocheck(aTHX_ mrg->tracked_offsets))) {
            // trackme case
            srl_stack_pop_nocheck(mrg->tracked_offsets);

            // store offset to future class name tag (stringish),
            // but at the moment we programm reaches this point the output buffer doesn't
            // contain OBJECT tag yet. In other words, BODY_POS_OFS(mrg->obuf) return location
            // of OBJECT tag where as we need to store location of classname tag. To workaround
            // simply add one which is correct offset if OBJECT tag will be issues.
            // In case deduplication (OBJECTV tag) ptable_entry->value will be updated accordingly.
            ptable_entry = srl_store_tracked_offset(aTHX_ mrg, itag_offset, BODY_POS_OFS(mrg->obuf) + 1);
        }
    }

    strtag_ptr = mrg->ibuf.pos++; // skip string tag in input buffer

    if (strtag == SRL_HDR_BINARY || strtag == SRL_HDR_STR_UTF8 || strtag >= SRL_HDR_SHORT_BINARY_LOW) {
        UV length = strtag >= SRL_HDR_SHORT_BINARY_LOW
                  ? SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(strtag)
                  : srl_read_varint_uv_length(aTHX_ &mrg->ibuf, " while reading BINARY or STR_UTF8");

        /* total_length is size of classname including STRTAG and varint (for BINARY or STR_UTF8) */
        UV total_length = length + (mrg->ibuf.pos - strtag_ptr);

        assert((mrg->ibuf.pos - strtag_ptr) > 0);
        assert((mrg->ibuf.pos - strtag_ptr) <= SRL_MAX_VARINT_LENGTH);

        strtable_entry_ptr strtable_entry = srl_lookup_classname(aTHX_ mrg, strtag_ptr, total_length, &ok);

        if (ok) {
            // issue OBJECTV || OBJECTV_FREEZE tag
            U8 outtag = (objtag == SRL_HDR_OBJECT ? SRL_HDR_OBJECTV : SRL_HDR_OBJECTV_FREEZE);
            srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), outtag, strtable_entry->offset);
            mrg->ibuf.pos += length;

            if (expect_false(ptable_entry)) {
                // update value in ptable entry
                // This is needed because if any of following tags will reffer to
                // this one as COPY we need to point them to original string.
                // By Sereal spec a COPY tag cannot reffer to another COPY tag.
                ptable_entry->value = INT2PTR(void *, strtable_entry->offset);
            }
        } else if (strtable_entry) {
            // issue OBJECT tag and update strtable entry
            srl_buf_cat_char_nocheck(MRG2ENC(mrg), objtag);

            mrg->ibuf.pos = (char*) strtag_ptr; // reset input buffer to strta
            strtable_entry->offset = BODY_POS_OFS(mrg->obuf);
            srl_buf_copy_content_nocheck(aTHX_ mrg, total_length);

            STRTABLE_ASSERT_ENTRY(mrg->classname_deduper_tbl, strtable_entry);
            STRTABLE_ASSERT_ENTRY_STR(mrg->classname_deduper_tbl, strtable_entry,
                                      mrg->ibuf.pos - total_length, total_length);
        } else {
            // issue OBJECT tag
            srl_buf_cat_char_nocheck(MRG2ENC(mrg), objtag);

            mrg->ibuf.pos = (char*) strtag_ptr;
            srl_buf_copy_content_nocheck(aTHX_ mrg, total_length);
        }
    } else if (strtag == SRL_HDR_COPY) {
        UV offset = srl_read_varint_uv_offset(aTHX_ &mrg->ibuf, " while reading COPY");
        offset = srl_lookup_tracked_offset(aTHX_ mrg, offset); // convert ibuf offset to obuf offset

        U8 newtag = *(mrg->obuf.body_pos + offset);
        if (expect_false(newtag != SRL_HDR_BINARY && newtag != SRL_HDR_STR_UTF8 && newtag < SRL_HDR_SHORT_BINARY_LOW)) {
            SRL_ERROR_BAD_COPY(mrg->ibuf, newtag);
        }

        srl_buf_cat_varint(aTHX_ MRG2ENC(mrg), strtag, offset);
        //otag_offset = offset;
    } else {
        SRL_ERROR_UNEXPECTED(mrg->ibuf, strtag, "stringish");
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    srl_merge_single_value(aTHX_ mrg);
}

SRL_STATIC_INLINE ptable_entry_ptr
srl_store_tracked_offset(pTHX_ srl_merger_t *mrg, UV from, UV to)
{
    // 0 is a bad offset for all Sereal formats

    assert(to > 0);
    assert(from > 0);

    SRL_MERGER_TRACE("srl_store_tracked_offset: %lu -> %lu", from, to);
    return PTABLE_store(SRL_GET_TRACKED_OFFSETS_TBL(mrg), INT2PTR(void *, from), INT2PTR(void *, to));
}

SRL_STATIC_INLINE UV
srl_lookup_tracked_offset(pTHX_ srl_merger_t *mrg, UV offset)
{
    void *res = PTABLE_fetch(SRL_GET_TRACKED_OFFSETS_TBL(mrg), INT2PTR(void *, offset));
    if (expect_false(!res))
        SRL_ERRORf1(mrg->ibuf, "bad target offset %lu", offset);

    UV len = PTR2UV(res);
    SRL_MERGER_TRACE("srl_lookup_tracked_offset: %lu -> %lu", offset, len);
    if (expect_false(mrg->obuf.body_pos + len >= mrg->obuf.pos)) {
        SRL_ERRORf3(mrg->obuf, "Corrupted packet. Offset %lu points past current iposition %lu in packet with length of %lu bytes long",
                    (unsigned long) offset, (unsigned long) BUF_POS_OFS(mrg->obuf), (unsigned long) BUF_SIZE(mrg->obuf));
    }

    return len;
}

SRL_STATIC_INLINE strtable_entry_ptr
srl_lookup_string(pTHX_ srl_merger_t *mrg, const char *src, STRLEN len, int *ok)
{
    *ok = 0;
    if (len <= 3 || len > STRTABLE_MAX_STR_SIZE || !SRL_MRG_HAVE_OPTION(mrg, SRL_F_DEDUPE_STRINGS))
        return NULL;

    strtable_entry_ptr ent = STRTABLE_insert(SRL_GET_STRING_DEDUPER_TBL(mrg), src, len, ok);
    assert(ent != NULL);

    if (*ok) {
        SRL_MERGER_TRACE("srl_lookup_string: got duplicate '%.*s' target %lld",
                         (int) len, src, ent->offset);
    } else {
        SRL_MERGER_TRACE("srl_lookup_string: not found duplicate '%.*s'",
                         (int) len, src);
    }

    return ent;
}

SRL_STATIC_INLINE strtable_entry_ptr
srl_lookup_classname(pTHX_ srl_merger_t *mrg, const char *src, STRLEN len, int *ok)
{
    *ok = 0;
    if (len <= 3 || len > STRTABLE_MAX_STR_SIZE || !SRL_MRG_HAVE_OPTION(mrg, SRL_F_DEDUPE_STRINGS))
        return NULL;

    strtable_entry_ptr ent = STRTABLE_insert(SRL_GET_CLASSNAME_DEDUPER_TBL(mrg), src, len, ok);
    assert(ent != NULL);

    if (*ok) {
        SRL_MERGER_TRACE("srl_lookup_classname: got duplicate '%.*s' target %lld",
                         (int) len, src, ent->offset);
    } else {
        SRL_MERGER_TRACE("srl_lookup_classname: not found duplicate '%.*s'",
                         (int) len, src);
    }

    return ent;
}

SRL_STATIC_INLINE void
srl_buf_copy_content_nocheck(pTHX_ srl_merger_t *mrg, size_t len)
{
    GROW_BUF(mrg->obuf, len);

    Copy(mrg->ibuf.pos, mrg->obuf.pos, len, char);
    mrg->ibuf.pos += len;
    mrg->obuf.pos += len;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE void
srl_copy_varint(pTHX_ srl_merger_t *mrg)
{
    unsigned int lshift = 0;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    GROW_BUF(mrg->obuf, SRL_MAX_VARINT_LENGTH);

    while (BUF_NOT_DONE(mrg->ibuf) && *mrg->ibuf.pos & 0x80) {
        *mrg->obuf.pos++ = *mrg->ibuf.pos++;
        lshift += 7;

        if (expect_false(lshift > (sizeof(UV) * 8)))
            SRL_ERROR(mrg->ibuf, "varint too big");
    }

    if (expect_true(BUF_NOT_DONE(mrg->ibuf))) {
        *mrg->obuf.pos++ = *mrg->ibuf.pos++;
    } else {
        SRL_ERROR(mrg->ibuf, "varint terminated prematurely");
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

/* VARINT function
 * copy-pasted from srl_decoder.h
 */

SRL_STATIC_INLINE UV
srl_read_varint_uv(pTHX_ srl_buffer_t *buf)
{
    if (expect_true(BUF_SPACE(*buf) > 10))
        return srl_read_varint_uv_nocheck(aTHX_ buf);
    else
        return srl_read_varint_uv_safe(aTHX_ buf);
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_safe(pTHX_ srl_buffer_t *buf)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (BUF_NOT_DONE(*buf) && *buf->pos & 0x80) {
        uv |= ((UV) (*buf->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            SRL_ERROR(*buf, "varint too big");
    }

    if (expect_true(BUF_NOT_DONE(*buf))) {
        uv |= ((UV) *buf->pos++ << lshift);
    } else {
        SRL_ERROR(*buf, "varint terminated prematurely");
    }

    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_buffer_t *buf)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*buf->pos & 0x80) {
        uv |= ((UV) (*buf->pos++ & 0x7F) << lshift);
        lshift += 7;

        if (expect_false(lshift > (sizeof(UV) * 8)))
            SRL_ERROR(*buf, "varint too big");
    }

    uv |= ((UV) (*buf->pos++) << lshift);
    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    if (expect_false(buf->body_pos + len >= buf->pos)) {
        SRL_ERRORf4(*buf, "Corrupted packet%s. Offset %lu points past current iposition %lu in packet with length of %lu bytes long",
                    errstr, (unsigned long)len, (unsigned long) BUF_POS_OFS(*buf), (unsigned long) BUF_SIZE(*buf));
    }

    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_length(pTHX_ srl_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    ASSERT_BUF_SPACE(*buf, len, errstr);
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_count(pTHX_ srl_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    if (len > I32_MAX) {
        SRL_ERRORf3(*buf, "Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
    }

    return len;
}

SRL_STATIC_INLINE IV
srl_validate_header_version_pv_len(pTHX_ char *strdata, STRLEN len)
{
    if (len >= SRL_MAGIC_STRLEN + 3) {
        /* + 3 above because:
         * at least one version/flag byte,
         * one byte for header len,
         * one type byte (smallest payload)
         */

        /* Do NOT do *((U32*)strdata at least for these reasons:
         * (1) Unaligned access can "Bus error" on you
         *     (char* can be much less aligned than U32).
         * (2) In ILP64 even if aligned the U32 would be 64 bits wide,
         *     and the deref would read 8 bytes, more than the smallest
         *     (valid) message.
         * (3) Endianness.
         */
        U8 version_encoding= strdata[SRL_MAGIC_STRLEN];
        U8 version= version_encoding & SRL_PROTOCOL_VERSION_MASK;

        if (memEQ(SRL_MAGIC_STRING, strdata, SRL_MAGIC_STRLEN)) {
            if ( 0 < version && version < 3 ) {
                return version_encoding;
            }
        } else if (memEQ(SRL_MAGIC_STRING_HIGHBIT, strdata, SRL_MAGIC_STRLEN)) {
            if ( 3 <= version ) {
                return version_encoding;
           }
        } else if (memEQ(SRL_MAGIC_STRING_HIGHBIT_UTF8, strdata, SRL_MAGIC_STRLEN)) {
            return 0;
        }
    }

    return -1;
}

/* Decompress a Snappy-compressed document body and put the resulting
 * document body back in the place of the old compressed blob. */
SRL_STATIC_INLINE UV
srl_decompress_body_snappy(pTHX_ srl_buffer_t *buf, int incremental)
{
    SV *buf_sv;
    uint32_t decompressed_len;
    int decompress_ok, snappy_header_len;

    const STRLEN compressed_len
        = incremental
        ? (STRLEN) srl_read_varint_uv_length(aTHX_ buf, " while reading compressed packet size")
        : (STRLEN) (buf->end - buf->pos);

    char *ptr, *old_pos = buf->pos;
    const ptrdiff_t sereal_header_len = buf->pos - buf->start;
    UV bytes_consumed = compressed_len + sereal_header_len;

    /* All bufl's above here, or we break C89 compilers */

    snappy_header_len = csnappy_get_uncompressed_length(buf->pos,
                                                        compressed_len,
                                                        &decompressed_len);

    if (snappy_header_len == CSNAPPY_E_HEADER_BAD)
        SRL_ERROR(*buf, "Invalid Snappy header in Snappy-compressed Sereal packet");

    /* Allocate output buffer and swap it into place within the bufoder. */
    buf_sv = sv_2mortal(newSV(sereal_header_len + decompressed_len + 1));
    ptr = (char *) SvPVX(buf_sv);

    buf->start = ptr;
    buf->pos = buf->start + sereal_header_len;
    buf->end = buf->pos + decompressed_len;

    decompress_ok = csnappy_decompress_noheader(old_pos + snappy_header_len,
                                                compressed_len - snappy_header_len,
                                                buf->pos,
                                                &decompressed_len);

    if (expect_false(decompress_ok != 0))
        SRL_ERRORf1(*buf, "Snappy decompression of Sereal packet payload failed with error %i!", decompress_ok);

    return bytes_consumed;
}

/* Update a varint anywhere in the output stream with defined start and end
 * positions. This can produce non-canonical varints and is useful for filling
 * pre-allocated varints. */
SRL_STATIC_INLINE void
srl_update_varint_from_to(pTHX_ char *varint_start, char *varint_end, UV number)
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
srl_init_snappy_workmem(pTHX_ srl_merger_t *mrg)
{
    /* Lazy working buffer alloc */
    if (expect_false(mrg->snappy_workmem == NULL)) {
        /* Cleaned up automatically by the cleanup handler */
        Newx(mrg->snappy_workmem, CSNAPPY_WORKMEM_BYTES, char);
        if (mrg->snappy_workmem == NULL)
            croak("Out of memory!");
    }
}
