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

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)
#define SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag) ((tag) & 15)
#define SRL_HDR_HASHREF_LEN_FROM_TAG(tag) ((tag) & 15)

#define ASSERT_BUF_SPACE(buf, len, msg) STMT_START {        \
    if (expect_false((UV) BUF_SPACE((buf)) < (UV) (len))) { \
        croak("Unexpected termination of packet%s, "         \
              "want %lu bytes, only have %lu available",     \
              (msg), (UV) (len), (UV) BUF_SPACE((buf)));    \
    }                                                        \
} STMT_END

//#define SRL_MRG_HAVE_OPTION(mrg, flag_num) ((mrg)->flags & flag_num)
//#define SRL_MRG_SET_OPTION(mrg, flag_num) ((mrg)->flags |= flag_num)

#include "srl_merger.h"
#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_inline.h"
#include "../Encoder/srl_buffer.h"

/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE UV srl_read_varint_uv_safe(pTHX_ srl_buffer_t *buf);
SRL_STATIC_INLINE UV srl_read_varint_uv_nocheck(pTHX_ srl_buffer_t *buf);
SRL_STATIC_INLINE UV srl_read_varint_uv(pTHX_ srl_buffer_t *buf);
SRL_STATIC_INLINE UV srl_read_varint_uv_offset(pTHX_ srl_buffer_t *buf, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_length(pTHX_ srl_buffer_t *buf, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_count(pTHX_ srl_buffer_t *buf, const char * const errstr);

SRL_STATIC_INLINE void srl_buf_copy_content(pTHX_ srl_merger_t *mrg, size_t len);
SRL_STATIC_INLINE void srl_copy_varint(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE srl_merger_t * srl_empty_merger_struct(pTHX);                         /* allocate an empty merger struct - flags still to be set up */
SRL_STATIC_INLINE void srl_reset_input_buffer(pTHX_ srl_merger_t *mrg, SV *src);        /* reset input buffer (ibuf) */
SRL_STATIC_INLINE void srl_build_track_table(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE void srl_merge_items(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE IV srl_dedupe_string(pTHX_ srl_merger_t *mrg, const char* src, STRLEN len, UV offset);

srl_merger_t *
srl_build_merger_struct(pTHX_ HV *opt)
{
    srl_merger_t *mrg;
    SV **svp;

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
                      (unsigned long)mrg->protocol_version);
            }
        }
    }

    //DEBUG_ASSERT_BUF_SANE(mrg);
    return mrg;
}

void
srl_destroy_merger(pTHX_ srl_merger_t *mrg)
{
    srl_buf_free_buffer(aTHX_ &mrg->obuf);

    if (mrg->tracked_offsets) {
        SvREFCNT_dec(mrg->tracked_offsets);
        mrg->tracked_offsets = NULL;
    }

    if (mrg->tracked_offsets_with_duplicates) {
        SvREFCNT_dec(mrg->tracked_offsets_with_duplicates);
        mrg->tracked_offsets_with_duplicates = NULL;
    }

    if (mrg->string_deduper_hv) {
        SvREFCNT_dec(mrg->string_deduper_hv);
        mrg->string_deduper_hv = NULL;
    }

    Safefree(mrg);
}

void
srl_merger_append(pTHX_ srl_merger_t *mrg, SV *src)
{
    assert(mrg != NULL);

    srl_reset_input_buffer(mrg, src);
    mrg->ibuf.pos += 6;
    mrg->ibuf.body_pos = mrg->ibuf.pos;

    srl_build_track_table(mrg);
    mrg->ibuf.pos = mrg->ibuf.body_pos;
    srl_merge_items(mrg);
}

SV *
srl_merger_finish(pTHX_ srl_merger_t *mrg)
{
    assert(mrg != NULL);
    return newSVpvn(mrg->obuf.start, BUF_POS_OFS(mrg->obuf));
}

SRL_STATIC_INLINE srl_merger_t *
srl_empty_merger_struct(pTHX)
{
    srl_merger_t *mrg;
    Newx(mrg, 1, srl_merger_t);
    if (mrg == NULL)
        croak("Out of memory");

    /* Init buffer struct */
    if (expect_false(srl_buf_init_buffer(aTHX_ &(mrg->obuf), INITIALIZATION_SIZE) != 0)) {
        Safefree(mrg);
        croak("Out of memory");
    }

    mrg->tracked_offsets = NULL;
    mrg->tracked_offsets_with_duplicates = NULL;
    mrg->string_deduper_hv = NULL;

    return mrg;
}

SRL_STATIC_INLINE void
srl_reset_input_buffer(pTHX_ srl_merger_t *mrg, SV *src)
{
    STRLEN len;
    char *tmp;

    tmp = (char*) SvPV(src, len);
    mrg->ibuf.start = mrg->ibuf.pos = tmp;
    mrg->ibuf.end = tmp + len;
}

SRL_STATIC_INLINE void
srl_build_track_table(pTHX_ srl_merger_t *mrg)
{
    U8 tag;
    SV **sv_offset_ptr;
    UV offset, last_offset, length;
    int i, avlen;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);

    if (mrg->tracked_offsets) {
        av_clear(mrg->tracked_offsets);
    } else {
        mrg->tracked_offsets = newAV();
    }

    if (mrg->tracked_offsets_with_duplicates) {
        av_clear(mrg->tracked_offsets_with_duplicates);
    } else {
        mrg->tracked_offsets_with_duplicates = newAV();
    }

    while (BUF_NOT_DONE(mrg->ibuf)) {
        tag = *mrg->ibuf.pos;
        if (expect_false(tag & SRL_HDR_TRACK_FLAG)) {
            tag = tag & ~SRL_HDR_TRACK_FLAG;
            offset = BODY_POS_OFS(mrg->ibuf);
            av_push(mrg->tracked_offsets_with_duplicates, newSVuv(offset));
        }

#ifdef DEBUG
        warn("srl_build_track_table tag %d (0x%X) at %d", tag, tag, (int) BUF_POS_OFS(mrg->ibuf));
#endif

        mrg->ibuf.pos++; // to let warn report correct offset

        if (IS_SRL_HDR_SHORT_BINARY(tag)) {
            mrg->ibuf.pos += (int) SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            continue;
        } else if (tag <= SRL_HDR_NEG_HIGH || IS_SRL_HDR_ARRAYREF(tag) || IS_SRL_HDR_HASHREF(tag)) {
            continue;
        }

        switch (tag) {
            case SRL_HDR_VARINT:
            case SRL_HDR_ZIGZAG:
                srl_read_varint_uv(&mrg->ibuf); // TODO test/implement srl_skip_varint()
                break;

            case SRL_HDR_FLOAT:         mrg->ibuf.pos += 4;     break;
            case SRL_HDR_DOUBLE:        mrg->ibuf.pos += 8;     break;
            case SRL_HDR_LONG_DOUBLE:   mrg->ibuf.pos += 16;    break;

            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:
                length = srl_read_varint_uv_length(&mrg->ibuf, " while reading BINARY or STR_UTF8");
                mrg->ibuf.pos += length;
                break;

            case SRL_HDR_HASH:
            case SRL_HDR_ARRAY:
                srl_read_varint_uv_count(&mrg->ibuf, " while reading ARRAY or HASH");
                break;

            case SRL_HDR_COPY:
            case SRL_HDR_ALIAS:
            case SRL_HDR_REFP:
            case SRL_HDR_OBJECTV:
            case SRL_HDR_OBJECTV_FREEZE:
                offset = srl_read_varint_uv_offset(&mrg->ibuf, " while reading COPY/ALIAS/REFP/OBJECTV/OBJECTV_FREEZE");
                av_push(mrg->tracked_offsets_with_duplicates, newSVuv(offset));
                break;

            case SRL_HDR_PAD:
            case SRL_HDR_REFN:
            case SRL_HDR_WEAKEN:
            case SRL_HDR_UNDEF:
            case SRL_HDR_CANONICAL_UNDEF:
            case SRL_HDR_TRUE:
            case SRL_HDR_FALSE:
            case SRL_HDR_EXTEND:
            case SRL_HDR_REGEXP:
            case SRL_HDR_OBJECT:
            case SRL_HDR_OBJECT_FREEZE:
                // noop
                break;

            default:
                croak("unexpected"); // TODO SRL_ERROR_UNEXPECTED(dec,tag, " single value");
                break;
        }
    }

    if ((avlen = av_top_index(mrg->tracked_offsets_with_duplicates) + 1)) {
        // sort offsets
        sortsv(AvARRAY(mrg->tracked_offsets_with_duplicates), avlen, Perl_sv_cmp_locale);

        last_offset = -1;

        // remove duplicates
        for (i = 0; i < avlen; ++i) {
            if (!(sv_offset_ptr = av_fetch(mrg->tracked_offsets_with_duplicates, i, 0)))
                croak("sv_offset_ptr is NULL"); // TODO

            UV offset = SvUV(*sv_offset_ptr);
            if (last_offset != offset) {
                last_offset = offset;
                av_push(mrg->tracked_offsets, SvREFCNT_inc(*sv_offset_ptr));
            }
        }

#ifdef DEBUG
        avlen = av_top_index(mrg->tracked_offsets);
        for (i = 0; i <= avlen; ++i) {
            sv_offset_ptr= av_fetch(mrg->tracked_offsets, i, 0);
            warn("tracked_offsets: idx %d offset %d SvREFCNT(%d)\n", i, (int) SvUV(*sv_offset_ptr), SvREFCNT(*sv_offset_ptr));
        }
#endif
    }
}

SRL_STATIC_INLINE void
srl_merge_items(pTHX_ srl_merger_t *mrg)
{
    U8 tag;
    UV length;
    AV *stack;
    SSize_t stack_level;
    SV **stack_item_ptr;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);

    // stack is needed for three things:
    // - keep track of expected things
    // - verify document consistency
    stack = sv_2mortal(newAV());

    while (BUF_NOT_DONE(mrg->ibuf)) {
        tag = *mrg->ibuf.pos;
        tag = tag & ~SRL_HDR_TRACK_FLAG;

        while ((stack_level = av_top_index(stack)) >= 0) {
            stack_item_ptr = av_fetch(stack, stack_level, 0);
            if (expect_false(stack_item_ptr == NULL || *stack_item_ptr == NULL)) {
                croak("broken stack 2"); // TODO
            }

            if (SvIV(*stack_item_ptr) > 0)
                break;

            av_pop(stack);
        }

        if (expect_false(stack_level < 0)) {
            warn("!!");
            break; // TODO
        }

#ifdef DEBUG
        warn("srl_merge_items tag %d (0x%X) at %d", tag, tag, (int) BUF_POS_OFS(mrg->ibuf));
#endif

        if (IS_SRL_HDR_SHORT_BINARY(tag)) {
            // TODO dedupe strings
            srl_buf_copy_content(mrg, 1);
            srl_buf_copy_content(mrg, SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag));
        } else if (tag <= SRL_HDR_NEG_HIGH) {
            srl_buf_copy_content(mrg, 1);
        } else if (IS_SRL_HDR_ARRAYREF(tag)) {
            srl_buf_copy_content(mrg, 1);
            av_push(stack, newSViv(SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag)));
        } else if (IS_SRL_HDR_HASHREF(tag)) {
            srl_buf_copy_content(mrg, 1);
            av_push(stack, newSViv(2 * SRL_HDR_HASHREF_LEN_FROM_TAG(tag)));
        } else {
            switch (tag) {
                case SRL_HDR_VARINT:
                case SRL_HDR_ZIGZAG:
                    srl_copy_varint(mrg);
                    break;

                case SRL_HDR_FLOAT:         srl_buf_copy_content(mrg, 5);     break;
                case SRL_HDR_DOUBLE:        srl_buf_copy_content(mrg, 9);     break;
                case SRL_HDR_LONG_DOUBLE:   srl_buf_copy_content(mrg, 17);    break;

                case SRL_HDR_PAD:
                case SRL_HDR_REFN:
                case SRL_HDR_WEAKEN:
                case SRL_HDR_EXTEND:
                    // TODO do not decrement stack counter
                    srl_buf_copy_content(mrg, 1);
                    break;

                case SRL_HDR_TRUE:
                case SRL_HDR_FALSE:
                case SRL_HDR_UNDEF:
                case SRL_HDR_CANONICAL_UNDEF:
                    srl_buf_copy_content(mrg, 1);
                    break;

                case SRL_HDR_BINARY:
                case SRL_HDR_STR_UTF8:
                    mrg->ibuf.pos++; // skip tag in input buffer
                    length = srl_read_varint_uv_length(&mrg->ibuf, " while reading BINARY or STR_UTF8");

                    IV offset = srl_dedupe_string(mrg, mrg->ibuf.pos, length, (UV) BODY_POS_OFS(mrg->ibuf));
                    if (offset == -1) {
                        // see string first time
                        srl_buf_cat_varint((srl_encoder_t*) mrg, tag, length);
                        srl_buf_copy_content(mrg, length);
                    } else {
                        // issue COPY tag
                        srl_buf_cat_varint((srl_encoder_t*) mrg, SRL_HDR_COPY, (UV) offset);
                        mrg->ibuf.pos += (int) length;
                    }

                    break;

                case SRL_HDR_HASH:
                case SRL_HDR_ARRAY:
                    srl_buf_copy_content(mrg, 1);
                    length = srl_read_varint_uv_count(&mrg->ibuf, " while reading ARRAY or HASH");
                    srl_buf_cat_varint((srl_encoder_t*) mrg, 0, length);

                    av_push(stack, newSViv(tag == SRL_HDR_HASH ? (int) length * 2 : (int) length));
                    break;

    //            case SRL_HDR_COPY:
    //            case SRL_HDR_ALIAS:
    //            case SRL_HDR_REFP:
    //            case SRL_HDR_OBJECTV:
    //            case SRL_HDR_OBJECTV_FREEZE:
    //                offset = srl_read_varint_uv_offset(&mrg->ibuf, " while reading COPY/ALIAS/REFP/OBJECTV/OBJECTV_FREEZE");
    //                av_push(mrg->tracked_offsets_with_duplicates, newSVuv(offset));
    //                break;
    //
    //            case SRL_HDR_REGEXP:
    //            case SRL_HDR_OBJECT:
    //            case SRL_HDR_OBJECT_FREEZE:
    //                // noop
    //                break;

                default:
                    // TODO SRL_ERROR_UNEXPECTED(dec,tag, " single value");
                    croak("unexpected tag %d (0x%X) at %d", tag, tag, (int) BUF_POS_OFS(mrg->ibuf));
                    break;
            }
        }
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE IV 
srl_dedupe_string(pTHX_ srl_merger_t *mrg, const char* src, STRLEN len, UV offset)
{
    if (expect_false(mrg->string_deduper_hv == NULL)) {
        mrg->string_deduper_hv = newHV();
    }

    SV **offset_ptr = hv_fetch(mrg->string_deduper_hv, src, len, 1);
    if (expect_false(offset_ptr == NULL)) {
        croak("out of memory (hv_fetch returned NULL)");
    }

    if (SvIOK(*offset_ptr)) {
        return SvUV(*offset_ptr);
    }

    sv_setiv(*offset_ptr, offset);
    return (UV) -1;
}

SRL_STATIC_INLINE void
srl_buf_copy_content(pTHX_ srl_merger_t *mrg, size_t len)
{
    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    BUF_SIZE_ASSERT(mrg->obuf, len);

    // TODO replace by DEBUG_ASSERT_BUF_SPACE
    if (BUF_SPACE(mrg->ibuf) < (ptrdiff_t) len)
        croak("input buffer is too small");

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
    BUF_SIZE_ASSERT(mrg->obuf, 10); // TODO check varint max size

    while (BUF_NOT_DONE(mrg->ibuf) && *mrg->ibuf.pos & 0x80) {
        *mrg->obuf.pos++ = *mrg->ibuf.pos++;
        lshift += 7;

        if (expect_false(lshift > (sizeof(UV) * 8)))
            croak("varint too big");
            //SRL_ERROR("varint too big");
    }

    if (expect_true(BUF_NOT_DONE(mrg->ibuf))) {
        *mrg->obuf.pos++ = *mrg->ibuf.pos++;
    } else {
        croak("varint terminated prematurely");
        //SRL_ERROR("varint terminated prematurely");
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
            croak("varint too big");
            //SRL_ERROR("varint too big");
    }

    if (expect_true(BUF_NOT_DONE(*buf))) {
        uv |= ((UV) *buf->pos++ << lshift);
    } else {
        croak("varint terminated prematurely");
        //SRL_ERROR("varint terminated prematurely");
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
            croak("varint too big");
            //SRL_ERROR("varint too big");
    }

    uv |= ((UV) (*buf->pos++) << lshift);
    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_buffer_t *buf, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ buf);
    if (buf->body_pos + len >= buf->pos) {
        croak("Corrupted packet");
        //SRL_ERRORf4("Corrupted packet%s. Offset %lu points past current iposition %lu in packet with length of %lu bytes long",
        //        errstr, (unsigned long)len, (unsigned long)BUF_POS_OFS(mrg), (unsigned long)mrg->buf_len);
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
        croak("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
        //SRL_ERRORf3("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
    }

    return len;
}
