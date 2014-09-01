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

#define IBUF_SPACE(mrg) ((mrg)->ibuf_end - (mrg)->ipos)
#define IBUF_POS_OFS(dec) ((mrg)->ipos - (mrg)->ibuf_start)
#define IBODY_POS_OFS(enc) ((mrg)->ipos - (mrg)->ibody_pos)
#define IBUF_NOT_DONE(mrg) ((mrg)->ipos < (mrg)->ibuf_end)
#define ASSERT_IBUF_SPACE(mrg, len, msg) STMT_START {        \
    if (expect_false((UV) IBUF_SPACE((mrg)) < (UV) (len))) { \
        croak("Unexpected termination of packet%s, "         \
              "want %lu bytes, only have %lu available",     \
              (msg), (UV) (len), (UV) IBUF_SPACE((mrg)));    \
    }                                                        \
} STMT_END

//#define SRL_MRG_HAVE_OPTION(mrg, flag_num) ((mrg)->flags & flag_num)
//#define SRL_MRG_SET_OPTION(mrg, flag_num) ((mrg)->flags |= flag_num)
//
//#define SRL_SET_IBODY_POS(mrg, pos_ptr) ((mrg)->ibody_pos = pos_ptr)
//#define SRL_UPDATE_IBODY_POS(mrg) STMT_START {                                  \
//    if (expect_false(SRL_MRG_HAVE_OPTION((mrg), SRL_F_DECODER_PROTOCOL_V1))) {  \
//        SRL_SET_BODY_POS(mrg, (mrg)->ibuf_start)                                \
//    } else {                                                                    \
//        SRL_SET_BODY_POS(mrg, (mrg)->ipos-1);                                   \
//    }                                                                           \
//} STMT_END

#include "srl_merger.h"
#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_inline.h"
#include "../Encoder/srl_buffer.h"

/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE UV srl_read_varint_uv_safe(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE UV srl_read_varint_uv_nocheck(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE UV srl_read_varint_uv(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE UV srl_read_varint_uv_offset(pTHX_ srl_merger_t *mrg, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_length(pTHX_ srl_merger_t *mrg, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_count(pTHX_ srl_merger_t *mrg, const char * const errstr);

SRL_STATIC_INLINE srl_merger_t * srl_empty_merger_struct(pTHX);                         /* allocate an empty merger struct - flags still to be set up */
SRL_STATIC_INLINE void srl_reset_input_buffer(pTHX_ srl_merger_t *mrg, SV *src); /* reset ibuf_* field */
SRL_STATIC_INLINE void srl_build_track_table(pTHX_ srl_merger_t *mrg);

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
srl_destroy_merger(pTHX_ srl_merger_t *mrg) {
    //srl_buf_free_buffer(aTHX_ &mrg->obuf);

    if (mrg->tracked_offsets) {
        SvREFCNT_dec(mrg->tracked_offsets);
        mrg->tracked_offsets = NULL;
    }

    if (mrg->tracked_offsets_with_duplicates) {
        SvREFCNT_dec(mrg->tracked_offsets_with_duplicates);
        mrg->tracked_offsets_with_duplicates = NULL;
    }

    Safefree(mrg);
}

void
srl_merger_append(pTHX_ srl_merger_t *mrg, SV *src) {
    assert(mrg != NULL);

    srl_reset_input_buffer(mrg, src);
    mrg->ipos += 6;
    mrg->ibody_pos = mrg->ipos;

    srl_build_track_table(mrg);
}

char *
srl_merger_finish(pTHX_ srl_merger_t *mrg) {
    assert(mrg);
    return "plop";
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
    return mrg;
}

SRL_STATIC_INLINE void
srl_reset_input_buffer(pTHX_ srl_merger_t *mrg, SV *src) {
    STRLEN len;
    unsigned char *tmp;

    tmp = (unsigned char*) SvPV(src, len);
    mrg->ibuf_start = mrg->ipos = tmp;
    mrg->ibuf_end = tmp + len;

    // TODO reset tracked_offsets
}

SRL_STATIC_INLINE void
srl_build_track_table(pTHX_ srl_merger_t *mrg) {
    U8 tag;
    SV *sv_offset;
    SV **sv_offset_ptr;
    UV offset, last_offset;
    int i, avlen;

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

    while (IBUF_NOT_DONE(mrg)) {
        tag = *mrg->ipos++;
        if (expect_false(tag & SRL_HDR_TRACK_FLAG)) {
            tag = tag & ~SRL_HDR_TRACK_FLAG;
            sv_offset = sv_2mortal(newSVuv(IBODY_POS_OFS(mrg)));
            av_push(mrg->tracked_offsets_with_duplicates, sv_offset);
        }

#ifdef DEBUG
        warn("tag %d (0x%X) at %d", tag, tag, (int) IBUF_POS_OFS(mrg));
#endif

        if (IS_SRL_HDR_SHORT_BINARY(tag)) {
            mrg->ipos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            continue;
        } else if (tag <= SRL_HDR_NEG_HIGH || IS_SRL_HDR_ARRAYREF(tag) || IS_SRL_HDR_HASHREF(tag)) {
            continue;
        }

        switch (tag) {
            case SRL_HDR_VARINT:
            case SRL_HDR_ZIGZAG:
                mrg->ipos += srl_read_varint_uv(mrg);
                break;

            case SRL_HDR_FLOAT:         mrg->ipos += 4;     break;
            case SRL_HDR_DOUBLE:        mrg->ipos += 8;     break;
            case SRL_HDR_LONG_DOUBLE:   mrg->ipos += 16;    break;

            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:
                mrg->ipos += srl_read_varint_uv_length(mrg, " while reading BINARY or STR_UTF8");
                break;

            case SRL_HDR_HASH:
            case SRL_HDR_ARRAY:
                srl_read_varint_uv_count(mrg, " while reading ARRAY or HASH");
                break;

            case SRL_HDR_COPY:
            case SRL_HDR_ALIAS:
            case SRL_HDR_REFP:
            case SRL_HDR_OBJECTV:
            case SRL_HDR_OBJECTV_FREEZE:
                offset = srl_read_varint_uv_offset(mrg, " while reading COPY/ALIAS/REFP/OBJECTV/OBJECTV_FREEZE");
                sv_offset = sv_2mortal(newSVuv(offset));
                av_push(mrg->tracked_offsets_with_duplicates, sv_offset);
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

    if (av_len(mrg->tracked_offsets_with_duplicates) + 1) {
        // sort offsets
        sortsv(
            AvARRAY(mrg->tracked_offsets_with_duplicates),
            av_top_index(mrg->tracked_offsets_with_duplicates) + 1,
            Perl_sv_cmp_locale
        );

        last_offset = -1;
        avlen = av_top_index(mrg->tracked_offsets_with_duplicates);

        // remove duplicates
        for (i = 0; i <= avlen; ++i) {
            if (!(sv_offset_ptr = av_fetch(mrg->tracked_offsets_with_duplicates, i, 0)))
                croak("sv_offset_ptr is NULL"); // TODO

            UV offset = SvUV(*sv_offset_ptr);
            if (last_offset != offset) {
                last_offset = offset;
                av_push(mrg->tracked_offsets, sv_mortalcopy(*sv_offset_ptr));
            }
        }

#ifdef DEBUG
        avlen = av_top_index(mrg->tracked_offsets);
        for (i = 0; i <= avlen; ++i) {
            sv_offset_ptr= av_fetch(mrg->tracked_offsets, i, 0);
            warn("tracked_offsets: idx %d offset %d\n", i, (int) SvUV(*sv_offset_ptr));
        }
#endif
    }
}

/* VARINT function
 * copy-pasted from srl_decoder.h
 */

SRL_STATIC_INLINE UV
srl_read_varint_uv(pTHX_ srl_merger_t *mrg)
{
    if (expect_true(IBUF_SPACE(mrg) > 10))
        return srl_read_varint_uv_nocheck(aTHX_ mrg);
    else
        return srl_read_varint_uv_safe(aTHX_ mrg);
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_safe(pTHX_ srl_merger_t *mrg)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (IBUF_NOT_DONE(mrg) && *mrg->ipos & 0x80) {
        uv |= ((UV) (*mrg->ipos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            croak("varint too big");
            //SRL_ERROR("varint too big");
    }

    if (expect_true(IBUF_NOT_DONE(mrg))) {
        uv |= ((UV) *mrg->ipos++ << lshift);
    } else {
        croak("varint terminated prematurely");
        //SRL_ERROR("varint terminated prematurely");
    }

    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_merger_t *mrg)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*mrg->ipos & 0x80) {
        uv |= ((UV) (*mrg->ipos++ & 0x7F) << lshift);
        lshift += 7;

        if (expect_false( lshift > (sizeof(UV) * 8) ))
            croak("varint too big");
            //SRL_ERROR("varint too big");
    }

    uv |= ((UV) (*mrg->ipos++) << lshift);
    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_merger_t *mrg, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ mrg);
    if (mrg->ibody_pos + len >= mrg->ipos) {
        croak("Corrupted packet");
        //SRL_ERRORf4("Corrupted packet%s. Offset %lu points past current iposition %lu in packet with length of %lu bytes long",
        //        errstr, (unsigned long)len, (unsigned long)BUF_POS_OFS(mrg), (unsigned long)mrg->buf_len);
    }

    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_length(pTHX_ srl_merger_t *mrg, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ mrg);
    ASSERT_IBUF_SPACE(mrg, len, errstr);
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_count(pTHX_ srl_merger_t *mrg, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ mrg);
    if (len > I32_MAX) {
        croak("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
        //SRL_ERRORf3("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
    }

    return len;
}
