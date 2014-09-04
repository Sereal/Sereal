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

#define SRL_GET_STRING_DEDUPER_AV(mrg) ((mrg)->string_deduper_hv == NULL          \
                                         ? srl_init_string_deduper_hv(aTHX_ mrg)  \
                                         : (mrg)->string_deduper_hv)

#define SRL_GET_TRACKED_OFFSETS_HV(mrg) ((mrg)->tracked_offsets_hv == NULL        \
                                         ? srl_init_tracked_offsets_hv(aTHX_ mrg) \
                                         : (mrg)->tracked_offsets_hv )

#define SRL_GET_TRACKED_OFFSETS_AV(mrg) ((mrg)->tracked_offsets_av == NULL        \
                                         ? srl_init_tracked_offsets_av(aTHX_ mrg) \
                                         : (mrg)->tracked_offsets_av )

//#define SRL_MERGER_TRACE(msg, args...) warn((msg), args)
#define SRL_MERGER_TRACE(msg, args...)

#ifndef NDEBUG
#define SRL_PUSH_CNT_TO_PARSER_STACK(mrg, tag, cnt) STMT_START {                      \
    SRL_MERGER_TRACE("tag %d (0x%X) pushed %d on parser stack", (tag), (tag), (cnt)); \
    av_push((mrg)->parser_stack, newSViv(cnt));                                       \
} STMT_END
#else
#define SRL_PUSH_CNT_TO_PARSER_STACK(mrg, tag, cnt) av_push((mrg)->parser_stack, newSViv(cnt))
#endif

#ifndef NDEBUG
#define SRL_POP_PARSER_STACK(mrg) STMT_START {             \
    SRL_MERGER_TRACE("poped value from parser stack", 0);  \
    SvREFCNT_dec(av_pop((mrg)->parser_stack));             \
} STMT_END
#else
#define SRL_POP_PARSER_STACK(mrg) SvREFCNT_dec(av_pop(mrg->parser_stack))
#endif

#ifndef NDEBUG
#define DEBUG_ASSERT_PARSER_STACK_VALUE(val) STMT_START {                \
    if (expect_false((val) < 0))                                         \
        warn("Unexpected value on parser stack: %d", (val));             \
    assert((val) >= 0);                                                  \
} STMT_END
#else
#define DEBUG_ASSERT_PARSER_STACK_VALUE assert((val) >= 0);
#endif

#ifndef NDEBUG
#define DEBUG_ASSERT_PARSER_STACK(mrg) STMT_START {                      \
    int i, len = av_top_index((mrg)->parser_stack) + 1;                  \
    if (expect_false(len <= 0))                                          \
        warn("Unexpected length of parser stack: %d", len);              \
                                                                         \
    assert(len > 0);                                                     \
                                                                         \
    for (i = 0; i < len; i++) {                                          \
        SV **svptr = av_fetch((mrg)->parser_stack, i, 0);                \
        assert(svptr != NULL && *svptr != NULL);                         \
                                                                         \
        int val = (int) SvIV(*svptr);                                    \
        if (expect_false(val < 0))                                       \
            warn("Invalid value on parser stack: i %d, val %d", i, val); \
                                                                         \
        assert(val >= 0);                                                \
    }                                                                    \
} STMT_END
#else
#define DEBUG_ASSERT_PARSER_STACK(mrg) ((void)0)
#endif

#ifndef NDEBUG
#define SRL_REPORT_CURRENT_TAG(mrg, tag)         \
    SRL_MERGER_TRACE(                            \
         "%s: tag %d (0x%X) at abs %d, rel %d",  \
         __FUNCTION__, (tag), (tag),             \
         (int) BUF_POS_OFS(mrg->ibuf),           \
         (int) BODY_POS_OFS(mrg->ibuf)           \
    )
#else
#define SRL_REPORT_CURRENT_TAG(mrg, tag) ((void) 0)
#endif

#define ASSERT_BUF_SPACE(buf, len, msg) STMT_START {         \
    if (expect_false((UV) BUF_SPACE((buf)) < (UV) (len))) {  \
        croak("Unexpected termination of packet%s, "         \
              "want %lu bytes, only have %lu available",     \
              (msg), (UV) (len), (UV) BUF_SPACE((buf)));     \
    }                                                        \
} STMT_END

#define GROW_BUF(buf, minlen) STMT_START {                            \
    DEBUG_ASSERT_BUF_SANE(buf);                                       \
    if (BUF_NEED_GROW(buf, minlen)) {                                 \
        srl_buf_grow_nocheck(aTHX_ &(buf), (BUF_SIZE(buf) + minlen)); \
        DEBUG_ASSERT_BUF_SANE(buf);                                   \
    }                                                                 \
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

SRL_STATIC_INLINE void srl_buf_copy_content(pTHX_ srl_merger_t *mrg, size_t len, const char * const errstr);
SRL_STATIC_INLINE void srl_buf_copy_content_nocheck(pTHX_ srl_merger_t *mrg, size_t len);
SRL_STATIC_INLINE void srl_copy_varint(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE HV * srl_init_string_deduper_hv(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE HV * srl_init_tracked_offsets_hv(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE AV * srl_init_tracked_offsets_av(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE srl_merger_t * srl_empty_merger_struct(pTHX);                         /* allocate an empty merger struct - flags still to be set up */
SRL_STATIC_INLINE void srl_reset_input_buffer(pTHX_ srl_merger_t *mrg, SV *src);        /* reset input buffer (ibuf) */
SRL_STATIC_INLINE void srl_build_track_table(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE void srl_merge_items(pTHX_ srl_merger_t *mrg);

SRL_STATIC_INLINE int srl_expected_top_elements(pTHX_ srl_merger_t *mrg);
SRL_STATIC_INLINE UV srl_lookup_string(pTHX_ srl_merger_t *mrg, const char* src, STRLEN len, UV offset);
SRL_STATIC_INLINE UV srl_lookup_tracked_offset(pTHX_ srl_merger_t *mrg, UV from, UV to);

#define SvSIOK(sv) ((SvFLAGS(sv) & (SVf_IOK|SVf_IVisUV)) == SVf_IOK)
#define SvNSIV(sv) (SvNOK(sv) ? SvNVX(sv) : (SvSIOK(sv) ? SvIVX(sv) : sv_2nv(sv)))
SRL_STATIC_INLINE I32
S_sv_ncmp(pTHX_ SV *a, SV *b)
{
    NV nv1 = SvNSIV(a);
    NV nv2 = SvNSIV(b);
    return nv1 < nv2 ? -1 : nv1 > nv2 ? 1 : 0;
}

SRL_STATIC_INLINE HV *
srl_init_tracked_offsets_hv(pTHX_ srl_merger_t *mrg)
{
    mrg->tracked_offsets_hv = newHV();
    return mrg->tracked_offsets_hv;
}

SRL_STATIC_INLINE HV *
srl_init_string_deduper_hv(pTHX_ srl_merger_t *mrg)
{
    mrg->string_deduper_hv = newHV();
    return mrg->string_deduper_hv;
}

SRL_STATIC_INLINE AV *
srl_init_tracked_offsets_av(pTHX_ srl_merger_t *mrg)
{
    mrg->tracked_offsets_av = newAV();
    return mrg->tracked_offsets_av;
}

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

    if (mrg->parser_stack) {
        SvREFCNT_dec(mrg->parser_stack);
        mrg->parser_stack = NULL;
    }

    if (mrg->tracked_offsets_hv) {
        SvREFCNT_dec(mrg->tracked_offsets_hv);
        mrg->tracked_offsets_hv = NULL;
    }

    if (mrg->tracked_offsets_av) {
        SvREFCNT_dec(mrg->tracked_offsets_av);
        mrg->tracked_offsets_av = NULL;
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

    mrg->obuf.body_pos = mrg->obuf.start - 1;

    srl_reset_input_buffer(mrg, src);
    mrg->ibuf.pos += 6;
    mrg->ibuf.body_pos = mrg->ibuf.pos - 1;

    srl_build_track_table(mrg);
    mrg->ibuf.pos = mrg->ibuf.body_pos + 1;
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

    mrg->tracked_offsets_hv = NULL;
    mrg->tracked_offsets_av = NULL;
    mrg->string_deduper_hv = NULL;
    mrg->parser_stack = NULL;

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
    SV *offset_sv;
    UV offset, last_offset, length;
    int i, avlen;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);

    if (mrg->tracked_offsets_av) {
        av_clear(mrg->tracked_offsets_av);
    }

    while (BUF_NOT_DONE(mrg->ibuf)) {
        tag = *mrg->ibuf.pos;
        if (expect_false(tag & SRL_HDR_TRACK_FLAG)) {
            tag = tag & ~SRL_HDR_TRACK_FLAG;
            offset = BODY_POS_OFS(mrg->ibuf);
            av_push(SRL_GET_TRACKED_OFFSETS_AV(mrg), newSVuv(offset));
        }

        SRL_REPORT_CURRENT_TAG(mrg, tag);
        mrg->ibuf.pos++;

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
                av_push(SRL_GET_TRACKED_OFFSETS_AV(mrg), newSVuv(offset));
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

    if (mrg->tracked_offsets_av && (avlen = av_top_index(mrg->tracked_offsets_av) + 1) > 0) {
        // sort offsets
        sortsv(AvARRAY(mrg->tracked_offsets_av), avlen, S_sv_ncmp);

        // remove duplicated
        last_offset = -1;

        for (i = 0; i < avlen; i++) {
            offset_sv = av_shift(mrg->tracked_offsets_av);
            sv_2mortal(offset_sv);

            offset = SvUV(offset_sv);
            if (last_offset != offset) {
                last_offset = offset;
                av_push(mrg->tracked_offsets_av, SvREFCNT_inc(offset_sv));
            }
        }

#ifndef NDEBUG
        avlen = av_top_index(mrg->tracked_offsets_av) + 1;

        for (i = 0; i < avlen; ++i) {
            SV **sv_offset_ptr = av_fetch(mrg->tracked_offsets_av, i, 0);
            SRL_MERGER_TRACE("tracked_offsets: offset dedups idx %d offset %d SvREFCNT(%d)\n",
                             i, (int) SvUV(*sv_offset_ptr), SvREFCNT(*sv_offset_ptr));
        }
#endif
    }
}

SRL_STATIC_INLINE void
srl_merge_items(pTHX_ srl_merger_t *mrg)
{
    U8 tag;
    UV length, offset;
    SSize_t stack_level;
    SV **stack_item_sv_ptr;
    UV itag_offset, otag_offset;
    int stack_item_value;
    bool trackme;

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);

    // parser_stack is needed for two things:
    // - keep track of expected things
    // - verify document consistency

    if (mrg->parser_stack) {
        av_clear(mrg->parser_stack);
    } else {
        mrg->parser_stack = newAV();
    }

    // push amount to expected top element to pasrser_stack
    SRL_PUSH_CNT_TO_PARSER_STACK(mrg, -1, srl_expected_top_elements(mrg));
    DEBUG_ASSERT_PARSER_STACK(mrg);

    //while (BUF_NOT_DONE(mrg->ibuf)) {
    while (1) {
        while ((stack_level = av_top_index(mrg->parser_stack)) >= 0) {
            stack_item_sv_ptr = av_fetch(mrg->parser_stack, stack_level, 0);
            if (expect_false(stack_item_sv_ptr == NULL || *stack_item_sv_ptr == NULL)) {
                croak("av_fetch returned NULL");
            }

            stack_item_value = SvIV(*stack_item_sv_ptr);
            DEBUG_ASSERT_PARSER_STACK_VALUE(stack_item_value);

            if (stack_item_value > 0)
                break;

            SRL_POP_PARSER_STACK(mrg);
        }

        if (expect_false(stack_level < 0)) {
            break; // stack is empty, no more expected elements to parse
        }

        DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
        DEBUG_ASSERT_BUF_SANE(mrg->obuf);

        tag = *mrg->ibuf.pos;
        tag = tag & ~SRL_HDR_TRACK_FLAG;
        SRL_REPORT_CURRENT_TAG(mrg, tag);
        itag_offset = BODY_POS_OFS(mrg->ibuf);
        otag_offset = BODY_POS_OFS(mrg->obuf);

        trackme = mrg->tracked_offsets_av
                  && av_top_index(mrg->tracked_offsets_av) >= 0
                  && BODY_POS_OFS(mrg->ibuf) == SvIV(*av_fetch(mrg->tracked_offsets_av, 0, 0));

        DEBUG_ASSERT_PARSER_STACK(mrg);
        DEBUG_ASSERT_PARSER_STACK_VALUE(stack_item_value);

        if (IS_SRL_HDR_SHORT_BINARY(tag)) {
            length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);

            // +1 because need to respect tag
            ASSERT_BUF_SPACE(mrg->ibuf, length + 1, " while reading SHORT_BINARY");
            offset = srl_lookup_string(mrg, mrg->ibuf.pos + 1, length, BODY_POS_OFS(mrg->ibuf));

            if (offset) {
                // issue COPY tag
                srl_buf_cat_varint((srl_encoder_t*) mrg, SRL_HDR_COPY, offset);
                mrg->ibuf.pos += length + 1;
            } else {
                // see string first time
                srl_buf_copy_content_nocheck(mrg, length + 1);
            }
        } else if (tag <= SRL_HDR_NEG_HIGH) {
            srl_buf_copy_content_nocheck(mrg, 1);
        } else if (IS_SRL_HDR_ARRAYREF(tag)) {
            srl_buf_copy_content_nocheck(mrg, 1);
            SRL_PUSH_CNT_TO_PARSER_STACK(mrg, tag, SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag));
        } else if (IS_SRL_HDR_HASHREF(tag)) {
            srl_buf_copy_content_nocheck(mrg, 1);
            SRL_PUSH_CNT_TO_PARSER_STACK(mrg, tag, SRL_HDR_HASHREF_LEN_FROM_TAG(tag) * 2);
        } else {
            switch (tag) {
                case SRL_HDR_VARINT:
                case SRL_HDR_ZIGZAG:
                    srl_buf_copy_content_nocheck(mrg, 1);
                    srl_copy_varint(mrg);
                    break;

                case SRL_HDR_FLOAT:         srl_buf_copy_content(mrg, 5,  " while copying FLOAT");       break;
                case SRL_HDR_DOUBLE:        srl_buf_copy_content(mrg, 9,  " while copying DOUBLE");      break;
                case SRL_HDR_LONG_DOUBLE:   srl_buf_copy_content(mrg, 17, " while copying LONG_DOUBLE"); break;

                case SRL_HDR_PAD:
                case SRL_HDR_REFN:
                case SRL_HDR_WEAKEN:
                case SRL_HDR_EXTEND:
                    // this set of tags are not real items,
                    // and stack_item_value should not be decremented,
                    // but at the end of the loop I dont want to create if-branch
                    // so, I simply increment counter to level furter decremetion
                    stack_item_value++;
                    srl_buf_copy_content_nocheck(mrg, 1);
                    break;

                case SRL_HDR_TRUE:
                case SRL_HDR_FALSE:
                case SRL_HDR_UNDEF:
                case SRL_HDR_CANONICAL_UNDEF:
                    srl_buf_copy_content_nocheck(mrg, 1);
                    break;

                case SRL_HDR_BINARY:
                case SRL_HDR_STR_UTF8:
                    mrg->ibuf.pos++; // skip tag in input buffer
                    length = srl_read_varint_uv_length(&mrg->ibuf, " while reading BINARY or STR_UTF8");
                    offset = srl_lookup_string(mrg, mrg->ibuf.pos, length, (UV) BODY_POS_OFS(mrg->ibuf));

                    if (offset) {
                        // issue COPY tag
                        srl_buf_cat_varint((srl_encoder_t*) mrg, SRL_HDR_COPY, offset);
                        mrg->ibuf.pos += (int) length;
                    } else {
                        // see string first time
                        srl_buf_cat_varint((srl_encoder_t*) mrg, tag, length);
                        srl_buf_copy_content_nocheck(mrg, length); // srl_read_varint_uv_length() do space assertion
                    }

                    break;

                case SRL_HDR_HASH:
                case SRL_HDR_ARRAY:
                    srl_buf_copy_content_nocheck(mrg, 1);
                    length = srl_read_varint_uv_count(&mrg->ibuf, " while reading ARRAY or HASH");
                    srl_buf_cat_varint((srl_encoder_t*) mrg, 0, length);

                    SRL_PUSH_CNT_TO_PARSER_STACK(mrg, tag, tag == SRL_HDR_HASH ? (int) length * 2 : (int) length);
                    break;

                case SRL_HDR_COPY:
                    mrg->ibuf.pos++; // skip tag in input buffer
                    offset = srl_read_varint_uv_offset(&mrg->ibuf, " while reading COPY/ALIAS/REFP/OBJECTV/OBJECTV_FREEZE");
                    offset = srl_lookup_tracked_offset(mrg, offset, 0); // convert ibuf offset to obuf offset
                    srl_buf_cat_varint((srl_encoder_t*) mrg, tag, offset);
                    break;

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

        DEBUG_ASSERT_PARSER_STACK_VALUE(stack_item_value);
        SvIV_set(*stack_item_sv_ptr, --stack_item_value);

        if (expect_false(trackme)) {
            SvREFCNT_dec(av_shift(mrg->tracked_offsets_av));
            srl_lookup_tracked_offset(mrg, itag_offset, otag_offset);
        }
    }

    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    DEBUG_ASSERT_BUF_SANE(mrg->obuf);
}

SRL_STATIC_INLINE int
srl_expected_top_elements(pTHX_ srl_merger_t *mrg)
{
    return 1;
}

SRL_STATIC_INLINE UV
srl_lookup_tracked_offset(pTHX_ srl_merger_t *mrg, UV from, UV to)
{
    unsigned int offset = (unsigned int) from; // TODO check types
    SV **offset_ptr = hv_fetch(SRL_GET_TRACKED_OFFSETS_HV(mrg), (const char *) &offset, sizeof(offset), to ? 1 : 0);
    if (expect_false(offset_ptr == NULL)) {
        if (!to) croak("bad target offset"); // TODO
        croak("out of memory (hv_fetch returned NULL)");
    }

    if (to) {
        SRL_MERGER_TRACE("srl_lookup_tracked_offset: store offset %lu => %lu", from, to);
        sv_setuv(*offset_ptr, to);
        return 0;
    }

    if (expect_false(!SvIOK(*offset_ptr))) {
        croak("tracked_offsets_hv contains not a number"); //TODO
    }

    UV len = SvUV(*offset_ptr);
    if (expect_false(mrg->obuf.body_pos + len >= mrg->obuf.pos)) {
        croak("srl_lookup_tracked_offset: corrupted packet");
    }

    return len;
}

SRL_STATIC_INLINE UV
srl_lookup_string(pTHX_ srl_merger_t *mrg, const char* src, STRLEN len, UV offset)
{
    SV **offset_ptr = hv_fetch(SRL_GET_STRING_DEDUPER_AV(mrg), src, len, 1);
    if (expect_false(offset_ptr == NULL)) {
        croak("out of memory (hv_fetch returned NULL)");
    }

    SRL_MERGER_TRACE("srl_lookup_string: %sfound duplicate for '%.*s'",
                     SvOK(*offset_ptr) ? "" : "not ", (int) len, src);

    if (SvOK(*offset_ptr)) {
        return SvUV(*offset_ptr);
    }

    sv_setuv(*offset_ptr, offset);
    return (UV) 0; // 0 is invalid offset for all Sereal versions
}

SRL_STATIC_INLINE void
srl_buf_copy_content(pTHX_ srl_merger_t *mrg, size_t len, const char * const errstr)
{
    DEBUG_ASSERT_BUF_SANE(mrg->ibuf);
    ASSERT_BUF_SPACE(mrg->ibuf, len, errstr);
    srl_buf_copy_content_nocheck(mrg, len);
}

SRL_STATIC_INLINE void
srl_buf_copy_content_nocheck(pTHX_ srl_merger_t *mrg, size_t len)
{
    GROW_BUF(mrg->obuf, len);

    // TODO profile for len == 1
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
    if (expect_false(buf->body_pos + len >= buf->pos)) {
        croak("Corrupted packet%s. Offset %lu points past current iposition %lu in packet with length of %lu bytes long",
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
        croak("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
        //SRL_ERRORf3("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is imipossible.", errstr, len, I32_MAX);
    }

    return len;
}
