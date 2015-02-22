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

#define srl_reader_t srl_iterator_t

#include "srl_common.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "srl_iterator.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_reader_decompress.h"

typedef struct {
    UV offset;
    U32 cnt;
    U8 tag;
} srl_stack_type_t;

#define srl_stack_push_and_set(iter, tag, length) STMT_START {  \
    srl_stack_type_t * stack_ptr;                               \
    srl_stack_push_ptr((iter)->stack, stack_ptr);               \
    stack_ptr->offset = SRL_RB_BODY_POS_OFS((iter));            \
    stack_ptr->cnt = (length);                                  \
    stack_ptr->tag = (tag);                                     \
} STMT_END

#define srl_stack_type_t srl_stack_type_t // TODO
#include "srl_stack.h"

srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt);
void srl_destroy_iterator(pTHX_ srl_iterator_t *iter);

srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt)
{
    srl_iterator_t *iter = NULL;
    Newx(iter, 1, srl_iterator_t);
    if (iter == NULL)
        croak("Out of memory");

    SRL_RB_CLEAR(iter);
    iter->tmp_buf_owner = NULL;

    // TODO
    /* if (expect_false(srl_stack_init(aTHX_ mrg->stack, 16) != 0)) {
        Safefree(mrg->tracked_offsets);
        mrg->tracked_offsets = NULL;
        croak("Out of memory");
    } */

    /* load options */
    if (opt != NULL) {
        /* svp = hv_fetchs(opt, "dedupe_strings", 0);
        if (svp && SvTRUE(*svp))
            SRL_iter_SET_OPTION(iter, SRL_F_DEDUPE_STRINGS); */
    }

    return iter;
}

void srl_destroy_iterator(pTHX_ srl_iterator_t *iter)
{
    if (iter->stack)
        srl_stack_destroy(iter->stack);

    if (iter->tmp_buf_owner)
        SvREFCNT_dec(iter->tmp_buf_owner);

    Safefree(iter);
    return;
}

void srl_set_document(pTHX_ srl_iterator_t *iter, SV *src)
{
    STRLEN len;
    UV header_len;
    U8 encoding_flags;
    srl_reader_char_ptr tmp;

    tmp = (srl_reader_char_ptr) SvPV(src, len);
    iter->rb_start = iter->rb_pos = tmp;
    iter->rb_end = iter->rb_start + len;

    IV proto_version_and_encoding_flags_int = srl_validate_header_version_pv_len(aTHX_ iter->rb_start, len);

    if (proto_version_and_encoding_flags_int < 1) {
        if (proto_version_and_encoding_flags_int == 0)
            SRL_RDR_ERROR(iter, "Bad Sereal header: It seems your document was accidentally UTF-8 encoded");
        else
            SRL_RDR_ERROR(iter, "Bad Sereal header: Not a valid Sereal document.");
    }

    iter->rb_pos += 5;
    encoding_flags = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_ENCODING_MASK);
    iter->r_protocol_version = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_VERSION_MASK);

    if (expect_false(iter->r_protocol_version > 3 || iter->r_protocol_version < 1)) {
        SRL_RDR_ERRORf1(iter, "Unsupported Sereal protocol version %u", (unsigned int) iter->r_protocol_version);
    }

    // skip header in any case
    header_len = srl_read_varint_uv_length(aTHX_ iter, " while reading header");
    iter->rb_pos += header_len;

    if (encoding_flags == SRL_PROTOCOL_ENCODING_RAW) {
        /* no op */
    } else if (   encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY
               || encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL)
    {
        srl_decompress_body_snappy(iter, encoding_flags, &iter->tmp_buf_owner);
        if (iter->tmp_buf_owner) SvREFCNT_inc(iter->tmp_buf_owner);
    } else if (encoding_flags == SRL_PROTOCOL_ENCODING_ZLIB) {
        srl_decompress_body_zlib(iter, &iter->tmp_buf_owner);
        if (iter->tmp_buf_owner) SvREFCNT_inc(iter->tmp_buf_owner);
    } else {
        SRL_RDR_ERROR(iter, "Sereal document encoded in an unknown format");
    }

    SRL_RB_UPDATE_BODY_POS(iter);
    DEBUG_ASSERT_RB_SANE(iter);
}

UV srl_next_n(pTHX_ srl_iterator_t *iter, UV n)
{
    U8 tag;
    UV length;
    srl_stack_t *stack = iter->stack;
    IV current_stack_pos, expected_stack_pos;

    DEBUG_ASSERT_RB_SANE(iter);

    if (srl_stack_empty(stack))
        croak("Stack is empty");

    current_stack_pos = SRL_STACK_POS(stack);
    expected_stack_pos = current_stack_pos;

    while (expect_true(n-- && SRL_RB_NOT_DONE(iter))) {
        tag = *iter->rb_pos++ & ~SRL_HDR_TRACK_FLAG;
        SRL_RDR_REPORT_TAG(iter, tag);

        while (stack->ptr->cnt-- == 0) {
            if (srl_stack_empty(stack)) goto return_value;
            srl_stack_pop_nocheck(stack);
            current_stack_pos--;
        }

        switch (tag) {
            CASE_SRL_HDR_POS:
            CASE_SRL_HDR_NEG:               /* noop */                                                  break;
            CASE_SRL_HDR_SHORT_BINARY:      iter->rb_pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);     break;
            case SRL_HDR_VARINT:
            case SRL_HDR_ZIGZAG:            srl_skip_varint(aTHX_ iter);                                break;
            case SRL_HDR_FLOAT:             iter->rb_pos += 4;                                          break;
            case SRL_HDR_DOUBLE:            iter->rb_pos += 8;                                          break;
            case SRL_HDR_LONG_DOUBLE:       iter->rb_pos += 16;                                         break;

            case SRL_HDR_TRUE:
            case SRL_HDR_FALSE:
            case SRL_HDR_UNDEF:
            case SRL_HDR_CANONICAL_UNDEF:   /* noop */                                                  break;

            case SRL_HDR_REFN:
            case SRL_HDR_ALIAS:
            case SRL_HDR_WEAKEN:            n++; continue; /* fake n */                                 break;

            case SRL_HDR_PAD:
                while (SRL_RB_NOT_DONE(iter) && *iter->rb_pos++ == SRL_HDR_PAD) {};
                continue;

            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:      
                length = srl_read_varint_uv_length(aTHX_ iter, " while reading BINARY or STR_UTF8");
                iter->rb_pos += length;
                break;

            case SRL_HDR_HASH:
                length = srl_read_varint_uv_count(aTHX_ iter, " while reading ARRAY or HASH");
                srl_stack_push_and_set(iter, tag, length * 2);
                current_stack_pos++;
                break;

            case SRL_HDR_ARRAY:
                length = srl_read_varint_uv_count(aTHX_ iter, " while reading ARRAY or HASH");
                srl_stack_push_and_set(iter, tag, length);
                current_stack_pos++;
                break;

            CASE_SRL_HDR_HASHREF:
                length = SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
                srl_stack_push_and_set(iter, tag, length * 2);
                current_stack_pos++;
                break;

            CASE_SRL_HDR_ARRAYREF:
                length = SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag);
                srl_stack_push_and_set(iter, tag, length);
                current_stack_pos++;
                break;

            /* case SRL_HDR_OBJECTV: */
            /* case SRL_HDR_OBJECTV_FREEZE: */
            /* case SRL_HDR_COPY: */
            /* case SRL_HDR_REFP: */
            /* case SRL_HDR_REGEXP: */
            /* case SRL_HDR_OBJECT: */
            /* case SRL_HDR_OBJECT_FREEZE: */

            default:
                SRL_RDR_ERROR_UNIMPLEMENTED(iter, tag, "");
                break;
        }

        /* we reached expected level */
        if (current_stack_pos == expected_stack_pos)
            break;
    }

   return_value:
    DEBUG_ASSERT_RB_SANE(iter);
    return SRL_RB_BODY_POS_OFS(iter);
}

IV srl_eof(pTHX_ srl_iterator_t *iter)
{
    DEBUG_ASSERT_RB_SANE(iter);
    return SRL_RB_DONE(iter) ? 1 : 0;
}

UV srl_offset(pTHX_ srl_iterator_t *iter)
{
    DEBUG_ASSERT_RB_SANE(iter);
    return (UV) SRL_RB_BODY_POS_OFS(iter);
}

UV srl_object_type(pTHX_ srl_iterator_t *iter)
{
    U8 tag = *iter->rb_pos & ~SRL_HDR_TRACK_FLAG;
    switch (tag) {
    }

    return 0;
}
