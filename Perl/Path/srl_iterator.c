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

#ifndef NDEBUG
#   if DEBUG > 1
#       define TRACE_READER 1
#       define TRACE_STACK 1
#   endif
#endif

#include "srl_common.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "srl_iterator.h"
#include "srl_reader_misc.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_reader_decompress.h"

// XXX use srl_decoder as shared object???
#include "../Decoder/srl_decoder.h"
#include "../Decoder/srl_decoder.c"

typedef struct {
    UV offset;      // offset of the tag
    U32 count;      // number of child objects
    U32 idx;        // index of current object, in negative format
    U8 tag;
} srl_stack_type_t;

#define srl_stack_push_and_set(iter, tag, length) STMT_START {  \
    srl_stack_type_t * _stack_ptr;                              \
    srl_stack_push_ptr((iter)->stack, _stack_ptr);              \
    _stack_ptr->offset = SRL_RDR_BODY_POS_OFS((iter->pbuf));    \
    _stack_ptr->count = (length);                               \
    _stack_ptr->idx = (length);                                 \
    _stack_ptr->tag = (tag);                                    \
} STMT_END

#define srl_stack_type_t srl_stack_type_t
#include "srl_stack.h"

#define SRL_ITER_BASE_ERROR_FORMAT              "Sereal::Path::Iterator: Error in %s:%u "
#define SRL_ITER_BASE_ERROR_ARGS                __FILE__, __LINE__

#define SRL_ITER_ERROR(msg)                     croak(SRL_ITER_BASE_ERROR_FORMAT "%s", SRL_ITER_BASE_ERROR_ARGS, (msg))
#define SRL_ITER_ERRORf1(fmt, var)              croak(SRL_ITER_BASE_ERROR_FORMAT fmt,  SRL_ITER_BASE_ERROR_ARGS, (var))
#define SRL_ITER_ERRORf2(fmt, var1, var2)       croak(SRL_ITER_BASE_ERROR_FORMAT fmt,  SRL_ITER_BASE_ERROR_ARGS, (var1), (var2))

#define SRL_ITER_TRACE(msg, args...)            SRL_RDR_TRACE("%s" msg, srl_debug_tabulator(iter), ## args)

#define SRL_ITER_REPORT_TAG(iter, tag) STMT_START {                                 \
    SRL_RDR_TRACE(                                                                  \
        "%s"                                                                        \
        "Reader: tag SRL_HDR_%s (int: %d hex: 0x%x) at ofs %"UVuf" body_ofs %"UVuf, \
        srl_debug_tabulator((iter)),                                                \
        SRL_TAG_NAME((tag)),                                                        \
        (tag), (tag),                                                               \
        (UV) SRL_RDR_POS_OFS((iter)->pbuf),                                         \
        (UV) SRL_RDR_BODY_POS_OFS((iter)->pbuf)                                     \
    );                                                                              \
} STMT_END

#define SRL_ITER_ASSERT_EOF(iter, msg) STMT_START {                                 \
    if (expect_false(SRL_RDR_DONE((iter)->pbuf))) {                                 \
        SRL_RDR_ERROR_EOF((iter)->pbuf, (msg));                                     \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_STACK(iter) STMT_START {                                    \
    if (expect_false(srl_stack_empty((iter)->stack))) {                             \
        SRL_ITER_ERROR("Stack is empty! Inconsistent state!");                      \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_ARRAY_ON_STACK(iter) STMT_START {                           \
    U8 _tag = iter->stack->ptr->tag;                                                \
    if (expect_false(                                                               \
            _tag != SRL_HDR_ARRAY                                                   \
         && (_tag < SRL_HDR_ARRAYREF_LOW || _tag > SRL_HDR_ARRAYREF_HIGH)           \
       ))                                                                           \
    {                                                                               \
        SRL_RDR_ERRORf1(&iter->buf, "expect to have ARRAY tag on stack but got %s", \
                        SRL_TAG_NAME(_tag));                                        \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_HASH_ON_STACK(iter) STMT_START {                            \
    U8 _tag = iter->stack->ptr->tag;                                                \
    if (expect_false(                                                               \
            _tag != SRL_HDR_HASH                                                    \
         && (_tag < SRL_HDR_HASHREF_LOW || _tag > SRL_HDR_HASHREF_HIGH)             \
       ))                                                                           \
    {                                                                               \
        SRL_RDR_ERRORf1(&iter->buf, "expect to have HASH tag on stack but got %s",  \
                        SRL_TAG_NAME(_tag));                                        \
    }                                                                               \
} STMT_END

/* function declaration */
SRL_STATIC_INLINE void srl_step_internal(pTHX_ srl_iterator_t *iter);

/* wrappers */
UV srl_eof(pTHX_ srl_iterator_t *iter)                { return SRL_RDR_DONE(iter->pbuf) ? 1 : 0; }
UV srl_offset(pTHX_ srl_iterator_t *iter)             { return SRL_RDR_BODY_POS_OFS(iter->pbuf); }

SRL_STATIC_INLINE const char *
srl_debug_tabulator(pTHX_ srl_iterator_t *iter)
{
    int i = 0;
    static char buf[1024];
    for (; i < SRL_STACK_DEPTH(iter->stack); ++i) buf[i] = '-';
    buf[i] = '\0';
    return (const char *) buf; // XXX
}

srl_iterator_t *
srl_build_iterator_struct(pTHX_ HV *opt)
{
    srl_iterator_t *iter = NULL;
    Newx(iter, 1, srl_iterator_t);
    if (iter == NULL)
        croak("Out of memory");

    srl_stack_t *stack = NULL;
    Newx(stack, 1, srl_stack_t);
    if (stack == NULL) {
        Safefree(iter);
        croak("Out of memory");
    }

    // TODO inline stack
    // TODO keep fixed stack size ???
    if (expect_false(srl_stack_init(aTHX_ stack, 1024) != 0)) {
        Safefree(iter);
        Safefree(stack);
        croak("Out of memory");
    }

    SRL_RDR_CLEAR(&iter->buf);
    iter->pbuf = &iter->buf;
    iter->stack = stack;
    iter->tmp_buf_owner = NULL;
    iter->first_tag_offset = 0;
    iter->dec = NULL;

    /* load options */
    if (opt != NULL) {
        /* svp = hv_fetchs(opt, "dedupe_strings", 0);
        if (svp && SvTRUE(*svp))
            SRL_iter_SET_OPTION(iter, SRL_F_DEDUPE_STRINGS); */
    }

    return iter;
}

void
srl_destroy_iterator(pTHX_ srl_iterator_t *iter)
{
    if (iter->stack) {
        srl_stack_destroy(aTHX_ iter->stack);
        Safefree(iter->stack);
    }

    if (iter->tmp_buf_owner)
        SvREFCNT_dec(iter->tmp_buf_owner);

    if (iter->dec)
        srl_destroy_decoder((srl_decoder_t*) iter->dec);

    Safefree(iter);
    return;
}

void
srl_set_document(pTHX_ srl_iterator_t *iter, SV *src)
{
    STRLEN len;
    UV header_len;
    U8 encoding_flags;
    srl_reader_char_ptr tmp;
    IV proto_version_and_encoding_flags_int;

    /* clear previous buffer */
    if (iter->tmp_buf_owner) {
        SvREFCNT_dec(iter->tmp_buf_owner);
        iter->tmp_buf_owner = NULL;
    }

    tmp = (srl_reader_char_ptr) SvPV(src, len);
    iter->buf.start = iter->buf.pos = tmp;
    iter->buf.end = iter->buf.start + len;

    proto_version_and_encoding_flags_int = srl_validate_header_version(aTHX_ iter->buf.start, len);

    if (proto_version_and_encoding_flags_int < 1) {
        if (proto_version_and_encoding_flags_int == 0)
            SRL_RDR_ERROR(iter->pbuf, "Bad Sereal header: It seems your document was accidentally UTF-8 encoded");
        else
            SRL_RDR_ERROR(iter->pbuf, "Bad Sereal header: Not a valid Sereal document.");
    }

    iter->buf.pos += 5;
    iter->buf.encoding_flags = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_ENCODING_MASK);
    iter->buf.protocol_version = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_VERSION_MASK);
    encoding_flags = iter->buf.encoding_flags;

    if (expect_false(iter->buf.protocol_version > 3 || iter->buf.protocol_version < 1)) {
        SRL_RDR_ERRORf1(iter->pbuf, "Unsupported Sereal protocol version %u", (unsigned int) iter->buf.protocol_version);
    }

    // skip header in any case
    header_len = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading header");
    iter->buf.pos += header_len;

    if (encoding_flags == SRL_PROTOCOL_ENCODING_RAW) {
        /* no op */
    } else if (   encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY
               || encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL)
    {
        srl_decompress_body_snappy(aTHX_ iter->pbuf, &iter->tmp_buf_owner);
        SvREFCNT_inc(iter->tmp_buf_owner);
    } else if (encoding_flags == SRL_PROTOCOL_ENCODING_ZLIB) {
        srl_decompress_body_zlib(aTHX_ iter->pbuf, &iter->tmp_buf_owner);
        SvREFCNT_inc(iter->tmp_buf_owner);
    } else {
        SRL_RDR_ERROR(iter->pbuf, "Sereal document encoded in an unknown format");
    }

    SRL_RDR_UPDATE_BODY_POS(iter->pbuf);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    iter->first_tag_offset = SRL_RDR_BODY_POS_OFS(iter->pbuf);
    srl_reset(aTHX_ iter);
}

void
srl_reset(pTHX_ srl_iterator_t *iter)
{
    U8 tag = '\0';
    SRL_ITER_TRACE();
    srl_stack_clear(iter->stack);
    srl_stack_push_and_set(iter, tag, 1);
    iter->buf.pos = iter->buf.body_pos + iter->first_tag_offset;
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

/* Main routine. Caller must ensure that EOF is NOT reached */
SRL_STATIC_INLINE void
srl_step_internal(pTHX_ srl_iterator_t *iter)
{
    U8 tag;
    UV length;
    srl_stack_type_t *stack_ptr_orig;
    srl_stack_t *stack = iter->stack;
    IV stack_depth_orig = SRL_STACK_DEPTH(stack); // keep track of original depth

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

read_again:
    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    /* No code which decrease step, next or stack's counters should be added here.
     * Otherwise the counters will be decreased twicer for tags like REFN, ALIAS, etc. */

    switch (tag) {
        CASE_SRL_HDR_SHORT_BINARY:
            iter->buf.pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            break;

        case SRL_HDR_HASH:
            length = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            if (length > 0) srl_stack_push_and_set(iter, tag, length * 2);
            break;

        case SRL_HDR_ARRAY:
            length = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
            if (length > 0) srl_stack_push_and_set(iter, tag, length);
            break;

        CASE_SRL_HDR_HASHREF:
            length = SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            if (length > 0) srl_stack_push_and_set(iter, tag, length * 2);
            break;

        CASE_SRL_HDR_ARRAYREF:
            length = SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag);
            if (length > 0) srl_stack_push_and_set(iter, tag, length);
            break;

        CASE_SRL_HDR_POS:
        CASE_SRL_HDR_NEG:
            break;

        case SRL_HDR_VARINT:
        case SRL_HDR_ZIGZAG:
            srl_skip_varint(aTHX_ iter->pbuf);
            break;

        case SRL_HDR_FLOAT:         iter->buf.pos += 4;      break;
        case SRL_HDR_DOUBLE:        iter->buf.pos += 8;      break;
        case SRL_HDR_LONG_DOUBLE:   iter->buf.pos += 16;     break;

        case SRL_HDR_TRUE:
        case SRL_HDR_FALSE:
        case SRL_HDR_UNDEF:
        case SRL_HDR_CANONICAL_UNDEF:
            break;

        case SRL_HDR_REFN:
        case SRL_HDR_ALIAS:
        case SRL_HDR_WEAKEN:
            goto read_again;

        case SRL_HDR_PAD:
            while (SRL_RDR_NOT_DONE(iter->pbuf) && *iter->buf.pos++ == SRL_HDR_PAD) {};
            goto read_again;

        case SRL_HDR_BINARY:
        case SRL_HDR_STR_UTF8:
            length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading BINARY or STR_UTF8");
            iter->buf.pos += length;
            break;

        case SRL_HDR_COPY:
        case SRL_HDR_REFP:
            srl_skip_varint(aTHX_ iter->pbuf);
            break;

        /* case SRL_HDR_OBJECTV: */
        /* case SRL_HDR_OBJECTV_FREEZE: */
        /* case SRL_HDR_REGEXP: */
        /* case SRL_HDR_OBJECT: */
        /* case SRL_HDR_OBJECT_FREEZE: */

        default:
            SRL_RDR_ERROR_UNIMPLEMENTED(iter->pbuf, tag, "");
            break;
    }

    /* stack might has been grown */
    stack_ptr_orig = stack->begin + stack_depth_orig;

    /* if parsed object is a container (i.e. HASH/ARRAY/OBJECT)
     * this decrease counter on parent's stack */
    stack_ptr_orig->idx--;

    /* stack magic :-) */
    SRL_ITER_TRACE("Iterator: stack_ptr_orig: idx=%d depth=%d; stack->ptr: idx=%d depth=%d",
                 (int) stack_ptr_orig->idx, (int) (stack_ptr_orig - stack->begin),
                 (int) stack->ptr->idx,     (int) (stack->ptr - stack->begin));

    /* stack_ptr_orig == stack->ptr makes sure that we don't wrap parent's
     * stack before we finished processing child's one */
    while (   stack_ptr_orig == stack->ptr
           && stack_ptr_orig->idx == 0)
    {
        srl_stack_pop_nocheck(stack); // we asserted stack_ptr_orig before
        stack_ptr_orig = stack->ptr;

        if (expect_false(srl_stack_empty(stack))) {
            SRL_ITER_TRACE("Iterator: end of stack reached");
            break;
        }
    }

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

void
srl_step_in(pTHX_ srl_iterator_t *iter, UV n)
{
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_TRACE("n=%"UVuf, n);

    SRL_ITER_ASSERT_STACK(iter);
    if (expect_false(n == 0)) return;

    while (expect_true(SRL_RDR_NOT_DONE(iter->pbuf))) {
        srl_step_internal(aTHX_ iter);

        if (--n == 0) {
            SRL_ITER_TRACE("Iterator: Did expected number of steps");
            break;
        }
    }

    if (expect_false(n != 0)) {
        SRL_ITER_ERRORf1("Failed to do %"UVuf" steps. Likely EOF was reached", n);
    }

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

void
srl_next(pTHX_ srl_iterator_t *iter, UV n)
{
    srl_stack_t *stack = iter->stack;
    IV expected_depth = SRL_STACK_DEPTH(stack);

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_TRACE("n=%"UVuf, n);

    SRL_ITER_ASSERT_STACK(iter);
    if (expect_false(n == 0)) return;
    if (expect_false(stack->ptr->idx == 0))
        SRL_ITER_ERROR("Nothing to parse at this depth");

    while (expect_true(SRL_RDR_NOT_DONE(iter->pbuf))) {
        srl_step_internal(aTHX_ iter);

        if (SRL_STACK_DEPTH(stack) <= expected_depth && --n == 0) {
            SRL_ITER_TRACE("Iterator: Did expected number of steps at depth %"IVdf, expected_depth);
            break;
        }
    }

    if (expect_false(n != 0)) {
        SRL_ITER_ERRORf1("Failed to do %"UVuf" next steps. Likely EOF was reached", n);
    }

    if (expect_false(SRL_STACK_DEPTH(stack) > expected_depth)) {
        SRL_ITER_ERRORf2("next() led to wrong stack depth, expected=%"IVdf", actual=%"IVdf,
                          expected_depth, SRL_STACK_DEPTH(stack));
    }

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

UV
srl_next_at_depth(pTHX_ srl_iterator_t *iter, UV expected_depth) {
    srl_stack_t *stack = iter->stack;
    IV current_depth = SRL_STACK_DEPTH(stack);

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_TRACE("expected_depth=%"UVuf, expected_depth);

    SRL_ITER_ASSERT_STACK(iter);
    if (expect_false(expected_depth == current_depth))
        return current_depth;

    if (expect_false(expected_depth > current_depth)) {
        SRL_ITER_ERRORf2("srl_next_at_depth() can only go downstairs,"
                         "so expect_depth=%"UVuf" > current_depth=%"IVdf,
                         expected_depth, current_depth);
    }

    while (expect_true(SRL_RDR_NOT_DONE(iter->pbuf))) {
        srl_step_internal(aTHX_ iter);

        current_depth = SRL_STACK_DEPTH(stack);
        SRL_ITER_TRACE("Iterator: current_depth=%"IVdf" expected_depth=%"UVuf, current_depth, expected_depth);

        if (current_depth == (IV) expected_depth) {
            SRL_ITER_TRACE("Iterator: Reached expected stack depth: %"UVuf, expected_depth);
            break;
        } else if (current_depth < (IV) expected_depth) {
            /* This situation is possible when last child object is parsed. In this case
             * child's Sereal representation will end that the same offset as parent's one. */
            SRL_ITER_TRACE("Iterator: srl_continue_until_depth() led to depth lower then expected, "
                         "expected=%"UVuf", actual=%"IVdf, (IV) expected_depth, current_depth);
            break;
        }
    }

    if (expect_false(current_depth > (IV) expected_depth)) {
        SRL_ITER_ERRORf2("Next() led to wrong stack depth, expected=%"IVdf", actual=%"IVdf,
                          expected_depth, current_depth);
    }

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return current_depth;
}

void
srl_step_out(pTHX_ srl_iterator_t *iter, UV n)
{
    UV offset;
    srl_stack_t *stack = iter->stack;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_TRACE("n=%"UVuf, n);

    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    SRL_ITER_ASSERT_STACK(iter);
    // if (expect_false(n == 0)) return; XXX keep it as a feature?

    while (n--) {
        srl_stack_pop_nocheck(stack);

        if (expect_false(srl_stack_empty(stack))) {
            SRL_ITER_ERROR("It was last object on stack, no more parents");
        }
    }

    offset = stack->ptr->offset; 
    iter->buf.pos = iter->buf.body_pos + offset;
    stack->ptr->idx = stack->ptr->count;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

void
srl_array_goto(pTHX_ srl_iterator_t *iter, I32 idx)
{
    U32 s_idx;
    srl_stack_t *stack = iter->stack;
    srl_stack_type_t *stack_ptr = stack->ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_TRACE("idx=%d", idx);

    SRL_ITER_ASSERT_EOF(iter, "array element");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_ARRAY_ON_STACK(iter);

    if (idx >= 0) {
        s_idx = stack->ptr->count - idx;
        if (idx >= stack->ptr->count) {
            SRL_ITER_ERRORf2("Index is out of range, idx=%d count=%u",
                             idx, stack->ptr->count);
        }
    } else {
        s_idx = -idx;
        if (s_idx > stack->ptr->count) {
            SRL_ITER_ERRORf2("Index is out of range, idx=%d count=%u",
                             idx, stack->ptr->count);
        }
    }

    if (s_idx == stack->ptr->idx) {
        return; // already at expected position
    } else if (s_idx > stack->ptr->idx) {
        SRL_ITER_ERRORf2("Can't go backwards, idx=%d, count=%u",
                         idx, stack->ptr->count);
    }

    srl_next(aTHX_ iter, stack->ptr->idx - s_idx);
    assert(stack->ptr->idx == s_idx);
}

SV *
srl_hash_key(pTHX_ srl_iterator_t *iter)
{
    U8 tag;
    SV *result;
    UV length, offset;
    srl_reader_char_ptr orig_pos = iter->buf.pos;

    SRL_ITER_ASSERT_EOF(iter, "stringish");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_HASH_ON_STACK(iter);

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        CASE_SRL_HDR_SHORT_BINARY:
            length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            break;

        case SRL_HDR_BINARY:
            length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading BINARY");
            break;

        case SRL_HDR_STR_UTF8:      
            // TODO deal with UTF8
            length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading STR_UTF8");
            break;

        case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY tag");
            iter->buf.pos = iter->buf.body_pos + offset;

            /* Note we do NOT validate these items, as we have already read them
             * and if they were a problem we would not be here to process them! */

            tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
            SRL_ITER_REPORT_TAG(iter, tag);
            iter->buf.pos++;

            switch (tag) {
                CASE_SRL_HDR_SHORT_BINARY:
                    length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                    break;

                case SRL_HDR_BINARY:
                    SET_UV_FROM_VARINT(iter->pbuf, length, iter->buf.pos);
                    break;

                case SRL_HDR_STR_UTF8:
                    // TODO deal with UTF8
                    SET_UV_FROM_VARINT(iter->pbuf, length, iter->buf.pos);
                    break;

                default:
                    SRL_RDR_ERROR_BAD_COPY(iter->pbuf, SRL_HDR_HASH);
            }

            break;

        default:
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "stringish");
    }

    if (expect_false(iter->buf.pos + length >= iter->buf.end)) {
        SRL_RDR_ERROR_EOF(iter->pbuf, "string content");
    }

    result = sv_2mortal(newSVpvn((const char *) iter->buf.pos, length));
    iter->buf.pos = orig_pos; // restore original position
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return result;
}

/* Function looks for name key in current hash. If the key is found, the function stops
 * at the key's value object. If the key is not found, the function traverses
 * entire hash and stops after the end of the hash */

IV
srl_hash_exists(pTHX_ srl_iterator_t *iter, const char *name, STRLEN name_len)
{
    U8 tag;
    UV length, offset, idx;
    const char *key_ptr;

    srl_stack_t *stack = iter->stack;
    IV stack_depth = SRL_STACK_DEPTH(stack);
    srl_stack_type_t *stack_ptr = stack->ptr;
#   define SRL_KEY_NO_FOUND (0) /* 0 is invalid offset */

    SRL_ITER_ASSERT_EOF(iter, "stringish");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_HASH_ON_STACK(iter);

    assert(stack_ptr->idx > 0);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_TRACE("name=%.*s", (int) name_len, name);

    /* if key is not in the hash and we're processeing
     * last last element, stack can be empty here */
    while (!srl_stack_empty(stack) && stack_ptr->idx--) {
        // assert that we're on the same stack depth
        assert(iter->stack->ptr == stack_ptr);
        DEBUG_ASSERT_RDR_SANE(iter->pbuf);

        /* at the end of an iteration we call srl_next which can
         * lead to invalid stack pointer (moved below current stack)
         * if current pair is the last on in hash. In order to correctly handle
         * this case we store stack index BEFORE processing and check it later */
        idx = stack_ptr->idx;

        tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
        SRL_ITER_REPORT_TAG(iter, tag);
        iter->buf.pos++;

        switch (tag) {
            CASE_SRL_HDR_SHORT_BINARY:
                length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                key_ptr = (const char *) iter->buf.pos;
                iter->buf.pos += length;
                break;

            case SRL_HDR_BINARY:
                length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading BINARY");
                key_ptr = (const char *) iter->buf.pos;
                iter->buf.pos += length;
                break;

            case SRL_HDR_STR_UTF8:      
                // TODO deal with UTF8
                length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading STR_UTF8");
                key_ptr = (const char *) iter->buf.pos;
                iter->buf.pos += length;
                break;

            case SRL_HDR_COPY:
                offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY tag");
                key_ptr = (const char *) iter->buf.body_pos + offset;
                tag = *key_ptr & ~SRL_HDR_TRACK_FLAG;
                key_ptr++;

                /* Note we do NOT validate these items, as we have already read them
                 * and if they were a problem we would not be here to process them! */

                switch (tag) {
                    CASE_SRL_HDR_SHORT_BINARY:
                        length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                        break;

                    case SRL_HDR_BINARY:
                        SET_UV_FROM_VARINT(iter->pbuf, length, key_ptr);
                        break;

                    case SRL_HDR_STR_UTF8:
                        // TODO deal with UTF8
                        SET_UV_FROM_VARINT(iter->pbuf, length, key_ptr);
                        break;

                    default:
                        SRL_RDR_ERROR_BAD_COPY(iter->pbuf, SRL_HDR_HASH);
                }

                break;

            default:
                SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "stringish");
        }

        if (expect_false((srl_reader_char_ptr) key_ptr >= iter->buf.end)) {
            SRL_RDR_ERROR_EOF(iter->pbuf, "string content");
        }

        if (   length == name_len
            && strncmp(name, key_ptr, name_len) == 0)
        {
            SRL_ITER_TRACE("Iterator: found key '%.*s' at offset %"UVuf,
                         (int) name_len, name, SRL_RDR_BODY_POS_OFS(iter->pbuf));
            return SRL_RDR_BODY_POS_OFS(iter->pbuf);
        }

        srl_next(aTHX_ iter, 1);
        if (--idx == 0) break;

        /* stack might have been modified */
        assert(stack_depth <= SRL_STACK_DEPTH(stack));
        stack_ptr = stack->begin + stack_depth;
    }

    /* XXX what if stack_ptr->idx == 0 here ??? */
    SRL_ITER_TRACE("Iterator: didn't found key '%.*s'", (int) name_len, name);
    return SRL_KEY_NO_FOUND;
}

IV
srl_hash_exists_sv(pTHX_ srl_iterator_t *iter, SV *name)
{
    STRLEN name_len;
    const char *name_ptr = SvPV(name, name_len);
    return srl_hash_exists(aTHX_ iter, name_ptr, name_len);
}

UV
srl_object_info(pTHX_ srl_iterator_t *iter, UV *length_ptr)
{
    U8 tag;
    UV length, offset, type = 0;
    srl_reader_char_ptr orig_pos = iter->buf.pos;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    if (length_ptr) *length_ptr = 0;

read_again:
    SRL_ITER_ASSERT_EOF(iter, "serialized object");

    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        case SRL_HDR_PAD:
        case SRL_HDR_REFN:
        case SRL_HDR_WEAKEN:
            /* advanced pointer to the object */
            goto read_again;

        case SRL_HDR_HASH:
            type = SRL_ITER_OBJ_IS_HASH;
            if (length_ptr) *length_ptr = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            break;

        CASE_SRL_HDR_HASHREF:
            type = SRL_ITER_OBJ_IS_HASH;
            if (length_ptr) *length_ptr = SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            break;

        case SRL_HDR_ARRAY:
            type = SRL_ITER_OBJ_IS_ARRAY;
            if (length_ptr) *length_ptr = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
            break;

        CASE_SRL_HDR_ARRAYREF:
            type = SRL_ITER_OBJ_IS_ARRAY;
            if (length_ptr) *length_ptr = SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag);
            break;

        CASE_SRL_HDR_POS:
        CASE_SRL_HDR_NEG:
        CASE_SRL_HDR_SHORT_BINARY:
        case SRL_HDR_BINARY:
        case SRL_HDR_STR_UTF8:      
        case SRL_HDR_VARINT:
        case SRL_HDR_ZIGZAG:
        case SRL_HDR_FLOAT:
        case SRL_HDR_DOUBLE:
        case SRL_HDR_LONG_DOUBLE:
        case SRL_HDR_TRUE:
        case SRL_HDR_FALSE:
        case SRL_HDR_UNDEF:
        case SRL_HDR_CANONICAL_UNDEF:
            type = SRL_ITER_OBJ_IS_SCALAR;
            break;

        /* case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            goto read_again; */ // TODO

        default:
            iter->buf.pos = orig_pos;
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "ARRAY or HASH or SCALAR");
    }

    iter->buf.pos = orig_pos;
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return type;
}

IV
srl_stack_depth(pTHX_ srl_iterator_t *iter)
{
    //SRL_ITER_ASSERT_STACK(iter);
    return SRL_STACK_DEPTH(iter->stack);
}

UV
srl_stack_index(pTHX_ srl_iterator_t *iter)
{
    SRL_ITER_ASSERT_STACK(iter);
    assert(iter->stack->ptr->count >= iter->stack->ptr->idx); 
    return (UV) (iter->stack->ptr->count - iter->stack->ptr->idx);
}

UV
srl_stack_info(pTHX_ srl_iterator_t *iter, UV *length_ptr)
{
    UV type = 0;
    srl_stack_t *stack = iter->stack;
    srl_stack_type_t *stack_ptr = stack->ptr;

    SRL_ITER_ASSERT_STACK(iter);
    if (length_ptr) *length_ptr = stack_ptr->count;

    switch (stack_ptr->tag) {
        case SRL_HDR_HASH:
        CASE_SRL_HDR_HASHREF:
            type = SRL_ITER_OBJ_IS_HASH;
            break;

        case SRL_HDR_ARRAY:
        CASE_SRL_HDR_ARRAYREF:
            type = SRL_ITER_OBJ_IS_ARRAY;
            break;

        default:
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, stack_ptr->tag, "ARRAY or HASH");
    }

    return type;
}

SV *
srl_decode(pTHX_ srl_iterator_t *iter)
{
    SV *into = sv_2mortal(newSV_type(SVt_NULL));
    if (!iter->dec)
        iter->dec = (void*) srl_build_decoder_struct(NULL, NULL);

    srl_decoder_t* dec = (srl_decoder_t*) iter->dec;
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    Copy(&iter->buf, &dec->buf, 1, srl_reader_buffer_t);
    DEBUG_ASSERT_RDR_SANE(dec->pbuf);

    srl_read_single_value(dec, into, NULL);
    return into;
}

