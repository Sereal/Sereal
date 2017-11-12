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
#include <assert.h>

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
#   if DEBUG > 4
#       define TRACE_STACK 1
#       define TRACE_READER 1
#       define TRACE_ITERATOR 1
#   endif
#endif

#if !defined(HAVE_CSNAPPY)
/* XXX
 * This is a dirty hack, snappy/csnappy_decompress.c will be included by srl_decoder.c
 * Also, read comments in srl_reader_decompress.h
 * */
/* #include "snappy/csnappy_decompress.c" */
#endif

#include "srl_common.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "srl_iterator.h"
#include "srl_decoder.h"
#include "srl_reader_misc.h"
#define WANT_SRL_RDR_TRACE /* allow use of SRL_RDR_TRACE */
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_reader_decompress.h"

#define SRL_RDR_BODY_POS_OFS_(buf) ((buf).pos - (buf).body_pos)
#define srl_stack_push_and_set(_iter, _tag, _length, _stack_ptr) STMT_START {   \
    srl_stack_push_ptr((_iter)->pstack, (_stack_ptr));                          \
    (_stack_ptr)->idx = 0;                                                      \
    (_stack_ptr)->length = (_length);                                           \
    (_stack_ptr)->first= SRL_RDR_BODY_POS_OFS_((_iter)->buf);                   \
    (_stack_ptr)->end = 0;                                                      \
    (_stack_ptr)->tag = (_tag);                                                 \
} STMT_END

#define SRL_ITER_STACK_PREALLOCATE (32)
#define SRL_ITER_STACK_ROOT_TAG SRL_HDR_PACKET_START
#define SRL_ITER_STACK_ON_ROOT(stack) ((stack)->ptr->tag == SRL_ITER_STACK_ROOT_TAG)

#define SRL_ITER_BASE_ERROR_FORMAT              "Sereal::Path::Iterator: Error in %s:%u "
#define SRL_ITER_BASE_ERROR_ARGS                __FILE__, __LINE__

#define SRL_ITER_ERROR(msg)                     croak(SRL_ITER_BASE_ERROR_FORMAT "%s", SRL_ITER_BASE_ERROR_ARGS, (msg))
#define SRL_ITER_ERRORf1(fmt, var)              croak(SRL_ITER_BASE_ERROR_FORMAT fmt,  SRL_ITER_BASE_ERROR_ARGS, (var))
#define SRL_ITER_ERRORf2(fmt, var1, var2)       croak(SRL_ITER_BASE_ERROR_FORMAT fmt,  SRL_ITER_BASE_ERROR_ARGS, (var1), (var2))
#define SRL_ITER_ERRORf3(fmt, var1, var2, var3) croak(SRL_ITER_BASE_ERROR_FORMAT fmt,  SRL_ITER_BASE_ERROR_ARGS, (var1), (var2), (var3))

#ifdef TRACE_ITERATOR
#   define SRL_ITER_TRACE(msg, args...) STMT_START {                                \
        fprintf(                                                                    \
            stderr,                                                                 \
            "%s%s:%d:%s(): "msg"\n",                                                \
            srl_debug_tabulator(iter),                                              \
            __FILE__, __LINE__, __func__, ## args                                   \
        );                                                                          \
    } STMT_END

#   define SRL_ITER_TRACE_WITH_POSITION(msg, args...) STMT_START {                  \
        SRL_ITER_TRACE(                                                             \
            "%s"msg" (ofs %"UVuf" body_ofs %"UVuf")",                               \
            srl_debug_tabulator(iter),                                              \
            ## args,                                                                \
            (UV) SRL_RDR_POS_OFS((iter)->pbuf),                                     \
            (UV) SRL_RDR_BODY_POS_OFS((iter)->pbuf)                                 \
        );                                                                          \
    } STMT_END

#   define SRL_ITER_REPORT_TAG(iter, tag) STMT_START {                              \
        SRL_ITER_TRACE(                                                             \
            "tag SRL_HDR_%s (hex: 0x%x) at ofs %"UVuf" body_ofs %"UVuf,             \
            SRL_TAG_NAME((tag)),                                                    \
            (tag),                                                                  \
            (UV) SRL_RDR_POS_OFS((iter)->pbuf),                                     \
            (UV) SRL_RDR_BODY_POS_OFS((iter)->pbuf)                                 \
        );                                                                          \
    } STMT_END
#   define SRL_ITER_REPORT_STACK_STATE(iter) STMT_START {                           \
        if (srl_stack_empty((iter)->pstack)) {                                      \
            SRL_ITER_TRACE(                                                         \
                "stack state depth=%"IVdf,                                          \
                SRL_STACK_DEPTH((iter)->pstack)                                     \
            );                                                                      \
        } else {                                                                    \
            srl_iterator_stack_ptr stack_ptr = (iter)->stack.ptr;                   \
            SRL_ITER_TRACE(                                                         \
                "%sstack state depth=%"IVdf" tag=SRL_HDR_%s (hex: 0x%x)"            \
                " idx=%d first=%"UVuf" length=%u end=%"UVuf,                        \
                srl_debug_tabulator(iter),                                          \
                SRL_STACK_DEPTH((iter)->pstack),                                    \
                SRL_TAG_NAME(stack_ptr->tag),                                       \
                stack_ptr->tag,                                                     \
                stack_ptr->idx,                                                     \
                stack_ptr->first,                                                   \
                stack_ptr->length,                                                  \
                stack_ptr->end                                                      \
            );                                                                      \
        }                                                                           \
    } STMT_END
#else
#   define SRL_ITER_TRACE(msg, args...)
#   define SRL_ITER_TRACE_WITH_POSITION(msg, args...)
#   define SRL_ITER_REPORT_TAG(iter, tag)
#   define SRL_ITER_REPORT_STACK_STATE(iter)
#endif

#define SRL_ITER_ASSERT_EOF(iter, msg) STMT_START {                                 \
    if (expect_false(SRL_RDR_DONE((iter)->pbuf))) {                                 \
        SRL_RDR_ERROR_EOF((iter)->pbuf, (msg));                                     \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_EOF_NONSTRICT(iter, msg) STMT_START {                       \
    if (expect_false((iter)->pbuf->pos > (iter)->pbuf->end)) {                      \
        SRL_RDR_ERROR_EOF((iter)->pbuf, (msg));                                     \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_STACK(iter) STMT_START {                                    \
    assert(!srl_stack_empty((iter)->pstack));                                       \
    if (expect_false((iter)->stack.ptr->idx >= (iter)->stack.ptr->length)) {        \
        SRL_ITER_ERRORf1("No elements at stack depth %"UVuf, (iter)->stack.depth);  \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_STACK_NONSTRICT(iter) STMT_START {                          \
    assert(!srl_stack_empty((iter)->pstack));                                       \
    if (expect_false((iter)->stack.ptr->idx > (iter)->stack.ptr->length)) {         \
        SRL_ITER_ERRORf1("No elements at stack depth %"UVuf, (iter)->stack.depth);  \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_ARRAY_ON_STACK(iter) STMT_START {                           \
    U8 _tag = iter->stack.ptr->tag;                                                 \
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
    U8 _tag = iter->stack.ptr->tag;                                                 \
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
SRL_STATIC_INLINE void srl_iterator_read_stringish(pTHX_ srl_iterator_t *iter, const char **str_out, STRLEN *str_length_out);
SRL_STATIC_INLINE void srl_iterator_read_object(pTHX_ srl_iterator_t *iter, int is_objectv, U8 *tag_out, UV *length_out);
SRL_STATIC_INLINE void srl_iterator_read_refn(pTHX_ srl_iterator_t *iter, U8 *tag_out, UV *length_out);
SRL_STATIC_INLINE UV   srl_iterator_read_refp(pTHX_ srl_iterator_t *iter, U8 *tag_out, UV *length_out);
SRL_STATIC_INLINE UV   srl_iterator_read_alias(pTHX_ srl_iterator_t *iter, int *is_ref_out, U8 *tag_out, UV *length_out);

/* wrappers */
UV srl_iterator_eof(pTHX_ srl_iterator_t *iter)     { return SRL_RDR_DONE(iter->pbuf) ? 1 : 0; }

SRL_STATIC_INLINE const char *
srl_debug_tabulator(pTHX_ srl_iterator_t *iter)
{
    int i = 0;
    static char buf[1024];
    for (; i < SRL_STACK_DEPTH(iter->pstack); ++i) buf[i] = '+';
    buf[i] = '\0';
    return (const char *) buf; /* XXX */
}

srl_iterator_t *
srl_build_iterator_struct(pTHX_ HV *opt)
{
    srl_iterator_t *iter = NULL;
    Newx(iter, 1, srl_iterator_t);
    if (iter == NULL) croak("Out of memory");
    srl_init_iterator(aTHX_ iter, opt);
    return iter;
}

void
srl_init_iterator(pTHX_ srl_iterator_t *iter, HV *opt)
{
    assert(iter != NULL);

    if (expect_false(srl_stack_init(aTHX_ &iter->stack, SRL_ITER_STACK_PREALLOCATE) != 0)) {
        Safefree(iter);
        croak("Out of memory");
    }

    SRL_RDR_CLEAR(&iter->buf);
    iter->pbuf = &iter->buf;
    iter->pstack = &iter->stack;
    iter->document = NULL;
    iter->dec = NULL;

    /* load options */
    if (opt != NULL) {
        /* svp = hv_fetchs(opt, "dedupe_strings", 0);
        if (svp && SvTRUE(*svp))
            SRL_iter_SET_OPTION(iter, SRL_F_DEDUPE_STRINGS); */
    }
}

void
srl_shallow_copy_iterator(pTHX_ srl_iterator_t *from, srl_iterator_t *to)
{
    srl_iterator_t *iter = from; /* for SRL_ITER_TRACE */
    assert(from != NULL);
    assert(to != NULL);
    SRL_ITER_TRACE("from=%p to=%p", from, to);

    if (expect_false(srl_stack_copy(aTHX_ &from->stack, &to->stack)) != 0)
        croak("Out of memory");

    /* it's assumed that buf holds buffer owned by sv */
    to->document = from->document;
    if (to->document) SvREFCNT_inc(to->document);       /* shallow document copy */
    Copy(&from->buf, &to->buf, 1, srl_reader_buffer_t); /* shallow buffer copy */

    to->pstack = &to->stack;
    to->pbuf = &to->buf;
    to->dec = NULL;

    assert(to->buf.pos == from->buf.pos);
}

void
srl_deinit_iterator(pTHX_ srl_iterator_t *iter)
{
    assert(iter != NULL);

    if (iter->dec)
        srl_destroy_decoder(aTHX_ iter->dec);

    if (iter->document)
        SvREFCNT_dec(iter->document);

    srl_stack_deinit(aTHX_ &iter->stack);
}

void
srl_destroy_iterator(pTHX_ srl_iterator_t *iter)
{
    srl_deinit_iterator(aTHX_ iter);
    Safefree(iter);
}

void
srl_iterator_set(pTHX_ srl_iterator_t *iter, SV *src)
{
    SV *sv;
    STRLEN len;
    UV header_len;
    U8 encoding_flags;
    U8 protocol_version;
    srl_reader_char_ptr tmp;
    IV proto_version_and_encoding_flags_int;
    srl_iterator_stack_ptr stack_ptr = NULL;

    if (iter->document) {
        SvREFCNT_dec(iter->document);
        iter->document = NULL;
    }

    iter->document = src;
    SvREFCNT_inc(iter->document);

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
    encoding_flags = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_ENCODING_MASK);
    protocol_version = (U8) (proto_version_and_encoding_flags_int & SRL_PROTOCOL_VERSION_MASK);

    if (expect_false(protocol_version > SRL_PROTOCOL_VERSION || protocol_version < 1)) {
        SRL_RDR_ERRORf1(iter->pbuf, "Unsupported Sereal protocol version %u", (unsigned int) protocol_version);
    }

    /* skip header in any case */
    header_len = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading header");
    iter->buf.pos += header_len;

    if (encoding_flags == SRL_PROTOCOL_ENCODING_RAW) {
        /* no op */
    } else if (   encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY
               || encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL)
    {
        srl_decompress_body_snappy(aTHX_ iter->pbuf, encoding_flags, &sv);
        SvREFCNT_dec(iter->document);
        SvREFCNT_inc(sv);
        iter->document = sv;
    } else if (encoding_flags == SRL_PROTOCOL_ENCODING_ZLIB) {
        srl_decompress_body_zlib(aTHX_ iter->pbuf, &sv);
        SvREFCNT_dec(iter->document);
        SvREFCNT_inc(sv);
        iter->document = sv;
    } else if (encoding_flags == SRL_PROTOCOL_ENCODING_ZSTD) {
        srl_decompress_body_zstd(aTHX_ iter->pbuf, &sv);
        SvREFCNT_dec(iter->document);
        SvREFCNT_inc(sv);
        iter->document = sv;
    } else {
        SRL_RDR_ERROR(iter->pbuf, "Sereal document encoded in an unknown format");
    }

    /* this function *MUST* be called after calling srl_decompress_body* */
    SRL_RDR_UPDATE_BODY_POS(iter->pbuf, protocol_version);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    srl_stack_push_and_set(iter, SRL_ITER_STACK_ROOT_TAG, 1, stack_ptr);
    srl_iterator_reset(aTHX_ iter);
}

void
srl_iterator_reset(pTHX_ srl_iterator_t *iter)
{
    srl_stack_t *stack = iter->pstack;

    while (!SRL_ITER_STACK_ON_ROOT(stack)) {
        srl_stack_pop(stack); /* does empty check internally */
    }

    srl_iterator_rewind(aTHX_ iter, 0);
}

IV
srl_iterator_unite(pTHX_ srl_iterator_t *iter)
{
    UV offset;
    srl_stack_t *stack = iter->pstack;

    if (expect_false(SRL_STACK_DEPTH(stack) <= 0))
        SRL_ITER_ERROR("There is nothing to unite. Please call disjoin first.");

    while (!SRL_ITER_STACK_ON_ROOT(stack)) {
        srl_stack_pop(stack);
    }

    offset = stack->ptr->first;
    srl_stack_pop(stack); /* remove SRL_ITER_STACK_ROOT_TAG */

    SRL_ITER_ASSERT_STACK(iter);
    iter->buf.pos = iter->buf.body_pos + offset;

    SRL_ITER_TRACE_WITH_POSITION("after unite");
    SRL_ITER_REPORT_STACK_STATE(iter);

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return iter->stack.depth;
}

IV
srl_iterator_disjoin(pTHX_ srl_iterator_t *iter)
{
    srl_iterator_stack_ptr stack_ptr;

    SRL_ITER_TRACE_WITH_POSITION("before disjoin");
    SRL_ITER_REPORT_STACK_STATE(iter);

    /* This record apart of being a boundary stores offset to idx's tag (i.e */
    /* current tag). By default stack keeps offset to tag's starting point */
    /* (i.e. continer located in the buf). */

    srl_stack_push_ptr(iter->pstack, stack_ptr);
    stack_ptr->first= SRL_RDR_BODY_POS_OFS(iter->pbuf); /* disjoint point */
    stack_ptr->tag = SRL_ITER_STACK_ROOT_TAG;
    stack_ptr->length = 1;
    stack_ptr->idx = 0;

    return iter->stack.depth;
}

/* srl_iterator_step_in() does N steps. Where step is a serialized object */

void
srl_iterator_step_in(pTHX_ srl_iterator_t *iter, UV n)
{
    U8 tag, otag;
    UV offset, length;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);
    if (expect_false(n == 0)) return;

    while (n) {
        DEBUG_ASSERT_RDR_SANE(iter->pbuf);
        SRL_ITER_ASSERT_STACK(iter);

        --n;

        /* Iterator decrement idx *before* parsing an element. This's done for simplicity. */
        /* Also, it has some sense. For scalars there is no difference in when idx is decremented.*/
        /* For arrays/hashes/and objects think this way: element at current depth will be */
        /* parsed as soon as iterator finished parsing element at depth+1. */
        stack_ptr->idx++;

    read_again:
        SRL_ITER_ASSERT_EOF(iter, "tag");
        tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
        SRL_ITER_REPORT_TAG(iter, tag);
        iter->buf.pos++;

        /* No code which decrease step, next or stack's counters should be added here.
         * Otherwise the counters will be decreased twicer for tags like REFN, WEAKEN , etc. */

        switch (tag & 0xE0) {
            case 0x0: /* POS_0 .. NEG_1 */
                break;

            case 0x40: /* ARRAYREF_0 .. HASHREF_15 */
                /* for HASHREF_0 .. HASHREF_15 multiple length by two */
                length = (tag & 0xF) << ((tag & 0x10) ? 1 : 0);
                srl_stack_push_and_set(iter, tag, length, stack_ptr);
                break;

            case 0x60: /* SHORT_BINARY_0 .. SHORT_BINARY_31 */
                iter->buf.pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                break;

            default:
                switch (tag) {
                    case SRL_HDR_REFN:
                        srl_iterator_read_refn(aTHX_ iter, &tag, &length);
                        srl_stack_push_and_set(iter, tag, length, stack_ptr);
                        break;

                    case SRL_HDR_REFP:
                        /* store offset for REFP tag. Will be used in srl_iterator_wrap_stack() */
                        stack_ptr->end = srl_iterator_read_refp(aTHX_ iter, &tag, &length);
                        srl_stack_push_and_set(iter, tag, length, stack_ptr);
                        break;

                    case SRL_HDR_OBJECT:
                    case SRL_HDR_OBJECT_FREEZE:
                        srl_iterator_read_object(aTHX_ iter, 0, &tag, &length);
                        srl_stack_push_and_set(iter, tag, length, stack_ptr);
                        break;

                    case SRL_HDR_OBJECTV:
                    case SRL_HDR_OBJECTV_FREEZE:
                        srl_iterator_read_object(aTHX_ iter, 1, &tag, &length);
                        srl_stack_push_and_set(iter, tag, length, stack_ptr);
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

                    case SRL_HDR_WEAKEN:
                        goto read_again;

                    case SRL_HDR_PAD:
                        while (SRL_RDR_NOT_DONE(iter->pbuf) && *iter->buf.pos++ == SRL_HDR_PAD) {};
                        goto read_again;

                    case SRL_HDR_BINARY:
                    case SRL_HDR_STR_UTF8:
                        length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading BINARY or STR_UTF8");
                        iter->buf.pos += length; /* TODO assert length */
                        break;

                    case SRL_HDR_COPY:
                        /* COPY is only used for deduping strings in hashes so consider as simple tag */
                        srl_skip_varint(aTHX_ iter->pbuf); break;
                        break;

                    case SRL_HDR_ALIAS: {
                        int is_ref = 0;
                        offset = srl_iterator_read_alias(aTHX_ iter, &is_ref, &tag, &length);
                        /* offset points to end of ALIAS + varint */
                        if (is_ref) {
                            stack_ptr->end = offset;
                            srl_stack_push_and_set(iter, tag, length, stack_ptr);
                        } else {
                            iter->buf.pos = iter->buf.body_pos + offset;
                            SRL_ITER_ASSERT_EOF(iter, "tag");
                        }

                        break;
                    }

                    default:
                        SRL_RDR_ERROR_UNIMPLEMENTED(iter->pbuf, tag, "");
                        break;
                }
        }
    }

    if (n == 0) SRL_ITER_TRACE_WITH_POSITION("Completed expected number of steps");
    else SRL_ITER_TRACE_WITH_POSITION("Didn't do all steps, still have %"UVuf, n);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

SRL_STATIC_INLINE void
srl_iterator_wrap_stack(pTHX_ srl_iterator_t *iter, IV expected_depth)
{
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;
    SRL_ITER_TRACE_WITH_POSITION("expected_depth=%"IVdf, expected_depth);
    SRL_ITER_REPORT_STACK_STATE(iter);

    while (    iter->stack.depth > expected_depth
            && stack_ptr->idx == stack_ptr->length
            && stack_ptr->tag != SRL_ITER_STACK_ROOT_TAG
          )
    {
        srl_stack_pop_nocheck(iter->pstack);
        SRL_ITER_REPORT_STACK_STATE(iter);
        stack_ptr = iter->stack.ptr;

        if (stack_ptr->end) {
            iter->buf.pos = iter->buf.body_pos + stack_ptr->end;
            stack_ptr->end = 0;

            SRL_ITER_TRACE_WITH_POSITION("wrap_stack restore offset");
            SRL_ITER_REPORT_STACK_STATE(iter);

            SRL_ITER_ASSERT_EOF_NONSTRICT(iter, "tag");
            DEBUG_ASSERT_RDR_SANE(iter->pbuf);
        }

        if (SRL_ITER_STACK_ON_ROOT(iter->pstack)) {
            SRL_ITER_TRACE_WITH_POSITION("root of stack reached");
        }
    }
}

void
srl_iterator_step_out(pTHX_ srl_iterator_t *iter, UV n)
{
    UV steps;
    IV expected_depth = iter->stack.depth - (IV) n;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);
    if (expect_false(n == 0)) return;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    /* step_out can we execute we all elements */
    /* are parsed on current stack level */
    SRL_ITER_ASSERT_STACK_NONSTRICT(iter);

    if (expect_false(expected_depth < 0)) {
        SRL_ITER_ERRORf1("Can't do %"UVuf" steps out", n);
    }

    while (iter->stack.depth > expected_depth) {
        stack_ptr = iter->stack.ptr;
        if (expect_false(stack_ptr->tag == SRL_ITER_STACK_ROOT_TAG)) {
            SRL_ITER_ERROR("Root of the stack is reached");
        }

        srl_iterator_next(aTHX_ iter, stack_ptr->length - stack_ptr->idx);
        assert(stack_ptr->idx == stack_ptr->length);
        srl_iterator_wrap_stack(aTHX_ iter, expected_depth);
    }

    if (expect_false(iter->stack.depth != expected_depth)) {
        SRL_ITER_ERRORf1("Failed to do %"UVuf" steps out", n);
    }

    SRL_ITER_TRACE_WITH_POSITION("expected depth: %"UVuf" idx=%u (length=%d at target depth)",
                                 expected_depth, stack_ptr->idx, stack_ptr->length);
}

/* srl_iterator_next() does N step on current stack.
 * It garantees that iterator remains on current stack level upon returing */

void
srl_iterator_next(pTHX_ srl_iterator_t *iter, UV n)
{
    U8 tag;
    UV length;
    IV expected_depth = iter->stack.depth;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    if (expect_false(n == 0)) return;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK(iter);

    while (1) {
        /* wrapping stack */
        srl_iterator_wrap_stack(aTHX_ iter, expected_depth);
        stack_ptr = iter->stack.ptr;

        /* checking conditions */
        if (iter->stack.depth == expected_depth) {
            if (n == 0) break;
            else n--;
        }

        SRL_ITER_ASSERT_STACK(iter);

        /* Doing next step! */
        /* Iterator increment idx *before* parsing an element. This's done for simplicity. */
        /* Also, it has some sense. For scalars there is no difference in when idx is decremented.*/
        /* For arrays/hashes/and objects think this way: element at current depth will be */
        /* parsed as soon as iterator finished parsing element at depth+1. */
        stack_ptr->idx++;

    read_again:
        SRL_ITER_ASSERT_EOF(iter, "EOF is reached");
        DEBUG_ASSERT_RDR_SANE(iter->pbuf);

        tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
        SRL_ITER_REPORT_TAG(iter, tag);
        iter->buf.pos++;

        /* No code which decrease step, next or stack's counters should be added here.
         * Otherwise the counters will be decreased twicer for tags like REFN, WEAKEN , etc. */

        switch (tag & 0xE0) {
            case 0x0: /* POS_0 .. NEG_1 */
                break;

            case 0x40: /* ARRAYREF_0 .. HASHREF_15 */
                /* for HASHREF_0 .. HASHREF_15 multiple length by two */
                length = (tag & 0xF) << ((tag & 0x10) ? 1 : 0);
                srl_stack_push_and_set(iter, tag, length, stack_ptr);
                break;

            case 0x60: /* SHORT_BINARY_0 .. SHORT_BINARY_31 */
                iter->buf.pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                break;

            default:
                switch (tag) {
                    case SRL_HDR_HASH:
                        length = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
                        srl_stack_push_and_set(iter, tag, length * 2, stack_ptr);
                        break;

                    case SRL_HDR_ARRAY:
                        length = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
                        srl_stack_push_and_set(iter, tag, length, stack_ptr);
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
                    case SRL_HDR_WEAKEN:
                        goto read_again;

                    case SRL_HDR_PAD:
                        while (SRL_RDR_NOT_DONE(iter->pbuf) && *iter->buf.pos++ == SRL_HDR_PAD) {};
                        goto read_again;

                    case SRL_HDR_BINARY:
                    case SRL_HDR_STR_UTF8:
                        length = srl_read_varint_uv_length(aTHX_ iter->pbuf,
                                                          " while reading BINARY or STR_UTF8");
                        iter->buf.pos += length;
                        break;

                    case SRL_HDR_COPY:
                    case SRL_HDR_REFP:
                    case SRL_HDR_ALIAS:
                        srl_skip_varint(aTHX_ iter->pbuf);
                        break;

                    case SRL_HDR_OBJECT:
                    case SRL_HDR_OBJECT_FREEZE:
                        srl_iterator_read_stringish(aTHX_ iter, NULL, NULL);
                        goto read_again;

                    case SRL_HDR_OBJECTV:
                    case SRL_HDR_OBJECTV_FREEZE:
                        srl_skip_varint(aTHX_ iter->pbuf);
                        goto read_again;

                    case SRL_HDR_REGEXP:
                        srl_iterator_read_stringish(aTHX_ iter, NULL, NULL);
                        srl_iterator_read_stringish(aTHX_ iter, NULL, NULL);
                        break;

                    default:
                        SRL_RDR_ERROR_UNIMPLEMENTED(iter->pbuf, tag, "");
                        break;
                }
        }
    }

    if (expect_false(n != 0)) {
        SRL_ITER_ERRORf1("Failed to do %"UVuf" next steps. Likely EOF was reached", n);
    }

    if (expect_false(iter->stack.depth != expected_depth)) {
        SRL_ITER_ERRORf2("next() led to wrong stack depth, expected=%"IVdf", actual=%"IVdf,
                          expected_depth, iter->stack.depth);
    }

    SRL_ITER_TRACE_WITH_POSITION("Did expected number of steps at depth %"IVdf, expected_depth);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

void
srl_iterator_rewind(pTHX_ srl_iterator_t *iter, UV n)
{
    srl_stack_t *stack = iter->pstack;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK_NONSTRICT(iter);

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);

    while (n--) {
        if (expect_false(SRL_ITER_STACK_ON_ROOT(stack))) {
            SRL_ITER_ERROR("Root of the stack is reached");
        }

        srl_stack_pop_nocheck(stack);
    }

    iter->stack.ptr->idx = 0;
    iter->buf.pos = iter->buf.body_pos + iter->stack.ptr->first;

    /* can't do strict checking here because */
    /* if rewinding after parsing entire body is possible */
    /* also if length == 0 check gives false positive */
    SRL_ITER_ASSERT_EOF_NONSTRICT(iter, "tag");
    SRL_ITER_ASSERT_STACK_NONSTRICT(iter);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    SRL_ITER_REPORT_STACK_STATE(iter);
}

void
srl_iterator_array_goto(pTHX_ srl_iterator_t *iter, I32 idx)
{
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;
    IV nidx = srl_iterator_array_exists(aTHX_ iter, idx);
    if (nidx == SRL_ITER_NOT_FOUND) {
        SRL_ITER_ERRORf1("Array index %d does not exists", idx);
    }

    if (nidx == stack_ptr->idx) return;
    if (nidx < stack_ptr->idx) {
        srl_iterator_rewind(aTHX_ iter, 0);
    }

    /* srl_iterator_next garantee that we remans on current stack */
    srl_iterator_next(aTHX_ iter, nidx - stack_ptr->idx);
    assert(stack_ptr->idx == nidx);
}

IV
srl_iterator_array_exists(pTHX_ srl_iterator_t *iter, I32 idx)
{
    I32 nidx;
    U32 length = iter->stack.ptr->length;

    SRL_ITER_ASSERT_STACK(iter);
    /* SRL_ITER_ASSERT_ARRAY_ON_STACK(iter); */ /* do not require array to be on stack */
    SRL_ITER_TRACE_WITH_POSITION("idx=%d", idx);

    nidx = srl_iterator_normalize_idx(aTHX_ idx, length);
    if (nidx < 0 || nidx >= (I32) length) {
        SRL_ITER_TRACE("Index is out of range, idx=%d nidx=%d length=%u", idx, nidx, length);
        return SRL_ITER_NOT_FOUND;
    }

    return nidx;
}

void
srl_iterator_hash_key(pTHX_ srl_iterator_t *iter, const char **keyname, STRLEN *keyname_length_out)
{
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;
    SRL_ITER_ASSERT_HASH_ON_STACK(iter);
    SRL_ITER_ASSERT_STACK(iter);

    if (expect_false(stack_ptr->idx % 2 != 0)) {
        SRL_ITER_ERRORf1("Current stack index %d is not hash key", stack_ptr->idx);
    }

    srl_iterator_read_stringish(aTHX_ iter, keyname, keyname_length_out);
    stack_ptr->idx++;
}

/* Function looks for name key in current hash. If the key is found, the function stops
 * at the key's value object. If the key is not found, the function traverses
 * entire hash and stops after the end of the hash. But remains stack unwrapper. */

IV
srl_iterator_hash_exists(pTHX_ srl_iterator_t *iter, const char *name, STRLEN name_length)
{
    const char *keyname;
    STRLEN keyname_length;

    srl_iterator_rewind(aTHX_ iter, 0);

    while (iter->stack.ptr->idx < iter->stack.ptr->length) {
        srl_iterator_hash_key(aTHX_ iter, &keyname, &keyname_length);
        if (keyname_length == name_length && memcmp(name, keyname, name_length) == 0) {
            SRL_ITER_TRACE_WITH_POSITION("found key '%.*s'", (int) name_length, name);
            return SRL_RDR_BODY_POS_OFS(iter->pbuf);
        }

        /* step over value, srl_iterator_next() remains on current stack */
        srl_iterator_next(aTHX_ iter, 1);
    }

    SRL_ITER_TRACE("didn't found key '%.*s'", (int) name_length, name);
    return SRL_ITER_NOT_FOUND;
}

U32
srl_iterator_info(pTHX_ srl_iterator_t *iter, UV *length_out, const char **classname_out, STRLEN *classname_lenght_out)
{
    U8 tag;
    UV offset;
    U32 type = 0;
    srl_reader_char_ptr orig_pos_objectv = NULL;
    srl_reader_char_ptr orig_pos = iter->buf.pos;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    if (length_out) *length_out = 1;
    if (classname_out) *classname_out = NULL;
    if (classname_lenght_out) *classname_lenght_out = 0;

read_again:
    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        case SRL_HDR_OBJECTV:
        case SRL_HDR_OBJECTV_FREEZE:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading OBJECTV or OBJECTV_FREEZE tag");
            orig_pos_objectv = iter->buf.pos;
            iter->buf.pos = iter->buf.body_pos + offset;
            srl_iterator_read_stringish(aTHX_ iter, classname_out, classname_lenght_out);
            iter->buf.pos = orig_pos_objectv;
            type |= SRL_ITERATOR_INFO_BLESSED;
            goto read_object;

        case SRL_HDR_OBJECT:
        case SRL_HDR_OBJECT_FREEZE:
            type |= SRL_ITERATOR_INFO_BLESSED;
            srl_iterator_read_stringish(aTHX_ iter, classname_out, classname_lenght_out);
            goto read_object;

        case SRL_HDR_REFP:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading REFP tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            /* fallthrough */

        case SRL_HDR_REFN:
            type |= SRL_ITERATOR_INFO_REF_TO;
            goto read_ref;

        CASE_SRL_HDR_HASHREF:
            type |= SRL_ITERATOR_INFO_HASH;
            type |= SRL_ITERATOR_INFO_REF_TO;
            if (length_out) *length_out= SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            break;

        CASE_SRL_HDR_ARRAYREF:
            type |= SRL_ITERATOR_INFO_ARRAY;
            type |= SRL_ITERATOR_INFO_REF_TO;
            if (length_out) *length_out = SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag);
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
            type |= SRL_ITERATOR_INFO_SCALAR;
            break;

        case SRL_HDR_REGEXP:
            type |= SRL_ITERATOR_INFO_REGEXP;
            break;

        case SRL_HDR_PAD:
        case SRL_HDR_WEAKEN:
            goto read_again;

        case SRL_HDR_COPY:
        case SRL_HDR_ALIAS:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY or ALIAS tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            goto read_again;

        case SRL_HDR_PACKET_START:
            type |= SRL_ITERATOR_INFO_ROOT;
            break;

        default:
            iter->buf.pos = orig_pos;
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "item tag");
    }

    goto finally;

read_object:
    /* we're here because blessed object is being parsed. type has */
    /* SRL_ITERATOR_INFO_BLESSED bit set. Since one can only bless references */
    /* in perl below switch expects only references. */

    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        case SRL_HDR_REFP:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading REFP tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            /* fallthrough */

        case SRL_HDR_REFN:
            type |= SRL_ITERATOR_INFO_REF_TO;
            goto read_item;

        CASE_SRL_HDR_HASHREF:
            type |= SRL_ITERATOR_INFO_REF_TO;
            type |= SRL_ITERATOR_INFO_HASH;
            if (length_out) *length_out= SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            break;

        CASE_SRL_HDR_ARRAYREF:
            type |= SRL_ITERATOR_INFO_REF_TO;
            type |= SRL_ITERATOR_INFO_ARRAY;
            if (length_out) *length_out= SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            break;

        case SRL_HDR_PAD:
            goto read_object;

        default:
            iter->buf.pos = orig_pos;
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "reference");
    }

    goto finally;

read_item:
    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        case SRL_HDR_REFP:
        case SRL_HDR_REFN:
        CASE_SRL_HDR_HASHREF:
        CASE_SRL_HDR_ARRAYREF:
            type |= SRL_ITERATOR_INFO_REF;
            break;

        case SRL_HDR_OBJECTV:
        case SRL_HDR_OBJECTV_FREEZE:
        case SRL_HDR_OBJECT:
        case SRL_HDR_OBJECT_FREEZE:
            /* assuming that all objects are references */
            type |= SRL_ITERATOR_INFO_REF;
            break;

        case SRL_HDR_HASH:
            type |= SRL_ITERATOR_INFO_HASH;
            if (length_out) *length_out = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            break;

        case SRL_HDR_ARRAY:
            type |= SRL_ITERATOR_INFO_ARRAY;
            if (length_out) *length_out = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
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
            type |= SRL_ITERATOR_INFO_SCALAR;
            break;

        case SRL_HDR_PAD:
            goto read_item;

        case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY or ALIAS tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            goto read_item;

        default:
            iter->buf.pos = orig_pos;
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "item tag");
    }

    goto finally;

read_ref:
    /* We're here because REFN or REFP tags are parsed. At this point type has */
    /* SRL_ITERATOR_INFO_REF_TO bit set. Below switch statement handles following cases: */
    /* * any reference tag => SRL_ITERATOR_INFO_REF */
    /* * any object tag    => SRL_ITERATOR_INFO_REF (becase in perl you can only bless references) */
    /* * HASH/ARRAY        => SRL_ITERATOR_INFO_HASH/ARRAY */
    /* * any scalar        => SRL_ITERATOR_INFO_SCALAR */
    /* * regexp            => SRL_HDR_REGEXP */

    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        case SRL_HDR_REFP:
        case SRL_HDR_REFN:
        CASE_SRL_HDR_HASHREF:
        CASE_SRL_HDR_ARRAYREF:
            type |= SRL_ITERATOR_INFO_REF;
            break;

        case SRL_HDR_OBJECTV:
        case SRL_HDR_OBJECTV_FREEZE:
        case SRL_HDR_OBJECT:
        case SRL_HDR_OBJECT_FREEZE:
            /* assuming that all objects are references */
            type |= SRL_ITERATOR_INFO_REF;
            break;

        case SRL_HDR_HASH:
            type |= SRL_ITERATOR_INFO_HASH;
            if (length_out) *length_out = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            break;

        case SRL_HDR_ARRAY:
            type |= SRL_ITERATOR_INFO_ARRAY;
            if (length_out) *length_out = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
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
            type |= SRL_ITERATOR_INFO_SCALAR;
            break;

        case SRL_HDR_REGEXP:
            type |= SRL_ITERATOR_INFO_REGEXP;
            break;

        case SRL_HDR_PAD:
        case SRL_HDR_WEAKEN:
            goto read_ref;

        case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            goto read_ref;

        default:
            iter->buf.pos = orig_pos;
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "item tag");
    }

finally:
    iter->buf.pos = orig_pos;
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return type;
}

SV *
srl_iterator_decode_and_next(pTHX_ srl_iterator_t *iter)
{
    SV *sv = srl_iterator_decode(aTHX_ iter);
    iter->pbuf->pos = iter->dec->pbuf->pos;
    iter->stack.ptr->idx++;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK_NONSTRICT(iter);
    SRL_ITER_ASSERT_EOF_NONSTRICT(iter, "tag");

    return sv;
}

SV *
srl_iterator_decode(pTHX_ srl_iterator_t *iter)
{
    U8 tag;
    SV *into;
    SRL_ITER_TRACE_WITH_POSITION("decode object at");
    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    SRL_ITER_ASSERT_STACK(iter);

    if (iter->dec) srl_clear_decoder_body_state(aTHX_ iter->dec);
    else iter->dec = srl_build_decoder_struct(aTHX_ NULL, NULL);

    Copy(&iter->buf, &iter->dec->buf, 1, srl_reader_buffer_t);
    DEBUG_ASSERT_RDR_SANE(iter->dec->pbuf);

    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);

    if (tag == SRL_HDR_ALIAS) {
        UV offset;
        srl_reader_char_ptr orig_pos = iter->buf.pos;

        iter->buf.pos++;
        offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading ALIAS tag");
        iter->buf.pos = orig_pos;

        iter->dec->pbuf->pos = iter->dec->pbuf->body_pos + offset;
        if (SRL_RDR_DONE(iter->dec->pbuf)) {
            SRL_RDR_ERROR_EOF(iter->dec->pbuf, "serialized object");
        }
    }

    into = sv_2mortal(newSV_type(SVt_NULL));
    srl_decode_single_value(aTHX_ iter->dec, into, NULL);
    return into;
}

SRL_STATIC_INLINE void
srl_iterator_read_refn(pTHX_ srl_iterator_t *iter, U8 *tag_out, UV *length_out)
{
    U8 tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_ASSERT_EOF(iter, "tag");
    SRL_ITER_REPORT_TAG(iter, tag);

    switch (tag) {
        case SRL_HDR_HASH:
            iter->buf.pos++;
            *length_out = 2 * srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            *tag_out = SRL_HDR_HASH;
            break;

        case SRL_HDR_ARRAY:
            iter->buf.pos++;
            *length_out = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
            *tag_out = SRL_HDR_ARRAY;
            break;

        default:
            *length_out = 1;
            *tag_out = tag;
            break;
    }
}

SRL_STATIC_INLINE UV
srl_iterator_read_refp(pTHX_ srl_iterator_t *iter, U8 *tag_out, UV *length_out)
{
    U8 tag;
    UV offset, refp_parsed_offset;
    SRL_ITER_ASSERT_EOF(iter, "tag offset");

    offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading REFP tag");
    refp_parsed_offset = SRL_RDR_BODY_POS_OFS(iter->pbuf);
    SRL_ITER_TRACE("refp_parsed_offset=%"UVuf, refp_parsed_offset);

    iter->buf.pos = iter->buf.body_pos + offset;
    SRL_ITER_ASSERT_EOF(iter, "tag");

    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);

    switch (tag) {
        case SRL_HDR_HASH:
            iter->buf.pos++;
            *length_out = 2 * srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            *tag_out = SRL_HDR_HASH;
            break;

        case SRL_HDR_ARRAY:
            iter->buf.pos++;
            *length_out = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
            *tag_out = SRL_HDR_ARRAY;
            break;

        default:
            *length_out = 1;
            *tag_out = tag;
            break;
    }

    return refp_parsed_offset;
}

SRL_STATIC_INLINE void
srl_iterator_read_object(pTHX_ srl_iterator_t *iter, int is_objectv, U8 *tag_out, UV *length_out)
{
    U8 tag;
    SRL_ITER_ASSERT_EOF(iter, "object name");

    if (is_objectv) {
        srl_skip_varint(aTHX_ iter->pbuf); /* <OFFSET-VARINT> */
    } else {
        srl_iterator_read_stringish(aTHX_ iter, NULL, NULL); /* <STR-TAG> */
    }

    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        CASE_SRL_HDR_HASHREF:
            *length_out = 2 * SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            *tag_out = tag;
            break;

        CASE_SRL_HDR_ARRAYREF:
            *length_out = SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            *tag_out = tag;
            break;

        case SRL_HDR_REFN:
            srl_iterator_read_refn(aTHX_ iter, tag_out, length_out);
            break;

        case SRL_HDR_REFP:
            srl_iterator_read_refp(aTHX_ iter, tag_out, length_out);
            break;

        default:
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "reference tag");
    }
}

SRL_STATIC_INLINE UV
srl_iterator_read_alias(pTHX_ srl_iterator_t *iter, int *is_ref_out, U8 *tag_out, UV *length_out)
{
    U8 tag;
    UV offset, alias_parsed_offset;
    SRL_ITER_ASSERT_EOF(iter, "tag offset");

    offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading ALIAS tag");
    alias_parsed_offset= SRL_RDR_BODY_POS_OFS(iter->pbuf);
    SRL_ITER_TRACE("alias_parsed_offset=%"UVuf, alias_parsed_offset);

    iter->buf.pos = iter->buf.body_pos + offset;
    SRL_ITER_ASSERT_EOF(iter, "tag");

    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
    SRL_ITER_REPORT_TAG(iter, tag);
    iter->buf.pos++;

    switch (tag) {
        CASE_SRL_HDR_HASHREF:
        CASE_SRL_HDR_ARRAYREF:
        case SRL_HDR_REFN:
        case SRL_HDR_REFP:
            *is_ref_out = 1;
            *length_out = 1;
            *tag_out = tag;
            break;

        case SRL_HDR_OBJECT:
        case SRL_HDR_OBJECT_FREEZE:
            srl_iterator_read_object(aTHX_ iter, 0, tag_out, length_out);
            *is_ref_out = 1;
            break;

        case SRL_HDR_OBJECTV:
        case SRL_HDR_OBJECTV_FREEZE:
            srl_iterator_read_object(aTHX_ iter, 1, tag_out, length_out);
            *is_ref_out = 1;
            break;

        default:
            *is_ref_out = 0;
            break;
    }

    return alias_parsed_offset;
}

SRL_STATIC_INLINE void
srl_iterator_read_stringish(pTHX_ srl_iterator_t *iter, const char **str_out, STRLEN *str_length_out)
{
    U8 tag;
    UV length, offset;
    srl_reader_char_ptr new_pos = NULL;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_EOF(iter, "stringish");

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
            /* TODO deal with UTF8 */
            length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading STR_UTF8");
            break;

        case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY tag");
            new_pos = iter->buf.pos;
            iter->buf.pos = iter->buf.body_pos + offset;
            SRL_ITER_ASSERT_EOF(iter, "stringish");

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
                    /* TODO deal with UTF8 */
                    length = srl_read_varint_uv_length(aTHX_ iter->pbuf, " while reading STR_UTF8");
                    break;

                default:
                    SRL_RDR_ERROR_BAD_COPY(iter->pbuf, SRL_HDR_COPY);
            }

            break;

        default:
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "stringish");
    }

    SRL_RDR_ASSERT_SPACE(iter->pbuf, length, " while reading stringish");
    if (str_out) *str_out = (const char *) iter->buf.pos;
    if (str_length_out) *str_length_out = length;

    /* set new bouf position */
    if (new_pos) iter->buf.pos = new_pos;
    else iter->buf.pos += length;
}
