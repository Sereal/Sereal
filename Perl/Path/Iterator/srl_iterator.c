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
// #include "snappy/csnappy_decompress.c"
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
    (_stack_ptr)->ridx = (_length);                                             \
    (_stack_ptr)->length = (_length);                                           \
    (_stack_ptr)->offset = SRL_RDR_BODY_POS_OFS_((_iter)->buf);                 \
    (_stack_ptr)->prev_depth= 0;                                                \
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
#   define SRL_ITER_TRACE(msg, args...)                                             \
        SRL_RDR_TRACE("%s " msg, srl_debug_tabulator(iter), ## args)

#   define SRL_ITER_TRACE_WITH_POSITION(msg, args...) STMT_START {                  \
        SRL_RDR_TRACE(                                                              \
            "%s " msg " (ofs %"UVuf" body_ofs %"UVuf")",                            \
            srl_debug_tabulator(iter),                                              \
            ## args,                                                                \
            (UV) SRL_RDR_POS_OFS((iter)->pbuf),                                     \
            (UV) SRL_RDR_BODY_POS_OFS((iter)->pbuf)                                 \
        );                                                                          \
    } STMT_END

#   define SRL_ITER_REPORT_TAG(iter, tag) STMT_START {                              \
        SRL_RDR_TRACE(                                                              \
            "%s tag SRL_HDR_%s (hex: 0x%x) at ofs %"UVuf" body_ofs %"UVuf,          \
            srl_debug_tabulator((iter)),                                            \
            SRL_TAG_NAME((tag)),                                                    \
            (tag),                                                                  \
            (UV) SRL_RDR_POS_OFS((iter)->pbuf),                                     \
            (UV) SRL_RDR_BODY_POS_OFS((iter)->pbuf)                                 \
        );                                                                          \
    } STMT_END
#   define SRL_ITER_REPORT_STACK_STATE(iter) STMT_START {                           \
        if (srl_stack_empty((iter)->pstack)) {                                      \
            SRL_RDR_TRACE(                                                          \
                "%s stack state depth=%"IVdf,                                       \
                srl_debug_tabulator((iter)),                                        \
                SRL_STACK_DEPTH((iter)->pstack)                                     \
            );                                                                      \
        } else {                                                                    \
            srl_iterator_stack_ptr stack_ptr = (iter)->stack.ptr;                   \
            SRL_RDR_TRACE(                                                          \
                "%s stack state depth=%"IVdf" tag=SRL_HDR_%s (hex: 0x%x)"           \
                " idx=%d (rdix=%d) offset=%"UVuf" length=%u",                       \
                srl_debug_tabulator((iter)),                                        \
                SRL_STACK_DEPTH((iter)->pstack),                                    \
                SRL_TAG_NAME(stack_ptr->tag),                                       \
                stack_ptr->tag,                                                     \
                stack_ptr->length - stack_ptr->ridx,                                \
                stack_ptr->ridx,                                                    \
                stack_ptr->offset,                                                  \
                stack_ptr->length                                                   \
            );                                                                      \
        }                                                                           \
    } STMT_END
#else
#   define SRL_ITER_TRACE(msg, args...)
#   define SRL_ITER_TRACE_WITH_POSITION(msg, args...)
#   define SRL_ITER_REPORT_TAG(iter, tag)
#   define SRL_ITER_REPORT_STACK_STATE(iter)
#endif

// note that it's different from SRL_RDR_DONE()
#define SRL_RDR_DONE_(buf) ((buf).pos > (buf).end)
#define SRL_ITER_ASSERT_EOF(iter, msg) STMT_START {                                 \
    if (expect_false(SRL_RDR_DONE_((iter)->buf))) {                                 \
        SRL_RDR_ERROR_EOF((iter)->pbuf, (msg));                                     \
    }                                                                               \
} STMT_END

#define SRL_ITER_ASSERT_STACK(iter) STMT_START {                                    \
    assert(!srl_stack_empty((iter)->pstack));                                       \
    if (expect_false(((int) (iter)->stack.ptr->ridx)) < 0) {                        \
        SRL_ITER_ERROR("Stack is empty! Inconsistent state!");                      \
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
SRL_STATIC_INLINE void srl_iterator_rewind_stack_position(pTHX_ srl_iterator_t *iter);

/* wrappers */
UV srl_iterator_eof(pTHX_ srl_iterator_t *iter)     { return SRL_RDR_DONE(iter->pbuf) ? 1 : 0; }
UV srl_iterator_offset(pTHX_ srl_iterator_t *iter)  { return SRL_RDR_BODY_POS_OFS(iter->pbuf); }

SRL_STATIC_INLINE const char *
srl_debug_tabulator(pTHX_ srl_iterator_t *iter)
{
    int i = 0;
    static char buf[1024];
    for (; i < SRL_STACK_DEPTH(iter->pstack); ++i) buf[i] = '+';
    buf[i] = '\0';
    return (const char *) buf; // XXX
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
    srl_iterator_t *iter = from; // for SRL_ITER_TRACE
    assert(from != NULL);
    assert(to != NULL);
    SRL_ITER_TRACE("from=%p to=%p", from, to);

    if (expect_false(srl_stack_copy(aTHX_ &from->stack, &to->stack)) != 0)
        croak("Out of memory");

    /* it's assumed that buf holds buffer owned by sv */
    to->document = from->document;
    if (to->document) SvREFCNT_inc(to->document);       // shallow document copy
    Copy(&from->buf, &to->buf, 1, srl_reader_buffer_t); // shallow buffer copy

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

    // skip header in any case
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
        srl_stack_pop(stack); // does empty check internally
    }

    srl_iterator_rewind_stack_position(aTHX_ iter);
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

    offset = stack->ptr->offset;
    srl_stack_pop(stack); // remove SRL_ITER_STACK_ROOT_TAG

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

    // This record apart of being a boundary stores offset to ridx's tag (i.e
    // current tag). By default stack keeps offset to tag's starting point
    // (i.e. continer located in the buf).

    srl_stack_push_ptr(iter->pstack, stack_ptr);
    stack_ptr->offset = SRL_RDR_BODY_POS_OFS(iter->pbuf); // disjoint point
    stack_ptr->tag = SRL_ITER_STACK_ROOT_TAG;
    stack_ptr->length = 1;
    stack_ptr->ridx = 1;

    return iter->stack.depth;
}


SRL_STATIC_INLINE void
srl_iterator_rewind_stack_position(pTHX_ srl_iterator_t *iter)
{
    SRL_ITER_ASSERT_STACK(iter);

    iter->stack.ptr->ridx = iter->stack.ptr->length;
    iter->buf.pos = iter->buf.body_pos + iter->stack.ptr->offset;

    SRL_ITER_TRACE_WITH_POSITION("after rewind");
    SRL_ITER_REPORT_STACK_STATE(iter);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

#define srl_iterator_wrap_stack(iter, expected_depth, stack_ptr) STMT_START {                   \
    SRL_ITER_TRACE_WITH_POSITION("wrap_stack depth=%"IVdf, (IV) (expected_depth));              \
    SRL_ITER_REPORT_STACK_STATE(iter);                                                          \
                                                                                                \
    assert((((IV) (expected_depth)) > iter->stack.depth) == 0);                                 \
                                                                                                \
    while (    (iter)->stack.depth != (IV) (expected_depth)                                     \
            && (stack_ptr)->ridx == 0                                                           \
            && (stack_ptr)->tag != SRL_ITER_STACK_ROOT_TAG                                      \
          )                                                                                     \
    {                                                                                           \
        srl_stack_pop_nocheck((iter)->pstack);                                                  \
        SRL_ITER_REPORT_STACK_STATE((iter));                                                    \
        (stack_ptr) = (iter)->stack.ptr;                                                        \
                                                                                                \
        if ((stack_ptr)->prev_depth) {                                                          \
            iter->buf.pos = iter->buf.body_pos + (stack_ptr)->prev_depth;                       \
            (stack_ptr)->prev_depth = 0;                                                        \
                                                                                                \
            /* Offset stored in prev_depth is offset of REFP tag. */                            \
            /* At the same time, ridx was already decremented for REFP (see comments in */      \
            /* srl_iterator_step_internal(). To compensate this, we increate ridx. */           \
            (stack_ptr)->ridx++;                                                                \
                                                                                                \
            SRL_ITER_TRACE_WITH_POSITION("wrap_stack restore offset");                          \
            DEBUG_ASSERT_RDR_SANE(iter->pbuf);                                                  \
            SRL_ITER_REPORT_STACK_STATE(iter);                                                  \
        }                                                                                       \
                                                                                                \
        if (SRL_ITER_STACK_ON_ROOT(iter->pstack))                                               \
            SRL_ITER_TRACE_WITH_POSITION("root of stack reached");                              \
    }                                                                                           \
} STMT_END

/* Main routine. Caller MUST ensure that EOF is NOT reached */
/* and MUST call srl_iterator_wrap_stack before calling srl_iterator_step_internal() */
#define srl_iterator_step_internal(iter, stack_ptr) STMT_START {                                \
    U8 tag;                                                                                     \
    UV length;                                                                                  \
                                                                                                \
    if (expect_false((stack_ptr)->ridx == 0))                                                   \
        SRL_ITER_ERRORf1("Nothing to parse at depth=%"IVdf, SRL_STACK_DEPTH(iter->pstack));     \
                                                                                                \
    /* Iterator decrement ridx *before* parsing an element. This's done for simplicity. */      \
    /* Also, it has some sense. For scalars there is no difference in when idx is decremented.*/\
    /* For arrays/hashes/and objects think this way: element at current depth will be */        \
    /* parsed as soon as iterator finished parsing element at depth+1. */                       \
    (stack_ptr)->ridx--;                                                                        \
                                                                                                \
    SRL_ITER_ASSERT_STACK(iter);                                                                \
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);                                                          \
                                                                                                \
read_again:                                                                                     \
    if (expect_false(SRL_RDR_DONE(iter->pbuf))) {                                               \
        SRL_ITER_ERROR("EOF is reached");                                                       \
    }                                                                                           \
                                                                                                \
    tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;                                                 \
    SRL_ITER_REPORT_TAG(iter, tag);                                                             \
    iter->buf.pos++;                                                                            \
                                                                                                \
    /* No code which decrease step, next or stack's counters should be added here.              \
     * Otherwise the counters will be decreased twicer for tags like REFN, WEAKEN , etc. */     \
                                                                                                \
    switch (tag & 0xE0) {                                                                       \
        case 0x0: /* POS_0 .. NEG_1 */                                                          \
            break;                                                                              \
                                                                                                \
        case 0x40: /* ARRAYREF_0 .. HASHREF_15 */                                               \
            /* for HASHREF_0 .. HASHREF_15 multiple length by two */                            \
            length = (tag & 0xF) << ((tag & 0x10) ? 1 : 0);                                     \
            srl_stack_push_and_set(iter, tag, length, stack_ptr);                               \
            break;                                                                              \
                                                                                                \
        case 0x60: /* SHORT_BINARY_0 .. SHORT_BINARY_31 */                                      \
            iter->buf.pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);                            \
            break;                                                                              \
                                                                                                \
        default:                                                                                \
            switch (tag) {                                                                      \
                case SRL_HDR_HASH:                                                              \
                    length = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH"); \
                    srl_stack_push_and_set(iter, tag, length * 2, stack_ptr);                   \
                    break;                                                                      \
                                                                                                \
                case SRL_HDR_ARRAY:                                                             \
                    length = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");\
                    srl_stack_push_and_set(iter, tag, length, stack_ptr);                       \
                    break;                                                                      \
                                                                                                \
                case SRL_HDR_VARINT:                                                            \
                case SRL_HDR_ZIGZAG:                                                            \
                    srl_skip_varint(aTHX_ iter->pbuf);                                          \
                    break;                                                                      \
                                                                                                \
                case SRL_HDR_FLOAT:         iter->buf.pos += 4;      break;                     \
                case SRL_HDR_DOUBLE:        iter->buf.pos += 8;      break;                     \
                case SRL_HDR_LONG_DOUBLE:   iter->buf.pos += 16;     break;                     \
                                                                                                \
                case SRL_HDR_TRUE:                                                              \
                case SRL_HDR_FALSE:                                                             \
                case SRL_HDR_UNDEF:                                                             \
                case SRL_HDR_CANONICAL_UNDEF:                                                   \
                    break;                                                                      \
                                                                                                \
                case SRL_HDR_REFN:                                                              \
                case SRL_HDR_WEAKEN:                                                            \
                    goto read_again;                                                            \
                                                                                                \
                case SRL_HDR_PAD:                                                               \
                    while (SRL_RDR_NOT_DONE(iter->pbuf) && *iter->buf.pos++ == SRL_HDR_PAD) {}; \
                    goto read_again;                                                            \
                                                                                                \
                case SRL_HDR_BINARY:                                                            \
                case SRL_HDR_STR_UTF8:                                                          \
                    length = srl_read_varint_uv_length(aTHX_ iter->pbuf,                        \
                                                      " while reading BINARY or STR_UTF8");     \
                    iter->buf.pos += length;                                                    \
                    break;                                                                      \
                                                                                                \
                case SRL_HDR_COPY:                                                              \
                case SRL_HDR_REFP:                                                              \
                case SRL_HDR_ALIAS:                                                             \
                    srl_skip_varint(aTHX_ iter->pbuf);                                          \
                    break;                                                                      \
                                                                                                \
                /* case SRL_HDR_OBJECTV: */                                                     \
                /* case SRL_HDR_OBJECTV_FREEZE: */                                              \
                /* case SRL_HDR_REGEXP: */                                                      \
                /* case SRL_HDR_OBJECT: */                                                      \
                /* case SRL_HDR_OBJECT_FREEZE: */                                               \
                                                                                                \
                default:                                                                        \
                    SRL_RDR_ERROR_UNIMPLEMENTED(iter->pbuf, tag, "");                           \
                    break;                                                                      \
            }                                                                                   \
    }                                                                                           \
                                                                                                \
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);                                                          \
} STMT_END

/* srl_iterator_step_in() does N steps. Where step is a serialized object */

void
srl_iterator_step_in(pTHX_ srl_iterator_t *iter, UV n)
{
    UV offset;
    U8 tag, otag;
    srl_reader_char_ptr orig_pos;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);

    while (n--) {
        srl_iterator_wrap_stack(iter, -1, stack_ptr);

        orig_pos = iter->buf.pos;
        tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
        SRL_ITER_REPORT_TAG(iter, tag);
        iter->buf.pos++;

        if (tag == SRL_HDR_REFP || tag == SRL_HDR_ALIAS) {
            /* store offset for REFP tag. Will be used in srl_iterator_wrap_stack() */
            stack_ptr->prev_depth = orig_pos - iter->buf.body_pos; // almost same sa SRL_RDR_BODY_POS_OFS

            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading REFP tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            DEBUG_ASSERT_RDR_SANE(iter->pbuf);

            tag = *iter->buf.pos & ~SRL_HDR_TRACK_FLAG;
            SRL_ITER_TRACE_WITH_POSITION("tag SRL_HDR_REFP points to tag SRL_HDR_%s",
                                         SRL_TAG_NAME(tag));

            switch (tag) {
                case SRL_HDR_HASH:
                case SRL_HDR_ARRAY:
                    break;

                case SRL_HDR_OBJECTV:
                case SRL_HDR_OBJECTV_FREEZE:
                case SRL_HDR_OBJECT:
                case SRL_HDR_OBJECT_FREEZE:
                    croak("not implemented OBJECT");

                CASE_SRL_HDR_HASHREF:
                CASE_SRL_HDR_ARRAYREF:
                    croak("not implemente HASHREF ARRAYREF");

                default:
                    stack_ptr->prev_depth = 0;
                    iter->buf.pos = orig_pos;
                    break;
            }
        } else {
            stack_ptr->prev_depth = 0;
            iter->buf.pos = orig_pos;
        }

        srl_iterator_step_internal(iter, stack_ptr);
    }

    SRL_ITER_TRACE_WITH_POSITION("Completed expected number of steps");
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

void
srl_iterator_step_out(pTHX_ srl_iterator_t *iter, UV n)
{
    U32 idx;
    IV depth = iter->stack.depth;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);

    if (expect_false(n == 0)) return;

    while (n--) {
        if (expect_false(stack_ptr->tag == SRL_ITER_STACK_ROOT_TAG)) {
            SRL_ITER_ERROR("Root of the stack is reached");
        }

        stack_ptr--;
        depth--;
    }

    idx = stack_ptr->length - stack_ptr->ridx;
    SRL_ITER_TRACE_WITH_POSITION("target depth: %"UVuf" idx=%u ridx=%d (length=%d at target depth)",
                                 depth, idx, stack_ptr->ridx, stack_ptr->length);

    assert(idx >= 0);
    assert(idx <= stack_ptr->length);
    assert(depth >= 0);

    srl_iterator_until(aTHX_ iter, (UV) depth, idx);
}

/* srl_iterator_next() does N step on current stack.
 * It garantees that iterator remains on current stack level upon returing */

void
srl_iterator_next(pTHX_ srl_iterator_t *iter, UV n)
{
    IV expected_depth = iter->stack.depth;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);

    if (expect_false(n == 0)) return;

    while (1) {
        srl_iterator_wrap_stack(iter, expected_depth, stack_ptr);

        if (iter->stack.depth == expected_depth) {
            if (n == 0) break;
            else n--;
        }

        srl_iterator_step_internal(iter, stack_ptr);
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

/* srl_iterator_until() moves iterator forward until expected stack level
 * (depth) and index is reached. It can only go down the stack. */

void
srl_iterator_until(pTHX_ srl_iterator_t *iter, UV depth, U32 idx) {
    I32 ridx;
    IV current_depth = iter->stack.depth;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("depth=%"UVuf" idx=%d", depth, idx);
    SRL_ITER_REPORT_STACK_STATE(iter);

    if (expect_false((IV) depth > current_depth)) {
        SRL_ITER_ERRORf2("srl_iterator_until() can only go forward, "
                         "so depth=%"UVuf" should not be greater then current_depth=%"IVdf,
                         depth, current_depth);
    }

    stack_ptr = iter->stack.begin + depth;
    ridx = stack_ptr->length - idx;

    if (expect_false((IV) ridx > stack_ptr->ridx)) {
        SRL_ITER_ERRORf3("srl_iterator_until() can only go forward, "
                         "so ridx=%u should not be greater then current "
                         "index (%u) at depth=%"IVdf,
                         ridx, stack_ptr->ridx, depth);
    }

    if (expect_false((IV) depth == current_depth && (IV) ridx == stack_ptr->ridx))
        return;

    stack_ptr = iter->stack.ptr;

    while (1) {
        srl_iterator_wrap_stack(iter, depth, stack_ptr);

        if (iter->stack.depth == (IV) depth) {
            if (stack_ptr->ridx == (IV) ridx) break;
            assert(((IV) ridx > stack_ptr->ridx) == 0);
        }

        srl_iterator_step_internal(iter, stack_ptr);
    }

    assert(stack_ptr->ridx == (IV) ridx);
    assert(iter->stack.depth == (IV) depth);
    SRL_ITER_TRACE_WITH_POSITION("Reached expected stack depth: %"UVuf " idx: %u ridx: %d", depth, idx, ridx);
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
}

void
srl_iterator_rewind(pTHX_ srl_iterator_t *iter, UV n)
{
    srl_stack_t *stack = iter->pstack;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("n=%"UVuf, n);
    SRL_ITER_REPORT_STACK_STATE(iter);

    while (n--) {
        if (expect_false(SRL_ITER_STACK_ON_ROOT(stack))) {
            SRL_ITER_ERROR("Root of the stack is reached");
        }

        srl_stack_pop_nocheck(stack);
    }

    srl_iterator_rewind_stack_position(aTHX_ iter);
}

IV
srl_iterator_array_goto(pTHX_ srl_iterator_t *iter, I32 idx)
{
    I32 nidx, ridx;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_EOF(iter, "array element");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_ARRAY_ON_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("idx=%d", idx);
    SRL_ITER_REPORT_STACK_STATE(iter);

    nidx = srl_iterator_normalize_idx(aTHX_ idx, stack_ptr->length);
    if (nidx < 0 || nidx >= (I32) stack_ptr->length) {
        SRL_ITER_TRACE("Index is out of range, idx=%d nidx=%d length=%u",
                       idx, nidx, stack_ptr->length);
        return SRL_ITER_NOT_FOUND;
    }

    ridx = stack_ptr->length - nidx;
    if (ridx == stack_ptr->ridx) {
        return SRL_RDR_BODY_POS_OFS(iter->pbuf); // already at expected position
    } else if (ridx > stack_ptr->ridx) {
        SRL_ITER_ERRORf2("Can't go backwards, ridx=%d, length=%u",
                         ridx, stack_ptr->length);
    }

    // srl_iterator_next garantee that we remans on current stack
    srl_iterator_next(aTHX_ iter, stack_ptr->ridx - ridx);
    assert(stack_ptr->ridx == ridx);
    return SRL_RDR_BODY_POS_OFS(iter->pbuf);
}

IV
srl_iterator_array_exists(pTHX_ srl_iterator_t *iter, I32 idx)
{
    I32 nidx;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_EOF(iter, "array element");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_ARRAY_ON_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("idx=%d", idx);
    SRL_ITER_REPORT_STACK_STATE(iter);

    nidx = srl_iterator_normalize_idx(aTHX_ idx, stack_ptr->length);
    if (nidx < 0 || nidx >= (I32) stack_ptr->length) {
        SRL_ITER_TRACE("Index is out of range, idx=%d nidx=%d length=%u",
                       idx, nidx, stack_ptr->length);
        return SRL_ITER_NOT_FOUND;
    }

    return nidx;
}

const char *
srl_iterator_hash_key(pTHX_ srl_iterator_t *iter, STRLEN *len_out)
{
    U8 tag;
    UV length, offset;
    const char *result = NULL;
    srl_reader_char_ptr orig_pos = iter->buf.pos;
    *len_out = 0;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_EOF(iter, "stringish");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_HASH_ON_STACK(iter);

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

    *len_out = length;
    result = (const char *) iter->buf.pos;
    iter->buf.pos = orig_pos; // restore original position
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return result;
}

SV *
srl_iterator_hash_key_sv(pTHX_ srl_iterator_t *iter)
{
    STRLEN length;
    const char *str = srl_iterator_hash_key(aTHX_ iter, &length);
    return sv_2mortal(newSVpvn(str, length));
}

/* Function looks for name key in current hash. If the key is found, the function stops
 * at the key's value object. If the key is not found, the function traverses
 * entire hash and stops after the end of the hash. But remains stack unwrapper. */

IV
srl_iterator_hash_exists(pTHX_ srl_iterator_t *iter, const char *name, STRLEN name_len)
{
    U8 tag;
    UV length, offset;
    const char *key_ptr;

    IV stack_depth = iter->stack.depth;
    srl_iterator_stack_ptr stack_ptr = iter->stack.ptr;

    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    SRL_ITER_ASSERT_EOF(iter, "stringish");
    SRL_ITER_ASSERT_STACK(iter);
    SRL_ITER_ASSERT_HASH_ON_STACK(iter);

    SRL_ITER_TRACE_WITH_POSITION("name=%.*s", (int) name_len, name);
    SRL_ITER_REPORT_STACK_STATE(iter);

    while (stack_ptr->ridx) {
        stack_ptr->ridx--; // do not make it be part of while clause
        SRL_ITER_ASSERT_STACK(iter);
        assert(stack_ptr->ridx % 2 == 1);
        assert(iter->stack.depth == stack_depth);
        DEBUG_ASSERT_RDR_SANE(iter->pbuf);

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
            && memcmp(name, key_ptr, name_len) == 0)
        {
            SRL_ITER_TRACE_WITH_POSITION("found key '%.*s'", (int) name_len, name);
            return SRL_RDR_BODY_POS_OFS(iter->pbuf);
        }

        // srl_iterator_next garantee that we remans on current stack
        srl_iterator_next(aTHX_ iter, 1);
        stack_ptr = iter->stack.ptr;
    }

    SRL_ITER_TRACE("didn't found key '%.*s'", (int) name_len, name);
    return SRL_ITER_NOT_FOUND;
}

IV
srl_iterator_hash_exists_sv(pTHX_ srl_iterator_t *iter, SV *name)
{
    STRLEN name_len;
    const char *name_ptr = SvPV(name, name_len);
    return srl_iterator_hash_exists(aTHX_ iter, name_ptr, name_len);
}

UV
srl_iterator_object_info(pTHX_ srl_iterator_t *iter, UV *length_ptr)
{
    U8 tag;
    UV offset, type = 0;
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
            type = SRL_ITERATOR_OBJ_IS_HASH;
            if (length_ptr) *length_ptr = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading HASH");
            break;

        CASE_SRL_HDR_HASHREF:
            type = SRL_ITERATOR_OBJ_IS_HASH;
            if (length_ptr) *length_ptr = SRL_HDR_HASHREF_LEN_FROM_TAG(tag);
            break;

        case SRL_HDR_ARRAY:
            type = SRL_ITERATOR_OBJ_IS_ARRAY;
            if (length_ptr) *length_ptr = srl_read_varint_uv_count(aTHX_ iter->pbuf, " while reading ARRAY");
            break;

        CASE_SRL_HDR_ARRAYREF:
            type = SRL_ITERATOR_OBJ_IS_ARRAY;
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
            type = SRL_ITERATOR_OBJ_IS_SCALAR;
            break;

        case SRL_HDR_REFP:
        case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter->pbuf, " while reading COPY tag");
            iter->buf.pos = iter->buf.body_pos + offset;
            goto read_again;

        default:
            iter->buf.pos = orig_pos;
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, tag, "ARRAY or HASH or SCALAR");
    }

    iter->buf.pos = orig_pos;
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);
    return type;
}

srl_iterator_stack_ptr
srl_iterator_stack(pTHX_ srl_iterator_t *iter)
{
    return srl_stack_empty(iter->pstack) ? NULL : iter->stack.ptr;
}

IV
srl_iterator_stack_depth(pTHX_ srl_iterator_t *iter)
{
    return SRL_STACK_DEPTH(iter->pstack);
}

UV
srl_iterator_stack_index(pTHX_ srl_iterator_t *iter)
{
    SRL_ITER_ASSERT_STACK(iter);
    assert((I32) iter->stack.ptr->length >= iter->stack.ptr->ridx);
    return (UV) (iter->stack.ptr->length - iter->stack.ptr->ridx);
}

UV
srl_iterator_stack_info(pTHX_ srl_iterator_t *iter, UV *length_ptr)
{
    UV type = 0;
    srl_stack_t *stack = iter->pstack;
    srl_iterator_stack_ptr stack_ptr = stack->ptr;

    SRL_ITER_ASSERT_STACK(iter);
    if (length_ptr) *length_ptr = stack_ptr->length;

    switch (stack_ptr->tag) {
        case SRL_ITER_STACK_ROOT_TAG:
            type = SRL_ITERATOR_OBJ_IS_ROOT;
            break;

        case SRL_HDR_HASH:
        CASE_SRL_HDR_HASHREF:
            type = SRL_ITERATOR_OBJ_IS_HASH;
            break;

        case SRL_HDR_ARRAY:
        CASE_SRL_HDR_ARRAYREF:
            type = SRL_ITERATOR_OBJ_IS_ARRAY;
            break;

        default:
            SRL_RDR_ERROR_UNEXPECTED(iter->pbuf, stack_ptr->tag, "ARRAY or HASH");
    }

    return type;
}

SV *
srl_iterator_decode(pTHX_ srl_iterator_t *iter)
{
    SV *into;
    SRL_ITER_ASSERT_EOF(iter, "serialized object");
    DEBUG_ASSERT_RDR_SANE(iter->pbuf);

    into = sv_2mortal(newSV_type(SVt_NULL));
    if (!iter->dec) iter->dec = srl_build_decoder_struct(aTHX_ NULL, NULL);

    Copy(&iter->buf, &iter->dec->buf, 1, srl_reader_buffer_t);
    DEBUG_ASSERT_RDR_SANE(iter->dec->pbuf);

    srl_decode_single_value(aTHX_ iter->dec, into, NULL);
    return into;
}
