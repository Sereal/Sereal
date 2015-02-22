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
#ifndef NDEBUG
#   define TRACE_READER 1
#   define TRACE_STACK 1
#endif

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

/* function declaration */
SRL_STATIC_INLINE UV srl_next_or_step(pTHX_ srl_iterator_t *iter, IV next, IV step);

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

    SRL_RB_CLEAR(iter);
    iter->stack = stack;
    iter->tmp_buf_owner = NULL;
    iter->first_tag_offset = 0;

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

    /* clear previous buffer */
    if (iter->tmp_buf_owner) {
        SvREFCNT_dec(iter->tmp_buf_owner);
        iter->tmp_buf_owner = NULL;
    }

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
        srl_decompress_body_snappy(aTHX_ iter, encoding_flags, &iter->tmp_buf_owner);
        SvREFCNT_inc(iter->tmp_buf_owner);
    } else if (encoding_flags == SRL_PROTOCOL_ENCODING_ZLIB) {
        srl_decompress_body_zlib(aTHX_ iter, &iter->tmp_buf_owner);
        SvREFCNT_inc(iter->tmp_buf_owner);
    } else {
        SRL_RDR_ERROR(iter, "Sereal document encoded in an unknown format");
    }

    SRL_RB_UPDATE_BODY_POS(iter);
    DEBUG_ASSERT_RB_SANE(iter);

    iter->first_tag_offset = SRL_RB_BODY_POS_OFS(iter);
    srl_reset(aTHX_ iter);
}

void
srl_reset(pTHX_ srl_iterator_t *iter)
{
    U8 tag = '\0';
    srl_stack_clear(iter->stack);
    srl_stack_push_and_set(iter, tag, 1); // TODO
    iter->rb_pos = iter->rb_body_pos + iter->first_tag_offset;
}

/* Main routine.
 * Negative values of next and step parameters mean
 * do unlimited number of steps accordingly.
 * Return value: current offset if a step successed, 0 otherwise */
SRL_STATIC_INLINE UV
srl_next_or_step(pTHX_ srl_iterator_t *iter, IV next, IV step)
{
    U8 tag;
    UV length;
    srl_stack_type_t *stack_ptr_orig;
    srl_stack_t *stack = iter->stack;
    UV current_stack_pos, expected_stack_pos;

    DEBUG_ASSERT_RB_SANE(iter);
    if (expect_false(SRL_RB_DONE(iter))) return 0;
    if (expect_false(srl_stack_empty(stack))) croak("Stack is empty");

    current_stack_pos = SRL_STACK_POS(stack);
    expected_stack_pos = current_stack_pos;

    /* Each full iteration of this loop is considered to be a step.
     *
     * Not every tag leads to stepping (i.e. full iteration):
     * - REFN, ALIAS, WEAKEN are modifiers for next step is hencethey are skipped
     * - PAD no comments
     * - others
     *
     * TODO next
     *
     * Effectively, step and next have same meaning as corresponding commands in gdb
     * - step is step inside
     * - next is step next (i.e. loop until object on same stack level is reached)
     */
    while (expect_true(SRL_RB_NOT_DONE(iter))) {
        DEBUG_ASSERT_RB_SANE(iter);

        /* save stack->ptr as it might be
         * amended through an iteration */
        stack_ptr_orig = stack->ptr; // TODO can't save stack_ptr_orig because stack may grow
        assert(stack_ptr_orig != NULL);

        tag = *iter->rb_pos & ~SRL_HDR_TRACK_FLAG;
        SRL_RDR_REPORT_TAG(iter, tag);
        iter->rb_pos++;

        /* No code which decrease step, next or stack's counters should be added here.
         * Otherwise the counters will be decreased twicer for tags like REFN, ALIAS, etc. */

        switch (tag) {
            CASE_SRL_HDR_SHORT_BINARY:
                iter->rb_pos += SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
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

            CASE_SRL_HDR_POS:
            CASE_SRL_HDR_NEG:
                break;

            case SRL_HDR_VARINT:
            case SRL_HDR_ZIGZAG:
                srl_skip_varint(aTHX_ iter);
                break;

            case SRL_HDR_FLOAT:         iter->rb_pos += 4;      break;
            case SRL_HDR_DOUBLE:        iter->rb_pos += 8;      break;
            case SRL_HDR_LONG_DOUBLE:   iter->rb_pos += 16;     break;

            case SRL_HDR_TRUE:
            case SRL_HDR_FALSE:
            case SRL_HDR_UNDEF:
            case SRL_HDR_CANONICAL_UNDEF:
                break;

            case SRL_HDR_REFN:
            case SRL_HDR_ALIAS:
            case SRL_HDR_WEAKEN:
                continue;

            case SRL_HDR_PAD:
                while (SRL_RB_NOT_DONE(iter) && *iter->rb_pos++ == SRL_HDR_PAD) {};
                continue;

            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:      
                length = srl_read_varint_uv_length(aTHX_ iter, " while reading BINARY or STR_UTF8");
                iter->rb_pos += length;
                break;

            case SRL_HDR_COPY:
                srl_skip_varint(aTHX_ iter);
                break;

            /* case SRL_HDR_OBJECTV: */
            /* case SRL_HDR_OBJECTV_FREEZE: */
            /* case SRL_HDR_REFP: */
            /* case SRL_HDR_REGEXP: */
            /* case SRL_HDR_OBJECT: */
            /* case SRL_HDR_OBJECT_FREEZE: */

            default:
                SRL_RDR_ERROR_UNIMPLEMENTED(iter, tag, "");
                break;
        }

        /* stack magic :-) */
        SRL_RB_TRACE("Iterator: stack_ptr_orig: cnt=%d level=%d; stack->ptr: cnt=%d level=%d",
                     (int) stack_ptr_orig->cnt, (int) (stack_ptr_orig - stack->begin),
                     (int) stack->ptr->cnt,     (int) (stack->ptr - stack->begin));

        stack_ptr_orig->cnt--;
        while (   stack_ptr_orig == stack->ptr
               && stack_ptr_orig->cnt == 0)
        {
            srl_stack_pop_nocheck(stack); // we asserted stack_ptr_orig before
            stack_ptr_orig = stack->ptr;
            current_stack_pos--;

            if (expect_false(srl_stack_empty(stack))) {
                SRL_RB_TRACE("Iterator: end of stack reached %d", 1); // TODO
                goto return_value;
            }
        }

        /* check if requred number of steps were done */
        if (--step == 0) {
            SRL_RB_TRACE("Iterator: Did expected number of steps %d", 1); // TODO
            break;
        }

        /* check if we reached expected level */
        if (current_stack_pos == expected_stack_pos && --next == 0) {
            SRL_RB_TRACE("Iterator: Reached expected stack pos (current: %"UVuf" expected: %"UVuf")",
                         current_stack_pos, expected_stack_pos);
            break;
        }

        DEBUG_ASSERT_RB_SANE(iter);
    }

return_value:
    /* assert that we remain on the stack level */
    if (SRL_RB_NOT_DONE(iter) && next >= 0)
        assert(current_stack_pos == expected_stack_pos);

    DEBUG_ASSERT_RB_SANE(iter);
    return SRL_RB_BODY_POS_OFS(iter);
}

UV
srl_next_n(pTHX_ srl_iterator_t *iter, UV next)
{
    return srl_next_or_step(iter, (IV) next, -1);
}

UV
srl_step_n(pTHX_ srl_iterator_t *iter, UV step)
{
    return srl_next_or_step(iter, -1, (IV) step);
}

UV
srl_eof(pTHX_ srl_iterator_t *iter)
{
    DEBUG_ASSERT_RB_SANE(iter);
    return SRL_RB_DONE(iter) ? 1 : 0;
}

UV
srl_offset(pTHX_ srl_iterator_t *iter)
{
    DEBUG_ASSERT_RB_SANE(iter);
    return (UV) SRL_RB_BODY_POS_OFS(iter);
}

SRL_STATIC_INLINE srl_reader_char_ptr
srl_advance_to_object(pTHX_ srl_iterator_t *iter, srl_reader_char_ptr pos)
{
    for (; expect_true(SRL_RB_NOT_DONE(iter)); ++pos) {
        switch (*pos & ~SRL_HDR_TRACK_FLAG) {
            case SRL_HDR_PAD:
            case SRL_HDR_REFN:
            case SRL_HDR_WEAKEN:
                continue;
            default:
                return pos;
        }
    }

    croak("eof reached"); // TODO
}

SV *
srl_get_key(pTHX_ srl_iterator_t *iter)
{
    U8 tag;
    SV *result;
    UV length, offset;
    srl_reader_char_ptr orig_pos;

    DEBUG_ASSERT_RB_SANE(iter);

    orig_pos = iter->rb_pos; // must restore upon return
    tag = *iter->rb_pos & ~SRL_HDR_TRACK_FLAG;
    SRL_RDR_REPORT_TAG(iter, tag);
    iter->rb_pos++;

    switch (tag) {
        CASE_SRL_HDR_SHORT_BINARY:
            length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            break;

        case SRL_HDR_BINARY:
            length = srl_read_varint_uv_length(aTHX_ iter, " while reading BINARY");
            break;

        case SRL_HDR_STR_UTF8:      
            // TODO deal with UTF8
            length = srl_read_varint_uv_length(aTHX_ iter, " while reading STR_UTF8");
            break;

        case SRL_HDR_COPY:
            offset = srl_read_varint_uv_offset(aTHX_ iter, " while reading COPY tag");
            iter->rb_pos = iter->rb_body_pos + offset;

            tag = *iter->rb_pos & ~SRL_HDR_TRACK_FLAG;
            SRL_RDR_REPORT_TAG(iter, tag);
            iter->rb_pos++;

            /* Note we do NOT validate these items, as we have already read them
             * and if they were a problem we would not be here to process them! */

            switch (tag) {
                CASE_SRL_HDR_SHORT_BINARY:
                    length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                    break;

                case SRL_HDR_BINARY:
                    SET_UV_FROM_VARINT(iter, length, iter->rb_pos);
                    break;

                case SRL_HDR_STR_UTF8:
                    // TODO deal with UTF8
                    SET_UV_FROM_VARINT(iter, length, iter->rb_pos);
                    break;

                default:
                    SRL_RDR_ERROR_BAD_COPY(iter, SRL_HDR_HASH);
            }

            break;

        default:
            SRL_RDR_ERROR_UNEXPECTED(iter, tag, "stringish");
    }

    result = newSVpvn((const char *) iter->rb_pos, length); 
    iter->rb_pos = orig_pos;
    DEBUG_ASSERT_RB_SANE(iter);
    return result;
}

IV
srl_find_key(pTHX_ srl_iterator_t *iter, SV *name)
{
    U8 tag;
    STRLEN name_len;
    UV length, offset;
    const char *key_ptr;
    const char *name_ptr = SvPV(name, name_len);
    srl_stack_type_t *stack_ptr = iter->stack->ptr; // TODO can't save stack_ptr_orig because stack may grow
    srl_stack_t *stack = iter->stack;
#   define SRL_KEY_NO_FOUND (-1)

    DEBUG_ASSERT_RB_SANE(iter);

    if (expect_false(srl_stack_empty(stack)))
        return SRL_KEY_NO_FOUND;

    switch (stack_ptr->tag) {
        case SRL_HDR_HASH:
        CASE_SRL_HDR_HASHREF:
            break;
        default:
            SRL_RDR_ERRORf1(
                iter,
                "expect to have HASH tag on stack but got %s",
                SRL_TAG_NAME(stack_ptr->tag)
            );
    }

    /* if key is not in the hash and we're processeing
     * last last element, stack can be empty here */
    while (!srl_stack_empty(stack) && stack_ptr->cnt--) {
        // assert that we're on the same stack level
        assert(iter->stack->ptr == stack_ptr);
        DEBUG_ASSERT_RB_SANE(iter);

        tag = *iter->rb_pos & ~SRL_HDR_TRACK_FLAG;
        SRL_RDR_REPORT_TAG(iter, tag);
        iter->rb_pos++;

        switch (tag) {
            CASE_SRL_HDR_SHORT_BINARY:
                length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                key_ptr = (const char *) iter->rb_pos;
                iter->rb_pos += length;
                break;

            case SRL_HDR_BINARY:
                length = srl_read_varint_uv_length(aTHX_ iter, " while reading BINARY");
                key_ptr = (const char *) iter->rb_pos;
                iter->rb_pos += length;
                break;

            case SRL_HDR_STR_UTF8:      
                // TODO deal with UTF8
                length = srl_read_varint_uv_length(aTHX_ iter, " while reading STR_UTF8");
                key_ptr = (const char *) iter->rb_pos;
                iter->rb_pos += length;
                break;

            case SRL_HDR_COPY:
                offset = srl_read_varint_uv_offset(aTHX_ iter, " while reading COPY tag");
                key_ptr = (const char *) iter->rb_body_pos + offset;
                tag = *key_ptr & ~SRL_HDR_TRACK_FLAG;
                key_ptr++;

                /* Note we do NOT validate these items, as we have already read them
                 * and if they were a problem we would not be here to process them! */

                switch (tag) {
                    CASE_SRL_HDR_SHORT_BINARY:
                        length = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                        break;

                    case SRL_HDR_BINARY:
                        SET_UV_FROM_VARINT(iter, length, key_ptr);
                        break;

                    case SRL_HDR_STR_UTF8:
                        // TODO deal with UTF8
                        SET_UV_FROM_VARINT(iter, length, key_ptr);
                        break;

                    default:
                        SRL_RDR_ERROR_BAD_COPY(iter, SRL_HDR_HASH);
                }

                break;

            default:
                SRL_RDR_ERROR_UNEXPECTED(iter, tag, "stringish");
        }

        if (expect_false(SRL_RB_DONE(iter)))
            SRL_RDR_ERROR_EOF(iter, "hash value");

        if (   length == name_len
            && memEQ(name_ptr, key_ptr, name_len))
        {
            return SRL_RB_BODY_POS_OFS(iter);
        }

        if (expect_false(srl_next_n(aTHX_ iter, 1) == 0))
            SRL_RDR_ERROR(iter, "stepping over hash value failed");
    }

    /* XXX what if stack_ptr->cnt == 0 here ??? */
    return SRL_KEY_NO_FOUND;
}

SV *
srl_object_type(pTHX_ srl_iterator_t *iter)
{
    U8 tag;
    srl_reader_char_ptr pos;

    pos = srl_advance_to_object(iter, iter->rb_pos);
    tag = *pos & ~SRL_HDR_TRACK_FLAG;

     /* TODO think about SRL_HDR_HASH without REFN
      * should we care about this case? */

    switch (tag) {
        case SRL_HDR_HASH:
        CASE_SRL_HDR_HASHREF:
            return newSVpv("HASH", 0);

        case SRL_HDR_ARRAY:
        CASE_SRL_HDR_ARRAYREF:
            return newSVpv("ARRAY", 0);

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
            return newSVpv("SCALAR", 0);
    }

    SRL_RDR_ERROR_UNEXPECTED(iter, tag, " HASH");
}

/* SRL_STATIC_INLINE void
srl_reset_to_offset(pTHX_ srl_iterator_t *iter, UV body_offset)
{
    UV stack_offset;
    srl_stack_t *stack = iter->stack;
    while (!srl_stack_empty(stack)) {
        stack_offset = stack->ptr->offset;
        if (stack_offset > body_offset) {
            srl_stack_pop_nocheck(stack);
        } else if (stack_offset == body_offset) {
            break;
        } else {
            croak("failed to find given offset on stack %"UVuf, body_offset); // TODO
        }
    }

    if (srl_stack_empty(stack))
        croak("srl_reset_to_offset led to empty stack"); // TODO

    iter->rb_pos = iter->rb_body_pos + body_offset;
    DEBUG_ASSERT_RB_SANE(iter);
} */
