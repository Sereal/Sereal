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

/*#define SRL_SPLITTER_TRACE(msg, args...) warn((msg), args) */
#define SRL_SPLITTER_TRACE(msg, args...)

#if !defined(HAVE_CSNAPPY)
# include "snappy/csnappy_decompress.c"
#endif

#include "srl_splitter.h"
#include "srl_common.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "ptable.h"
#include "srl_reader.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_reader_checksum.h"
#include "srl_reader_header.h"
#include "srl_reader_skip.h"
#include "srl_buffer.h"
#include "srl_compress.h"

#define DEFAULT_MAX_RECUR_DEPTH 10000
#define SRL_SPLITTER_STACK_PREALLOCATE 1024
#define SRL_SPLITTER_CHUNK_PREALLOCATE_MAX (1 << 20)

/* The '__$CNT__' placeholder of a header data template is rewritten into an
 * 8 byte VARINT, see Splitter.pm. */
#define SRL_HEADER_COUNT_VARINT_LENGTH 8

/* Defaults matching Sereal::Encoder */
#define DEFAULT_ZLIB_LEVEL MZ_DEFAULT_COMPRESSION
#define DEFAULT_ZSTD_LEVEL 3

#define SRL_SPLITTER_BUF(splitter) (&(splitter)->buf)

/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE int srl_splitter_parse(pTHX_ srl_splitter_t *splitter);
SRL_STATIC_INLINE void srl_splitter_read_tag(pTHX_ srl_splitter_t *splitter, U8 tag);
SRL_STATIC_INLINE void srl_splitter_read_string(pTHX_ srl_splitter_t *splitter, const U8 *tag_pos, UV len, int is_utf8);
SRL_STATIC_INLINE void srl_splitter_read_refp(pTHX_ srl_splitter_t *splitter);
SRL_STATIC_INLINE void srl_splitter_read_objectv(pTHX_ srl_splitter_t *splitter, int is_freeze);
SRL_STATIC_INLINE void srl_splitter_read_copy(pTHX_ srl_splitter_t *splitter);
SRL_STATIC_INLINE void srl_splitter_read_alias(pTHX_ srl_splitter_t *splitter);
SRL_STATIC_INLINE int srl_splitter_flush(pTHX_ srl_splitter_t *splitter, const U8 *end_pos, const U8 *next_start_pos);

/* status stack */

SRL_STATIC_INLINE void
srl_splitter_push(pTHX_ srl_splitter_t *splitter, UV val)
{
    srl_stack_push_val(&splitter->status_stack, val);
}

SRL_STATIC_INLINE UV
srl_splitter_pop(pTHX_ srl_splitter_t *splitter)
{
    UV val = srl_stack_peek_nocheck(aTHX_ &splitter->status_stack);
    srl_stack_pop_nocheck(&splitter->status_stack);
    return val;
}

/* push n values to parse; every value is at least one byte long */
SRL_STATIC_INLINE void
srl_splitter_push_values(pTHX_ srl_splitter_t *splitter, UV n)
{
    if (expect_false(n > (UV)SRL_RDR_SPACE_LEFT(SRL_SPLITTER_BUF(splitter))))
        SRL_RDR_ERRORf1(SRL_SPLITTER_BUF(splitter),
                        "Corrupted packet. Count %"UVuf" exceeds the remaining input", n);
    while (n-- > 0)
        srl_splitter_push(aTHX_ splitter, ST_VALUE);
}

/* enter a nested value: undone by the ST_DEEPNESS_UP pushed here */
SRL_STATIC_INLINE void
srl_splitter_deeper(pTHX_ srl_splitter_t *splitter)
{
    if (expect_false(++splitter->deepness > splitter->max_recursion_depth))
        SRL_RDR_ERRORf1(SRL_SPLITTER_BUF(splitter),
                        "Reached recursion limit (%"UVuf") during splitting", splitter->max_recursion_depth);
    srl_splitter_push(aTHX_ splitter, ST_DEEPNESS_UP);
}

/* parse the value at a back-reference, then come back to the current position */
SRL_STATIC_INLINE void
srl_splitter_jump(pTHX_ srl_splitter_t *splitter, UV offset)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    srl_splitter_push(aTHX_ splitter, (UV)(buf->pos - buf->start));
    srl_splitter_push(aTHX_ splitter, ST_ABSOLUTE_JUMP);
    srl_splitter_push(aTHX_ splitter, ST_VALUE);
    buf->pos = buf->body_pos + offset;
    splitter->chunk_iter_start = buf->pos;
}

/* chunk output */

SRL_STATIC_INLINE void
srl_splitter_cat(pTHX_ srl_splitter_t *splitter, const U8 *str, UV len)
{
    srl_buf_cat_str(&splitter->chunk, (const char *)str, len);
    splitter->chunk_size += len;
}

SRL_STATIC_INLINE void
srl_splitter_cat_varint(pTHX_ srl_splitter_t *splitter, U8 tag, UV value)
{
    const STRLEN before = BUF_POS_OFS(&splitter->chunk);
    srl_buf_cat_varint(aTHX_ &splitter->chunk, (char)tag, value);
    splitter->chunk_size += BUF_POS_OFS(&splitter->chunk) - before;
}

/* the offset, from the chunk body's point of view, that the input byte at
 * pos will have once it is copied into the chunk */
SRL_STATIC_INLINE UV
srl_splitter_chunk_offset(srl_splitter_t *splitter, const U8 *pos)
{
    return (UV)BODY_POS_OFS(&splitter->chunk) + (UV)(pos - splitter->chunk_iter_start);
}

/* Copy the input from chunk_iter_start up to end_pos (default: the current
 * position) into the chunk, then continue copying from next_start_pos
 * (default: the current position). Returns true if anything was copied. */
SRL_STATIC_INLINE int
srl_splitter_flush(pTHX_ srl_splitter_t *splitter, const U8 *end_pos, const U8 *next_start_pos)
{
    int flushed = 0;
    if (end_pos == NULL)
        end_pos = splitter->buf.pos;
    if (next_start_pos == NULL)
        next_start_pos = splitter->buf.pos;
    if (end_pos > splitter->chunk_iter_start) {
        srl_splitter_cat(aTHX_ splitter, splitter->chunk_iter_start, (UV)(end_pos - splitter->chunk_iter_start));
        flushed = 1;
    }
    splitter->chunk_iter_start = next_start_pos;
    return flushed;
}

/* construction and destruction */

SRL_STATIC_INLINE void
srl_splitter_read_options(pTHX_ srl_splitter_t *splitter, HV *opt)
{
    SV **svp;

    svp = hv_fetchs(opt, "chunk_size", 0);
    if (svp && SvOK(*svp))
        splitter->size_limit = SvUV(*svp);

    svp = hv_fetchs(opt, "max_recursion_depth", 0);
    splitter->max_recursion_depth = (svp && SvOK(*svp)) ? SvUV(*svp) : DEFAULT_MAX_RECUR_DEPTH;

    /* See also Splitter.pm's constants */
    svp = hv_fetchs(opt, "compress", 0);
    if (svp && SvOK(*svp)) {
        switch (SvIV(*svp)) {
            case 0: break;
            case 1: splitter->compress_flags = SRL_F_COMPRESS_SNAPPY_INCREMENTAL; break;
            case 2: splitter->compress_flags = SRL_F_COMPRESS_ZLIB; break;
            case 3: splitter->compress_flags = SRL_F_COMPRESS_ZSTD; break;
            default: croak("Invalid value for the 'compress' option");
        }
    }

    svp = hv_fetchs(opt, "compress_level", 0);
    if (svp && SvOK(*svp))
        splitter->compress_level = (int)SvIV(*svp);
    else
        splitter->compress_level = splitter->compress_flags == SRL_F_COMPRESS_ZSTD
                                 ? DEFAULT_ZSTD_LEVEL : DEFAULT_ZLIB_LEVEL;

    splitter->header_count_idx = -1;
    svp = hv_fetchs(opt, "header_data_template", 0);
    if (svp && SvOK(*svp)) {
        splitter->header_sv = newSVsv(*svp);
        splitter->header_str = SvPVbyte(splitter->header_sv, splitter->header_len);

        svp = hv_fetchs(opt, "header_count_idx", 0);
        if (svp && SvOK(*svp)) {
            splitter->header_count_idx = SvIV(*svp);
            if (splitter->header_count_idx < 0
                || (STRLEN)splitter->header_count_idx + SRL_HEADER_COUNT_VARINT_LENGTH > splitter->header_len)
            {
                croak("header_count_idx is outside header_data_template");
            }
        }
    }
}

/* Open the input and position the parser on the first element of the top
 * level array, with one ST_VALUE per element on the status stack. */
SRL_STATIC_INLINE void
srl_splitter_open_input(pTHX_ srl_splitter_t *splitter, SV *input)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    SV *decompressed;
    STRLEN len;
    U8 tag;

    /* A copy of the input, so that the caller can't change it under our
     * feet; with copy-on-write this does not copy the string. */
    splitter->input_sv = newSVsv(input);
    buf->start = buf->pos = (srl_reader_char_ptr)SvPVbyte(splitter->input_sv, len);
    buf->end = buf->start + len;

    splitter->input_protocol_version = srl_reader_open_document(aTHX_ buf, &splitter->checksum, &decompressed);
    if (decompressed) {
        SvREFCNT_dec(splitter->input_sv);
        splitter->input_sv = SvREFCNT_inc(decompressed);
    }

    SRL_RDR_ASSERT_SPACE(buf, 1, " while reading the top level tag");
    tag = *buf->pos++ & ~SRL_HDR_TRACK_FLAG;
    if (tag >= SRL_HDR_ARRAYREF_LOW && tag <= SRL_HDR_ARRAYREF_HIGH) {
        splitter->input_nb_elts = tag & SRL_MASK_ARRAYREF_COUNT;
    } else if (tag == SRL_HDR_REFN) {
        SRL_RDR_ASSERT_SPACE(buf, 1, " while reading the top level tag");
        tag = *buf->pos++ & ~SRL_HDR_TRACK_FLAG;
        if (tag != SRL_HDR_ARRAY)
            croak("first tag is REFN but next tag is not ARRAY");
        splitter->input_nb_elts = srl_read_varint_uv_count(aTHX_ buf, " while reading ARRAY");
    } else {
        croak("first tag is not an ArrayRef");
    }
    SRL_SPLITTER_TRACE(" * top level array of %"UVuf" elements", splitter->input_nb_elts);
    srl_splitter_push_values(aTHX_ splitter, splitter->input_nb_elts);
}

void
srl_destroy_splitter(pTHX_ srl_splitter_t *splitter)
{
    /* may be called on a partially built splitter */
    SvREFCNT_dec(splitter->input_sv);
    SvREFCNT_dec(splitter->header_sv);
    SvREFCNT_dec((SV *)splitter->dedupe_binary);
    SvREFCNT_dec((SV *)splitter->dedupe_utf8);
    if (splitter->offset_tbl)
        PTABLE_free(splitter->offset_tbl);
    if (splitter->status_stack.begin)
        srl_stack_deinit(aTHX_ &splitter->status_stack);
    if (splitter->chunk.start)
        srl_buf_free_buffer(aTHX_ &splitter->chunk);
    srl_destroy_snappy_workmem(aTHX_ splitter->snappy_workmem);
    Safefree(splitter);
}

/* Returns a new reference to a Sereal::Splitter object. The object exists
 * from the start, so that a croak while building it frees everything. */
SV *
srl_build_splitter_object(pTHX_ const char *class_name, HV *opt)
{
    srl_splitter_t *splitter;
    SV **svp;
    SV *input;
    SV *obj = sv_newmortal();

    svp = hv_fetchs(opt, "input", 0);
    if (!svp || !SvOK(*svp))
        croak("no input given");
    input = *svp;

    Newxz(splitter, 1, srl_splitter_t);
    sv_setref_pv(obj, class_name, (void *)splitter);

    splitter->pbuf = &splitter->buf;
    srl_checksum_config_from_hv(aTHX_ &splitter->checksum, opt);
    srl_splitter_read_options(aTHX_ splitter, opt);

    if (expect_false(srl_stack_init(aTHX_ &splitter->status_stack, SRL_SPLITTER_STACK_PREALLOCATE) != 0))
        croak("Out of memory");
    if (expect_false(srl_buf_init_buffer(aTHX_ &splitter->chunk,
                                         (splitter->size_limit < SRL_SPLITTER_CHUNK_PREALLOCATE_MAX
                                          ? splitter->size_limit : SRL_SPLITTER_CHUNK_PREALLOCATE_MAX) + 64) != 0))
        croak("Out of memory");
    splitter->offset_tbl = PTABLE_new();
    splitter->dedupe_binary = newHV();
    splitter->dedupe_utf8 = newHV();

    srl_splitter_open_input(aTHX_ splitter, input);

    /* now buf.pos is on the first array element */
    return SvREFCNT_inc(obj);
}

/* parsing */

SRL_STATIC_INLINE int
srl_splitter_parse(pTHX_ srl_splitter_t *splitter)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    int force_tracking_tag = 0;

    while (!srl_stack_empty(&splitter->status_stack)) {
        UV status = srl_splitter_pop(aTHX_ splitter);

        SRL_SPLITTER_TRACE("* ITERATING -- deepness value: %"UVuf, splitter->deepness);
        switch (status) {
        case ST_DEEPNESS_UP:
            splitter->deepness--;
            break;
        case ST_TRACK_NEXT_VALUE:
            force_tracking_tag = 1;
            break;
        case ST_DONT_CHECK_FOR_DUPLICATE:
            splitter->dont_check_for_duplicate = 1;
            break;
        case ST_VALUE: {
            U8 tag;

            /* PAD is not a value: copy it along and look at what follows */
            SRL_RDR_ASSERT_SPACE(buf, 1, " while reading a tag");
            while (*buf->pos == SRL_HDR_PAD) {
                buf->pos++;
                SRL_RDR_ASSERT_SPACE(buf, 1, " while reading a tag");
            }
            tag = *buf->pos;

            if (force_tracking_tag) {
                force_tracking_tag = 0;
                /* We were asked to turn this tag into a tracked tag: copy
                 * the chunk until just before the tag, insert the tag with
                 * the tracking bit set, and carry on after the tag. */
                srl_splitter_flush(aTHX_ splitter, buf->pos, buf->pos + 1);
                tag |= SRL_HDR_TRACK_FLAG;
                srl_splitter_cat(aTHX_ splitter, &tag, 1);
            }
            if (tag & SRL_HDR_TRACK_FLAG) {
                const UV origin_offset = (UV)SRL_RDR_BODY_POS_OFS(buf);
                splitter->tag_is_tracked = 1;
                tag &= ~SRL_HDR_TRACK_FLAG;
                if (!PTABLE_fetch(splitter->offset_tbl, INT2PTR(void *, origin_offset))) {
                    const UV new_offset = srl_splitter_chunk_offset(splitter, buf->pos);
                    SRL_SPLITTER_TRACE("    * tracking %"UVuf" -> %"UVuf, origin_offset, new_offset);
                    PTABLE_store(splitter->offset_tbl, INT2PTR(void *, origin_offset), INT2PTR(void *, new_offset));
                }
            }
            buf->pos++;
            srl_splitter_read_tag(aTHX_ splitter, tag);
            splitter->dont_check_for_duplicate = 0;
            splitter->tag_is_tracked = 0;
            break;
        }
        case ST_ABSOLUTE_JUMP:
            /* before jumping back, copy what we have parsed */
            srl_splitter_flush(aTHX_ splitter, NULL, NULL);
            buf->pos = buf->start + srl_splitter_pop(aTHX_ splitter);
            splitter->chunk_iter_start = buf->pos;
            break;
        default:
            croak("unknown stack value %"UVuf, status);
        }

        if (splitter->deepness == 0) {
            /* we have parsed a full element of the top level array */
            splitter->chunk_nb_elts++;
            if (splitter->chunk_size + (UV)(buf->pos - splitter->chunk_iter_start) >= splitter->size_limit) {
                srl_splitter_flush(aTHX_ splitter, NULL, NULL);
                return 1;
            }
        }
    }

    if (splitter->deepness != 0)
        croak("Something wrong happens: parsing finished but deepness is not zero");

    /* finished: we have a chunk if there is anything left to copy, or if
     * something was copied already */
    return srl_splitter_flush(aTHX_ splitter, NULL, NULL) || splitter->chunk_size > 0;
}

SRL_STATIC_INLINE void
srl_splitter_read_tag(pTHX_ srl_splitter_t *splitter, U8 tag)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    const U8 *tag_pos = buf->pos - 1;
    UV len;

    /* strings first, they may be deduplicated */
    if (tag >= SRL_HDR_SHORT_BINARY_LOW) {
        len = tag & SRL_MASK_SHORT_BINARY_LEN;
        SRL_RDR_ASSERT_SPACE(buf, len, " while reading SHORT_BINARY");
        srl_splitter_read_string(aTHX_ splitter, tag_pos, len, 0);
        return;
    }
    if (tag == SRL_HDR_BINARY || tag == SRL_HDR_STR_UTF8) {
        len = srl_read_varint_uv_length(aTHX_ buf, " while reading BINARY or STR_UTF8");
        srl_splitter_read_string(aTHX_ splitter, tag_pos, len, tag == SRL_HDR_STR_UTF8);
        return;
    }

    /* the other self-contained values are copied as they are */
    if (srl_reader_skip_scalar(aTHX_ buf, tag, splitter->input_protocol_version, 1))
        return;

    if (tag >= SRL_HDR_HASHREF_LOW && tag <= SRL_HDR_HASHREF_HIGH) {
        srl_splitter_deeper(aTHX_ splitter);
        srl_splitter_push_values(aTHX_ splitter, 2 * (UV)(tag & SRL_MASK_HASHREF_COUNT));
        return;
    }
    if (tag >= SRL_HDR_ARRAYREF_LOW && tag <= SRL_HDR_ARRAYREF_HIGH) {
        srl_splitter_deeper(aTHX_ splitter);
        srl_splitter_push_values(aTHX_ splitter, tag & SRL_MASK_ARRAYREF_COUNT);
        return;
    }

    switch (tag) {
        case SRL_HDR_HASH:
            len = srl_read_varint_uv_count(aTHX_ buf, " while reading HASH");
            srl_splitter_deeper(aTHX_ splitter);
            srl_splitter_push_values(aTHX_ splitter, 2 * len);
            break;
        case SRL_HDR_ARRAY:
            len = srl_read_varint_uv_count(aTHX_ buf, " while reading ARRAY");
            srl_splitter_deeper(aTHX_ splitter);
            srl_splitter_push_values(aTHX_ splitter, len);
            break;
        case SRL_HDR_REFN:
        case SRL_HDR_WEAKEN:
            srl_splitter_deeper(aTHX_ splitter);
            srl_splitter_push(aTHX_ splitter, ST_VALUE);
            break;
        case SRL_HDR_OBJECT:
        case SRL_HDR_OBJECT_FREEZE:
        case SRL_HDR_REGEXP:
            /* class name and object, or pattern and modifiers */
            srl_splitter_deeper(aTHX_ splitter);
            srl_splitter_push(aTHX_ splitter, ST_VALUE);
            srl_splitter_push(aTHX_ splitter, ST_VALUE);
            break;
        case SRL_HDR_REFP:            srl_splitter_read_refp(aTHX_ splitter);        break;
        case SRL_HDR_OBJECTV:         srl_splitter_read_objectv(aTHX_ splitter, 0);  break;
        case SRL_HDR_OBJECTV_FREEZE:  srl_splitter_read_objectv(aTHX_ splitter, 1);  break;
        case SRL_HDR_ALIAS:           srl_splitter_read_alias(aTHX_ splitter);       break;
        case SRL_HDR_COPY:            srl_splitter_read_copy(aTHX_ splitter);        break;
        default:
            SRL_RDR_ERROR_UNIMPLEMENTED(buf, tag, SRL_TAG_NAME(tag));
    }
}

/* buf->pos is at the first byte of the string, len bytes long, whose tag
 * is at tag_pos. If the chunk already has the same string, replace this one
 * with a COPY of it; otherwise remember where it will be in the chunk. */
SRL_STATIC_INLINE void
srl_splitter_read_string(pTHX_ srl_splitter_t *splitter, const U8 *tag_pos, UV len, int is_utf8)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    HV *seen = is_utf8 ? splitter->dedupe_utf8 : splitter->dedupe_binary;
    SV **svp;

    if (splitter->dont_check_for_duplicate || len > I32_MAX) {
        buf->pos += len;
        return;
    }

    svp = hv_fetch(seen, (const char *)buf->pos, (I32)len, 0);
    if (svp) {
        SRL_SPLITTER_TRACE("   * found duplicate at %"UVuf, SvUV(*svp));
        srl_splitter_flush(aTHX_ splitter, tag_pos, buf->pos + len);
        srl_splitter_cat_varint(aTHX_ splitter,
                                splitter->tag_is_tracked ? SRL_HDR_COPY | SRL_HDR_TRACK_FLAG : SRL_HDR_COPY,
                                SvUV(*svp));
    } else {
        const UV offset = srl_splitter_chunk_offset(splitter, tag_pos);
        SRL_SPLITTER_TRACE("   * string at %"UVuf, offset);
        (void)hv_store(seen, (const char *)buf->pos, (I32)len, newSVuv(offset), 0);
    }
    buf->pos += len;
}

SRL_STATIC_INLINE void
srl_splitter_read_refp(pTHX_ srl_splitter_t *splitter)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    const U8 *tag_pos = buf->pos - 1;
    const UV offset = srl_read_varint_uv_offset(aTHX_ buf, " while reading REFP");
    void *new_offset;

    /* copy what precedes the REFP, which is replaced */
    srl_splitter_flush(aTHX_ splitter, tag_pos, NULL);

    new_offset = PTABLE_fetch(splitter->offset_tbl, INT2PTR(void *, offset));
    if (new_offset) {
        /* the referent is in this chunk: point at it */
        srl_splitter_cat_varint(aTHX_ splitter,
                                splitter->tag_is_tracked ? SRL_HDR_REFP | SRL_HDR_TRACK_FLAG : SRL_HDR_REFP,
                                PTR2UV(new_offset));
    } else {
        /* otherwise copy the referent here, behind a REFN */
        U8 refn = splitter->tag_is_tracked ? SRL_HDR_REFN | SRL_HDR_TRACK_FLAG : SRL_HDR_REFN;
        srl_splitter_cat(aTHX_ splitter, &refn, 1);
        srl_splitter_deeper(aTHX_ splitter);
        srl_splitter_jump(aTHX_ splitter, offset);
    }
}

SRL_STATIC_INLINE void
srl_splitter_read_objectv(pTHX_ srl_splitter_t *splitter, int is_freeze)
{
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    const U8 *tag_pos = buf->pos - 1;
    const UV offset = srl_read_varint_uv_offset(aTHX_ buf, " while reading OBJECTV");
    U8 tag = is_freeze ? SRL_HDR_OBJECT_FREEZE : SRL_HDR_OBJECT;

    /* Replace the OBJECTV with an OBJECT and a copy of the class name;
     * string deduplication may turn the copy back into a COPY. */
    srl_splitter_flush(aTHX_ splitter, tag_pos, NULL);
    if (splitter->tag_is_tracked)
        tag |= SRL_HDR_TRACK_FLAG;
    srl_splitter_cat(aTHX_ splitter, &tag, 1);

    /* the object comes after the class name, and the stack pops in reverse */
    srl_splitter_deeper(aTHX_ splitter);
    srl_splitter_push(aTHX_ splitter, ST_VALUE);
    srl_splitter_jump(aTHX_ splitter, offset);
}

SRL_STATIC_INLINE void
srl_splitter_read_copy(pTHX_ srl_splitter_t *splitter)
{
    /* Copy the value at the offset in place of the COPY tag. That is simpler
     * than making sure the target is in the current chunk, and string
     * deduplication removes the duplication again. */
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    const U8 *tag_pos = buf->pos - 1;
    const UV offset = srl_read_varint_uv_offset(aTHX_ buf, " while reading COPY");

    srl_splitter_flush(aTHX_ splitter, tag_pos, NULL);
    srl_splitter_deeper(aTHX_ splitter);
    srl_splitter_jump(aTHX_ splitter, offset);
    if (splitter->tag_is_tracked) {
        /* The COPY tag had its tracking bit set, so whatever it points at
         * becomes a tracked value here, and must not itself be turned into
         * a COPY. The stack pops in reverse order. */
        srl_splitter_push(aTHX_ splitter, ST_TRACK_NEXT_VALUE);
        srl_splitter_push(aTHX_ splitter, ST_DONT_CHECK_FOR_DUPLICATE);
    }
}

SRL_STATIC_INLINE void
srl_splitter_read_alias(pTHX_ srl_splitter_t *splitter)
{
    /* an ALIAS tag cannot be tracked itself */
    srl_reader_buffer_t *buf = SRL_SPLITTER_BUF(splitter);
    const U8 *tag_pos = buf->pos - 1;
    const UV offset = srl_read_varint_uv_offset(aTHX_ buf, " while reading ALIAS");
    void *new_offset;

    srl_splitter_flush(aTHX_ splitter, tag_pos, NULL);

    new_offset = PTABLE_fetch(splitter->offset_tbl, INT2PTR(void *, offset));
    if (new_offset) {
        srl_splitter_cat_varint(aTHX_ splitter, SRL_HDR_ALIAS, PTR2UV(new_offset));
    } else {
        /* otherwise duplicate the value here */
        srl_splitter_deeper(aTHX_ splitter);
        srl_splitter_jump(aTHX_ splitter, offset);
    }
}

/* chunks */

SV *
srl_splitter_next_chunk(pTHX_ srl_splitter_t *splitter)
{
    srl_buffer_t *chunk = &splitter->chunk;
    STRLEN header_len, count_ofs, count_len;
    U8 version = splitter->input_protocol_version;

    /* Chunk bodies are copied from the input, so a chunk has the protocol
     * version of the input; at least 3 for the magic string we write, and 4
     * for zstd compression. */
    if (version < 3)
        version = 3;
    if (version < 4 && splitter->compress_flags == SRL_F_COMPRESS_ZSTD)
        version = 4;

    /* tables are per chunk */
    PTABLE_clear(splitter->offset_tbl);
    hv_clear(splitter->dedupe_binary);
    hv_clear(splitter->dedupe_utf8);

    chunk->pos = chunk->start;
    splitter->chunk_size = 0;
    splitter->chunk_nb_elts = 0;
    splitter->chunk_iter_start = splitter->buf.pos;

    srl_buf_cat_str_s(chunk, SRL_MAGIC_STRING_HIGHBIT);
    srl_buf_cat_char(chunk, (char)version);
    if (splitter->header_len)
        srl_buf_cat_str(chunk, splitter->header_str, splitter->header_len);
    else
        srl_buf_cat_char(chunk, '\0');
    header_len = BUF_POS_OFS(chunk);
    SRL_UPDATE_BODY_POS(chunk, version);

    /* the element count is written as the largest possible count, and
     * shrunk to the real one once we know it */
    srl_buf_cat_char(chunk, SRL_HDR_REFN);
    srl_buf_cat_char(chunk, SRL_HDR_ARRAY);
    count_ofs = BUF_POS_OFS(chunk);
    srl_buf_cat_varint(aTHX_ chunk, 0, splitter->input_nb_elts);
    count_len = BUF_POS_OFS(chunk) - count_ofs;

    if (!srl_splitter_parse(aTHX_ splitter))
        return newSV(0);

    srl_update_varint_from_to(aTHX_ chunk->start + count_ofs,
                              chunk->start + count_ofs + count_len - 1,
                              splitter->chunk_nb_elts);

    if (splitter->header_count_idx != -1) {
        U8 *start = chunk->start + SRL_MAGIC_STRLEN + 1 + splitter->header_count_idx;
        srl_update_varint_from_to(aTHX_ start, start + SRL_HEADER_COUNT_VARINT_LENGTH - 1,
                                  splitter->chunk_nb_elts);
    }

    if (splitter->compress_flags)
        srl_compress_body(aTHX_ chunk, header_len, splitter->compress_flags,
                          splitter->compress_level, &splitter->snappy_workmem);

    return newSVpvn((const char *)chunk->start, BUF_POS_OFS(chunk));
}
