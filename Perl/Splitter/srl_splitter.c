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

#include "srl_protocol.h"

/*#define SRL_SPLITTER_TRACE(msg, args...) warn((msg), args) */
#define SRL_SPLITTER_TRACE(msg, args...)

#include "srl_splitter.h"
#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_inline.h"

#include "snappy/csnappy_decompress.c"
#include "miniz.h"

#include "uthash.h"

#define STACK_SIZE_INCR 1024

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)


#define SRL_SPLITTER_SET_OPTION(splitter, flag_num) ((splitter)->flags |= (flag_num))
#define SRL_SPLITTER_HAVE_OPTION(splitter, flag_num) ((splitter)->flags & (flag_num))


#define SRL_MAX_VARINT_LENGTH 11


/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE srl_splitter_t * srl_empty_splitter_struct(pTHX);
SRL_STATIC_INLINE void _parse_header(pTHX_ srl_splitter_t *splitter);
SRL_STATIC_INLINE UV _read_varint_uv_nocheck(srl_splitter_t *splitter);
SRL_STATIC_INLINE int _parse(pTHX_ srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_tag(pTHX_ srl_splitter_t * splitter, char tag);
SRL_STATIC_INLINE void _read_varint(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_zigzag(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_float(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_double(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_long_double(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_string(pTHX_ srl_splitter_t * splitter, bool is_utf8);
SRL_STATIC_INLINE void _read_weaken(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_refn(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_refp(pTHX_ srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_object(srl_splitter_t * splitter, bool is_freeze);
SRL_STATIC_INLINE void _read_objectv(pTHX_ srl_splitter_t * splitter, bool is_freeze);
SRL_STATIC_INLINE void _read_copy(pTHX_ srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_alias(pTHX_ srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_hash(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_array(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_regexp(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _update_varint_from_to(char *varint_start, char *varint_end, UV number);
SRL_STATIC_INLINE char* _set_varint_nocheck(char* buf, UV n);
SRL_STATIC_INLINE bool _maybe_flush_chunk (pTHX_ srl_splitter_t *splitter, char* end_pos, char* next_start_pos);
SRL_STATIC_INLINE void _empty_hashes(pTHX);
SRL_STATIC_INLINE void _check_for_duplicates(pTHX_ srl_splitter_t * splitter, char* binary_start_pos, UV len, bool is_utf8);
SRL_STATIC_INLINE void _cat_to_chunk(pTHX_ srl_splitter_t *splitter, char* str, UV str_len);
SRL_STATIC_INLINE UV stack_pop(srl_splitter_stack_t * stack);
SRL_STATIC_INLINE bool stack_is_empty(srl_splitter_stack_t * stack);
SRL_STATIC_INLINE void stack_push(srl_splitter_stack_t * stack, UV val);


SRL_STATIC_INLINE void _cat_to_chunk(pTHX_ srl_splitter_t *splitter, char* str, UV str_len) {
        sv_catpvn(splitter->chunk, str, str_len);
        splitter->chunk_current_offset += str_len;
        splitter->chunk_size += str_len;
}

typedef struct {
    char* key;         /* the string to dedup */
    UV value;          /* the position in the chunk */
    UT_hash_handle hh; /* makes this structure hashable */
} dedupe_el_t;

dedupe_el_t *dedupe_hashtable = NULL; /* important! initialize to NULL */
dedupe_el_t *dedupe_hashtable_utf8 = NULL; /* important! initialize to NULL */

typedef struct {
    UV key;            /* key, the original offset in input */
    UV value;          /* value, the new offset in the chunk */
    UT_hash_handle hh; /* makes this structure hashable */
} offset_el_t;

offset_el_t *offset_hashtable = NULL;    /* important! initialize to NULL */

SRL_STATIC_INLINE UV stack_pop(srl_splitter_stack_t * stack) {
    UV val = 0;
    if ( stack->top <= 0 )
        croak("Stack is empty");
    val = stack->data[stack->top-1];
    stack->top--;
    return val;
}

SRL_STATIC_INLINE bool stack_is_empty(srl_splitter_stack_t * stack) {
    return (stack->top == 0);
}

SRL_STATIC_INLINE void stack_push(srl_splitter_stack_t * stack, UV val) {
    if (stack->top >= stack->size) {
        UV new_size = stack->size + STACK_SIZE_INCR;
        UV* tmp;
        dTHX;
        Newxz(tmp, new_size, UV );
        memcpy(tmp, stack->data, stack->size * sizeof(UV));
        Safefree(stack->data);
        stack->data = tmp;
        stack->size = new_size;
    }
    stack->data[stack->top++] = val;
}

srl_splitter_t * srl_build_splitter_struct(pTHX_ HV *opt) {
    srl_splitter_t *splitter;
    SV **svp;
    STRLEN input_len;

    splitter = srl_empty_splitter_struct(aTHX);

    splitter->dont_check_for_duplicate = 0;
    splitter->tag_is_tracked = 0;

    /* load options */
    svp = hv_fetchs(opt, "input", 0);
    if (svp && SvOK(*svp)) {
        splitter->input_str = SvPV(*svp, input_len);
        splitter->pos = splitter->input_str;
        splitter->input_len = input_len;
        splitter->input_str_end = splitter->input_str + input_len;
        splitter->input_sv = SvREFCNT_inc(*svp);
        SRL_SPLITTER_TRACE("input_size %" UVuf, input_len);
    } else {
        croak ("no input given");
    }
    svp = hv_fetchs(opt, "chunk_size", 0);
    if (svp && SvOK(*svp)) {
        splitter->size_limit = SvUV(*svp);
        SRL_SPLITTER_TRACE("size_limit %" UVuf, splitter->size_limit);
    }

    splitter->compression_format = 0;
    svp = hv_fetchs(opt, "compress", 0);
    if (svp && SvOK(*svp)) {
        IV compression_format = SvIV(*svp);

        /* See also Splitter.pm's constants */
        switch (compression_format) {
        case 0:
            SRL_SPLITTER_TRACE("no compression %s", "");
            break;
        case 1:
            croak("incremental snappy compression not yet supported. Try gzip");
            break;
        case 2:
            splitter->compression_format = 2;
            SRL_SPLITTER_TRACE("gzip compression %s", "");
            break;
        default:
            croak("invalid valie for 'compress' parameter");
        }
    }

    splitter->header_str = NULL;
    splitter->header_sv = NULL;
    splitter->header_len = 0;
    svp = hv_fetchs(opt, "header_data_template", 0);
    if (svp && SvOK(*svp)) {
        STRLEN header_len;
        splitter->header_str = SvPV(*svp, header_len);
        splitter->header_sv = SvREFCNT_inc(*svp);
        splitter->header_len = header_len;
        SRL_SPLITTER_TRACE("header_data_template found, of length %lu", header_len);
    }

    splitter->header_count_idx = -1;
    svp = hv_fetchs(opt, "header_count_idx", 0);
    if (svp && SvOK(*svp)) {
        splitter->header_count_idx = SvIV(*svp);
        SRL_SPLITTER_TRACE("header_count_idx found, %ld", splitter->header_count_idx);
    }

    _parse_header(aTHX_ splitter);

    /* initialize stacks */
    srl_splitter_stack_t * status_stack;
    Newxz(status_stack, 1, srl_splitter_stack_t );
    Newxz(status_stack->data, STACK_SIZE_INCR, UV );
    status_stack->size = STACK_SIZE_INCR;
    status_stack->top = 0;
    splitter->status_stack = status_stack;

    /* initialize */
    splitter->deepness = 0;

    char tag = *(splitter->pos);
    splitter->pos++;
    if (IS_SRL_HDR_ARRAYREF(tag)) {
        int len = tag & 0xF;
        splitter->input_nb_elts = len;
        SRL_SPLITTER_TRACE(" * ARRAYREF of len, %d", len);
        while (len-- > 0) {
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else if (tag == SRL_HDR_REFN) {
        tag = *(splitter->pos);
        splitter->pos++;
        if (tag == SRL_HDR_ARRAY) {
            UV len = _read_varint_uv_nocheck(splitter);
            splitter->input_nb_elts = len;
            SRL_SPLITTER_TRACE(" * ARRAY of len, %lu", len);
            while (len-- > 0) {
                stack_push(splitter->status_stack, ST_VALUE);
            }
        } else {
            croak("first tag is REFN but next tag is not ARRAY");
        }
    } else {
        croak("first tag is not an ArrayRef");
    }

    /* now splitter->pos is on the first array element */
    return splitter;
}


void srl_destroy_splitter(pTHX_ srl_splitter_t *splitter) {
    _empty_hashes(aTHX);
    SvREFCNT_dec(splitter->input_sv);
    if (splitter->header_sv != NULL)
        SvREFCNT_dec(splitter->header_sv);

    if (splitter->status_stack) {
        if (splitter->status_stack->data != NULL)
            Safefree(splitter->status_stack->data);
        Safefree(splitter->status_stack);
    }

    Safefree(splitter);
}

SRL_STATIC_INLINE srl_splitter_t * srl_empty_splitter_struct(pTHX) {
    srl_splitter_t *splitter = NULL;
    Newx(splitter, 1, srl_splitter_t);
    if (splitter == NULL) {
        croak("Out of memory");
    }
    return splitter;
}

void _parse_header(pTHX_ srl_splitter_t *splitter) {
    int magic_string = 1;
    int high_magic_string = 1;

    U8 version_encoding;
    U8 version;
    U8 encoding_flags;
    UV header_len;

    int is_zlib_encoded = 0;
    int is_snappy_encoded = 0;
    int is_snappyincr_encoded = 0;

    /* SRL_MAGIC_STRLEN + PROTOCOL_LENGTH + OPTIONAL-HEADER-SIZE(at least 1 byte) + DATA(at least 1 byte) */
    if (splitter->input_len < SRL_MAGIC_STRLEN + 1 + 1 + 1){
        croak("input Sereal string lacks data");
    } else if ( (high_magic_string = strncmp(splitter->input_str, SRL_MAGIC_STRING, SRL_MAGIC_STRLEN))
                  && (magic_string = strncmp(splitter->input_str, SRL_MAGIC_STRING_HIGHBIT, SRL_MAGIC_STRLEN)) ) {
        croak("input Sereal string has wrong Sereal magic");
    }

    splitter->pos += SRL_MAGIC_STRLEN;

    version_encoding = (U8)*(splitter->pos);
    version = (U8)(version_encoding & SRL_PROTOCOL_VERSION_MASK);
    encoding_flags = (U8)(version_encoding & SRL_PROTOCOL_ENCODING_MASK);

    if (      version <= 0
              || ( version < 3 && high_magic_string )
              || ( version > 2 && magic_string ) ) {
        croak("unsupported Sereal versions/protocol");
    }

    switch(encoding_flags) {

   case SRL_PROTOCOL_ENCODING_RAW:
        /* no op */
        SRL_SPLITTER_TRACE("encoding is raw %s", "");
        break;

    case SRL_PROTOCOL_ENCODING_SNAPPY:
        SRL_SPLITTER_TRACE("encoding is snappy %s", "");
        is_snappy_encoded = 1;
        break;

    case SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL:
        SRL_SPLITTER_TRACE("encoding is snappy incr %s", "");
        is_snappy_encoded = is_snappyincr_encoded = 1;
        break;

    case SRL_PROTOCOL_ENCODING_ZLIB:
        SRL_SPLITTER_TRACE("encoding is zlib %s", "");
        is_zlib_encoded = 1;
        break;

    default:
        croak("Sereal document encoded in an unknown format");
    }

    SRL_SPLITTER_TRACE("header version is %hhu", version);

    /* move after protocol version */
    splitter->pos += 1;
    
    header_len= _read_varint_uv_nocheck(splitter);

    SRL_SPLITTER_TRACE("header len is %lu", header_len);

    /*TODO: add code for processing the header */
    splitter->pos += header_len;

    if (version < 2) {
        splitter->input_body_pos = splitter->input_str;
    } else {
        splitter->input_body_pos = splitter->pos;
    }

    if (is_snappy_encoded) {
        UV compressed_len;
        uint32_t uncompressed_len;
        int decompress_ok;
        char * new_input_str;

        if (is_snappyincr_encoded) {
            compressed_len = _read_varint_uv_nocheck(splitter);
        } else {
            compressed_len = splitter->input_len - (splitter->pos - splitter->input_str);
        }
        SRL_SPLITTER_TRACE("snappy compressed len %"UVuf, compressed_len);
        /* splitter->pos is now at start of compressed payload */

        int snappy_header_len;
        char *old_pos;
        old_pos = splitter->pos;
        snappy_header_len = csnappy_get_uncompressed_length(
            (char *)old_pos,
            compressed_len,
            &uncompressed_len
        );
        if (snappy_header_len == CSNAPPY_E_HEADER_BAD) {
            croak("invalid Snappy header in Snappy-compressed Sereal packet");
        }

        /* allocate a new SV for uncompressed data */
        SvREFCNT_dec(splitter->input_sv);
        splitter->input_sv = newSVpvs("");
        new_input_str = SvGROW(splitter->input_sv, uncompressed_len);

        decompress_ok = csnappy_decompress_noheader((char *) (old_pos + snappy_header_len),
                                                    compressed_len - snappy_header_len,
                                                    (char *) new_input_str,
                                                    &uncompressed_len);
        if ( decompress_ok != 0 ) {
            croak("Snappy decompression of Sereal packet payload failed");
        }

        splitter->input_str = new_input_str;
        SRL_SPLITTER_TRACE(" decompress OK: uncompressed length: %d\n", uncompressed_len);

        splitter->pos = splitter->input_str;;
        splitter->input_len = uncompressed_len;
        splitter->input_body_pos = splitter->pos;

    } else if (is_zlib_encoded) {

        UV uncompressed_len = _read_varint_uv_nocheck(splitter);
        UV compressed_len = _read_varint_uv_nocheck(splitter);
        char * new_input_str;

        /* splitter->pos is now at start of compressed payload */
        SRL_SPLITTER_TRACE("unzipping %s", "");
        SRL_SPLITTER_TRACE("compressed_len : %" UVuf, compressed_len);
        SRL_SPLITTER_TRACE("uncompressed_len : %" UVuf, uncompressed_len);

                 
        mz_ulong tmp = uncompressed_len;

        /* allocate a new SV for uncompressed data */
        SvREFCNT_dec(splitter->input_sv);
        splitter->input_sv = newSVpvs("");
        new_input_str = SvGROW(splitter->input_sv, uncompressed_len);

        char *compressed = splitter->pos;

        int decompress_ok = mz_uncompress( (unsigned char *) new_input_str,
                                           &tmp,
                                           (const unsigned char *) compressed,
                                           compressed_len );

        if (decompress_ok != Z_OK)
            croak("ZLIB decompression of Sereal packet payload failed");

        splitter->input_str = new_input_str;
        SRL_SPLITTER_TRACE(" decompress OK: length %lu\n", uncompressed_len);

        splitter->pos = splitter->input_str;
        splitter->input_len = (STRLEN)tmp;
        splitter->input_body_pos = splitter->pos;

    }
}

SRL_STATIC_INLINE int _parse(pTHX_ srl_splitter_t * splitter) {

    char tag;

    bool force_tracking_tag = 0;

    while( ! stack_is_empty(splitter->status_stack) ) {
        UV status = stack_pop(splitter->status_stack);
        UV absolute_offset;

        SRL_SPLITTER_TRACE("* ITERATING -- deepness value: %d", splitter->deepness);
        switch(status) {
        case ST_DEEPNESS_UP:
            splitter->deepness--;
            SRL_SPLITTER_TRACE(" * DEEPNESS UP -- deepness value: %d", splitter->deepness);
            break;
        case ST_TRACK_NEXT_VALUE:
	    force_tracking_tag = 1;
	    break;
        case ST_DONT_CHECK_FOR_DUPLICATE:
	    splitter->dont_check_for_duplicate = 1;
	    break;
        case ST_VALUE:
            tag = *(splitter->pos);
            SRL_SPLITTER_TRACE(" * VALUE tag %d -- deepness value: %d", tag, splitter->deepness);
	    if (force_tracking_tag) {
		force_tracking_tag = 0;
		/* we were asked to turn this tag into a tracked tag. First
		   save chunk until the current position, then insert the tag
		   with the traking bit set, then carry on after the tag. */
		/* This will flush the chunk until just before the tag ( first
		   pos is non inclusive), and continue the chunk after the tag
		   (hence the splitter->pos + 1) */
		_maybe_flush_chunk(aTHX_ splitter, splitter->pos, splitter->pos + 1);
		/* Here we insert the tag with the tracking bit set, in place of the original tag */
		tag = tag | SRL_HDR_TRACK_FLAG;
		_cat_to_chunk(aTHX_ splitter, &tag, 1 );
	    }
            if (tag & SRL_HDR_TRACK_FLAG) {
		splitter->tag_is_tracked = 1;
                tag = tag & ~SRL_HDR_TRACK_FLAG;
                SRL_SPLITTER_TRACE("    * tag must be tracked, %ld\n", splitter->pos - splitter->input_body_pos);

                offset_el_t *element = NULL;

                UV origin_offset = splitter->pos - splitter->input_body_pos + 1;
                UV new_offset    = splitter->chunk_current_offset + (splitter->pos - splitter->chunk_iter_start);

                HASH_FIND(hh, offset_hashtable, &origin_offset, sizeof(UV), element);

                if(element == NULL) {
                    Newx(element, 1, offset_el_t);
                    element->key = origin_offset;
                    element->value = new_offset;
                    SRL_SPLITTER_TRACE("    * adding %lu -> %lu\n", element->key, element->value);
                    HASH_ADD_KEYPTR(hh, offset_hashtable, &(element->key), sizeof(UV), element);
                }
            }
	    /* move after the tag */
            splitter->pos++;
            _read_tag(aTHX_ splitter, tag);
	    splitter->dont_check_for_duplicate = 0;
	    splitter->tag_is_tracked = 0;
            break;
        case ST_ABSOLUTE_JUMP:
            /* before jumping, flush the chunk */
            _maybe_flush_chunk(aTHX_ splitter, NULL, NULL);
            absolute_offset = stack_pop(splitter->status_stack);
            SRL_SPLITTER_TRACE("  * ABSOLUTE_JUMP to %lu", (UV) ( (char*)absolute_offset - splitter->input_str ) );
            splitter->pos = (char*) absolute_offset;
            splitter->chunk_iter_start = splitter->pos;
            break;
        default:
            croak("unknown stack value %lu", status);
        }
        if ( splitter->deepness == 0) {
            /* Here it means we have properly parsed a full VALUE, so we have
               an additional array element in our chunk */
            splitter->chunk_nb_elts++;
            if ( (UV)(splitter->chunk_size +
                      splitter->pos - splitter->chunk_iter_start) >= splitter->size_limit) {
                _maybe_flush_chunk(aTHX_ splitter, NULL, NULL);
                return 1;
            }
        }
    }
    SRL_SPLITTER_TRACE("* END ITERATING (deepness value: %d)", splitter->deepness);
    if (splitter->deepness != 0)
        croak("Something wrong happens: parsing finished but deepness is not zero");

    /* iteration is finished, if we had to flush something return success */
    if (_maybe_flush_chunk(aTHX_ splitter, NULL, NULL))
        return 1;

    /* maybe we didn't have to flush but the chunk is not empty: return success */
    if (splitter->chunk_size > 0)
        return 1;

    /* otherwise, no data anymore, return failure */
    return 0;
}

void _check_for_duplicates(pTHX_ srl_splitter_t * splitter, char* binary_start_pos, UV len, bool is_utf8) {
    dedupe_el_t *element = NULL;
    if (splitter->dont_check_for_duplicate) {
	splitter->pos += len;
	return;
    }
    if (is_utf8) {
        HASH_FIND(hh, dedupe_hashtable_utf8, splitter->pos, len, element);
    } else {
        HASH_FIND(hh, dedupe_hashtable, splitter->pos, len, element);
    }
    if ( element != NULL) {
        SRL_SPLITTER_TRACE("   * FOUND DEDUP key %s value %lu", element->key, element->value);
        _maybe_flush_chunk(aTHX_ splitter, binary_start_pos, splitter->pos + len);

        /* the copy tag */
        char tmp[SRL_MAX_VARINT_LENGTH];
        tmp[0] = ( splitter->tag_is_tracked ? SRL_HDR_COPY | SRL_HDR_TRACK_FLAG : SRL_HDR_COPY );
        _cat_to_chunk(aTHX_ splitter, tmp, 1 );

        UV len = (UV) (_set_varint_nocheck(tmp, element->value) - tmp);
        _cat_to_chunk(aTHX_ splitter, tmp, len);

    } else {
        UV offset = splitter->chunk_current_offset + ( binary_start_pos - splitter->chunk_iter_start);
        Newx(element, 1, dedupe_el_t);
        element->key = "";
        element->value = offset;
        SRL_SPLITTER_TRACE("   * ADDED DEDUP offset %lu", offset);
        if (is_utf8) {
            HASH_ADD_KEYPTR(hh, dedupe_hashtable_utf8, splitter->pos, len, element);
        } else {
            HASH_ADD_KEYPTR(hh, dedupe_hashtable, splitter->pos, len, element);
        }
    }

    splitter->pos += len;
    return;
}

void
_read_tag(pTHX_ srl_splitter_t * splitter, char tag)
{
    /* first, self-contained tags*/
    if ( tag <= SRL_HDR_POS_HIGH ) {
        SRL_SPLITTER_TRACE(" * POS INTEGER %d", (int)tag);
    } else if ( tag <= SRL_HDR_NEG_HIGH) {
        SRL_SPLITTER_TRACE(" * NEG INTEGER %d", (int)tag - 32);
    } else if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {
        UV len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        SRL_SPLITTER_TRACE(" * SHORT BINARY of length %lu", len);
        char *binary_start_pos = splitter->pos - 1;
        _check_for_duplicates(aTHX_ splitter, binary_start_pos, len, 0);
    } else if ( IS_SRL_HDR_HASHREF(tag) ) {
        int len = tag & 0xF;
        SRL_SPLITTER_TRACE(" * SHORT HASHREF of length %d", len);
        splitter->deepness++;
        stack_push(splitter->status_stack, ST_DEEPNESS_UP);
        while (len-- > 0) {
            stack_push(splitter->status_stack, ST_VALUE);
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else if ( IS_SRL_HDR_ARRAYREF(tag) ) {
        int len = tag & 0xF;
        SRL_SPLITTER_TRACE(" * SHORT ARRAY of length %d", len);
        splitter->deepness++;
        stack_push(splitter->status_stack, ST_DEEPNESS_UP);
        while (len-- > 0) {
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else {
        switch (tag) {
            case SRL_HDR_VARINT:         _read_varint(splitter);       break;
            case SRL_HDR_ZIGZAG:         _read_zigzag(splitter);       break;
            case SRL_HDR_FLOAT:          _read_float(splitter);        break;
            case SRL_HDR_DOUBLE:         _read_double(splitter);       break;
            case SRL_HDR_LONG_DOUBLE:    _read_long_double(splitter);  break;
            case SRL_HDR_TRUE:           /* no op */                      break;
            case SRL_HDR_FALSE:          /* no op */                      break;
            case SRL_HDR_CANONICAL_UNDEF:
            case SRL_HDR_UNDEF:          /* no op */                      break;
            case SRL_HDR_BINARY:         _read_string(aTHX_ splitter, 0);    break;
            case SRL_HDR_STR_UTF8:       _read_string(aTHX_ splitter, 1);    break;
            case SRL_HDR_WEAKEN:         _read_weaken(splitter);       break;
            case SRL_HDR_REFN:           _read_refn(splitter);         break;
            case SRL_HDR_REFP:           _read_refp(aTHX_ splitter);         break;
            case SRL_HDR_HASH:           _read_hash(splitter);         break;
            case SRL_HDR_ARRAY:          _read_array(splitter);        break;
            case SRL_HDR_OBJECT:         _read_object(splitter, 0);    break;
            case SRL_HDR_OBJECT_FREEZE:  _read_object(splitter, 1);    break;
            case SRL_HDR_OBJECTV:        _read_objectv(aTHX_ splitter, 0);   break;
            case SRL_HDR_OBJECTV_FREEZE: _read_objectv(aTHX_ splitter, 1);   break;
            case SRL_HDR_ALIAS:          _read_alias(aTHX_ splitter);        break;
            case SRL_HDR_COPY:           _read_copy(aTHX_ splitter);         break;
            case SRL_HDR_EXTEND:         /* no op */                      break;
            case SRL_HDR_REGEXP:         _read_regexp(splitter);       break;
            case SRL_HDR_PAD:            /* no op */                      break;
            default:                     croak("Unexpected tag value");   break;
        }
    }
}


SRL_STATIC_INLINE void _read_varint(srl_splitter_t * splitter) {
    _read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * VARINT %s", "");
}

SRL_STATIC_INLINE void _read_zigzag(srl_splitter_t * splitter) {
    _read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * ZIGZAG %s", "");
}

SRL_STATIC_INLINE void _read_float(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * FLOAT %s", "");
    splitter->pos += sizeof(float);
}

SRL_STATIC_INLINE void _read_double(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * DOUBLE %s", "");
    splitter->pos += sizeof(double);
}

SRL_STATIC_INLINE void _read_long_double(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * LONG DOUBLE %s", "");
    splitter->pos += sizeof(long double);
}

SRL_STATIC_INLINE void _read_string(pTHX_ srl_splitter_t * splitter, bool is_utf8) {
    char *binary_start_pos = splitter->pos - 1;
    UV len = _read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * STRING of length %lu", len);
    _check_for_duplicates(aTHX_ splitter, binary_start_pos, len, is_utf8);
}

SRL_STATIC_INLINE void _read_weaken(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * WEAKEN, %s", "");
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE);
}

SRL_STATIC_INLINE void _read_refn(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * REFN, %s", "");
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE);
}

SRL_STATIC_INLINE void _read_refp(pTHX_ srl_splitter_t * splitter) {
    /* we save the position at the refp tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = _read_varint_uv_nocheck(splitter);

    if (offset == 0)
        croak("REFP offset is zero !");

    SRL_SPLITTER_TRACE(" * REFP, must jump to offset %lu, from input_body_pos %lu, then back here %lu.",
                       offset,
                       splitter->input_body_pos - splitter->input_str,
                       splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the REFP tag */
    _maybe_flush_chunk(aTHX_ splitter, saved_pos, NULL);

    /* search in the mapping hash */
    offset_el_t *element = NULL;
    HASH_FIND(hh, offset_hashtable, &offset, sizeof(UV), element);
    if (element != NULL) {
        UV new_offset = element->value;
        /* insert a refp */
        char tmp_str[SRL_MAX_VARINT_LENGTH];
        tmp_str[0] = ( splitter->tag_is_tracked ? SRL_HDR_REFP | SRL_HDR_TRACK_FLAG : SRL_HDR_REFP );
        _cat_to_chunk(aTHX_ splitter, tmp_str, 1 );
        
        /* append the offset as a varint */
        UV varint_len = (UV) (_set_varint_nocheck(tmp_str, new_offset) - tmp_str);
        _cat_to_chunk(aTHX_ splitter, tmp_str, varint_len);
    } else {

        /* otherwise insert an refn tag instead of the refp, and copy the pointed content */
        char tmp_str[1];
        tmp_str[0] = ( splitter->tag_is_tracked ? SRL_HDR_REFN | SRL_HDR_TRACK_FLAG : SRL_HDR_REFN );
        _cat_to_chunk(aTHX_ splitter, tmp_str, 1 );

        splitter->deepness++;
        stack_push(splitter->status_stack, ST_DEEPNESS_UP);

        stack_push(splitter->status_stack, (UV)splitter->pos);
        stack_push(splitter->status_stack, ST_ABSOLUTE_JUMP);

        stack_push(splitter->status_stack, ST_VALUE);

        /* then do the jump */
        char * landing_pos = splitter->input_body_pos + offset - 1;
        splitter->pos = landing_pos;
        splitter->chunk_iter_start = landing_pos;
    }
}

SRL_STATIC_INLINE void _read_object(srl_splitter_t * splitter, bool is_freeze) {
    SRL_SPLITTER_TRACE(" * OBJECT%s, %s", (is_freeze ? "FREEZE" : ""), "");
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE); /* for the class name */
    stack_push(splitter->status_stack, ST_VALUE); /* for the object struct */
}

SRL_STATIC_INLINE void _read_objectv(pTHX_ srl_splitter_t * splitter, bool is_freeze) {
    /* we save the position at the objectv tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = _read_varint_uv_nocheck(splitter);
    if (offset == 0)
        croak("OBJECTV offset is zero !");

    SRL_SPLITTER_TRACE(" * OBJECTV%s, jump to offset %lu, from input_body_pos %lu, then back here %lu.",
            (is_freeze ? "FREEZE" : ""), offset,
            splitter->input_body_pos - splitter->input_str,
            splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the OBJECTV tag */
    _maybe_flush_chunk(aTHX_ splitter, saved_pos, NULL);

    /* insert an object tag instead of the objectv tag. The check_for_duplicate
     * will hopefully remove duplication */
    char tmp_str[1];
    tmp_str[0] = (is_freeze ? SRL_HDR_OBJECT_FREEZE : SRL_HDR_OBJECT);
    if (splitter->tag_is_tracked) {
	tmp_str[0] = (tmp_str[0] | SRL_HDR_TRACK_FLAG);
    }
    _cat_to_chunk(aTHX_ splitter, tmp_str, 1 );

    /* set the instructions in the stack. Warning, we are pushing, so the order
       will be reversed when we pop */
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);

    stack_push(splitter->status_stack, ST_VALUE); /* for the object item-tag */

    stack_push(splitter->status_stack, (UV)splitter->pos);
    stack_push(splitter->status_stack, ST_ABSOLUTE_JUMP);

    stack_push(splitter->status_stack, ST_VALUE); /* parse the pointed value */

    /* then do the jump */
    char * landing_pos = splitter->input_body_pos + offset - 1;
    splitter->pos = landing_pos;
    splitter->chunk_iter_start = landing_pos;

}

SRL_STATIC_INLINE void _read_copy(pTHX_ srl_splitter_t * splitter) {

    /* When we see a copy tag, we do the copy immediately, it makes things
     * easier than trying to recompute the pointer, make sure it points to
     * something in the current chunk, etc. Instead we do the copy. But have no
     * fear, the check_for_duplicates will be called later on and will remove
     * duplication that may have been introduced here */

    /* we save the position at the copy tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = _read_varint_uv_nocheck(splitter);
    if (offset == 0)
        croak("COPY offset is zero !");

    SRL_SPLITTER_TRACE(" * COPY, must jump to offset %lu, from input_body_pos %lu, then back here %lu.",
                       offset,
                       splitter->input_body_pos - splitter->input_str,
                       splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the COPY tag */
    _maybe_flush_chunk(aTHX_ splitter, saved_pos, NULL);

    /* set the instructions in the stack. Warning, we are pushing, so the order
       will be reversed when we pop */
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);

    stack_push(splitter->status_stack, (UV)splitter->pos);
    stack_push(splitter->status_stack, ST_ABSOLUTE_JUMP);

    /* parse the pointed value */
    stack_push(splitter->status_stack, ST_VALUE);
    if (splitter->tag_is_tracked) {
	/* It may be that the original COPY tag (that we are replacing by the
	 * pointed value) had his tracking bit set. In this case, when we do
	 * the copy, we need to promote whatever is pointed by the copy tag to
	 * a tracked element. That's the ST_TRACK_NEXT_VALUE command that we
	 * add here in the state stack */

	stack_push(splitter->status_stack, ST_TRACK_NEXT_VALUE);

	/* Also tells that the next ST_VALUE should not be checked for duplication */
	stack_push(splitter->status_stack, ST_DONT_CHECK_FOR_DUPLICATE);

    }

    /* then do the jump */
    char * landing_pos = splitter->input_body_pos + offset - 1;
    splitter->pos = landing_pos;
    splitter->chunk_iter_start = landing_pos;
}

SRL_STATIC_INLINE void _read_alias(pTHX_ srl_splitter_t * splitter) {

    /* Note: In this fonction we don't handle the case where the alias tag is
     * tracked (as we do in _read_copy), because alias tag *cannot* be tracked.
     * It can't have its high bit set. */

    /* we save the position at the alias tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = _read_varint_uv_nocheck(splitter);

    if (offset == 0)
        croak("ALIAS offset is zero !");

    SRL_SPLITTER_TRACE(" * ALIAS, must jump to offset %lu, from input_body_pos %lu, then back here %lu.",
                       offset,
                       splitter->input_body_pos - splitter->input_str,
                       splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the ALIAS tag */
    _maybe_flush_chunk(aTHX_ splitter, saved_pos, NULL);

    /* search in the mapping hash */
    offset_el_t *element = NULL;
    HASH_FIND(hh, offset_hashtable, &offset, sizeof(UV), element);
    if (element != NULL) {
        UV new_offset = element->value;
        /* insert an ALIAS tag */
        char tmp_str[SRL_MAX_VARINT_LENGTH];
        tmp_str[0] = SRL_HDR_ALIAS;
        _cat_to_chunk(aTHX_ splitter, tmp_str, 1 );
        
        /* append the offset as a varint */
        UV varint_len = (UV) (_set_varint_nocheck(tmp_str, new_offset) - tmp_str);
        _cat_to_chunk(aTHX_ splitter, tmp_str, varint_len);
    } else {

        /* otherwise duplicate the data here */

        splitter->deepness++;
        stack_push(splitter->status_stack, ST_DEEPNESS_UP);

        stack_push(splitter->status_stack, (UV)splitter->pos);
        stack_push(splitter->status_stack, ST_ABSOLUTE_JUMP);

        stack_push(splitter->status_stack, ST_VALUE);

        /* then do the jump */
        char * landing_pos = splitter->input_body_pos + offset - 1;
        splitter->pos = landing_pos;
        splitter->chunk_iter_start = landing_pos;
    }

}

SRL_STATIC_INLINE void _read_hash(srl_splitter_t * splitter) {
    UV len = _read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * HASH of len, %lu", len);
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    while (len-- > 0) {
        stack_push(splitter->status_stack, ST_VALUE);
        stack_push(splitter->status_stack, ST_VALUE);
    }
    return;
}

SRL_STATIC_INLINE void _read_array(srl_splitter_t * splitter) {
    UV len = _read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * ARRAY of len, %lu", len);
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    while (len-- > 0) {
        stack_push(splitter->status_stack, ST_VALUE);
    }
    return;
}

SRL_STATIC_INLINE void _read_regexp(srl_splitter_t * splitter) {
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE);
    stack_push(splitter->status_stack, ST_VALUE);
}

void _empty_hashes(pTHX) {
    dedupe_el_t *elt_d, *tmp_d;
    HASH_ITER(hh, dedupe_hashtable, elt_d, tmp_d) {
        HASH_DEL(dedupe_hashtable,elt_d);  /* delete; dedupe_hashtable advances to next */
        Safefree(elt_d);
    }

    HASH_ITER(hh, dedupe_hashtable_utf8, elt_d, tmp_d) {
        HASH_DEL(dedupe_hashtable_utf8,elt_d);  /* delete; dedupe_hashtable_utf8 advances to next */
        Safefree(elt_d);
    }

    offset_el_t *elt_o, *tmp_o;
    HASH_ITER(hh, offset_hashtable, elt_o, tmp_o) {
        HASH_DEL(offset_hashtable, elt_o);  /* delete; offset_hashtable advances to next */
        Safefree(elt_o);
    }
}

SV* srl_splitter_next_chunk(pTHX_ srl_splitter_t * splitter) {

    /* create a new chunk */

    /* empty the dedupe_hashtable and offset_hashtable */
    _empty_hashes(aTHX);

    /* zero length Perl string */
    splitter->chunk = newSVpvn("", 0);
    splitter->chunk_size = 0;
    splitter->chunk_start = splitter->pos;
    splitter->chunk_iter_start = splitter->pos;
    splitter->chunk_nb_elts = 0;

    splitter->chunk_body_pos = splitter->chunk_start;

    /* allocate the chunk to avoid multiple memory allocation */
    SvGROW(splitter->chunk, splitter->size_limit + 50);

    /* for some reason, jump offset start at 1 in sereal spec, go figure why */
    splitter->chunk_current_offset = 1;
        
    UV chunk_header_len = 0;
    /* srl magic */
    sv_catpvn(splitter->chunk, SRL_MAGIC_STRING_HIGHBIT, SRL_MAGIC_STRLEN);
    splitter->chunk_body_pos += SRL_MAGIC_STRLEN;
    chunk_header_len += SRL_MAGIC_STRLEN;

    /* srl version-type type=raw, version=3 */
    sv_catpvn(splitter->chunk, "\3", 1);
    splitter->chunk_body_pos += 1;
    chunk_header_len += 1;

    if ( ! splitter->header_len) {
        /* no srl header */
        sv_catpvn(splitter->chunk, "\0", 1);
        splitter->chunk_body_pos += 1;
        chunk_header_len += 1;
    } else {
        sv_catpvn(splitter->chunk, splitter->header_str, splitter->header_len);
        splitter->chunk_body_pos += splitter->header_len;
        chunk_header_len += splitter->header_len;
    }

    char tmp_str[SRL_MAX_VARINT_LENGTH];
    tmp_str[0] = SRL_HDR_REFN;
    sv_catpvn(splitter->chunk, tmp_str, 1);
    splitter->chunk_current_offset += 1;

    tmp_str[0] = SRL_HDR_ARRAY;
    sv_catpvn(splitter->chunk, tmp_str, 1);
    splitter->chunk_current_offset += 1;

    /* append the varint of the maximum array's number of elements */
    UV varint_len = (UV) (_set_varint_nocheck(tmp_str, splitter->input_nb_elts) - tmp_str);
    /* This is the char number where we're going to write the varint */
    UV varint_pos = SvCUR(splitter->chunk);
    sv_catpvn(splitter->chunk, tmp_str, varint_len);
    splitter->chunk_current_offset += varint_len;

    int found = _parse(aTHX_ splitter);
    if (found) {
        char * varint_start = SvPVX(splitter->chunk) + varint_pos;
        char * varint_end = varint_start + varint_len - 1;
        _update_varint_from_to(varint_start, varint_end, splitter->chunk_nb_elts);

        if (splitter->header_count_idx != -1) {
            /* chunk + magic size + version size + header varint size(8) + index where the count is */
            char * header_count_varint_start = SvPVX(splitter->chunk) + SRL_MAGIC_STRLEN + 1 + splitter->header_count_idx;
            /* note: instead of 8, it should be SRL_MAX_VARINT_LENGTH,
               srl_decoder.c:831 is buggy: decoding of varint only support
               varint of size 8 bytes, instead of 11 */
            char * header_count_varint_end = header_count_varint_start + 8 - 1;
            _update_varint_from_to(header_count_varint_start, header_count_varint_end, splitter->chunk_nb_elts);
        }

        if (splitter->compression_format == 0) /* no compression */
            return splitter->chunk;

        /* gzip compression: reforge the SV completely */

        UV uncompressed_body_length = SvCUR(splitter->chunk) - chunk_header_len;
        SRL_SPLITTER_TRACE(" * UNCOMPRESS BODY_LENGTH %lu", uncompressed_body_length);

        size_t dest_len;
        /* Get uncompressed payload and total packet output (after compression) lengths */
        dest_len = chunk_header_len + 1 + (size_t)mz_compressBound(uncompressed_body_length) + (2 * SRL_MAX_VARINT_LENGTH);

        SV* compressed_chunk = newSVpvn("", 0);
        sv_catpvn(compressed_chunk, SvPVX(splitter->chunk), chunk_header_len);


        SRL_SPLITTER_TRACE(" * ALLOCATED DEST LEN %lu", dest_len);
        char * compressed_pos = SvGROW(compressed_chunk, dest_len);

        compressed_pos += chunk_header_len;

        /* append the varint of the uncompressed length */
        compressed_pos = _set_varint_nocheck(compressed_pos, uncompressed_body_length);
        /* append the varint of the compressed length */
        char* varint_start2 = compressed_pos;
        compressed_pos = _set_varint_nocheck(compressed_pos, dest_len);
        char* varint_end2 = compressed_pos -1;


                splitter->compression_level = MZ_DEFAULT_COMPRESSION;

                mz_ulong dl = (mz_ulong)dest_len;
                int status = mz_compress2(
                    (unsigned char *) compressed_pos,
                    &dl,
                    (const unsigned char *)(SvPVX(splitter->chunk) + chunk_header_len),
                    (mz_ulong)uncompressed_body_length,
                    splitter->compression_level
                );
                (void)status;
                assert(status == Z_OK);
                dest_len = (size_t)dl;

        SRL_SPLITTER_TRACE(" * COMPRESSED LEN %lu", dest_len);

        _update_varint_from_to(varint_start2, varint_end2, dest_len);

        SvCUR_set(compressed_chunk, compressed_pos - SvPVX(compressed_chunk) + dest_len);

	/* This means "sereal version = 3, and compression method = gzip" */
        (SvPVX(compressed_chunk))[4] = 0x33;

        sv_2mortal(splitter->chunk);
        return compressed_chunk;
    } else {
        sv_2mortal(splitter->chunk);
        return &PL_sv_undef;
    }
}


SRL_STATIC_INLINE char* _set_varint_nocheck(char* buf, UV n) {
    while (n >= 0x80) {             /* while we are larger than 7 bits long */
        *(buf++) = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        n = n >> 7;                 /* shift off the 7 least significant bits */
    }
    *(buf++) = n;                   /* encode the last 7 bits without the high bit being set */
    return buf;
}



SRL_STATIC_INLINE UV _read_varint_uv_nocheck(srl_splitter_t *splitter) {

    UV result = 0;
    unsigned lshift = 0;

    while (*(splitter->pos) & 0x80) {
        result |= ((UV)( *(splitter->pos) & 0x7F) << lshift);
        lshift += 7;
        splitter->pos++;
    }

    result |= ((UV)*(splitter->pos) << lshift);
    splitter->pos++;
    
    return result;
}

/* Update a varint anywhere in the output stream with defined start and end
 * positions. This can produce non-canonical varints and is useful for filling
 * pre-allocated varints. */
SRL_STATIC_INLINE void _update_varint_from_to(char *varint_start, char *varint_end, UV number) {
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

SRL_STATIC_INLINE bool _maybe_flush_chunk (pTHX_ srl_splitter_t *splitter, char* end_pos, char* next_start_pos) {
    UV len;
    bool did_we_flush = 0;
    if (end_pos == NULL)
        end_pos = splitter->pos;
    if (next_start_pos == NULL)
        next_start_pos = splitter->pos;
    if (end_pos > splitter->chunk_iter_start) {
        len = (UV) (end_pos - splitter->chunk_iter_start);
        _cat_to_chunk(aTHX_ splitter, splitter->chunk_iter_start, len );
        did_we_flush = 1;
    }
    /* still do that, if the caller wanted to set a special next_start_pos */
    splitter->chunk_iter_start = next_start_pos;
    return did_we_flush;
}
