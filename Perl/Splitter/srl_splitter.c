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

#define SRL_SPLITTER_TRACE(msg, args...) warn((msg), args)
//#define SRL_SPLITTER_TRACE(msg, args...)

#include "srl_splitter.h"
#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_inline.h"

#include "snappy/csnappy_decompress.c"
#include "miniz.h"

#include "uthash.h"

#define STACK_SIZE_INCR 64

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)


#define SRL_SPLITTER_SET_OPTION(splitter, flag_num) ((splitter)->flags |= (flag_num))
#define SRL_SPLITTER_HAVE_OPTION(splitter, flag_num) ((splitter)->flags & (flag_num))


#define SRL_MAX_VARINT_LENGTH 11


/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE srl_splitter_t * srl_empty_splitter_struct(pTHX);
void srl_parse_header(pTHX_ srl_splitter_t *splitter);
SRL_STATIC_INLINE UV srl_read_varint_uv_nocheck(pTHX_ srl_splitter_t *splitter);

int _parse(srl_splitter_t * splitter);
SRL_STATIC_INLINE void _read_tag(srl_splitter_t * splitter, char tag);
SRL_STATIC_INLINE void srl_read_varint(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_zigzag(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_float(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_double(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_long_double(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_string(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_weaken(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_refn(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_refp(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_object(srl_splitter_t * splitter, bool is_freeze);
SRL_STATIC_INLINE void srl_read_objectv(srl_splitter_t * splitter, bool is_freeze);
SRL_STATIC_INLINE void srl_read_copy(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_alias(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_extend(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_hash(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_array(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_regexp(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_update_varint_from_to(char *varint_start, char *varint_end, UV number);
char* _set_varint_nocheck(char* buf, UV n);
bool _maybe_flush_chunk (srl_splitter_t *splitter, char* end_pos, char* next_start_pos);
void _empty_hashes();
void _check_for_duplicates(srl_splitter_t * splitter, char* binary_start_pos, UV len);

typedef struct {
    char* key;         /* the string to dedup */
    UV value;          /* the position in the chunk */
    UT_hash_handle hh; /* makes this structure hashable */
} dedupe_el_t;

dedupe_el_t *dedupe_hashtable = NULL; /* important! initialize to NULL */

typedef struct {
    UV key;            /* key, the original offset in input */
    UV value;          /* value, the new offset in the chunk */
    UT_hash_handle hh; /* makes this structure hashable */
} offset_el_t;

offset_el_t *offset_hashtable = NULL;    /* important! initialize to NULL */

UV stack_pop(srl_splitter_stack_t * stack) {
    UV val = 0;
    if ( stack->top <= 0 )
        croak("Stack is empty");
    val = stack->data[stack->top-1];
    stack->top--;
    return val;
}

bool stack_is_empty(srl_splitter_stack_t * stack) {
    return (stack->top == 0);
}

void stack_push(srl_splitter_stack_t * stack, UV val) {
    if (stack->top >= stack->size) {
        UV new_size = stack->size + STACK_SIZE_INCR;
        UV* tmp;
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

    srl_parse_header(splitter);

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
            UV len = srl_read_varint_uv_nocheck(splitter);
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
    _empty_hashes();
    SvREFCNT_dec(splitter->input_sv);
    if (splitter->status_stack->data != NULL) {
        Safefree(splitter->status_stack->data);
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

void srl_parse_header(pTHX_ srl_splitter_t *splitter) {
    int magic_string = 1;
    int high_magic_string = 1;

    U8 version_encoding;
    U8 version;
    U8 encoding_flags;
    UV header_len;

    int is_zlib_encoded = 0;
    int is_snappy_encoded = 0;
    int is_snappyincr_encoded = 0;

    // SRL_MAGIC_STRLEN + PROTOCOL_LENGTH + OPTIONAL-HEADER-SIZE(at least 1 byte) + DATA(at least 1 byte)
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

    // move after protocol version
    splitter->pos += 1;
    
    header_len= srl_read_varint_uv_nocheck(aTHX_ splitter);

    SRL_SPLITTER_TRACE("header len is %lu", header_len);

    //TODO: add code for processing the header
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
            compressed_len = srl_read_varint_uv_nocheck(splitter);
        } else {
            compressed_len = splitter->input_len - (splitter->pos - splitter->input_str);
        }
        SRL_SPLITTER_TRACE("snappy compressed len %"UVuf, compressed_len);
        // splitter->pos is now at start of compressed payload

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

        // allocate a new SV for uncompressed data
        sv_2mortal(splitter->input_sv);
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

        UV uncompressed_len = srl_read_varint_uv_nocheck(splitter);
        UV compressed_len = srl_read_varint_uv_nocheck(splitter);
        char * new_input_str;

        // splitter->pos is now at start of compressed payload
        SRL_SPLITTER_TRACE("unzipping %s", "");
        SRL_SPLITTER_TRACE("compressed_len : %" UVuf, compressed_len);
        SRL_SPLITTER_TRACE("uncompressed_len : %" UVuf, uncompressed_len);

                 
        mz_ulong tmp = uncompressed_len;

        // allocate a new SV for uncompressed data
        sv_2mortal(splitter->input_sv);
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

int _parse(srl_splitter_t * splitter) {

    char tag;

    while( ! stack_is_empty(splitter->status_stack) ) {
        UV status = stack_pop(splitter->status_stack);
        UV absolute_offset;

        SRL_SPLITTER_TRACE("* ITERATING -- deepness value: %d", splitter->deepness);
        switch(status) {
        case ST_DEEPNESS_UP:
            splitter->deepness--;
            SRL_SPLITTER_TRACE(" * DEEPNESS UP -- deepness value: %d", splitter->deepness);
            break;
        case ST_VALUE:
            tag = *(splitter->pos);
            SRL_SPLITTER_TRACE(" * VALUE tag %d -- deepness value: %d", tag, splitter->deepness);
            if (tag & SRL_HDR_TRACK_FLAG) {
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
            splitter->pos++;
            _read_tag(splitter, tag);
            break;
        case ST_ABSOLUTE_JUMP:
            /* before jumping, flush the chunk */
            _maybe_flush_chunk(splitter, NULL, NULL);
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
                _maybe_flush_chunk(splitter, NULL, NULL);
                return 1;
            }
        }
    }
    SRL_SPLITTER_TRACE("------------ END ITERATING ------- deepness value: %d", splitter->deepness);
    if (splitter->deepness != 0)
        croak("Something wrong happens: parsing finished but deepness is not zero");

    /* iteration is finished, if we had to flush something return success */
    if (_maybe_flush_chunk(splitter, NULL, NULL))
        return 1;

    /* maybe we didn't have to flush but the chunk is not empty: return success */
    if (splitter->chunk_size > 0)
        return 1;

    /* otherwise, no data anymore, return failure */
    return 0;
}

void _check_for_duplicates(srl_splitter_t * splitter, char* binary_start_pos, UV len) {
    dedupe_el_t *element = NULL;
    HASH_FIND(hh, dedupe_hashtable, splitter->pos, len, element);
    if ( element != NULL) {
        SRL_SPLITTER_TRACE("   * FOUND DEDUP key %s value %lu", element->key, element->value);
        _maybe_flush_chunk(splitter, binary_start_pos, splitter->pos + len);

        /* the copy tag */
        char tmp[SRL_MAX_VARINT_LENGTH];
        tmp[0] = 0x2f;
        sv_catpvn(splitter->chunk, tmp, 1 );
        splitter->chunk_current_offset += 1;
        splitter->chunk_size += 1;

        UV len = (UV) (_set_varint_nocheck(tmp, element->value) - tmp);
        sv_catpvn(splitter->chunk, tmp, len);
        splitter->chunk_current_offset += len;
        splitter->chunk_size += len;

    } else {
        UV offset = splitter->chunk_current_offset + ( binary_start_pos - splitter->chunk_iter_start);
        Newx(element, 1, dedupe_el_t);
        element->key = "";
        element->value = offset;
        SRL_SPLITTER_TRACE("   * ADDED DEDUP offset %lu", offset);
        HASH_ADD_KEYPTR(hh, dedupe_hashtable, splitter->pos, len, element);
    }

    splitter->pos += len;
    return;
}

void
_read_tag(srl_splitter_t * splitter, char tag)
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
        _check_for_duplicates(splitter, binary_start_pos, len);
    } else if ( IS_SRL_HDR_HASHREF(tag) ) {
        int len = tag & 0xF;
        SRL_SPLITTER_TRACE(" * SHORT HASHREF of length %d", len);
        while (len-- > 0) {
            splitter->deepness++;
            stack_push(splitter->status_stack, ST_DEEPNESS_UP);
            stack_push(splitter->status_stack, ST_VALUE);
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else if ( IS_SRL_HDR_ARRAYREF(tag) ) {
        int len = tag & 0xF;
        SRL_SPLITTER_TRACE(" * SHORT ARRAY of length %d", len);
        while (len-- > 0) {
            splitter->deepness++;
            stack_push(splitter->status_stack, ST_DEEPNESS_UP);
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else {
        switch (tag) {
            case SRL_HDR_VARINT:         srl_read_varint(splitter);       break;
            case SRL_HDR_ZIGZAG:         srl_read_zigzag(splitter);       break;
            case SRL_HDR_FLOAT:          srl_read_float(splitter);        break;
            case SRL_HDR_DOUBLE:         srl_read_double(splitter);       break;
            case SRL_HDR_LONG_DOUBLE:    srl_read_long_double(splitter);  break;
            case SRL_HDR_TRUE:           /* no op */                      break;
            case SRL_HDR_FALSE:          /* no op */                      break;
            case SRL_HDR_CANONICAL_UNDEF:
            case SRL_HDR_UNDEF:          /* no op */                      break;
            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:       srl_read_string(splitter);       break;
            case SRL_HDR_WEAKEN:         srl_read_weaken(splitter);       break;
            case SRL_HDR_REFN:           srl_read_refn(splitter);         break;
            case SRL_HDR_REFP:           srl_read_refp(splitter);         break;
            case SRL_HDR_HASH:           srl_read_hash(splitter);         break;
            case SRL_HDR_ARRAY:          srl_read_array(splitter);        break;
            case SRL_HDR_OBJECT:         srl_read_object(splitter, 0);    break;
            case SRL_HDR_OBJECT_FREEZE:  srl_read_object(splitter, 1);    break;
            case SRL_HDR_OBJECTV:        srl_read_objectv(splitter, 0);   break;
            case SRL_HDR_OBJECTV_FREEZE: srl_read_object(splitter, 1);    break;
            case SRL_HDR_ALIAS:          srl_read_alias(splitter);        break;
            case SRL_HDR_COPY:           srl_read_copy(splitter);         break;
            case SRL_HDR_EXTEND:         srl_read_extend(splitter);       break;
            case SRL_HDR_REGEXP:         srl_read_regexp(splitter);       break;
            case SRL_HDR_PAD:            /* no op */                      break;
            default:                     croak("Unexpected tag value");   break;
        }
    }
}


SRL_STATIC_INLINE void srl_read_varint(srl_splitter_t * splitter) {
    srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * VARINT %s", "");
}

SRL_STATIC_INLINE void srl_read_zigzag(srl_splitter_t * splitter) {
    srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * ZIGZAG %s", "");
}

SRL_STATIC_INLINE void srl_read_float(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * FLOAT %s", "");
    splitter->pos += sizeof(float);
}

SRL_STATIC_INLINE void srl_read_double(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * DOUBLE %s", "");
    splitter->pos += sizeof(double);
}

SRL_STATIC_INLINE void srl_read_long_double(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * LONG DOUBLE %s", "");
    splitter->pos += sizeof(long double);
}

SRL_STATIC_INLINE void srl_read_string(srl_splitter_t * splitter) {
    char *binary_start_pos = splitter->pos;
    UV len = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * STRING of length %lu", len);
    _check_for_duplicates(splitter, binary_start_pos, len);
    splitter->pos+= len;
}

SRL_STATIC_INLINE void srl_read_weaken(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * WEAKEN, %s", "");
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE);
}

SRL_STATIC_INLINE void srl_read_refn(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * REFN, %s", "");
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE);
}

SRL_STATIC_INLINE void srl_read_refp(srl_splitter_t * splitter) {
    /* we save the position at the refp tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = srl_read_varint_uv_nocheck(splitter);

    if (offset == 0)
        croak("REFP offset is zero !");

    SRL_SPLITTER_TRACE(" * REFP, must jump to offset %lu, from input_body_pos %lu, then back here %lu.",
                       offset,
                       splitter->input_body_pos - splitter->input_str,
                       splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the REFP tag */
    _maybe_flush_chunk(splitter, saved_pos, NULL);

    /* search in the mapping hash */
    offset_el_t *element = NULL;
    HASH_FIND(hh, offset_hashtable, &offset, sizeof(UV), element);
    if (element != NULL) {
        SRL_SPLITTER_TRACE(" ####### 1 %s", "");
        UV new_offset = element->value;
        SRL_SPLITTER_TRACE(" ####### 2 %s", "");
        SRL_SPLITTER_TRACE(" ####### 3 %lu", element->key);
        /* insert a refp */
        char tmp_str[SRL_MAX_VARINT_LENGTH];
        tmp_str[0] = 0x29;
        sv_catpvn(splitter->chunk, tmp_str, 1 );
        splitter->chunk_current_offset += 1;
        splitter->chunk_size += 1;
        
        /* append the offset as a varint */
        UV varint_len = (UV) (_set_varint_nocheck(tmp_str, new_offset) - tmp_str);
        sv_catpvn(splitter->chunk, tmp_str, varint_len);
        splitter->chunk_current_offset += varint_len;
        splitter->chunk_size += varint_len;
    } else {

        /* otherwise insert an refn tag instead of the refp */
        char tmp_str[1];
        tmp_str[0] = 0x28;
        sv_catpvn(splitter->chunk, tmp_str, 1 );
        splitter->chunk_current_offset += 1;
        splitter->chunk_size += 1;

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

SRL_STATIC_INLINE void srl_read_object(srl_splitter_t * splitter, bool is_freeze) {
    SRL_SPLITTER_TRACE(" * OBJECT%s, %s", (is_freeze ? "FREEZE" : ""), "");
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE); /* for the class name */
    stack_push(splitter->status_stack, ST_VALUE); /* for the object struct */
}

SRL_STATIC_INLINE void srl_read_objectv(srl_splitter_t * splitter, bool is_freeze) {
    /* we save the position at the objectv tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = srl_read_varint_uv_nocheck(splitter);
    if (offset == 0)
        croak("OBJECTV offset is zero !");

    SRL_SPLITTER_TRACE(" * OBJECTV%s, jump to offset %lu, from input_body_pos %lu, then back here %lu.",
            (is_freeze ? "FREEZE" : ""), offset,
            splitter->input_body_pos - splitter->input_str,
            splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the OBJECTV tag */
    _maybe_flush_chunk(splitter, saved_pos, NULL);

    /* insert an object tag instead of the objectv tag */
    char tmp_str[1];
    tmp_str[0] = (is_freeze ? 0x32 : 0x2c);
    sv_catpvn(splitter->chunk, tmp_str, 1 );
    splitter->chunk_current_offset += 1;
    splitter->chunk_size += 1;

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

SRL_STATIC_INLINE void srl_read_copy(srl_splitter_t * splitter) {
    /* we save the position at the copy tag */
    char* saved_pos = splitter->pos - 1;
    UV offset = srl_read_varint_uv_nocheck(splitter);
    if (offset == 0)
        croak("COPY offset is zero !");

    SRL_SPLITTER_TRACE(" * COPY, must jump to offset %lu, from input_body_pos %lu, then back here %lu.",
                       offset,
                       splitter->input_body_pos - splitter->input_str,
                       splitter->pos - splitter->input_str);

    /* if we have to flush the chunk first, let's do it, until before the COPY tag */
    _maybe_flush_chunk(splitter, saved_pos, NULL);

    /* set the instructions in the stack. Warning, we are pushing, so the order
       will be reversed when we pop */
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);

    stack_push(splitter->status_stack, (UV)splitter->pos);
    stack_push(splitter->status_stack, ST_ABSOLUTE_JUMP);

    /* parse the pointed value */
    stack_push(splitter->status_stack, ST_VALUE);

    /* then do the jump */
    char * landing_pos = splitter->input_body_pos + offset - 1;
    splitter->pos = landing_pos;
    splitter->chunk_iter_start = landing_pos;
}

SRL_STATIC_INLINE void srl_read_alias(srl_splitter_t * splitter) {
    srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * ALIAS, %s", "");

    // if the alias offset is out of bound, then we do like copy, and register the offset reference

}

SRL_STATIC_INLINE void srl_read_extend(srl_splitter_t * splitter) {
    croak("extend unimplemented");
}

SRL_STATIC_INLINE void srl_read_hash(srl_splitter_t * splitter) {
    UV len = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * HASH of len, %lu", len);
    while (len-- > 0) {
        splitter->deepness++;
        stack_push(splitter->status_stack, ST_DEEPNESS_UP);
        stack_push(splitter->status_stack, ST_VALUE);
        stack_push(splitter->status_stack, ST_VALUE);
    }
    return;
}

SRL_STATIC_INLINE void srl_read_array(srl_splitter_t * splitter) {
    UV len = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * ARRAY of len, %lu", len);
    while (len-- > 0) {
        splitter->deepness++;
        stack_push(splitter->status_stack, ST_DEEPNESS_UP);
        stack_push(splitter->status_stack, ST_VALUE);
    }
    return;
}

SRL_STATIC_INLINE void srl_read_regexp(srl_splitter_t * splitter) {
    splitter->deepness++;
    stack_push(splitter->status_stack, ST_DEEPNESS_UP);
    stack_push(splitter->status_stack, ST_VALUE);
    stack_push(splitter->status_stack, ST_VALUE);
}

void _empty_hashes() {
    dedupe_el_t *elt_d, *tmp_d;
    HASH_ITER(hh, dedupe_hashtable, elt_d, tmp_d) {
        HASH_DEL(dedupe_hashtable,elt_d);  /* delete; dedupe_hashtable advances to next */
        Safefree(elt_d);
    }

    offset_el_t *elt_o, *tmp_o;
    HASH_ITER(hh, offset_hashtable, elt_o, tmp_o) {
        HASH_DEL(offset_hashtable, elt_o);  /* delete; offset_hashtable advances to next */
        Safefree(elt_o);
    }
}

SV* srl_splitter_next_chunk(srl_splitter_t * splitter) {

    /* create a new chunk */

    /* empty the dedupe_hashtable and offset_hashtable */
    _empty_hashes();

    /* zero length Perl string */
    splitter->chunk = newSVpvn("", 0);
    splitter->chunk_size = 0;
    splitter->chunk_start = splitter->pos;
    splitter->chunk_iter_start = splitter->pos;
    splitter->chunk_nb_elts = 0;

    splitter->chunk_body_pos = splitter->chunk_start;

    /* for some reason, jump offset start at 1 in sereal spec, go figure why */
    splitter->chunk_current_offset = 1;
        
    /* srl magic */
    sv_catpvn(splitter->chunk, SRL_MAGIC_STRING_HIGHBIT, SRL_MAGIC_STRLEN);
    splitter->chunk_body_pos += SRL_MAGIC_STRLEN;

    char tmp_str[SRL_MAX_VARINT_LENGTH];
    /* srl version-type type=raw, version=3 */
    sv_catpvn(splitter->chunk, "\3", 1);
    splitter->chunk_body_pos += 1;

    /* srl header suffix size: 0 */
    sv_catpvn(splitter->chunk, "\0", 1);
    splitter->chunk_body_pos += 1;

    tmp_str[0] = 0x28; /* REFN */
    sv_catpvn(splitter->chunk, tmp_str, 1);
    splitter->chunk_current_offset += 1;

    tmp_str[0] = 0x2b; /* ARRAY */
    sv_catpvn(splitter->chunk, tmp_str, 1);
    splitter->chunk_current_offset += 1;

    /* append the varint of the maximum array's number of elements */
    UV varint_len = (UV) (_set_varint_nocheck(tmp_str, splitter->input_nb_elts) - tmp_str);
    SRL_SPLITTER_TRACE(" ---- VARINT LEN %lu value %lu", varint_len, splitter->input_nb_elts);
    /* This is the car number where we're going to write the varint */
    UV varint_pos = SvCUR(splitter->chunk);
    sv_catpvn(splitter->chunk, tmp_str, varint_len);
    splitter->chunk_current_offset += varint_len;

    int found = _parse(splitter);
    if (found) {
        char * varint_start = SvPVX(splitter->chunk) + varint_pos;
        char * varint_end = varint_start + varint_len - 1;
        srl_update_varint_from_to(varint_start, varint_end, splitter->chunk_nb_elts);
        return splitter->chunk;
    }
    return &PL_sv_undef;
}


char* _set_varint_nocheck(char* buf, UV n) {
    while (n >= 0x80) {             /* while we are larger than 7 bits long */
        *(buf++) = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
        n = n >> 7;                 /* shift off the 7 least significant bits */
    }
    *(buf++) = n;                   /* encode the last 7 bits without the high bit being set */
    return buf;
}



SRL_STATIC_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_splitter_t *splitter) {

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
SRL_STATIC_INLINE void
srl_update_varint_from_to(pTHX_ char *varint_start, char *varint_end, UV number)
{
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

bool _maybe_flush_chunk (srl_splitter_t *splitter, char* end_pos, char* next_start_pos) {
    UV len;
    bool did_we_flush = 0;
    if (end_pos == NULL)
        end_pos = splitter->pos;
    if (next_start_pos == NULL)
        next_start_pos = splitter->pos;
    if (end_pos > splitter->chunk_iter_start) {
        len = (UV) (end_pos - splitter->chunk_iter_start);
        sv_catpvn(splitter->chunk, splitter->chunk_iter_start, len );
        splitter->chunk_size += len;
        splitter->chunk_current_offset += len;
        did_we_flush = 1;
    }
    /* still do that, if the caller wanted to set a special next_start_pos */
    splitter->chunk_iter_start = next_start_pos;
    return did_we_flush;
}
