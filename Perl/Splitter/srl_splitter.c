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

/* void */
/* next_chunk(splitter, src) */
/*     Sereal::Splitter splitter; */
/*     SV *src */
/*   PPCODE: */
/*     srl_splitter_next_chunk(aTHX splitter, src); */



/* #define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN) */
/* #define SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_ARRAYREF_COUNT) */
/* #define SRL_HDR_HASHREF_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_HASHREF_COUNT) */

#define SRL_SPLITTER_TRACE(msg, args...) warn((msg), args)
//#define SRL_SPLITTER_TRACE(msg, args...)

#include "srl_splitter.h"
#include "srl_common.h"
#include "ptable.h"
#include "srl_protocol.h"
#include "srl_inline.h"

#include "snappy/csnappy_decompress.c"
#include "miniz.h"


#define STACK_SIZE_INCR 64

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)


#define SRL_SPLITTER_SET_OPTION(splitter, flag_num) ((splitter)->flags |= (flag_num))
#define SRL_SPLITTER_HAVE_OPTION(splitter, flag_num) ((splitter)->flags & (flag_num))

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
SRL_STATIC_INLINE void srl_read_object(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_objectv(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_copy(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_alias(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_extend(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_hash(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_array(srl_splitter_t * splitter);
SRL_STATIC_INLINE void srl_read_regexp(srl_splitter_t * splitter);


UV
stack_pop(srl_splitter_stack_t * stack)
{
    UV val = 0;
    if (stack->top > 0){
        val = stack->data[stack->top-1];
        stack->top--;
    } else {
        croak("Stack is empty");
    }
    return val;
}

bool stack_is_empty(srl_splitter_stack_t * stack) {
    return (stack->top == 0);
}

void
stack_push(srl_splitter_stack_t * stack, UV val)
{
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

srl_splitter_t *
srl_build_splitter_struct(pTHX_ HV *opt)
{
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
        splitter->chunk_size = SvUV(*svp);
        SRL_SPLITTER_TRACE("chunk_size %" UVuf, splitter->chunk_size);
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
    splitter->cant_split_here = 0;

    /* srl_splitter_stack_t * output_stack; */
    /* Newxz(output_stack, 1, srl_splitter_stack_t ); */
    /* Newxz(output_stack->data, STACK_SIZE_INCR, UV ); */
    /* output_stack->size = STACK_SIZE_INCR; */
    /* output_stack->top = 0; */
    /* splitter->output_stack = output_stack; */

    char tag = *(splitter->pos);
    splitter->pos++;
    if (IS_SRL_HDR_ARRAYREF(tag)) {
        int len = tag & 0xF;
        while (len-- > 0) {
            stack_push(splitter->status_stack, ST_VALUE);
        }
        /* OK */
    } else if (tag == SRL_HDR_REFN) {
        tag = *(splitter->pos);
        splitter->pos++;
        if (tag == SRL_HDR_ARRAY) {
            UV len = srl_read_varint_uv_nocheck(splitter);
            SRL_SPLITTER_TRACE(" * ARRAY of len, %lu", len);
            while (len-- > 0) {
                stack_push(splitter->status_stack, ST_VALUE);
            }
            /* OK */
        } else {
            croak("first tag is REFN but next tag is not ARRAY");
        }
    } else {
        croak("first tag is not an ArrayRef");
    }

    
    return splitter;
}

void
srl_destroy_splitter(pTHX_ srl_splitter_t *splitter)
{
    SvREFCNT_dec(splitter->input_sv);
    if (splitter->status_stack->data != NULL) {
        Safefree(splitter->status_stack->data);
    }
    /* if (splitter->output_stack->data != NULL) { */
    /*     Safefree(splitter->output_stack->data); */
    /* } */
    Safefree(splitter);
}

SRL_STATIC_INLINE srl_splitter_t *
srl_empty_splitter_struct(pTHX)
{
    srl_splitter_t *splitter = NULL;
    Newx(splitter, 1, srl_splitter_t);
    if (splitter == NULL) {
        croak("Out of memory");
    }
    return splitter;
}


void
srl_parse_header(pTHX_ srl_splitter_t *splitter)
{
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
        splitter->body_pos = splitter->input_str;
    } else {
        splitter->body_pos = splitter->pos;
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
        splitter->body_pos = splitter->pos;

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

        int decompress_ok = mz_uncompress(
                                          (unsigned char *) new_input_str,
                                          &tmp,
                                          (const unsigned char *) compressed,
                                          compressed_len
                                          );

        if (decompress_ok != Z_OK) {
            croak("ZLIB decompression of Sereal packet payload failed");
        }

        splitter->input_str = new_input_str;
        SRL_SPLITTER_TRACE(" decompress OK: length %lu\n", uncompressed_len);

        splitter->pos = splitter->input_str;
        splitter->input_len = (STRLEN)tmp;
        splitter->body_pos = splitter->pos;

    }
}

/* void */
/* srl_splitter_find_where_to_cut(srl_splitter_t * splitter) */
/* { */
/*     UV status; */
/*     UV deepness = 0; */
/*     while( (status = stack_pop(splitter->status_stack)) != ST_DONE ) { */
/*         switch(status) { */
/*         case ST_TRACK: */
/*             /\* get current element from current array of stuff *\/ */
/*         case ST_VALUE: */
/*             char tag = *(splitter->pos); */
/*             if (tag & SRL_HDR_TRACK_FLAG) { */
/*                 tag = tag & ~SRL_HDR_TRACK_FLAG; */
/*                 SRL_SPLITTER_TRACE("tag must be tracked, pushing in track_Stack: %d\n", splitter->pos - splitter->body_pos); */
/*                 stack_push(splitter->track_stack, splitter->pos - splitter->body_pos); */
/*                 stack_push(splitter->status_stack, ST_TRACK); */
/*             } */
/*             break; */
/*         case ST_ARRAY_CLOSE: */
/*         case ST_HASH_CLOSE: */
/*         } */
/*     } */

/*     croak("Something is wrong, I parsed the whole input blob but didn't found desired deepness"); */
/* } */

int _parse(srl_splitter_t * splitter) {

    /* char tag = *(splitter->pos); */
    /* splitter->pos++; */
    /* if (is_hash_or_array(tag)) { */
        
    /* } */

    /* sv_catpvn(splitter->prefix, ); */

    /*         if (tag & SRL_HDR_TRACK_FLAG) { */
    /*             tag = tag & ~SRL_HDR_TRACK_FLAG; */
    /*             SRL_SPLITTER_TRACE("tag must be tracked, pushing in track_Stack: %d\n", splitter->pos - splitter->body_pos); */
    /*             stack_push(splitter->track_stack, splitter->pos - splitter->body_pos); */
    /*             stack_push(splitter->status_stack, ST_TRACK); */
    /*         } */
    

    char tag;

    while( ! stack_is_empty(splitter->status_stack) ) {
        UV status = stack_pop(splitter->status_stack);

        SRL_SPLITTER_TRACE("* ITERATING -- cant_split value: %d", splitter->cant_split_here);
        switch(status) {
        case ST_TRACK:
            /* get current element from current array of stuff */
            break;
        case ST_CAN_SPLIT_AGAIN:
            splitter->cant_split_here--;
            SRL_SPLITTER_TRACE("  * SPLIT AGAIN -- cant_split value: %d", splitter->cant_split_here);
            break;
        case ST_VALUE:
            tag = *(splitter->pos);
            SRL_SPLITTER_TRACE("  * VALUE tag %d -- cant_split value: %d", tag, splitter->cant_split_here);
            if (tag & SRL_HDR_TRACK_FLAG) {
                tag = tag & ~SRL_HDR_TRACK_FLAG;
                SRL_SPLITTER_TRACE("tag must be tracked, %ld\n", splitter->pos - splitter->body_pos);
                stack_push(splitter->status_stack, splitter->pos - splitter->body_pos);
                stack_push(splitter->status_stack, ST_TRACK);
            }
            splitter->pos++;
            _read_tag(splitter, tag);
            break;
        }
        if ( splitter->cant_split_here == 0 &&
             (UV)(splitter->current_chunk_size +
              splitter->pos - splitter->current_chunk_start) >= splitter->chunk_size) {
            SRL_SPLITTER_TRACE(" ----------- FLUSH CHUNK, size is %li", (splitter->pos - splitter->current_chunk_start));
            sv_catpvn(splitter->current_chunk, splitter->current_chunk_start, splitter->pos - splitter->current_chunk_start);
            return 1;
        }
    }
    SRL_SPLITTER_TRACE("------------ END ITERATING ------- cant_split value: %d", splitter->cant_split_here);
    if (splitter->cant_split_here != 0) {
        croak("Something wrong happens: parsing finished but cant_split_here is not zero");
    }

    if (splitter->pos - splitter->current_chunk_start > 0) {
        SRL_SPLITTER_TRACE(" ----------- LAST FLUSH CHUNK, size is %li", (splitter->pos - splitter->current_chunk_start));
        sv_catpvn(splitter->current_chunk, splitter->current_chunk_start, splitter->pos - splitter->current_chunk_start);
        return 1;
    }

    return 0;
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
        int len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        SRL_SPLITTER_TRACE(" * SHORT BINARY of length %d", len);
        splitter->pos += len;
    } else if ( IS_SRL_HDR_HASHREF(tag) ) {
        int len = tag & 0xF;
        SRL_SPLITTER_TRACE(" * SHORT HASHREF of length %d", len);
        stack_push(splitter->status_stack, ST_HASH_CLOSE);
        while (len-- > 0) {
            splitter->cant_split_here++;
            stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
            stack_push(splitter->status_stack, ST_VALUE);
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else if ( IS_SRL_HDR_ARRAYREF(tag) ) {
        int len = tag & 0xF;
        SRL_SPLITTER_TRACE(" * SHORT ARRAY of length %d", len);
        stack_push(splitter->status_stack, ST_ARRAY_CLOSE);
        while (len-- > 0) {
            splitter->cant_split_here++;
            stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
            stack_push(splitter->status_stack, ST_VALUE);
        }
    } else {
        switch (tag) {
            case SRL_HDR_VARINT:        srl_read_varint(splitter);       break;
            case SRL_HDR_ZIGZAG:        srl_read_zigzag(splitter);       break;
            case SRL_HDR_FLOAT:         srl_read_float(splitter);        break;
            case SRL_HDR_DOUBLE:        srl_read_double(splitter);       break;
            case SRL_HDR_LONG_DOUBLE:   srl_read_long_double(splitter);  break;
            case SRL_HDR_TRUE:          /* no op */                      break;
            case SRL_HDR_FALSE:         /* no op */                      break;
            case SRL_HDR_CANONICAL_UNDEF:
            case SRL_HDR_UNDEF:         /* no op */                      break;
            case SRL_HDR_BINARY:
            case SRL_HDR_STR_UTF8:      srl_read_string(splitter);    break;
            case SRL_HDR_WEAKEN:        srl_read_weaken(splitter);       break;
            case SRL_HDR_REFN:          srl_read_refn(splitter);         break;
            case SRL_HDR_REFP:          srl_read_refp(splitter);         break;
            case SRL_HDR_OBJECT_FREEZE:
            case SRL_HDR_OBJECT:        srl_read_object(splitter);  break;
            case SRL_HDR_OBJECTV_FREEZE:
            case SRL_HDR_OBJECTV:       srl_read_objectv(splitter); break;
            case SRL_HDR_COPY:          srl_read_copy(splitter);         break;
            case SRL_HDR_ALIAS:         srl_read_alias(splitter);        break;
            case SRL_HDR_EXTEND:        srl_read_extend(splitter);       break;
            case SRL_HDR_HASH:          srl_read_hash(splitter);      break;
            case SRL_HDR_ARRAY:         srl_read_array(splitter);     break;
            case SRL_HDR_REGEXP:        srl_read_regexp(splitter);       break;
            case SRL_HDR_PAD:           /* no op */                      break;
            default:                    croak("Unexpected tag value");   break;
        }
    }
}


SRL_STATIC_INLINE void srl_read_varint(srl_splitter_t * splitter) {
    UV uv = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * VARINT %s", "");
}

SRL_STATIC_INLINE void srl_read_zigzag(srl_splitter_t * splitter) {
    UV uv = srl_read_varint_uv_nocheck(splitter);
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
    UV len = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * STRING of length %lu", len);
    splitter->pos+= len;
}

SRL_STATIC_INLINE void srl_read_weaken(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * WEAKEN, %s", "");
    stack_push(splitter->status_stack, ST_VALUE);
}

SRL_STATIC_INLINE void srl_read_refn(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * REFN, %s", "");
    splitter->cant_split_here++;
    stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
    stack_push(splitter->status_stack, ST_VALUE);
}

SRL_STATIC_INLINE void srl_read_refp(srl_splitter_t * splitter) {
    UV offset = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * REFP, %s", "");
}

SRL_STATIC_INLINE void srl_read_object(srl_splitter_t * splitter) {
    SRL_SPLITTER_TRACE(" * OBJECT, %s", "");
    splitter->cant_split_here++;
    stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
    stack_push(splitter->status_stack, ST_VALUE); /* for the class name */
    stack_push(splitter->status_stack, ST_VALUE); /* for the object struct */
}

SRL_STATIC_INLINE void srl_read_objectv(srl_splitter_t * splitter) {
    UV offset = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * OBJECTV, %s", "");
    splitter->cant_split_here++;
    stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
    stack_push(splitter->status_stack, ST_VALUE); /* for the object struct */
}

SRL_STATIC_INLINE void srl_read_copy(srl_splitter_t * splitter) {
    UV offset = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * COPY, must jump to offset %lu, then back.", offset);

    // MAYBE do that if the offset is out of the chunk
    /* /\* We'll have to jump back here *\/ */
    /* stack_push(splitter->status_stack, UV(splitter->pos)); */
    /* stack_push(splitter->status_stack, ST_ABSOLUTE_JUMP); */
    /* stack_push(splitter->status_stack, ST_VALUE); */

    /* /\* then do the jump *\/ */
    /* decoder->pos = decoder->body_pos + offset; */
    
}

SRL_STATIC_INLINE void srl_read_alias(srl_splitter_t * splitter) {
    UV offset = srl_read_varint_uv_nocheck(splitter);
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
        splitter->cant_split_here++;
        stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
        stack_push(splitter->status_stack, ST_VALUE);
        stack_push(splitter->status_stack, ST_VALUE);
    }
    return;
}

SRL_STATIC_INLINE void srl_read_array(srl_splitter_t * splitter) {
    UV len = srl_read_varint_uv_nocheck(splitter);
    SRL_SPLITTER_TRACE(" * ARRAY of len, %lu", len);
    while (len-- > 0) {
        splitter->cant_split_here++;
        stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
        stack_push(splitter->status_stack, ST_VALUE);
    }
    return;
}

SRL_STATIC_INLINE void srl_read_regexp(srl_splitter_t * splitter) {
    splitter->cant_split_here++;
    stack_push(splitter->status_stack, ST_CAN_SPLIT_AGAIN);
    stack_push(splitter->status_stack, ST_VALUE);
    stack_push(splitter->status_stack, ST_VALUE);
}

SV*
srl_splitter_next_chunk(srl_splitter_t * splitter)
{

    /* create a new chunk */

    /* zero length Perl string */
    splitter->current_chunk = newSVpvn("", 0);
    splitter->current_chunk_size = 0;
    splitter->current_chunk_start = splitter->pos;
        
    /* srl magic */
    sv_catpvn(splitter->current_chunk, SRL_MAGIC_STRING_HIGHBIT, SRL_MAGIC_STRLEN);

    UV max_len = splitter->input_len;

    int found = _parse(splitter);
    if (found) {
        return splitter->current_chunk;
    }
    return &PL_sv_undef;
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
