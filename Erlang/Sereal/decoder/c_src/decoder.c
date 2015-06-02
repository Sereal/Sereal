
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "decoder.h"
#include "srl_protocol.h"
#include "utils.h"

#include "snappy/csnappy_decompress.c"
#include "miniz.h"

#include "uthash.h"

#define STACK_SIZE_INCR 64
#define NUM_BUF_LEN 32

#define DECODE_ARRAYREF_TO_LIST 1

#if WINDOWS || WIN32
#define snprintf  _snprintf
#endif

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)

enum {

    ST_DONE,
    ST_VALUE,
    ST_INVALID,

    ST_ARRAY_CLOSE,
    ST_HASH_PAIR,
    ST_HASH_CLOSE,

    ST_JUMP,
    ST_JUMP_FROM_ZERO,

    ST_TRACK

} SrlState;

typedef struct {
    ErlNifEnv*   env;
    SerealConstants*   atoms;

    ERL_NIF_TERM input;
    ErlNifBinary bin;


    char*        buffer;
    int          pos;
    int          len;

    char*        status_stask_data;
    int          status_stack_size;
    int          status_stack_top;

    int*         ref_stack_data;
    int          ref_stack_size;
    int          ref_stack_top;

    int          header_parsed;
    int          body_pos;

    size_t       bytes_per_iter;
    int          options;

} Decoder;

// -------------------------------------------------------

ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *d);
ERL_NIF_TERM dec_error(Decoder* d, const char* atom);

// -------------------------------------------------------

struct reference_struct {
    int pos;                    /* key, the position in the srl document */
    ERL_NIF_TERM term;
    UT_hash_handle hh;         /* makes this structure hashable */
};

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    SerealConstants* st = enif_alloc(sizeof(SerealConstants));
    if(st == NULL) {
        // no diagnostics?
        return 1;
    }

    init_sereal_constants(env, st);

    st->resource_decoder = enif_open_resource_type (
                                env,
                                NULL,
                                "decoder",
                                decoder_destroy,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                NULL
                            );

    *priv = (void*) st;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ErlNifFunc funcs[] =
{
    {"nif_decoder_init",    2, decoder_init},
    {"nif_decoder_iterate", 4, decoder_iterate}
};

ERL_NIF_INIT(decoder, funcs, &load, &reload, &upgrade, &unload);

struct reference_struct *references_hash = NULL;    /* important! initialize to NULL */

void add_reference(int pos_to_add, ERL_NIF_TERM val) {
    struct reference_struct *s;

    s = malloc(sizeof(struct reference_struct));
    s->pos = pos_to_add;
    s->term = val;
    HASH_ADD_INT( references_hash, pos, s );  /* pos: name of key field */
    debug_print("added item from pos %d\n", s->pos);
}

struct reference_struct *find_reference(int pos) {
    struct reference_struct *s;
    HASH_FIND_INT( references_hash, &pos, s );  /* s: output pointer */
    return s;
}


Decoder*
decoder_new(ErlNifEnv* env)
{
    SerealConstants* st = (SerealConstants*) enif_priv_data(env);

    Decoder* result = enif_alloc_resource(st->resource_decoder, sizeof(Decoder));

    if(result == NULL) {
        return NULL;
    }

    result->header_parsed = 0;
    result->atoms = st;

    result->bytes_per_iter = 4096;

    result->pos = -1;
    result->len = -1;
    result->buffer = NULL;

    // status stack
    result->status_stask_data = (char*) enif_alloc(STACK_SIZE_INCR * sizeof(char));

    if(result->status_stask_data == NULL){
        dec_error(result, "Stack allocation failed");
        return NULL;
    }

    result->status_stack_size = STACK_SIZE_INCR;
    result->status_stack_top = 0;
    result->options = 0;

    memset(result->status_stask_data, ST_INVALID, result->status_stack_size);

    result->status_stask_data[0] = ST_DONE;
    result->status_stack_top++;

    result->status_stask_data[1] = ST_VALUE;
    result->status_stack_top++;


    // references stack

    result->ref_stack_data = (int*) enif_alloc(STACK_SIZE_INCR * sizeof(int));

    if(result->ref_stack_data == NULL){
        dec_error(result, "Stack allocation failed");
        return NULL;
    }

    result->ref_stack_size = STACK_SIZE_INCR;
    result->ref_stack_top = 0;

    return result;
}

void
dec_init(Decoder* decoder, ErlNifEnv* env, ERL_NIF_TERM input, ErlNifBinary* bin)
{
    decoder->env   = env;
    decoder->input = input;

    decoder->buffer = (char*) bin->data;
    decoder->len    = bin->size;

    // pos'd like to be more forceful on this check so that when
    // we run a second iteration of the decoder we are sure
    // that we're using the same binary. Unfortunately, I don't
    // think there's a value to base this assertion on.
    if(decoder->pos < 0) {
        decoder->pos = 0;

    } else {
        assert(decoder->pos <= decoder->len && "mismatched binary lengths");
    }
}

void
decoder_destroy(ErlNifEnv* env, void* obj)
{
    Decoder* decoder = (Decoder*) obj;

    if(decoder->status_stask_data != NULL) {
        enif_free(decoder->status_stask_data);
    }
}

ERL_NIF_TERM
dec_error(Decoder* d, const char* atom)
{
    ERL_NIF_TERM pos = enif_make_int(d->env, d->pos+1);
    ERL_NIF_TERM msg = make_atom(d->env, atom);
    ERL_NIF_TERM ret = enif_make_tuple2(d->env, pos, msg);

    return enif_make_tuple2(d->env, d->atoms->atom_error, ret);
}

char
dec_current(Decoder* d)
{
    char result;

    if(d->status_stack_top > 0){
        result = d->status_stask_data[d->status_stack_top-1];
    
    } else {
        result = 0;
        dec_error(d, "Fetching data from empty stack");
    }

    return result;
}


int
status_stack_top(Decoder* d)
{
    return d->status_stack_top;
}

void
status_stack_push(Decoder* d, char val)
{
    if(d->status_stack_top >= d->status_stack_size) {
        int new_sz = d->status_stack_size + STACK_SIZE_INCR;
        char* tmp = (char*) enif_alloc(new_sz * sizeof(char));

        memset(tmp, ST_INVALID, new_sz);
        memcpy(tmp, d->status_stask_data, d->status_stack_size * sizeof(char));

        enif_free(d->status_stask_data);
        
        d->status_stask_data = tmp;
        d->status_stack_size = new_sz;
    }

    d->status_stask_data[d->status_stack_top++] = val;
}

void
status_stack_pop(Decoder* decoder, char val)
{
    assert(decoder->status_stask_data[decoder->status_stack_top-1] == val && "Popped invalid state.");

    if(decoder->status_stack_top > 0){
        decoder->status_stask_data[decoder->status_stack_top-1] = ST_INVALID;
        decoder->status_stack_top--;

    } else {
        dec_error(decoder, "Stack is empty");
    }
}



void
ref_stack_push(Decoder* d, int val)
{
    if(d->ref_stack_top >= d->ref_stack_size) {
        int new_sz = d->ref_stack_size + STACK_SIZE_INCR;
        int* tmp = (int*) enif_alloc(new_sz * sizeof(int));

        memcpy(tmp, d->ref_stack_data, d->ref_stack_size * sizeof(int));

        enif_free(d->ref_stack_data);
        
        d->ref_stack_data = tmp;
        d->ref_stack_size = new_sz;
    }

    d->ref_stack_data[d->ref_stack_top++] = val;
}

int
ref_stack_pop(Decoder* d)
{
    int val = 0;
    if (d->ref_stack_top > 0){
        val = d->ref_stack_data[d->ref_stack_top-1];
        d->ref_stack_top--;
    } else {
        dec_error(d, "Stack is empty");
    }
    return val;
}

ERL_NIF_TERM
make_array(ErlNifEnv* env, ERL_NIF_TERM list)
{
    ERL_NIF_TERM item;

    ERL_NIF_TERM result = enif_make_list(env, 0);
    while(enif_get_list_cell(env, list, &item, &list)) {
        result = enif_make_list_cell(env, item, result);
    }

    return result;
}

ERL_NIF_TERM
decoder_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* argv[] should contain Data and Options */
    if(argc != 2) {
        return enif_make_badarg(env);
    }

    SerealConstants* st = (SerealConstants*) enif_priv_data(env);

    Decoder* decoder = decoder_new(env);

    if(decoder == NULL) {
        return make_error(st, env, "Internal_error 1");
    }

    ERL_NIF_TERM arguments[] = {
        argv[0],
        enif_make_resource(env, decoder),
        enif_make_list(env, 0),
        enif_make_list(env, 0)
    };

    enif_release_resource(decoder);

    ERL_NIF_TERM opts = argv[1];
    if(!enif_is_list(env, opts)) {
        return enif_make_badarg(env);
    }

    // TODO: add support for other options:
    // refuse_snappy, refuse_objects, no_bless_objects, 
    // validate_utf8(?), max_num_hash_entries, incremental(?), use_undef 
    int arity;
    const ERL_NIF_TERM* tuple;

    ERL_NIF_TERM option;
    while (enif_get_list_cell(env, opts, &option, &opts)) {
        
        if(!enif_get_tuple(env, option, &arity, &tuple)) {
            return parse_error( st, env, "Options should be tuple", option );
        }

        if( !enif_compare(tuple[0], st->atom_bytes_per_iter) ) {
            if ( arity == 2 && enif_get_uint(env, tuple[1], &decoder->bytes_per_iter) ) {

            } else{
                return parse_error( st, env, "Bytes per iteration should be a number value", option );
            }

        } else if ( !enif_compare(tuple[0] , st->atom_arrayref_to_list) ) {
            decoder->options |= DECODE_ARRAYREF_TO_LIST;

        } else {
            return parse_error( st, env, "Not supported option: ", tuple[0] );
        }
    }


    return decoder_iterate( env
                          , sizeof(arguments) / sizeof(arguments[0]) 
                          , arguments);
}

ERL_NIF_TERM
decoder_iterate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    SerealConstants* st = (SerealConstants*) enif_priv_data(env);
    
    Decoder* decoder;
    ErlNifBinary input;

    if(   argc != 4 
      || !enif_inspect_binary(env, argv[0], &input)
      || !enif_get_resource(env, argv[1], st->resource_decoder, (void**) &decoder)
      || !enif_is_list(env, argv[2])
      || !enif_is_list(env, argv[3])) {

        return enif_make_badarg(env);
    }

    dec_init(decoder, env, argv[0], &input);
    ERL_NIF_TERM objs = argv[2];
    ERL_NIF_TERM curr = argv[3];

    size_t start = decoder->pos;
    debug_print("Starting parsing from pos: %zu\n", start);

    ErlNifSInt64 header_size;

    /* First time: check header */
    if(! decoder->header_parsed ) {

        debug_print("Parsing header from %zu\n", start);

        decoder->header_parsed = 1;

        const char* errorMsg = NULL;

        int magic_string = 1,
            high_magic_string = 1;

        int version_encoding;
        int version;
        int encoding_flags;

        int is_zlib_encoded = 0;
        int is_snappy_encoded = 0;
        int is_snappyincr_encoded = 0;

        // SRL_MAGIC_STRLEN + PROTOCOL_LENGTH + OPTIONAL-HEADER-SIZE(at least 1 byte) + DATA(at least 1 byte)
        if (decoder->len < SRL_MAGIC_STRLEN + 1 + 1 + 1){
            errorMsg = "Sereal lacks data";

            // yeah, this is correct. check usage
        } else if (  (high_magic_string = strncmp((const char*) decoder->buffer, SRL_MAGIC_STRING, SRL_MAGIC_STRLEN))
                  && (magic_string = strncmp((const char*) decoder->buffer, SRL_MAGIC_STRING_HIGHBIT, SRL_MAGIC_STRLEN))) {

            errorMsg = "Wrong magic string for Sereal";

        }

        version_encoding = decoder->buffer[SRL_MAGIC_STRLEN];
        version = version_encoding & SRL_PROTOCOL_VERSION_MASK;
        encoding_flags = version_encoding & SRL_PROTOCOL_ENCODING_MASK;

        if (      version <= 0
             || ( version < 3 && high_magic_string )
             || ( version > 2 && magic_string ) ) {
            errorMsg = "Unsupported Sereal versions/protocol";
        }

        switch(encoding_flags) {

            case SRL_PROTOCOL_ENCODING_RAW:
                /* no op */
                debug_print("Encoding is Raw\n");            
                break;

            case SRL_PROTOCOL_ENCODING_SNAPPY:
                debug_print("Encoding is Snappy\n");            
                is_snappy_encoded = 1;
                break;

            case SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL:
                debug_print("Encoding is Snappy Incr\n");            
                is_snappy_encoded = is_snappyincr_encoded = 1;            
                break;

            case SRL_PROTOCOL_ENCODING_ZLIB:
                debug_print("Encoding is Zlib\n");            
                is_zlib_encoded = 1;
                break;
            
            default:
                errorMsg = "Sereal document encoded in an unknown format";
        }

        if(errorMsg != NULL){
            return dec_error(decoder, errorMsg);
        }

        debug_print("Header version is %d\n", version);

        // move after magic string and protocol version
        decoder->pos += SRL_MAGIC_STRLEN + 1;
    
        header_size = srl_read_varint_int64_nocheck(decoder);

        debug_print("header size: %lu, d->pos = %d\n", header_size, decoder->pos);
    
        //TODO: add code for processing the header
        decoder->pos += header_size;

        ErlNifSInt64 compressed_len;
        uint32_t uncompressed_len;
        int decompress_ok;
        int header_len;
        ErlNifBinary uncompressed;

        if (version < 2) {
            decoder->body_pos = 0;
        } else {
            decoder->body_pos = decoder->pos - 1;
        }

        if (is_snappy_encoded) {
            if (is_snappyincr_encoded) {
                compressed_len = srl_read_varint_int64_nocheck(decoder);
            } else {
                compressed_len = decoder->len - decoder->pos;
            }            
            debug_print("snappy compressed len %lu\n", compressed_len);

            // decoder->pos is now at start of compressed payload
            debug_print("unsnappying\n");

            unsigned char *old_pos;
            old_pos = (unsigned char * ) (decoder->buffer + decoder->pos * sizeof(unsigned char) );
            header_len = csnappy_get_uncompressed_length(
                (char *)old_pos,
                compressed_len,
                &uncompressed_len
            );
            if (header_len == CSNAPPY_E_HEADER_BAD) {
                return dec_error(decoder, "Invalid Snappy header in Snappy-compressed Sereal packet");
            }

            /* allocate a new buffer for uncompressed data*/
            enif_alloc_binary((size_t) uncompressed_len, &uncompressed);

            decompress_ok = csnappy_decompress_noheader((char *) (old_pos + header_len),
                compressed_len - header_len,
                (char *) uncompressed.data,
                &uncompressed_len);
            if ( decompress_ok != 0 ) {
                return dec_error(decoder, "Snappy decompression of Sereal packet payload failed");
            }
             debug_print(" decompress OK: %s\n", uncompressed.data);

             // we fix decoder pos and len, then immediately return, to allow
             // Erlang to iterate again. No need to set input and buffer,
             // because they'll be irrelevant as soon as we return to Erlang,
             // and will be set again at the start of the next iteration

             decoder->pos = 0;
             decoder->len = uncompressed_len;
             decoder->body_pos = -1;

             ERL_NIF_TERM new_input = enif_make_binary(env, &uncompressed);

             return enif_make_tuple5 (
                    env,
                    st->atom_partial,
                    argv[1],
                    objs,
                    curr,
                    new_input
             );

        } else if (is_zlib_encoded) {

             ErlNifSInt64 uncompressed_len = srl_read_varint_int64_nocheck(decoder);
             ErlNifSInt64 compressed_len = srl_read_varint_int64_nocheck(decoder);

             // decoder->pos is now at start of compressed payload
             debug_print("unzipping\n");
             debug_print(" compressed_len : %ld\n", compressed_len);
             debug_print(" uncompressed_len : %ld\n", uncompressed_len);

             
             mz_ulong tmp = uncompressed_len;
             ErlNifBinary uncompressed;

             /* allocate a new buffer for uncompressed data*/
             enif_alloc_binary((size_t) uncompressed_len, &uncompressed);

             unsigned char *compressed = (unsigned char * ) (decoder->buffer + decoder->pos * sizeof(unsigned char) );

             int decompress_ok = mz_uncompress(
                                           (unsigned char *) uncompressed.data,
                                           &tmp,
                                           (const unsigned char *) compressed,
                                           compressed_len
                                           );

             debug_print(" decompress OK: %i\n", decompress_ok);
             if (decompress_ok != Z_OK) { 
                 return dec_error(decoder, "ZLIB decompression of Sereal packet payload failed");
             }

             // we fix decoder pos and len, then immediately return, to allow
             // Erlang to iterate again. No need to set input and buffer,
             // because they'll be irrelevant as soon as we return to Erlang,
             // and will be set again at the start of the next iteration

             decoder->pos = 0;
             decoder->len = tmp;
             decoder->body_pos = -1;

             ERL_NIF_TERM new_input = enif_make_binary(env, &uncompressed);

             return enif_make_tuple5(
                    env,
                    st->atom_partial,
                    argv[1],
                    objs,
                    curr,
                    new_input
                );
         }


    }

    int len;
    float float_value;
    double double_value;

    ErlNifSInt64 int64_value;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val = decoder->atoms->atom_undefined;
    ERL_NIF_TERM result;

    while(1) {

        debug_print("==LOOP== iter. state: %d, pos = %d\n", dec_current(decoder), decoder->pos);

        /* check for processed data to bypass starvation */
        if(should_yield(decoder->pos - start, decoder->bytes_per_iter)) {

            debug_print("==YIELDING== state: %d, pos = %d\n", dec_current(decoder), decoder->pos);
            
            consume_timeslice(env, decoder->pos - start, decoder->bytes_per_iter);
            
            return enif_make_tuple4(
                    env,
                    st->atom_partial,
                    argv[1],
                    objs,
                    curr
                );
        }

        if ( dec_current(decoder) == ST_DONE ) {
            
            debug_print("current state: ST_DONE\n");

            if(decoder->pos == decoder->len){
                result = make_ok(st, env, curr);

            } else {
                result = dec_error(decoder, "Wrong structured Sereal");
            }

            goto done;
        }

        if ( dec_current(decoder) == ST_TRACK ) {
            
            ERL_NIF_TERM item_to_track;

            ERL_NIF_TERM ignore_me;

            status_stack_pop(decoder, ST_TRACK);

            debug_print("current state: ST_TRACK\n");
            if (! enif_get_list_cell(env, curr, &item_to_track, &ignore_me)) {
                result = dec_error(decoder, "internal_error");
                goto done;
            }
            debug_print("got item\n");
            int pos = ref_stack_pop(decoder);
            debug_print("adding item for pos %d\n", pos);
            add_reference(pos, item_to_track);
            continue;
        }

        if ( dec_current(decoder) == ST_JUMP ) {
            status_stack_pop(decoder, ST_JUMP);
            int jump;
            jump = ref_stack_pop(decoder);
            debug_print("JUMPING TO %d + %d\n", jump, decoder->body_pos);
            decoder->pos = jump + decoder->body_pos;
            continue;
        }

        if ( dec_current(decoder) == ST_JUMP_FROM_ZERO ) {
            status_stack_pop(decoder, ST_JUMP_FROM_ZERO);
            int jump;
            jump = ref_stack_pop(decoder);
            debug_print("JUMPING TO %d\n", jump);
            decoder->pos = jump;
            continue;
        }

        if ( dec_current(decoder) == ST_ARRAY_CLOSE ) {

            status_stack_pop(decoder, ST_ARRAY_CLOSE);

            /* if option is on we convert ARRAYREFs to Erlang lists, not arrays */
            if (decoder->options & DECODE_ARRAYREF_TO_LIST) {
                val = make_array(env, curr);

                if(!enif_get_list_cell(env, objs, &curr, &objs)) {
                    result = dec_error(decoder, "Internal_error 2");
                    goto done;
                }

                curr = enif_make_list_cell(env, val, curr);
                continue;

            } else {
                // arrays are converted in Erlang to moduled arrays
                return enif_make_tuple4 (
                        env,
                        st->atom_convert,
                        argv[1],
                        objs,
                        curr
                 );
            }

        }

        if ( dec_current(decoder) == ST_HASH_PAIR ) {

            status_stack_pop(decoder, ST_HASH_PAIR);

            enif_get_list_cell(env, curr, &val, &curr);
            enif_get_list_cell(env, curr, &key, &curr);
 
#if SEREAL_MAP_SUPPORT
            ERL_NIF_TERM new_map;
            if ( !enif_make_map_put(env, curr, key, val, &new_map) ){
                return make_error(st, env, "Map decoding failed: ill-formed key-value pair");
            } 
            curr = new_map;
#else 
            val  = enif_make_tuple2(env, key, val);
            curr = enif_make_list_cell(env, val, curr);
#endif

            continue;
        }

        if ( dec_current(decoder) == ST_HASH_CLOSE ) {

            status_stack_pop(decoder, ST_HASH_CLOSE);

#if SEREAL_MAP_SUPPORT
            val = curr;
#else
            /* val = make_hash(env, curr, list_len); */
            val = enif_make_tuple1(env, curr);
#endif

            if(!enif_get_list_cell(env, objs, &curr, &objs)) {
                result = dec_error(decoder, "internal_error 3");
                goto done;
            }

            curr = enif_make_list_cell(env, val, curr);
            continue;
        }

        if (decoder->pos >= decoder->len) {
            result = dec_error(decoder, "internal_error 4");
            goto done;
            break;
        }

        unsigned char tag = decoder->buffer[decoder->pos];

        if (tag & SRL_HDR_TRACK_FLAG) {
            tag = tag & ~SRL_HDR_TRACK_FLAG;
            debug_print("tag must be tracked\n");

            ref_stack_push(decoder, decoder->pos - decoder->body_pos);

            status_stack_pop(decoder, ST_VALUE);
            status_stack_push(decoder, ST_TRACK);
            status_stack_push(decoder, ST_VALUE);
            debug_print("pushed ST_TRACK\n");
        }

        switch(dec_current(decoder)) {
            case ST_VALUE:
                debug_print("current state ST_VALUE\n");

                if ( tag <= SRL_HDR_POS_HIGH ) {
                    debug_print("POSITIVE INTEGER tag %d, d->pos = %d\n", (int)tag, decoder->pos);

                    decoder->pos++;
                    status_stack_pop(decoder, ST_VALUE);

                    val = enif_make_int(decoder->env, (int)tag);
                    curr = enif_make_list_cell(env, val, curr);

                } else if ( tag <= SRL_HDR_NEG_HIGH) {
                    debug_print("NEGATIVE INTEGER tag %d, d->pos = %d\n", (int)tag, decoder->pos);

                    decoder->pos++;
                    status_stack_pop(decoder, ST_VALUE);
                    
                    /* Small NEGs are from 16 to 31 in reverse order: (-16, -15.. , -1) */
                    val  = enif_make_int(decoder->env, (int)tag - 32);
                    curr = enif_make_list_cell(env, val, curr);
                
                } else if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {

                    len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                    decoder->pos++;

                    debug_print("SHORT_BINARY of len %d, d->pos = %d\n", len, decoder->pos);
                    status_stack_pop(decoder, ST_VALUE);

                    val  = enif_make_sub_binary(decoder->env, decoder->input, decoder->pos, len);
                    curr = enif_make_list_cell(env, val, curr);
                    
                    debug_print("SHORT_BINARY value = %*c\n", len, tag);
                    decoder->pos += len;

                } else if ( IS_SRL_HDR_HASHREF(tag) ) {

                    len = tag & 0xF;
                    decoder->pos++;

                    debug_print("SHORT HASHREF of len %d, d->pos = %d\n", len, decoder->pos);
                    status_stack_pop(decoder, ST_VALUE);

                    objs = enif_make_list_cell(env, curr, objs);

#if SEREAL_MAP_SUPPORT
                    curr = enif_make_new_map(env);
#else 
                    // create the temp list to store array elements in it
                    curr = enif_make_list(env, 0);
#endif
                    status_stack_push(decoder, ST_HASH_CLOSE);
                    while (len-- > 0) {
                        status_stack_push(decoder, ST_HASH_PAIR);
                        status_stack_push(decoder, ST_VALUE);
                        status_stack_push(decoder, ST_VALUE);
                    }

                } else if ( IS_SRL_HDR_ARRAYREF(tag) ) {

                    len = tag & 0xF;
                    decoder->pos++;
                    
                    debug_print("SHORT ARRAY of len %d, d->pos = %d\n", len, decoder->pos);
                    status_stack_pop(decoder, ST_VALUE);

                    // create the temp list to store array elements in it
                    objs = enif_make_list_cell(env, curr, objs);
                    curr = enif_make_list(env, 0);

                    status_stack_push(decoder, ST_ARRAY_CLOSE);
                    while (len-- > 0) {
                        status_stack_push(decoder, ST_VALUE);
                    }

                } else {

                 switch(tag) {

                    case SRL_HDR_VARINT:
                        debug_print("VARINT, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        int64_value = srl_read_varint_int64_nocheck(decoder);

                        val  = enif_make_int64(decoder->env, int64_value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        debug_print("VARINT value = %ld\n", int64_value);
                        break;

                    case SRL_HDR_ZIGZAG:
                        debug_print("ZIGZAG, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        ErlNifUInt64 _value = srl_read_varint_int64_nocheck(decoder);
                        
                        ErlNifSInt64 value = -( (_value + 1 ) / 2 );

                        val = enif_make_int64(decoder->env, value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        break;

                    case SRL_HDR_FLOAT:
                        debug_print("FLOAT, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        float_value = *((float *) &(decoder->buffer[decoder->pos]));
                        debug_print("FLOAT value = %f\n", float_value);
                        
                        decoder->pos += sizeof(float);

                        val = enif_make_double(decoder->env, (double) float_value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        break;

                    case SRL_HDR_DOUBLE:
                        debug_print("DOUBLE, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        double_value = *((double *) &(decoder->buffer[decoder->pos]));
                        debug_print("DOUBLE value = %f\n", double_value);

                        decoder->pos += sizeof(double);

                        val = enif_make_double(decoder->env, (double) double_value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        break;

                    case SRL_HDR_LONG_DOUBLE:
                        //TODO: add support
                        debug_print("LONG DOUBLE (not supported), d->pos = %d\n", decoder->pos);
                        result = dec_error(decoder, "long double not supported");

                        goto done;
                        break;

                    case SRL_HDR_UNDEF:
                        debug_print("UNDEF, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        curr = enif_make_list_cell(decoder->env, decoder->atoms->atom_undefined, curr);

                        break;

                    case SRL_HDR_BINARY:

                        debug_print("BINARY, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;
                        
                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        
                        val  = enif_make_sub_binary(decoder->env, decoder->input, decoder->pos, int64_value);
                        curr = enif_make_list_cell(env, val, curr);
                        
                        decoder->pos += int64_value;
                        
                        break;

                    case SRL_HDR_STR_UTF8:
                        debug_print("STR_UTF8, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;
                        
                        int64_value = srl_read_varint_int64_nocheck(decoder);

                        val  = enif_make_sub_binary(decoder->env, decoder->input, decoder->pos, int64_value);
                        curr = enif_make_list_cell(env, val, curr);
                        
                        decoder->pos += int64_value;

                        break;

                    case SRL_HDR_REFN:
                        debug_print("REFN - ignored, d->pos = %d\n", decoder->pos);
                        decoder->pos++;
                        break;

                    case SRL_HDR_REFP:
                        debug_print("REFP, d->pos = %d\n", decoder->pos);
                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        int64_value = (int)srl_read_varint_int64_nocheck(decoder);
                        debug_print("REFP, MUST LOOK FOR %d\n", (int)int64_value);
                        
                        struct reference_struct *ref_s;

                        ref_s = find_reference((int)int64_value);
                        if (ref_s == NULL) {
                            result = dec_error(decoder, "failed to find ref");
                            goto done;
                        }
                        debug_print("found the reference!\n");
                        ERL_NIF_TERM term_to_duplicate = ref_s->term;
                        debug_print("P1\n");
                        val = enif_make_copy(env, term_to_duplicate);
                        debug_print("P2\n");
                        curr = enif_make_list_cell(env, val, curr);
                        debug_print("P3\n");

                        break;

                    case SRL_HDR_HASH: {
                        debug_print("HASH d->pos = %d\n", decoder->pos);

                        decoder->pos++;
                        status_stack_pop(decoder, ST_VALUE);

                        // create the temp list to store hash elements in it
                        objs = enif_make_list_cell(env, curr, objs);
                        
#if SEREAL_MAP_SUPPORT
                        curr = enif_make_new_map(env);
#else 
                        curr = enif_make_list(env, 0);
#endif

                        status_stack_push(decoder, ST_HASH_CLOSE);
                        
                        // read the hash length
                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        debug_print("HASH: %ld pairs\n", int64_value);

                        while (int64_value-- > 0) {
                            status_stack_push(decoder, ST_HASH_PAIR);
                            status_stack_push(decoder, ST_VALUE);
                            status_stack_push(decoder, ST_VALUE);
                        }
                    }
                    break;

                    case SRL_HDR_ARRAY:
                        debug_print("ARRAY, d->pos = %d\n", decoder->pos);

                        decoder->pos++;
                        status_stack_pop(decoder, ST_VALUE);

                        // create the temp list to store array elements in it
                        objs = enif_make_list_cell(env, curr, objs);
                        curr = enif_make_list(env, 0);

                        status_stack_push(decoder, ST_ARRAY_CLOSE);

                        // read the array length
                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        while (int64_value-- > 0) {
                            status_stack_push(decoder, ST_VALUE);
                        }
                        break;

                    case SRL_HDR_OBJECT:
                        // TODO: add support
                        debug_print("OBJECT, d->pos = %d\n", decoder->pos);
                        result = dec_error(decoder, "OBJECT not supported");
                        break;

                    case SRL_HDR_OBJECTV:
                        // TODO: add support
                        debug_print("OBJECTV, d->pos = %d\n", decoder->pos);
                        result = dec_error(decoder, "OBJECTV not supported");
                        break;

                    case SRL_HDR_ALIAS:
                        debug_print("ALIAS is handled like COPY, deferring to COPY, d->pos = %d\n", decoder->pos);

                        // no break, we handle ALIAS like COPY

                    case SRL_HDR_COPY:
                        debug_print("COPY, d->pos = %d\n", decoder->pos);
                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        debug_print("COPY, MUST JUMP TO %d + %d\n", decoder->body_pos, (int)int64_value);

                        debug_print("THEN WE'LL JUMP TO HERE: %d\n", decoder->pos);
                        ref_stack_push(decoder, decoder->pos);

                        // jump to the refered position
                        decoder->pos = (int)int64_value + decoder->body_pos;

                        // push the fact that we need to read a value, then come back here.
                        status_stack_push(decoder, ST_JUMP_FROM_ZERO);
                        status_stack_push(decoder, ST_VALUE);

                        break;

                    case SRL_HDR_WEAKEN:
                        debug_print("WEAKEN - ignored, d->pos = %d\n", decoder->pos);
                        decoder->pos++;
                        break;

                    case SRL_HDR_REGEXP:
                        // TODO: add support
                        debug_print("REGEXP, d->pos = %d\n", decoder->pos);
                        result = dec_error(decoder, "REGEXP not supported");
                        break;

                    case SRL_HDR_OBJECT_FREEZE:
                        // TODO: add support
                        debug_print("OBJECT, d->pos = %d\n", decoder->pos);
                        result = dec_error(decoder, "OBJECT not supported");
                        break;

                    case SRL_HDR_OBJECTV_FREEZE:
                        // TODO: add support
                        debug_print("OBJECTV, d->pos = %d\n", decoder->pos);
                        result = dec_error(decoder, "OBJECTV not supported");
                        break;

                    case SRL_HDR_CANONICAL_UNDEF:
                        debug_print("CANONICAL UNDEF, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        curr = enif_make_list_cell(decoder->env, decoder->atoms->atom_undefined, curr);
                        break;

                    case SRL_HDR_FALSE:
                        debug_print("FALSE, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        curr = enif_make_list_cell(decoder->env, decoder->atoms->atom_false, curr);
                        break;

                    case SRL_HDR_TRUE:
                        debug_print("TRUE, d->pos = %d\n", decoder->pos);

                        status_stack_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        curr = enif_make_list_cell(decoder->env, decoder->atoms->atom_true, curr);
                        break;

                    case SRL_HDR_PAD:
                        debug_print("PAD - ignored, d->pos = %d\n", decoder->pos);
                        decoder->pos++;
                        break;
                        
                    default:
                        result = dec_error(decoder, "invalid_sereal");
                        goto done;
                    }
                }
                break;

        default:
            result = dec_error(decoder, "Invalid internal state");
            goto done;
        }

    }

    if(dec_current(decoder) != ST_DONE) {
        result = dec_error(decoder, "Truncated Sereal");

    } else {
        result = val;
    }

done:
    consume_timeslice(env, decoder->pos - start, decoder->bytes_per_iter);
    return result;
}

//TODO:  UInt64 isn't a good mapping for varint, should find another one
ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *decoder) {

    ErlNifUInt64 result = 0;
    unsigned     lshift = 0;

    while (decoder->buffer[decoder->pos] & 0x80) {
        result |= ((ErlNifUInt64)(decoder->buffer[decoder->pos] & 0x7F) << lshift);
        lshift += 7;
        decoder->pos++;
    }

    result |= ((ErlNifUInt64)(decoder->buffer[decoder->pos]) << lshift);
    decoder->pos++;
    
    return result;
}


