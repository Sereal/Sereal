
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "sereal_decoder.h"
#include "srl_protocol.h"

#include "miniz.h"

#define debug_print(fmt, ...)                                           \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,       \
                                __LINE__, __func__, ##__VA_ARGS__); } while (0)


#define STACK_SIZE_INCR 64
#define NUM_BUF_LEN 32

#if WINDOWS || WIN32
#define snprintf  _snprintf
#endif

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)

enum {

    ST_VALUE,
    ST_OBJECT,
    ST_DONE,
    ST_INVALID,

    ST_ARRAY_CLOSE,
    ST_HASH_PAIR,
    ST_HASH_CLOSE

} SrlState;

typedef struct {
    ErlNifEnv*      env;
    sereal_decoder_st*       atoms;

    ERL_NIF_TERM    input;
    ErlNifBinary    bin;

    size_t          bytes_per_iter;
    int             is_partial;

    char*           buffer;
    int             pos;
    int             len;

    char*           st_data;
    int             st_size;
    int             st_top;

    int             header_parsed;
} Decoder;

// -------------------------------------------------------

ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *d);
ERL_NIF_TERM dec_error(Decoder* d, const char* atom);

// -------------------------------------------------------

Decoder*
decoder_new(ErlNifEnv* env)
{
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);

    Decoder* result = enif_alloc_resource(st->resource_decoder, sizeof(Decoder));

    if(result == NULL) {
        return NULL;
    }

    result->header_parsed = 0;
    result->atoms = st;

    result->is_partial = 0;
    result->bytes_per_iter = DEFAULT_BYTES_PER_ITER;

    result->pos = -1;
    result->len = -1;
    result->buffer = NULL;

    result->st_data = (char*) enif_alloc(STACK_SIZE_INCR * sizeof(char));

    if(result->st_data == NULL){
        dec_error(result, "Stack allocation failed");
        return NULL;
    }

    result->st_size = STACK_SIZE_INCR;
    result->st_top = 0;

    memset(result->st_data, ST_INVALID, result->st_size);

    result->st_data[0] = ST_DONE;
    result->st_top++;

    result->st_data[1] = ST_VALUE;
    result->st_top++;

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

    if(decoder->st_data != NULL) {
        enif_free(decoder->st_data);
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

    if(d->st_top > 0){
        result = d->st_data[d->st_top-1];
    
    } else {
        result = 0;
        dec_error(d, "Fetching data from empty stack");
    }

    return result;
}


int
dec_top(Decoder* d)
{
    return d->st_top;
}

void
dec_push(Decoder* d, char val)
{
    if(d->st_top >= d->st_size) {
        int new_sz = d->st_size + STACK_SIZE_INCR;
        char* tmp = (char*) enif_alloc(new_sz * sizeof(char));

        memset(tmp, ST_INVALID, new_sz);
        memcpy(tmp, d->st_data, d->st_size * sizeof(char));

        enif_free(d->st_data);
        
        d->st_data = tmp;
        d->st_size = new_sz;
    }

    d->st_data[d->st_top++] = val;
}

void
dec_pop(Decoder* decoder, char val)
{
    assert(decoder->st_data[decoder->st_top-1] == val && "Popped invalid state.");

    if(decoder->st_top > 0){
        decoder->st_data[decoder->st_top-1] = ST_INVALID;
        decoder->st_top--;

    } else {
        dec_error(decoder, "Stack is empty");
    }
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

    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);

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
    ERL_NIF_TERM val;
    while(enif_get_list_cell(env, opts, &val, &opts)) {

        if(get_bytes_per_iter(env, val, &(decoder->bytes_per_iter))) {
            continue;

        } else {
            return enif_make_badarg(env);
        }
    }

    return decoder_iterate( env
                          , sizeof(arguments) / sizeof(arguments[0]) 
                          , arguments);
}

ERL_NIF_TERM
decoder_iterate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);
    
    Decoder* decoder;
    ErlNifBinary input;

    if( argc != 4 
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
            errorMsg = "Unsupported Sereal protocol";
        }

        if (encoding_flags == SRL_PROTOCOL_ENCODING_RAW) {
            debug_print("Encoding is Raw\n");            
            /* no op */
        }
        else
        if (    encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY
             || encoding_flags == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL) {
            debug_print("Encoding is Snappy\n");            
            errorMsg = "Snappy encoding is not supported. Try zlib";
        } else
        if (encoding_flags == SRL_PROTOCOL_ENCODING_ZLIB)
        {
            debug_print("Encoding is Zlib\n");            
            is_zlib_encoded = 1;
        }
        else
        {
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

         if (is_zlib_encoded) {

             ErlNifSInt64 uncompressed_len = srl_read_varint_int64_nocheck(decoder);
             ErlNifSInt64 compressed_len = srl_read_varint_int64_nocheck(decoder);

             // decoder->pos is now at start of compressed payload
             debug_print("unzipping\n");
             unsigned char *old_pos;
             old_pos = (unsigned char * ) (decoder->buffer + decoder->pos * sizeof(unsigned char) );

             int decompress_ok; 
             mz_ulong tmp; 

             ErlNifBinary uncompressed;

             debug_print(" compressed_len : %ld\n", compressed_len);
             debug_print(" uncompressed_len : %ld\n", uncompressed_len);

             /* allocate a new buffer for uncompressed data*/
             enif_alloc_binary((size_t) uncompressed_len, &uncompressed);

             tmp = uncompressed_len;

             decompress_ok = mz_uncompress(
                                           (unsigned char *) uncompressed.data,
                                           &tmp,
                                           (const unsigned char *)old_pos,
                                           compressed_len
                                           );

             debug_print(" decompress OK: %i\n", decompress_ok);
             if (decompress_ok != Z_OK) { 
                 return dec_error(decoder, "ZLIB decompression of Sereal packet payload failed with error");
             }

             // we fix decoder pos and len, then immediately return, to allow
             // Erlang to iterate again. No need to set input and buffer,
             // because they'll be irrelevant as soon as we return to Erlang,
             // and will be set again at the start of the next iteration

             decoder->pos = 0;
             decoder->len = uncompressed_len;

             ERL_NIF_TERM new_input = enif_make_binary(env, &uncompressed);

            return enif_make_tuple5(
                    env,
                    st->atom_iter,
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
                    st->atom_iter,
                    argv[1],
                    objs,
                    curr
                );
        }

        if ( dec_current(decoder) == ST_DONE ) {
            
            debug_print("current state: ST_DONE\n");
            result = make_ok(st, env, curr);

            goto done;
        }

        if ( dec_current(decoder) == ST_ARRAY_CLOSE ) {

            dec_pop(decoder, ST_ARRAY_CLOSE);
            val = make_array(env, curr);

            if(!enif_get_list_cell(env, objs, &curr, &objs)) {
                result = dec_error(decoder, "Internal_error 2");
                goto done;
            }

            curr = enif_make_list_cell(env, val, curr);
            continue;
        }

        if ( dec_current(decoder) == ST_HASH_PAIR ) {

            dec_pop(decoder, ST_HASH_PAIR);
            
            enif_get_list_cell(env, curr, &val, &curr);
            enif_get_list_cell(env, curr, &key, &curr);
            
            val  = enif_make_tuple2(env, key, val);
            curr = enif_make_list_cell(env, val, curr);

            continue;
        }

        if ( dec_current(decoder) == ST_HASH_CLOSE ) {

            dec_pop(decoder, ST_HASH_CLOSE);

            /* val = make_hash(env, curr, list_len); */
            val = enif_make_tuple1(env, curr);

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
            // TODO: no diagnostics? 
            tag = tag & ~SRL_HDR_TRACK_FLAG;
        }

        switch(dec_current(decoder)) {
            case ST_VALUE:
                debug_print("current state ST_VALUE\n");

                if ( tag <= SRL_HDR_POS_HIGH ) {
                    debug_print("POSITIVE INTEGER tag %d, d->pos = %d\n", (int)tag, decoder->pos);

                    decoder->pos++;
                    dec_pop(decoder, ST_VALUE);

                    val = enif_make_int(decoder->env, (int)tag);
                    curr = enif_make_list_cell(env, val, curr);

                } else if ( tag <= SRL_HDR_NEG_HIGH) {
                    debug_print("NEGATIVE INTEGER tag %d, d->pos = %d\n", (int)tag, decoder->pos);

                    decoder->pos++;
                    dec_pop(decoder, ST_VALUE);
                    
                    /* Small NEGs are from 16 to 31 in reverse order: (-16, -15.. , -1) */
                    val  = enif_make_int(decoder->env, (int)tag - 32);
                    curr = enif_make_list_cell(env, val, curr);
                
                } else if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {

                    len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                    decoder->pos++;

                    debug_print("SHORT_BINARY of len %d, d->pos = %d\n", len, decoder->pos);
                    dec_pop(decoder, ST_VALUE);

                    val  = enif_make_sub_binary(decoder->env, decoder->input, decoder->pos, len);
                    curr = enif_make_list_cell(env, val, curr);
                    
                    debug_print("SHORT_BINARY value = %*c\n", len, decoder->buffer[decoder->pos]);
                    decoder->pos += len;

                } else if ( IS_SRL_HDR_HASHREF(tag) ) {

                    len = tag & 0xF;
                    decoder->pos++;

                    debug_print("SHORT HASHREF of len %d, d->pos = %d\n", len, decoder->pos);
                    dec_pop(decoder, ST_VALUE);

                    // create the temp list to store array elements in it
                    objs = enif_make_list_cell(env, curr, objs);
                    curr = enif_make_list(env, 0);

                    dec_push(decoder, ST_HASH_CLOSE);
                    while (len-- > 0) {
                        dec_push(decoder, ST_HASH_PAIR);
                        dec_push(decoder, ST_VALUE);
                        dec_push(decoder, ST_VALUE);
                    }

                } else if ( IS_SRL_HDR_ARRAYREF(tag) ) {

                    len = tag & 0xF;
                    decoder->pos++;
                    
                    debug_print("SHORT ARRAY of len %d, d->pos = %d\n", len, decoder->pos);
                    dec_pop(decoder, ST_VALUE);

                    // create the temp list to store array elements in it
                    objs = enif_make_list_cell(env, curr, objs);
                    curr = enif_make_list(env, 0);

                    dec_push(decoder, ST_ARRAY_CLOSE);
                    while (len-- > 0) {
                        dec_push(decoder, ST_VALUE);
                    }

                } else {

                 switch(decoder->buffer[decoder->pos]) {

                    case SRL_HDR_VARINT:
                        debug_print("VARINT, d->pos = %d\n", decoder->pos);

                        dec_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        int64_value = srl_read_varint_int64_nocheck(decoder);

                        val  = enif_make_int64(decoder->env, int64_value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        debug_print("VARINT value = %ld\n", int64_value);
                        break;

                    case SRL_HDR_ZIGZAG:
                        debug_print("ZIGZAG, d->pos = %d\n", decoder->pos);

                        dec_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        ErlNifUInt64 _value = srl_read_varint_int64_nocheck(decoder);
                        
                        int64_value = _value >> 1;

                        if (int64_value & 1){
                            int64_value = ~int64_value;
                        }
                        
                        val = enif_make_int64(decoder->env, int64_value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        break;

                    case SRL_HDR_FLOAT:
                        debug_print("FLOAT, d->pos = %d\n", decoder->pos);

                        dec_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        float_value = *((float *) &(decoder->buffer[decoder->pos]));
                        debug_print("FLOAT value = %f\n", float_value);
                        
                        decoder->pos += sizeof(float);

                        val = enif_make_double(decoder->env, (double) float_value);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        break;

                    case SRL_HDR_DOUBLE:
                        debug_print("DOUBLE, d->pos = %d\n", decoder->pos);

                        dec_pop(decoder, ST_VALUE);
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

                        dec_pop(decoder, ST_VALUE);
                        decoder->pos++;

                        curr = enif_make_list_cell(decoder->env, decoder->atoms->atom_undefined, curr);

                        break;

                    case SRL_HDR_BINARY:

                        debug_print("BINARY, d->pos = %d\n", decoder->pos);

                        dec_pop(decoder, ST_VALUE);
                        decoder->pos++;
                        
                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        
                        val  = enif_make_sub_binary(decoder->env, decoder->input, decoder->pos, int64_value);
                        curr = enif_make_list_cell(env, val, curr);
                        
                        decoder->pos += int64_value;
                        
                        break;

                    case SRL_HDR_STR_UTF8:
                        debug_print("STR_UTF8, d->pos = %d\n", decoder->pos);

                        dec_pop(decoder, ST_VALUE);
                        decoder->pos++;
                        
                        int64_value = srl_read_varint_int64_nocheck(decoder);

                        val  = enif_make_sub_binary(decoder->env, decoder->input, decoder->pos, int64_value);
                        curr = enif_make_list_cell(env, val, curr);
                        
                        decoder->pos += int64_value;

                        break;

                    case SRL_HDR_REFN:
                        // TODO: add support
                        debug_print("REFN, d->pos = %d\n", decoder->pos);
                        decoder->pos++;
                        break;

                    case SRL_HDR_HASH:
                        debug_print("HASH d->pos = %d\n", decoder->pos);

                        decoder->pos++;
                        dec_pop(decoder, ST_VALUE);

                        // create the temp list to store array elements in it
                        objs = enif_make_list_cell(env, curr, objs);
                        curr = enif_make_list(env, 0);

                        dec_push(decoder, ST_HASH_CLOSE);
                        
                        // read the hash length
                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        debug_print("HASH: %ld pairs\n", int64_value);

                        while (int64_value-- > 0) {
                            dec_push(decoder, ST_HASH_PAIR);
                            dec_push(decoder, ST_VALUE);
                            dec_push(decoder, ST_VALUE);
                        }

                        break;

                    case SRL_HDR_ARRAY:
                        debug_print("ARRAY, d->pos = %d\n", decoder->pos);

                        decoder->pos++;
                        dec_pop(decoder, ST_VALUE);

                        // create the temp list to store array elements in it
                        objs = enif_make_list_cell(env, curr, objs);
                        curr = enif_make_list(env, 0);

                        dec_push(decoder, ST_ARRAY_CLOSE);

                        // read the array length
                        int64_value = srl_read_varint_int64_nocheck(decoder);
                        while (int64_value-- > 0) {
                            dec_push(decoder, ST_VALUE);
                        }

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

    } else if(decoder->is_partial) {
        result = enif_make_tuple2(env, decoder->atoms->atom_partial, val);
    
    } else {
        result = val;
    }

done:
    consume_timeslice(env, decoder->pos - start, decoder->bytes_per_iter);
    return result;
}

//TODO:  UInt64 isn't a good mapping for varint, should find another one
ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *decoder)
{
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
