#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <math.h>

#include "erl_nif.h"
#include "srl_protocol.h"

#include "snappy/csnappy_compress.c"

#include "miniz.h"

#include "utils.h"

#define PARSE_ERROR(MSG) parse_error( sereal_constants, env, MSG, term )  
                                     
typedef struct {
    ErlNifBinary buffer;
    unsigned int index;

    int options;

    int zlib_level;
    int snappy_level;
    
    int bytes_per_iteration;

} EncoderData;

static int get_type(ErlNifEnv*, SerealConstants*, ERL_NIF_TERM);

static int init_encoder_data(EncoderData* encoder_data);

static ERL_NIF_TERM parse_options(ErlNifEnv* env, SerealConstants *st, EncoderData* encoder_data, ERL_NIF_TERM options);

static void prepend(EncoderData *encoder_data, char* buffer, int len);
static void write_byte(EncoderData* encoder_data, unsigned char c);
static void write_bytes(EncoderData* encoder_data, char cs[]);
static void write_n_bytes(EncoderData* encoder_data, char *cs, int len);
static void write_header(ErlNifEnv* env, EncoderData* encoder_data);

static void encode_varint(ErlNifUInt64);

static ERL_NIF_TERM zlib_compress(SerealConstants *sereal_constants, ErlNifEnv *env, EncoderData* encoder_data);
static ERL_NIF_TERM snappy_compress(SerealConstants *sereal_constants, ErlNifEnv *env, EncoderData* encoder_data);

ERL_NIF_TERM srl_encoder_setup(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]);
ERL_NIF_TERM srl_encoder_parse(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]);

static ERL_NIF_TERM encoder_finalize(SerealConstants *sereal_constants, ErlNifEnv *env, EncoderData *encoder_data);

static void encoder_destroy(ErlNifEnv* env, void* obj);

enum TAGS {
    SMALL_POS,
    SMALL_NEG,
    VARINT,
    ZIGZAG,
    FLOAT, // not supported
    DOUBLE, 
    LONG_DOUBLE, // not supported
    UNDEF,
    BINARY, 
    ATOM,
    BOOLEAN,
    LIST,
    TUPLE,
    MAP,
    TUPLE_MAP
};

#define BUF_SIZE 4096
static char buffer[BUF_SIZE];

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    SerealConstants* st = enif_alloc(sizeof(SerealConstants));
    if(st == NULL) {
        // no diagnostics?
        return 1;
    }

    init_sereal_constants(env, st);
    st->resource_encoder = enif_open_resource_type (
                               env,
                               NULL,
                               "encoder",
                               encoder_destroy,
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
    {"srl_encoder_setup",   2, srl_encoder_setup},
    {"srl_encoder_parse",   2, srl_encoder_parse},
};

ERL_NIF_INIT(encoder, funcs, &load, &reload, &upgrade, &unload);

ERL_NIF_TERM srl_encoder_setup(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]) {

    debug_print("Starting...\n");

    SerealConstants* sereal_constants = enif_priv_data(env);
    EncoderData *encoder_data = (EncoderData*) enif_alloc_resource( sereal_constants->resource_encoder, 
                                                                    sizeof(EncoderData) );
    if ( encoder_data == NULL ) {
        return make_error(sereal_constants, env, "Allocation for EncoderData failed");
    }

    ERL_NIF_TERM error = NULL;
    if ( !init_encoder_data(encoder_data) ) {
        return make_error(sereal_constants, env, "Initialization for EncoderData failed");
    }

    if ( error = parse_options(env, sereal_constants, encoder_data, arguments[1]) ) {
        return error;
    }

    ERL_NIF_TERM encoder_resource = enif_make_resource(env, encoder_data);

    ERL_NIF_TERM result = enif_make_tuple3(
                            env,
                            sereal_constants->atom_partial,
                            arguments[0],
                            encoder_resource
                          );

    debug_print("Releasing resource\n");
    enif_release_resource(encoder_data);

    return result;
}

ERL_NIF_TERM srl_encoder_parse(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]) {

    debug_print("Starting...\n");

    SerealConstants* sereal_constants = enif_priv_data(env);

    ERL_NIF_TERM input            = arguments[0];
    ERL_NIF_TERM encoder_resource = arguments[1];

    int input_len = 0;
    if ( !enif_get_list_length(env, input, &input_len) ) {
        return make_error(sereal_constants, env, "Ill-formed input: failed to get the length");
    }

    EncoderData* encoder_data = NULL;

    if ( !enif_get_resource(env, encoder_resource, sereal_constants->resource_encoder, &encoder_data) ) {
        return make_error(sereal_constants, env, "Failed to convert resource to EncoderData");
    }

    int          index;
    int          intValue;
    char         charValue;
    char        *charPtr;
    double       dblValue;
    unsigned     uintValue;
    ErlNifBinary binValue;
    ErlNifSInt64 int64Value;
    ErlNifUInt64 uint64Value;

    int previous_size = 0;

    while ( input_len-- ) {

        /* 1. Check whether to return to Erlang space */
        int percent = encoder_data->index * 100 / encoder_data->bytes_per_iteration;
        if ( enif_consume_timeslice(env, percent) ) {

            debug_print("Yielding the process\n");
            return enif_make_tuple3(
                        env,
                        sereal_constants->atom_partial,
                        input,
                        encoder_resource
                      );
        }

        debug_print("%d items to decode\n", input_len + 1);

        ERL_NIF_TERM term;

        /* 2. Fetch next item to encode */
        if ( !enif_get_list_cell(env, input, &term, &input) ){
            goto done;
        }

        /* 3. Encode */
        switch (get_type(env, sereal_constants, term)) {

            case SMALL_NEG:
                debug_print("matched type = SMALL-NEG\n");
                if ( !enif_get_int(env, term, &intValue) ) {
                    return PARSE_ERROR( "Failed to extract integer value" );
                }
                intValue += 32;
                write_byte(encoder_data, (char)intValue);
                break;

            case SMALL_POS:
                debug_print("matched type = SMALL-POS\n");
                if ( !enif_get_int(env, term, &intValue) ){
                    return PARSE_ERROR( "Failed to extract integer value" );
                }
                write_byte(encoder_data, (char) intValue);
                break;

            case ZIGZAG:
                debug_print("matched type = ZIGZAG\n");
                if ( !enif_get_int64(env, term, &int64Value) ){
                    return PARSE_ERROR( "Failed to extract integer value" );
                }

                write_byte(encoder_data, SRL_HDR_ZIGZAG);

                /* a. convert to zigzag format */
                int64Value = 2 * abs(int64Value) - 1;

                /* b. encode as varint */
                encode_varint(int64Value);
                write_bytes(encoder_data, buffer);

                break;

            case VARINT:
                debug_print("matched_type = VARINT\n");
                if ( !enif_get_int64(env, term, &int64Value) ){
                    return PARSE_ERROR( "Failed to extract integer value" );
                }

                write_byte(encoder_data, SRL_HDR_VARINT);

                encode_varint(int64Value);
                write_bytes(encoder_data, buffer);

                break;

            case DOUBLE:
                debug_print("matched_type = DOUBLE\n");

                if ( !enif_get_double(env, term, &dblValue) ) {
                    return PARSE_ERROR( "Failed to extract double value" );
                }

                write_byte(encoder_data, SRL_HDR_DOUBLE);

                charPtr = &dblValue; 
                write_n_bytes(encoder_data, charPtr, 8);

                break;

            case UNDEF:
                debug_print("matched_type = UNDEF\n");
                write_byte(encoder_data, SRL_HDR_UNDEF);
                break;

            case BOOLEAN:

                debug_print("matched_type = BOOLEAN\n");

                charValue = !enif_compare(term, sereal_constants->atom_true)
                          ? SRL_HDR_TRUE
                          : SRL_HDR_FALSE;

                write_byte(encoder_data, charValue);

                break;

            case BINARY:
                debug_print("matched_type = BINARY\n");

                if ( !enif_inspect_binary(env, term, &binValue) ) {
                    return PARSE_ERROR( "Ill-formed binary: failed to read" );
                }

                /* default BINARY tag */
                if ( binValue.size <= 31 ) {
                    write_byte(encoder_data, binValue.size + SRL_HDR_SHORT_BINARY_LOW);

                } else {
                    write_byte(encoder_data, SRL_HDR_BINARY);

                    /* encode length */
                    encode_varint(binValue.size);
                    write_bytes(encoder_data, buffer);
                }

                write_n_bytes(encoder_data, binValue.data, binValue.size);

                break;

            case ATOM:
                /* Encode atom as a string */
                debug_print("matched_type = ATOM\n");

                if ( !enif_get_atom_length(env, term, &uintValue, ERL_NIF_LATIN1) ) {
                    return PARSE_ERROR( "Ill-formed atom: failed to get length" );
                }

                /* default BINARY tag */
                if ( uintValue <= 31 ) {
                    write_byte(encoder_data, uintValue + SRL_HDR_SHORT_BINARY_LOW);

                } else {
                    write_byte(encoder_data, SRL_HDR_BINARY);

                    /* encode length */
                    encode_varint(uintValue);
                    write_bytes(encoder_data, buffer);
                }

                if ( !enif_get_atom(env, term, buffer, uintValue + 1, ERL_NIF_LATIN1) ) {
                    return PARSE_ERROR( "Ill-formed atom");
                }
 
                write_bytes(encoder_data, buffer);

                break;

            case LIST /* ARRAY */ : {
                debug_print("matched_type = LIST\n");
                
                if ( !enif_get_list_length(env, term, &uintValue) ) {
                    return PARSE_ERROR( "Ill-formed list: failed to get the length" );
                }

                if ( uintValue <= 15 ) {
                    /* ARRAY_REF0..15 */
                    write_byte(encoder_data, uintValue + SRL_HDR_ARRAYREF_LOW);
                
                }  else {
                    write_byte(encoder_data, SRL_HDR_REFN);    
                    write_byte(encoder_data, SRL_HDR_ARRAY);    

                    /* encode length */
                    encode_varint(uintValue);
                    write_bytes(encoder_data, buffer);
                }

                /* Add list elements to unparse list of input, but before reverse it */
                enif_make_reverse_list(env, term, &term);

                ERL_NIF_TERM head;
                for (index = 0; index < uintValue; index++) {
                    enif_get_list_cell(env, term, &head, &term);
                    input = enif_make_list_cell(env, head, input);
                }
            }
            break;

            case TUPLE: {
                /* we pass tuple(tuple, array) back to erlang to convert it to list, useful for arrays(which are module based) */
                 return enif_make_tuple4(
                        env,
                        sereal_constants->atom_convert,
                        input,
                        encoder_resource,
                        term
                      );
            }
            break;

#if SEREAL_MAP_SUPPORT
            case MAP: {
                debug_print("matched_type = MAP\n");
                
                size_t map_size = 0;
                if ( !enif_get_map_size(env, term, &map_size) ) {
                    return PARSE_ERROR( "Ill-formed map: failed to get size" );
                }

                if ( map_size <= 15 ) {
                    /* HASH_REF0..15 */
                    write_byte(encoder_data, map_size + SRL_HDR_HASHREF_LOW);
                
                }  else {
                    write_byte(encoder_data, SRL_HDR_REFN);    
                    write_byte(encoder_data, SRL_HDR_HASH);    

                    /* encode length */
                    encode_varint(map_size);
                    write_bytes(encoder_data, buffer);
                }

                ErlNifMapIterator iterator;
                if ( !enif_map_iterator_create(env, term, &iterator, ERL_NIF_MAP_ITERATOR_HEAD) ) {
                    return PARSE_ERROR( "Ill-formed map: failed to get iterator" );
                }

                ERL_NIF_TERM key, value;
                do {
                    if ( !enif_map_iterator_get_pair(env, &iterator, &key, &value) ) {
                        return PARSE_ERROR( "Ill-formed map: failed to get pair" );
                    }

                    /* add `value`, then `key` to the input stack */
                    input = enif_make_list_cell(env, value, input);
                    input = enif_make_list_cell(env, key, input);

                } while ( enif_map_iterator_next(env, &iterator) );
            }
            break;
#endif /* SEREAL_MAP_SUPPORT */

            case TUPLE_MAP: {
                debug_print("matched_type = TUPLE-MAP\n");

                ERL_NIF_TERM* tuple;
                if( !enif_get_tuple(env, term, &intValue, &tuple) ){
                    return PARSE_ERROR( "Ill-formed tuple based map" );
                }

                ERL_NIF_TERM pair_list = tuple[0];

                if ( !enif_get_list_length(env, pair_list, &uintValue) ){
                    return PARSE_ERROR( "Ill-formed tuple based map" );
                }
                
                if ( uintValue <= 15 ) {
                    /* HASH_REF0..15 */
                    write_byte(encoder_data, uintValue + SRL_HDR_HASHREF_LOW);
                
                }  else {
                    write_byte(encoder_data, SRL_HDR_REFN);    
                    write_byte(encoder_data, SRL_HDR_HASH);    

                    /* encode length */
                    encode_varint(uintValue);
                    write_bytes(encoder_data, buffer);
                }

                ERL_NIF_TERM head;
                ERL_NIF_TERM tail = pair_list;

                while ( uintValue-- ) {

                    if ( !enif_get_list_cell(env, tail, &head, &tail) ) {
                        return parse_error( sereal_constants,
                                            env,
                                            "Failed to extract map key-value pair",
                                            tail );
                    }

                    ERL_NIF_TERM* key_value;
                    if ( !enif_get_tuple(env, head, &index, &key_value) ) {
                        return parse_error( sereal_constants,
                                            env,
                                            "Wrongly formatted key-value pair",
                                            head );
                    }

                    /* add `value`, then `key` to the input stack */
                    input = enif_make_list_cell(env, key_value[1], input);
                    input = enif_make_list_cell(env, key_value[0], input);
                }
            }
            break;

            default:
               return PARSE_ERROR( "Unknown type to encode" ); 
        }
        
        if ( !enif_get_list_length(env, input, &input_len) ) {
            return parse_error( sereal_constants, 
                                env, 
                                "Input is expected to be list",
                                input );
        }
    }

done:
    return encoder_finalize(sereal_constants, env, encoder_data);
}

static ERL_NIF_TERM encoder_finalize(SerealConstants *sereal_constants, ErlNifEnv *env, EncoderData *encoder_data) {

    debug_print("Finalizing encoder\n");

    ERL_NIF_TERM error;

    if (encoder_data->zlib_level != -1) {
        debug_print("Compressing as zlib level: %d\n", encoder_data->zlib_level);

        int uncompressed_length = encoder_data->index;

        if ( (error = zlib_compress(sereal_constants, env, encoder_data)) ) {
            return error;
        }

        encode_varint(encoder_data->index);
        prepend(encoder_data, buffer, strlen(buffer));

        encode_varint(uncompressed_length);
        prepend(encoder_data, buffer, strlen(buffer));

    } else if ( encoder_data->snappy_level != -1 ) {
        debug_print("Compressing as snappy level: %d\n", encoder_data->snappy_level);

        if ( (error = snappy_compress(sereal_constants, env, encoder_data)) ) {
            return error;
        }

        encode_varint(encoder_data->index);
        prepend(encoder_data, buffer, strlen(buffer));
    }

    debug_print("Writing header\n");
    write_header(env, encoder_data);

    ERL_NIF_TERM binary = enif_make_binary(env, &encoder_data->buffer);
    ERL_NIF_TERM result = enif_make_sub_binary(env, binary, 0, encoder_data->index);

    return result;
}


static int get_type(ErlNifEnv *env, SerealConstants *sereal_constants, ERL_NIF_TERM input){

    int result = -1;

    if (enif_is_number(env, input)) {

        double d;
        ErlNifSInt64 number;

        if (enif_get_int64(env, input, &number)){ 

            if (number < 0) {
                result = number < -16 ? ZIGZAG : SMALL_NEG; 

            } else {
                result = number <= 15 ? SMALL_POS : VARINT;
            }

        } else if (enif_get_double(env, input, &d)){
            result = DOUBLE;
        }
    
    } else if (enif_is_atom(env, input)){

        if (  !enif_compare(input, sereal_constants->atom_true)
           || !enif_compare(input, sereal_constants->atom_false)) {

            result = BOOLEAN;

        } else if ( !enif_compare(input, sereal_constants->atom_undefined) ) {
            result = UNDEF;

        } else {
            result = ATOM;
        }

    } else if (enif_is_binary(env, input)){
       result = BINARY; 

    } else if (enif_is_list(env, input)){
        result = LIST;

#if SEREAL_MAP_SUPPORT
    } else if (enif_is_map(env, input)){
        return MAP;
#endif 

    } else if (enif_is_tuple(env, input)){
        ERL_NIF_TERM* tuple;
        unsigned length = 0;

        enif_get_tuple(env, input, &length, &tuple);

        if (length == 1) {
            ERL_NIF_TERM first = tuple[0];

            result = enif_is_list(env, first)  
                   ? TUPLE_MAP
                   : TUPLE;
            
        } else {
            result = TUPLE;
        }
    }

    return result;
}

static inline size_t _max(size_t a, size_t b) {
    return (size_t []){a,b}[a < b];
}

static void ensure_size(EncoderData* encoder_data, size_t size) {
    if (encoder_data->buffer.size < size) {
        debug_print("Reallocating binary\n");

        /* useful for the case when buffer.size << 1 gets negative */
        size_t length = _max(encoder_data->buffer.size << 1, size);
        enif_realloc_binary(&encoder_data->buffer, length);
    }
}

static void write_byte(EncoderData* encoder_data, unsigned char c) {
    ensure_size(encoder_data, encoder_data->index + 1);
    encoder_data->buffer.data[encoder_data->index++] = c;
}

static void write_bytes(EncoderData* encoder_data, char cs[]) {
    int len = strlen(cs);
    write_n_bytes(encoder_data, cs, len);
}

static void write_n_bytes(EncoderData* encoder_data, char *cs, int len) {
    ensure_size(encoder_data, encoder_data->index + len);

    int i;
    for ( i = 0; i < len; i++ ) {
        write_byte(encoder_data, cs[i]);
    }
}

static ERL_NIF_TERM parse_options(ErlNifEnv *env, SerealConstants *sereal_constants, EncoderData* encoder_data, ERL_NIF_TERM options){

    debug_print("Starting...\n");

    int length;

    if ( !enif_get_list_length(env, options, &length) ) {
        return parse_error( sereal_constants,
                            env, 
                            "Failed to get options list length",
                            options );
    }

    debug_print("Parsing options, %d of options\n", length);

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = options;

    int i;
    for ( i = 0; i < length; i++ ) {

        debug_print("Extracting head\n");
        if ( !enif_get_list_cell(env, tail, &head, &tail) ) {
             return parse_error ( sereal_constants, 
                                 env, 
                                 "Failed to extract next option",
                                 tail );
        }

        debug_print("Reading next option tuple\n");

        int arity;
        ERL_NIF_TERM *option;

        if ( !enif_get_tuple(env, head, &arity, &option) ) {
            return parse_error( sereal_constants, 
                                env,
                                "Options should be in tuple format",
                                head );
        }

        if ( !enif_compare(option[0], sereal_constants->atom_zlib) ) {

            int level = 0;
            if (  arity == 2 && enif_get_int(env, option[1], &level) 
               && level >= 0 && level <= 9 ) {

                encoder_data->zlib_level = level;

            } else {
                return make_error( sereal_constants, 
                                   env, 
                                   "Compression level should be an integer between 0 and 9" );
            }

        } else if ( !enif_compare(option[0], sereal_constants->atom_snappy) ) {

            if(arity == 1) {
                encoder_data->snappy_level = 1;

            } else {
                return make_error( sereal_constants, 
                                   env,
                                   "Snappy should have no options" );
            }

        } else {
            debug_print("Unsupported option\n");
            return parse_error ( sereal_constants,
                                 env, 
                                 "Unsupported option",
                                 option[0] );

        }


    }

    return NULL;
}

static void prepend(EncoderData *encoder_data, char* buffer, int len) {

    ensure_size(encoder_data, encoder_data->index + len);

    /* move data to the tail */
    int i;
    for ( i = encoder_data->index - 1; i >= 0; i-- ) {
        encoder_data->buffer.data[i + len] = encoder_data->buffer.data[i];
    } 

    memcpy(encoder_data->buffer.data, buffer, len);
    encoder_data->index += len;
}

static void write_header(ErlNifEnv* env, EncoderData* encoder_data) {

    debug_print("Writing header\n");

    int HEADER_SIZE = SRL_MAGIC_STRLEN 
                    + 1 /* protocol version */  
                    + 1 /* optional suffix size */; 

    debug_print("Writing magic string\n");

    int i;
    for (i = 0; i < SRL_MAGIC_STRLEN; i++ ) {
        buffer[i] = SRL_MAGIC_STRING_HIGHBIT[i];
    }
    
    debug_print("Writing protocol version\n");
    buffer[4] = SRL_PROTOCOL_VERSION;

    if ( encoder_data->zlib_level != -1 ) {
        buffer[4] |= SRL_PROTOCOL_ENCODING_ZLIB;

    } else if ( encoder_data->snappy_level != -1 ) {
        buffer[4] |= SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL;
    }

    /* optional suffix size */
    buffer[5] = 0;

    prepend(encoder_data, buffer, HEADER_SIZE);
}

void encoder_destroy(ErlNifEnv* env, void* obj) {
}

static void encode_varint(ErlNifUInt64 intValue) {

    int index = 0;
    while (intValue > 0x7F) {
        unsigned char b = (intValue & 0x7F) | 0x80;
        buffer[index++] = b;
        intValue >>= 7;
    }

    buffer[index++] = intValue;
    buffer[index]   = 0;
}

static ERL_NIF_TERM zlib_compress(SerealConstants *sereal_constants, ErlNifEnv *env, EncoderData* encoder_data) {

    debug_print("Zlib compression\n");

    ErlNifBinary compressed; 
    if ( !enif_alloc_binary(encoder_data->buffer.size, &compressed) ) {
        return make_error(sereal_constants, env, "Allocation of compressed buffer failed");
    }

    int status;
    mz_ulong length = compressed.size;
    if ( MZ_OK != (status = compress2( compressed.data, 
                                       &length, 
                                       encoder_data->buffer.data, 
                                       encoder_data->index, 
                                       encoder_data->zlib_level )) ) {
        
        return make_error( sereal_constants,
                           env,
                           mz_error(status) );
    }

    enif_release_binary(&encoder_data->buffer);

    encoder_data->buffer = compressed;
    encoder_data->index  = length;

    debug_print("Compressed length: %u\n", length);

    return NULL;
}

static ERL_NIF_TERM snappy_compress(SerealConstants *sereal_constants, ErlNifEnv *env, EncoderData* encoder_data) {

    debug_print("Snappy compression\n");

    ErlNifBinary compressed; 

    if ( !enif_alloc_binary(encoder_data->buffer.size, &compressed) ) {
        return make_error(sereal_constants, env, "Allocation of compressed buffer failed");
    }

    int length = compressed.size;
    csnappy_compress( encoder_data->buffer.data, 
                      encoder_data->index, 
                      compressed.data, 
                      &length, 
                      buffer, 
                      12 /* buffer size=2^12 */);

    enif_release_binary(&encoder_data->buffer);

    encoder_data->buffer = compressed;
    encoder_data->index  = length;

    debug_print("Compressed length: %u\n", length);

    return NULL;
}

static int init_encoder_data(EncoderData* encoder_data){

    if ( !enif_alloc_binary(BUF_SIZE, &encoder_data->buffer) ) {
        return 0;
    }

    encoder_data->index   = 0;
    encoder_data->options = 0;
    encoder_data->zlib_level = encoder_data->snappy_level = -1;
    encoder_data->bytes_per_iteration = DEFAULT_BYTES_PER_ITERATION;

    return 1;
}


