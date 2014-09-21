#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "srl_protocol.h"

#include "sereal.h"

#include "utils.h"


#define PARSE_ERROR(MSG) parse_error( sereal_constants, env, MSG, input )  
                                     

typedef struct {
    ErlNifBinary buffer;
    unsigned int index;
    int options;
    
    int bytes_per_iteration;

} EncoderData;

ERL_NIF_TERM encoder_finish(ErlNifEnv *env, EncoderData *encoder_data);

static int get_type(ErlNifEnv*, ERL_NIF_TERM);

static int  parse_options(ErlNifEnv* env, EncoderData* encoder_data, ERL_NIF_TERM options);
static void add_header(ErlNifEnv* env, EncoderData* encoder_data);

static void write_byte(EncoderData* encoder_data, unsigned char c);
static void write_bytes(EncoderData* encoder_data, char cs[]);

static void encode_varint(ErlNifUInt64);

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
    MAP
};

#define BUF_SIZE 4096
static char buffer[BUF_SIZE];

ERL_NIF_TERM encoder_init(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]) {

    debug_print("Starting...\n");

    SerealConstants* sereal_constants = enif_priv_data(env);
    EncoderData *encoder_data = (EncoderData*)enif_alloc_resource( sereal_constants->resource_encoder, 
                                                                   sizeof(EncoderData) );

    if ( encoder_data == NULL ) {
        return make_error(sereal_constants, env, "Allocation for EncoderData failed");
    }

    debug_print("EncoderData is allocated\n");

    if ( !enif_alloc_binary(BUF_SIZE, &encoder_data->buffer) ) {
        return make_error(sereal_constants, env, "Allocation of buffer failed");
    }
    encoder_data->index = 0;

    if ( !parse_options(env, encoder_data, arguments[1]) ){
        return parse_error( sereal_constants, 
                            env, 
                            "Parsing options failed",
                            arguments[1] );
    }

    if (  encoder_data->bytes_per_iteration <= 0 
       || encoder_data->bytes_per_iteration > BUF_SIZE ) {

       encoder_data->bytes_per_iteration = 1024;
    }

    ERL_NIF_TERM encoder_resource = enif_make_resource(env, encoder_data);

    ERL_NIF_TERM result = enif_make_tuple3(
        env,
        sereal_constants->atom_iter,
        arguments[0],
        encoder_resource
    );

    debug_print("Releasing resource\n");
    enif_release_resource(encoder_data);

    return result;
}

ERL_NIF_TERM encoder_iterate(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]) {

    debug_print("Starting...\n");

    SerealConstants* sereal_constants = enif_priv_data(env);

    debug_print("validating input whether a list\n");

    ERL_NIF_TERM items = arguments[0];
    if ( !enif_is_list(env, items)) {
        return make_error(sereal_constants, env, "Wrong argument type passed as input: should be list");
    }

    int items_length = 0;
    if ( !enif_get_list_length(env, items, &items_length) ) {
        return make_error(sereal_constants, env, "Failed to get the length of input");
    }

    ERL_NIF_TERM encoder_resource = arguments[1];
    EncoderData* encoder_data = NULL;

    debug_print("Extracting EncoderData from stored resource\n");
    if ( !enif_get_resource(env, encoder_resource, sereal_constants->resource_encoder, &encoder_data) ) {
        return make_error(sereal_constants, env, "Failed to convert resource to EncoderData");
    }
    
    ERL_NIF_TERM status, value;

    int          index;
    int          intValue;
    char         charValue;
    char        *charPtr;
    double       dblValue;
    unsigned     uintValue;
    ErlNifBinary binValue;

    int previous_size = 0;

    while ( items_length-- ) {

        /* 1. Check whether to return to Erlang space */
        int percent = encoder_data->index * 100 / encoder_data->bytes_per_iteration;
        if ( enif_consume_timeslice(env, percent) ) {

            debug_print("Yielding the process\n");
            return enif_make_tuple3(
                        env,
                        sereal_constants->atom_iter,
                        items,
                        encoder_resource
                      );
        }

        debug_print("%d items to decode\n", items_length + 1);

        ERL_NIF_TERM input;

        /* 2. Fetch next item to encode */
        enif_get_list_cell(env, items, &input, &items);

        /* 3. Encode */
        switch (get_type(env, input)) {

            case SMALL_NEG:
                debug_print("matched type = SMALL-NEG\n");
                if ( !enif_get_int(env, input, &intValue) ) {
                    return PARSE_ERROR( "Failed to extract integer value" );
                }
                intValue += 32;
                write_byte(encoder_data, (char)intValue);
                break;

            case SMALL_POS:
                debug_print("matched type = SMALL-POS\n");
                if ( !enif_get_int(env, input, &intValue) ){
                    return PARSE_ERROR( "Failed to extract integer value" );
                }
                write_byte(encoder_data, (char) intValue);
                break;

            case ZIGZAG:
                debug_print("matched type = ZIGZAG\n");
                if ( !enif_get_int(env, input, &intValue) ){
                    return PARSE_ERROR( "Failed to extract integer value" );
                }

                intValue = -2 * intValue  - 1;
                encode_varint(intValue);
                write_byte(encoder_data, SRL_HDR_ZIGZAG);
                write_bytes(encoder_data, buffer);

                break;

            case VARINT:
                debug_print("matched_type = VARINT\n");
                if ( !enif_get_int(env, input, &intValue) ){
                    return PARSE_ERROR( "Failed to extract integer value" );
                }

                encode_varint(intValue);
                write_byte(encoder_data, SRL_HDR_VARINT);
                write_bytes(encoder_data, buffer);
                break;

            case DOUBLE:
                debug_print("matched_type = DOUBLE\n");

                if ( !enif_get_double(env, input, &dblValue) ) {
                    return PARSE_ERROR( "Failed to extract double value" );
                }

                charPtr = &dblValue; 

                write_byte(encoder_data, SRL_HDR_DOUBLE);
                for (index = 0; index < 8; index++){
                    write_byte(encoder_data, *charPtr);
                    charPtr++;
                }
                break;

            case UNDEF:
                debug_print("matched_type = UNDEF\n");
                
                if ( !enif_get_atom(env, input, buffer, strlen("undefined"), ERL_NIF_LATIN1) ) {
                    return PARSE_ERROR( "Failed to extract `undefined` atom" );
                }

                if ( strcmp(buffer, "undefined") ){
                    return PARSE_ERROR( "Expected `undefined` atom" );
                }

                write_byte(encoder_data, SRL_HDR_UNDEF);
                break;

            case BOOLEAN:

                debug_print("matched_type = BOOLEAN\n");

                if ( !enif_get_atom_length(env, input, &intValue, ERL_NIF_LATIN1) ){
                    return PARSE_ERROR( "Reading length for atom failed, parsing boolean\n" );
                }

                if ( !enif_get_atom(env, input, buffer, intValue, ERL_NIF_LATIN1) ) {
                    return PARSE_ERROR( "Reading atom failed, parsing boolean\n" );
                }
                
                if ( !strncmp(buffer, "true", strlen("true")) ) {
                    write_byte(encoder_data, SRL_HDR_TRUE);

                } else {
                    write_byte(encoder_data, SRL_HDR_FALSE);
                }
                break;

            case BINARY:
                debug_print("matched_type = BINARY\n");

                if ( !enif_inspect_binary(env, input, &binValue) ) {
                    return PARSE_ERROR( "Inspection of binary failed\n" );
                }

                /* default BINARY tag */
                charValue = SRL_HDR_BINARY;
                if ( binValue.size <= 31 ) {
                    charValue = binValue.size + SRL_HDR_SHORT_BINARY_LOW;
                }
                write_byte(encoder_data, charValue);

                for ( index = 0; index < binValue.size; index++ ) {
                    write_byte(encoder_data, binValue.data[index]);
                }

                break;

            case ATOM:
                /* Encode atom as a string */
                debug_print("matched_type = ATOM\n");

                if ( !enif_get_atom_length(env, input, &intValue, ERL_NIF_LATIN1) ) {
                    return PARSE_ERROR( "Failed to get atom length\n" );
                }

                if ( !enif_get_atom(env, input, buffer, intValue, ERL_NIF_LATIN1) ) {
                    return PARSE_ERROR( "Failed to extract atom\n");
                }

                charValue = SRL_HDR_BINARY;
                if ( intValue <= 31 ) {
                    charValue = intValue + SRL_HDR_SHORT_BINARY_LOW;
                }
                write_byte(encoder_data, charValue);
                write_bytes(encoder_data, buffer);

                break;

            case LIST /* ARRAY */ : {
                debug_print("matched_type = LIST\n");
                
                if ( !enif_get_list_length(env, input, &intValue) ) {
                    return PARSE_ERROR( "Extracting list length failed" );
                }

                debug_print("list length is: %d\n", intValue);
                if ( intValue <= 15 ) {
                    /* ARRAY_REF0..15 */
                    charValue = intValue + SRL_HDR_ARRAYREF_LOW;
                    write_byte(encoder_data, charValue);
                
                }  else {
                    write_byte(encoder_data, SRL_HDR_ARRAY);    

                    /* encode length */
                    encode_varint(intValue);
                    write_bytes(encoder_data, buffer);
                }

                ERL_NIF_TERM head, tail;
                tail = input;

                ERL_NIF_TERM reverse = enif_make_list(env, 0);

                for(index = 0; index < intValue; index++){
                    enif_get_list_cell(env, tail, &head, &tail);
                    reverse = enif_make_list_cell(env, head, reverse);
                }

                for(index = 0; index < intValue; index++){
                    enif_get_list_cell(env, reverse, &head, &reverse);
                    items = enif_make_list_cell(env, head, items);
                }
                
                }
                break;

            case TUPLE: {
                /* encode as a list */
                debug_print("matched_type = TUPLE\n");

                ERL_NIF_TERM* tuple;
                if ( !enif_get_tuple(env, input, &intValue, &tuple) ){
                    return PARSE_ERROR( "Extracting tuple failed\n" );
                }

                debug_print("tuple length is %d\n", intValue);

                if ( intValue <= 15 ) {
                    /* ARRAY_REF0..15 */
                    charValue = intValue + SRL_HDR_ARRAYREF_LOW;
                    write_byte(encoder_data, charValue);
                
                }  else {
                    write_byte(encoder_data, SRL_HDR_ARRAY);    

                    /* encode length */
                    encode_varint(intValue);
                    write_bytes(encoder_data, buffer);
                }

                debug_print("adding tuple elements to the items\n");
                while ( intValue-- ) {
                    items = enif_make_list_cell(env, tuple[intValue], items);
                }
            }
            break;

            case MAP: {
                debug_print("matched_type = MAP\n");

                ERL_NIF_TERM* tuple;
                if( !enif_get_tuple(env, input, &intValue, &tuple) ){
                    return PARSE_ERROR( "Wrongly encoded map format\n" );
                }

                ERL_NIF_TERM key_value_list = tuple[0];

                enif_get_list_length(env, key_value_list, &intValue);
                debug_print("map size is %d\n", intValue);
                
                if ( intValue <= 15 ) {
                    /* HASH_REF0..15 */
                    charValue = intValue + SRL_HDR_HASHREF_LOW;
                    write_byte(encoder_data, charValue);
                
                }  else {
                    write_byte(encoder_data, SRL_HDR_HASH);    

                    /* encode length */
                    encode_varint(intValue);
                    write_bytes(encoder_data, buffer);
                }

                ERL_NIF_TERM head, tail;
                tail = key_value_list;

                debug_print("adding map key-value pairs to the items\n");
                while ( intValue-- ) {

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

                    /* add `value`, then `key` to the items stack */
                    items = enif_make_list_cell(env, key_value[1], items);
                    items = enif_make_list_cell(env, key_value[0], items);
                }
            }
            break;

            default:
               return PARSE_ERROR( "Unknown type to encode" ); 
        }
        
        if ( !enif_get_list_length(env, items, &items_length) ) {
            return parse_error( sereal_constants, 
                                env, 
                                "Input is expected to be list",
                                items );
        }
    }

done:
    return encoder_finish(env, encoder_data);
}

ERL_NIF_TERM encoder_finish(ErlNifEnv *env, EncoderData *encoder_data) {
    debug_print("Starting\n");

    int compress = 0;
    if (compress) {
    }

    debug_print("Adding header\n");
    add_header(env, encoder_data);

    ERL_NIF_TERM binary = enif_make_binary(env, &encoder_data->buffer);
    ERL_NIF_TERM result = enif_make_sub_binary(env, binary, 0, encoder_data->index);

    return result;
}


static int get_type(ErlNifEnv *env, ERL_NIF_TERM input){

    int result = -1;

    if (enif_is_number(env, input)) {

        double d;
        ErlNifSInt64 number;

        if(enif_get_int64(env, input, &number)){ 

            if (number < 0) {
                result = number < -16 ? ZIGZAG : SMALL_NEG; 

            } else {
                result = number <= 15 ? SMALL_POS : VARINT;
            }

        } else if(enif_get_double(env, input, &d)){
            result = DOUBLE;
        }
    
    } else if (enif_is_atom(env, input)){

        unsigned length = 0;
        enif_get_atom_length(env, input, &length, ERL_NIF_LATIN1);     

        enif_get_atom(env, input, buffer, length + 1, ERL_NIF_LATIN1);

        if (  !strncmp(buffer, "true", length)
           || !strncmp(buffer, "false", length)) {

            result = BOOLEAN;

        } else if ( !strncmp(buffer, "undefined", length) ) {
            result = UNDEF;

        } else {
            result = ATOM;
        }


    } else if (enif_is_binary(env, input)){
       result = BINARY; 

    } else if (enif_is_list(env, input)){
        result = LIST;

    } else if (enif_is_tuple(env, input)){
        unsigned length = 0;
        ERL_NIF_TERM* tuple;

        if ( !enif_get_tuple(env, input, &length, &tuple) ){
            debug_print("");
        }

        if (length == 1) {
            ERL_NIF_TERM first = tuple[0];

            result = enif_is_list(env, first)  
                   ? MAP
                   : TUPLE;
            
        } else {
            result = TUPLE;
        }
    }

    return result;
}

static void write_byte(EncoderData* encoder_data, unsigned char c) {
    if (encoder_data->index == encoder_data->buffer.size) {
        debug_print("Reallocating binary\n");
        enif_realloc_binary(&encoder_data->buffer, encoder_data->buffer.size << 1);
    }

    encoder_data->buffer.data[encoder_data->index++] = c;
}

static void write_bytes(EncoderData* encoder_data, char cs[]) {
    // TODO: consider reallocating for a batch beforehand instead of doing it in `write_byte`
    int i, len = strlen(cs);
    for ( i = 0; i < len; i++){
        write_byte(encoder_data, cs[i]);
    }
}

static int parse_options(ErlNifEnv *env, EncoderData* encoder_data, ERL_NIF_TERM options_tuple){
    debug_print("Starting...\n");

    int arity, result;
    ERL_NIF_TERM* options; 

    if (enif_get_tuple(env, options_tuple, &arity, &options)){
        while(arity > 0){
            ERL_NIF_TERM flag = options[--arity];
            // do something
            enif_get_atom(env, flag, buffer, BUF_SIZE, ERL_NIF_LATIN1);
            debug_print("Option flag on: %s\n", buffer);
        }

        result = 1;

    } else {
        debug_print("Reading options failed\n");
        result = 0;
    }

    return result;
}

static void add_header(ErlNifEnv* env, EncoderData* encoder_data) {

    debug_print("Adding header\n");

    int HEADER_SIZE = SRL_MAGIC_STRLEN 
                    + 1 /* protocol version */  
                    + 1 /* optional suffix size */; 

    if ( encoder_data->buffer.size <= encoder_data->index + HEADER_SIZE ) {
        debug_print("Reallocating buffer\n");
        enif_realloc_binary( &encoder_data->buffer, encoder_data->index + HEADER_SIZE );
    }    

    debug_print("Moving data to add header");

    int i;
    for ( i = encoder_data->index - 1; i >= 0; i-- ) {
        encoder_data->buffer.data[i + HEADER_SIZE] = encoder_data->buffer.data[i];
    } 

    debug_print("Adding magic string\n");
    for (i = 0; i < SRL_MAGIC_STRLEN; i++ ) {
        encoder_data->buffer.data[i] = SRL_MAGIC_STRING[i];
    }
    
    debug_print("Adding protocol version\n");
    encoder_data->buffer.data[4] = SRL_PROTOCOL_VERSION;

    /* optional suffix size */
    encoder_data->buffer.data[5] = 0;

    encoder_data->index += HEADER_SIZE;
}

void encoder_destroy(ErlNifEnv* env, void* obj) {
    debug_print("Destroying EncoderData\n");
}

static void encode_varint(ErlNifUInt64 intValue) {
    int index = 0;

    while (intValue > 0x7F) {
        unsigned char b = (intValue & 0x7F) | 0x80;
        buffer[index++] = b;
        intValue >>= 7;
    }

    buffer[index++] = intValue;
    buffer[index] = 0;
}
