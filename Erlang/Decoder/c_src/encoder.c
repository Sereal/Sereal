#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "srl_protocol.h"

#include "sereal.h"

#include "utils.h"

#define STACK_SIZE_INCR 128

typedef struct {
    ErlNifBinary buffer;
    unsigned int index;
    int options;

} EncoderData;

static int get_type(ErlNifEnv*, ERL_NIF_TERM);

static void parse_options(ErlNifEnv* env, EncoderData* encoder_data, ERL_NIF_TERM options);
static void add_header(ErlNifEnv* env, EncoderData* encoder_data);

static void write_byte(EncoderData* encoder_data, unsigned char c);
static void write_bytes(EncoderData* encoder_data, char cs[]);

static char top(EncoderData* encoder_data);
static char pop(EncoderData* encoder_data);
static void push(EncoderData* encoder_data, char state);

static void encode_varint(ErlNifUInt64);

enum ST_TYPES {
    ST_DONE,
    ST_VALUE, 
    ST_LIST, // == TUPLE
    ST_MAP
};

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

#define BUF_SIZE 1024
static char buffer[BUF_SIZE];

ERL_NIF_TERM encoder_init(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]) {

    debug_print("In encoder init\n");

    sereal_st* st = enif_priv_data(env);
    debug_print("Size is : %d\n", sizeof(EncoderData));
    EncoderData *encoder_data = (EncoderData*)enif_alloc_resource( st->resource_encoder, sizeof(EncoderData) );

    if ( encoder_data == NULL ) {
        return make_error(st, env, "Allocation failed for encoder data");
    }

    debug_print("Encoder data is allocated\n");

    if ( !enif_alloc_binary(BUF_SIZE, &encoder_data->buffer) ) {
        debug_print("Binary allocation failed\n");
    }

    debug_print("Allocating stack\n");

    encoder_data->st_data = (char*) enif_alloc(STACK_SIZE_INCR * sizeof(char));
    encoder_data->st_index = 0;

    push(encoder_data, ST_DONE);
    push(encoder_data, ST_VALUE);

    encoder_data->index = 0;
    debug_print("Parsing options\n");
    parse_options(env, encoder_data, arguments[1]);

    ERL_NIF_TERM encoder_resource = enif_make_resource(env, encoder_data);

    ERL_NIF_TERM args[] = {
        arguments[0],
        encoder_resource
    };

    debug_print("Releasing resource\n");
    enif_release_resource(encoder_data);

    debug_print("Encoding data\n");
    return encoder_iterate(env, 2, args);
}

ERL_NIF_TERM finish_encoding(ErlNifEnv *env, EncoderData *encoder_data) {
    debug_print("Encoding finished\n");

    int compress = 0;
    if (compress) {
    }

    debug_print("Adding header\n");
    add_header(env, encoder_data);

    ERL_NIF_TERM binary = enif_make_binary(env, &encoder_data->buffer);
    ERL_NIF_TERM result = enif_make_sub_binary(env, binary, 0, encoder_data->index);

    return result;
}

ERL_NIF_TERM encoder_iterate(ErlNifEnv* env, int count, const ERL_NIF_TERM arguments[]) {

    debug_print("In encoder iterate\n");

    sereal_st* st = enif_priv_data(env);

    ERL_NIF_TERM input  = arguments[0];
    ERL_NIF_TERM encoder_resource = arguments[1];
    EncoderData* encoder_data = NULL;

    if ( !enif_get_resource(env, encoder_resource, st->resource_encoder, &encoder_data) ) {
        return make_error(st, env, "Failed to convert resource to object");
    }

    ERL_NIF_TERM status, value;

    int          index;
    int          intValue;
    char         charValue;
    char        *charPtr;
    double       dblValue;
    unsigned     uintValue;
    ErlNifBinary binValue;

    switch(get_type(env, input)) {
        case SMALL_NEG:
            debug_print("SMALL-NEG\n");
            if ( !enif_get_int(env, input, &intValue) ) {
                debug_print("Error getting int value\n");
            }
            intValue += 32;
            write_byte(encoder_data, (char)intValue);
            break;

        case SMALL_POS:
            debug_print("SMALL-POS\n");
            if ( !enif_get_int(env, input, &intValue) ){
                debug_print("Error extracting integer\n");
            }
            write_byte(encoder_data, (char) intValue);
            break;

        case ZIGZAG:
            debug_print("ZIGZAG\n");
            break;

        case VARINT:
            debug_print("VARINT\n");
            if ( !enif_get_int(env, input, &intValue) ){
                debug_print("Error extracting integer\n");
            }

            encode_varint(intValue);
            write_bytes(encoder_data, buffer);
            break;

        case DOUBLE:
            debug_print("DOUBLE\n");

            if ( !enif_get_double(env, input, &dblValue) ) {
                debug_print("Error extracting double\n");  
            }

            charPtr = &dblValue; 

            for (index = 0; index < 8; index++){
                write_byte(encoder_data, *charPtr);
                charPtr++;
            }
            
            break;

        case UNDEF:
            debug_print("UNDEF\n");

            
            if ( !enif_get_atom(env, input, buffer, strlen("undefined"), ERL_NIF_LATIN1) ) {
                debug_print("Can't read undefined atom\n");
            }

            /* sanity check? */
            if ( strcmp(buffer, "undefined") ){
                debug_print("Atom is not undefined however\n");
            }

            write_byte(encoder_data, 0x25);
            break;

        case BOOLEAN:

            debug_print("BOOLEAN");

            if ( !enif_get_atom_length(env, input, &intValue, ERL_NIF_LATIN1) ){
                debug_print("Reading length for atom failed\n");
            }

            if ( !enif_get_atom(env, input, buffer, intValue, ERL_NIF_LATIN1) ) {
                debug_print("Reading atom failed\n");

            }
            
            if ( !strncmp(buffer, "true", strlen("true")) ) {
                write_byte(encoder_data, 0x3b);

            } else {
                write_byte(encoder_data, 0x3a);
            }
            break;

        case BINARY:
            debug_print("BINARY\n");

            if ( !enif_inspect_binary(env, input, &binValue)) {
                debug_print("Can't inspect binary\n");
            }

            /* default BINARY tag */
            charValue = 0x26;
            if ( binValue.size <= 31 ) {
                charValue = binValue.size + 0x60 /* SHORT_BINARY0 */;
            }
            write_byte(encoder_data, charValue);

            for ( index = 0; index < binValue.size; index++ ) {
                write_byte(encoder_data, binValue.data[index]);
            }

            break;

        case ATOM:
            debug_print("ATOM\n");

            if ( !enif_get_atom_length(env, input, &intValue, ERL_NIF_LATIN1) ) {
                debug_print("Can't get atom length\n");
            }

            if ( !enif_get_atom(env, input, buffer, intValue, ERL_NIF_LATIN1) ) {
                debug_print("Couldn't get atom\n");
            }

            buffer[intValue] = 0;

            write_bytes(encoder_data, buffer);

            break;

        case LIST: {
            debug_print("LIST\n");
            
            enif_get_list_length(env, input, &intValue);
            debug_print("Length is: %d\n", intValue);

            if ( intValue <= 15 ) {
                /* ARRAY_REF0..15 */
                charValue = intValue + 0x40 /* ARRAY_REF0 */;
                write_byte(encoder_data, charValue);
            
            }  else {
                write_byte(encoder_data, 0x2b);    

                /* encode length */
                encode_varint(intValue);
                write_bytes(encoder_data, buffer);
            }

            ERL_NIF_TERM head, tail;
            tail = input;

            while ( intValue-- ) {

                enif_get_list_cell(env, tail, &head, &tail);
                ERL_NIF_TERM args[] = {
                    head,
                    encoder_resource                    
                };

                encoder_iterate(env, 2, args);
            }
            
            }
            break;
        case TUPLE: {
            debug_print("TUPLE\n");

            ERL_NIF_TERM* tuple;

            if ( !enif_get_tuple(env, input, &intValue, &tuple) ){
                debug_print("Reading tuple failed\n");
            }

            debug_print("Length is: %d\n", intValue);

            if ( intValue <= 15 ) {
                /* ARRAY_REF0..15 */
                charValue = intValue + 0x40 /* ARRAY_REF0 */;
                write_byte(encoder_data, charValue);
            
            }  else {
                write_byte(encoder_data, 0x2b);    

                /* encode length */
                encode_varint(intValue);
                write_bytes(encoder_data, buffer);
            }

            for (index = 0; index < intValue; index++){
                ERL_NIF_TERM args[] = {
                    tuple[index],
                    encoder_resource                    
                };

                encoder_iterate(env, 2, args);
            }
            
            }
            break;

        case MAP: {
            debug_print("MAP\n");

            ERL_NIF_TERM* tuple;
            if( !enif_get_tuple(env, input, &intValue, &tuple) ){
                debug_print("In map can't get tuple value\n");
            }

            ERL_NIF_TERM key_value_list = tuple[0];

            enif_get_list_length(env, key_value_list, &intValue);
            debug_print("Length is: %d\n", intValue);
            
            if ( intValue <= 15 ) {
                /* HASH_REF0..15 */
                charValue = intValue + 0x50 /* HASH_REF0 */;
                write_byte(encoder_data, charValue);
            
            }  else {
                write_byte(encoder_data, 0x2a);    

                /* encode length */
                encode_varint(intValue);
                write_bytes(encoder_data, buffer);
            }

            ERL_NIF_TERM head, tail;
            tail = key_value_list;

            while ( intValue-- ) {

                enif_get_list_cell(env, tail, &head, &tail);
                debug_print("Processing key-value: %d\n", intValue);

                ERL_NIF_TERM* key_value;
                if ( !enif_get_tuple(env, head, &index, &key_value)) {
                    continue;
                }

                debug_print("Looks like tuple\n");
                /* Encode key */
                ERL_NIF_TERM args[] = {
                    key_value[0],
                    encoder_resource                    
                };

                debug_print("Encoding key\n");
                encoder_iterate(env, 2, args);

                /* Encode value */
                ERL_NIF_TERM args1[] = {
                    key_value[1],
                    encoder_resource                    
                };

                debug_print("Encoding value\n");
                encoder_iterate(env, 2, args1);
            }
            }
            
            
            break;

        default:
            status = make_atom(env, "error");
            debug_print("unsupported type\n");
    }

done:
    return finish_encoding(env, encoder_data);
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

        debug_print("TUPLE LENGTH IS: %d\n", length);
        if (length == 1) {
            ERL_NIF_TERM first = tuple[0];
            debug_print("FIRST: %d=%d\n", length, enif_is_list(env, first));
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
    debug_print("Writing new byte\n");
    if (encoder_data->index == encoder_data->buffer.size) {
        debug_print("Reallocating binary\n");
        enif_realloc_binary(&encoder_data->buffer, encoder_data->buffer.size << 1);
    }

    encoder_data->buffer.data[encoder_data->index++] = c;
}

static void write_bytes(EncoderData* encoder_data, char cs[]) {
    debug_print("Writing new byte\n");

    // TODO: consider reallocating in a batch for a big string instead of doing it in `write_byte`
    int i, len = strlen(cs);
    for ( i = 0; i < len; i++){
        write_byte(encoder_data, cs[i]);
    }
}

static void parse_options(ErlNifEnv *env, EncoderData* encoder_data, ERL_NIF_TERM options_tuple){
    ERL_NIF_TERM* options; 
    int arity;

    if (enif_get_tuple(env, options_tuple, &arity, &options)){
        while(arity > 0){
            ERL_NIF_TERM flag = options[--arity];
            // do something
            enif_get_atom(env, flag, buffer, BUF_SIZE, ERL_NIF_LATIN1);
            debug_print("Option flag on: %s\n", buffer);
        }

    } else {
        debug_print("Reading options failed\n");
    }
}

static void add_header(ErlNifEnv* env, EncoderData* encoder_data) {
}

void encoder_destroy(ErlNifEnv* env, void* obj) {

    debug_print("Destroying encoder\n");

    EncoderData* encoder = (EncoderData*) obj;

    if(encoder->st_data != NULL) {
        enif_free(encoder->st_data);
    }

    debug_print("-End of destroying-\n");
}

static void push(EncoderData* encoder_data, char state) {
    // TODO: check if stack will not overflow
    encoder_data->st_data[encoder_data->st_index++] = state;
}

static char pop(EncoderData* encoder_data) {
    // TODO: check if stack is not empty
    char result = encoder_data->st_data[--encoder_data->st_index];
    return result;
}

static char top(EncoderData* encoder_data) {
    // TODO: check if stack is not empty
    char result = encoder_data->st_data[encoder_data->st_index];
    return result;
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
