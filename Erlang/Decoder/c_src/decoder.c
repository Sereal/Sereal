
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "sereal_decoder.h"
#include "srl_protocol.h"

#define debug_print(fmt, ...)                                           \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,       \
                                __LINE__, __func__, ##__VA_ARGS__); } while (0)


#define STACK_SIZE_INC 64
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

    ERL_NIF_TERM    arg;
    ErlNifBinary    bin;

    size_t          bytes_per_iter;
    int             is_partial;

    char*           p;
    int             i;
    int             len;

    char*           st_data;
    int             st_size;
    int             st_top;

    // -------

    int             body_pos;

} Decoder;

// -------------------------------------------------------

ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *d);
ERL_NIF_TERM dec_error(Decoder* d, const char* atom);

// -------------------------------------------------------

Decoder*
decoder_new(ErlNifEnv* env)
{
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);

    Decoder* result = enif_alloc_resource(st->res_dec, sizeof(Decoder));

    if(result == NULL) {
        return NULL;
    }

    result->atoms = st;

    result->bytes_per_iter = DEFAULT_BYTES_PER_ITER;
    result->is_partial = 0;

    result->p = NULL;
    result->len = -1;
    result->i = -1;

    result->st_data = (char*) enif_alloc(STACK_SIZE_INC * sizeof(char));

    if(result->st_data == NULL){
        dec_error(result, "Stack allocation failed");
        return NULL;
    }

    result->st_size = STACK_SIZE_INC;
    result->st_top = 0;

    memset(result->st_data, ST_INVALID, result->st_size);

    result->st_data[0] = ST_DONE;
    result->st_top++;

    result->st_data[1] = ST_VALUE;
    result->st_top++;

    return result;
}

void
dec_init(Decoder* d, ErlNifEnv* env, ERL_NIF_TERM arg, ErlNifBinary* bin)
{
    d->env = env;
    d->arg = arg;

    d->p = (char*) bin->data;
    d->len = bin->size;

    // I'd like to be more forceful on this check so that when
    // we run a second iteration of the decoder we are sure
    // that we're using the same binary. Unfortunately, I don't
    // think there's a value to base this assertion on.
    if(d->i < 0) {
        d->i = 0;

    } else {
        assert(d->i <= d->len && "mismatched binary lengths");
    }
}

void
dec_destroy(ErlNifEnv* env, void* obj)
{
    Decoder* d = (Decoder*) obj;

    if(d->st_data != NULL) {
        enif_free(d->st_data);
    }
}

ERL_NIF_TERM
dec_error(Decoder* d, const char* atom)
{
    ERL_NIF_TERM pos = enif_make_int(d->env, d->i+1);
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
        int new_sz = d->st_size + STACK_SIZE_INC;
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
    ERL_NIF_TERM result = enif_make_list(env, 0);
    ERL_NIF_TERM item;

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
    
    unsigned char tag;

    ErlNifSInt64 test;
    int len;

    float myfloat;
    double mydouble;

    Decoder* decoder;
    ErlNifBinary input;

    if( argc != 4 
     || !enif_inspect_binary(env, argv[0], &input)
     || !enif_get_resource(env, argv[1], st->res_dec, (void**) &decoder)
     || !enif_is_list(env, argv[2])
     || !enif_is_list(env, argv[3])) {

        return enif_make_badarg(env);
    }

    dec_init(decoder, env, argv[0], &input);
    ERL_NIF_TERM objs = argv[2];
    ERL_NIF_TERM curr = argv[3];

    ERL_NIF_TERM key;

    size_t start = decoder->i;
    debug_print("Starting parsing from pos: %zu\n", start);


    ERL_NIF_TERM val = decoder->atoms->atom_null;

    ErlNifSInt64 header_size;

    /* First time: check header */
    if(start == 0) {

        debug_print("Parsing header from %zu\n", start);
        debug_print("Header is : %s\n", input.data);

        const char* errorMsg = NULL;

        int magic_string = 1,
            high_magic_string = 1;

        // SRL_MAGIC_STRLEN + PROTOCOL_LENGTH + OPTIONAL-HEADER-SIZE(at least 1 byte) + DATA(at least 1 byte)
        if (input.size < SRL_MAGIC_STRLEN + 1 + 1 + 1){
            errorMsg = "Sereal lacks data";

            // yeah, this is correct. check usage
        } else if (  (high_magic_string = strncmp((const char*) input.data, SRL_MAGIC_STRING, SRL_MAGIC_STRLEN))
                  && (magic_string = strncmp((const char*) input.data, SRL_MAGIC_STRING_HIGHBIT, SRL_MAGIC_STRLEN))) {

            errorMsg = "Wrong magic string for Sereal";

        } else if (   input.data[SRL_MAGIC_STRLEN] == 0
                  || (input.data[SRL_MAGIC_STRLEN] <= 2 && high_magic_string)
                  || (input.data[SRL_MAGIC_STRLEN] > 2  && magic_string)){

            errorMsg = "Unsupported Sereal protocol";
        }

        if(errorMsg != NULL){
            return dec_error(decoder, errorMsg);
        }

        // move behind magic string and protocol version
        decoder->i += SRL_MAGIC_STRLEN + 1;
    
        header_size = srl_read_varint_int64_nocheck(decoder);

        debug_print("header size: %lu, d->i = %d\n", header_size, decoder->i);
    
        //TODO: add code for processing the header
        decoder->i += header_size;
        
        // now d->i is at the start of the body
        decoder->body_pos = decoder->i;
    }

    ERL_NIF_TERM result;

    while(1) {

        debug_print("==LOOP== iter. state: %d, pos = %d\n", dec_current(decoder), decoder->i);

        /* check for processed data to bypass starvation */
        if(should_yield(decoder->i - start, decoder->bytes_per_iter)) {

            debug_print("==YIELDING== state: %d, pos = %d\n", dec_current(decoder), decoder->i);
            
            consume_timeslice(env, decoder->i - start, decoder->bytes_per_iter);
            
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

        if (decoder->i >= input.size) {
            result = dec_error(decoder, "internal_error 4");
            goto done;
            break;
        }

        tag = decoder->p[decoder->i];

        if (tag & SRL_HDR_TRACK_FLAG) {
            // TODO: no diagnostics? 
            tag = tag & ~SRL_HDR_TRACK_FLAG;
        }

        switch(dec_current(decoder)) {
            case ST_VALUE:
                debug_print("current state ST_VALUE\n");

                if ( tag <= SRL_HDR_POS_HIGH ) {
                    debug_print("POSITIVE INTEGER tag %d, d->i = %d\n", (int)tag, decoder->i);

                    decoder->i++;
                    dec_pop(decoder, ST_VALUE);

                    val = enif_make_int(decoder->env, (int)tag);
                    curr = enif_make_list_cell(env, val, curr);

                } else if ( tag <= SRL_HDR_NEG_HIGH) {
                    debug_print("NEGATIVE INTEGER tag %d, d->i = %d\n", (int)tag, decoder->i);

                    decoder->i++;
                    dec_pop(decoder, ST_VALUE);
                    
                    /* Small NEGs are from 16 to 31 in reverse order: (-16, -15.. , -1) */
                    val  = enif_make_int(decoder->env, (int)tag - 32);
                    curr = enif_make_list_cell(env, val, curr);
                
                } else if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {

                    len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                    decoder->i++;

                    debug_print("SHORT_BINARY of len %d, d->i = %d\n", len, decoder->i);
                    dec_pop(decoder, ST_VALUE);

                    val  = enif_make_sub_binary(decoder->env, decoder->arg, decoder->i, len);
                    curr = enif_make_list_cell(env, val, curr);
                    
                    debug_print("SHORT_BINARY value = %*c\n", len, decoder->p[decoder->i]);
                    decoder->i += len;

                } else if ( IS_SRL_HDR_HASHREF(tag) ) {

                    len = tag & 0xF;
                    decoder->i++;

                    debug_print("SHORT HASHREF of len %d, d->i = %d\n", len, decoder->i);
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
                    decoder->i++;
                    
                    debug_print("SHORT ARRAY of len %d, d->i = %d\n", len, decoder->i);
                    dec_pop(decoder, ST_VALUE);

                    // create the temp list to store array elements in it
                    objs = enif_make_list_cell(env, curr, objs);
                    curr = enif_make_list(env, 0);

                    dec_push(decoder, ST_ARRAY_CLOSE);
                    while (len-- > 0) {
                        dec_push(decoder, ST_VALUE);
                    }

                } else {

                 switch(decoder->p[decoder->i]) {

                    case SRL_HDR_VARINT:
                        debug_print("VARINT, d->i = %d\n", decoder->i);
                        decoder->i++;

                        dec_pop(decoder, ST_VALUE);

                        /* enif_make_double(d->env, dval); */
                        test = srl_read_varint_int64_nocheck(decoder);

                        val  = enif_make_int64(decoder->env, test);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        debug_print("VARINT value = %ld\n", test);
                        break;

                    case SRL_HDR_ZIGZAG:
                        //TODO: add support
                        debug_print("ZIGZAG (not supported), d->i = %d\n", decoder->i);
                        result = dec_error(decoder, "zigzag not supported");

                        goto done;
                        break;

                    case SRL_HDR_FLOAT:
                        debug_print("FLOAT, d->i = %d\n", decoder->i);
                        decoder->i++;

                        dec_pop(decoder, ST_VALUE);

                        myfloat = *((float *) &(decoder->p[decoder->i]));
                        debug_print("FLOAT value = %f\n", myfloat);
                        
                        decoder->i += sizeof(float);

                        val = enif_make_double(decoder->env, (double) myfloat);
                        curr = enif_make_list_cell(decoder->env, val, curr);

                        break;
                    case SRL_HDR_DOUBLE:
                        debug_print("FLOAT, d->i = %d\n", decoder->i);
                        dec_pop(decoder, ST_VALUE);
                        decoder->i++;
                        mydouble = *((double *) &(decoder->p[decoder->i]));
                        debug_print("DOUBLE value = %f\n", mydouble);
                        decoder->i += sizeof(double);
                        val = enif_make_double(decoder->env, (double) mydouble);
                        curr = enif_make_list_cell(decoder->env, val, curr);
                        break;
                    case SRL_HDR_LONG_DOUBLE:
                        debug_print("LONG DOUBLE (not supported), d->i = %d\n", decoder->i);
                        result = dec_error(decoder, "long double not supported");
                        goto done;
                        break;
                    case SRL_HDR_UNDEF:
                        debug_print("UNDEF, d->i = %d\n", decoder->i);
                        dec_pop(decoder, ST_VALUE);
                        decoder->i++;
                        curr = enif_make_list_cell(decoder->env, decoder->atoms->atom_undefined, curr);
                        break;
                    case SRL_HDR_BINARY:
                        debug_print("BINARY, d->i = %d\n", decoder->i);
                        dec_pop(decoder, ST_VALUE);
                        decoder->i++;
                        test = srl_read_varint_int64_nocheck(decoder);
                        val = enif_make_sub_binary(decoder->env, decoder->arg, decoder->i, test);
                        curr = enif_make_list_cell(env, val, curr);
                        decoder->i += test;
                        break;
                    case SRL_HDR_STR_UTF8:
                        debug_print("STR_UTF8, d->i = %d\n", decoder->i);
                        dec_pop(decoder, ST_VALUE);
                        decoder->i++;
                        test = srl_read_varint_int64_nocheck(decoder);
                        val = enif_make_sub_binary(decoder->env, decoder->arg, decoder->i, test);
                        curr = enif_make_list_cell(env, val, curr);
                        decoder->i += test;
                        break;
                    case SRL_HDR_REFN:
                        debug_print("REFN, d->i = %d\n", decoder->i);
                        decoder->i++;
                        break;
                    case SRL_HDR_HASH:
                            debug_print("HASH d->i = %d\n", decoder->i);
                            decoder->i++;
                            dec_pop(decoder, ST_VALUE);

                            // create the temp list to store array elements in it
                            objs = enif_make_list_cell(env, curr, objs);
                            curr = enif_make_list(env, 0);

                            dec_push(decoder, ST_HASH_CLOSE);
                            // read the hash length
                            test = srl_read_varint_int64_nocheck(decoder);
                            debug_print("HASH: %ld pairs\n", test);
                            while (test-- > 0) {
                                dec_push(decoder, ST_HASH_PAIR);
                                dec_push(decoder, ST_VALUE);
                                dec_push(decoder, ST_VALUE);
                            }
                        break;
                    case SRL_HDR_ARRAY:
                            debug_print("ARRAY, d->i = %d\n", decoder->i);
                            decoder->i++;
                            dec_pop(decoder, ST_VALUE);

                            // create the temp list to store array elements in it
                            objs = enif_make_list_cell(env, curr, objs);
                            curr = enif_make_list(env, 0);

                            dec_push(decoder, ST_ARRAY_CLOSE);
                            // read the array length
                            test = srl_read_varint_int64_nocheck(decoder);
                            while (test-- > 0) {
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
    consume_timeslice(env, decoder->i - start, decoder->bytes_per_iter);
    return result;
}

//TODO:  UInt64 isn't a good mapping for varint, should find another one
ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *decoder)
{
    ErlNifUInt64 result = 0;
    unsigned     lshift = 0;

    while (decoder->p[decoder->i] & 0x80) {
        result |= ((ErlNifUInt64)(decoder->p[decoder->i] & 0x7F) << lshift);
        lshift += 7;
        decoder->i++;
    }

    result |= ((ErlNifUInt64)(decoder->p[decoder->i]) << lshift);
    decoder->i++;
    
    return result;
}
