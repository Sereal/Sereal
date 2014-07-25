
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
                                __LINE__, __func__, __VA_ARGS__); } while (0)


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

    st_value,
    st_object,
    st_done,
    st_invalid,

    st_array_close,
    st_hash_pair,
    st_hash_close

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
dec_new(ErlNifEnv* env)
{
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);
    Decoder* d = enif_alloc_resource(st->res_dec, sizeof(Decoder));

    if(d == NULL) {
        return NULL;
    }

    d->atoms = st;

    d->bytes_per_iter = DEFAULT_BYTES_PER_ITER;
    d->is_partial = 0;

    d->p = NULL;
    d->len = -1;
    d->i = -1;

    d->st_data = (char*) enif_alloc(STACK_SIZE_INC * sizeof(char));

    if(d->st_data == NULL){
        dec_error(d, "Stack allocation failed");
        return NULL;
    }

    d->st_size = STACK_SIZE_INC;
    d->st_top = 0;

    memset(d->st_data, st_invalid, d->st_size);

    d->st_data[0] = st_done;
    d->st_top++;

    d->st_data[1] = st_value;
    d->st_top++;

    return d;
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
dec_curr(Decoder* d)
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

        memset(tmp, st_invalid, new_sz);
        memcpy(tmp, d->st_data, d->st_size * sizeof(char));

        enif_free(d->st_data);
        
        d->st_data = tmp;
        d->st_size = new_sz;
    }

    d->st_data[d->st_top++] = val;
}

void
dec_pop(Decoder* d, char val)
{
    assert(d->st_data[d->st_top-1] == val && "Popped invalid state.");

    if(d->st_top > 0){
        d->st_data[d->st_top-1] = st_invalid;
        d->st_top--;

    } else {
        dec_error(d, "Stack is empty");
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
decode_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Decoder* d;
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);
    ERL_NIF_TERM tmp_argv[4];
    ERL_NIF_TERM opts;
    ERL_NIF_TERM val;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    d = dec_new(env);
    if(d == NULL) {
        return make_error(st, env, "Internal_error 1");
    }

    tmp_argv[0] = argv[0];
    tmp_argv[1] = enif_make_resource(env, d);
    tmp_argv[2] = enif_make_list(env, 0);
    tmp_argv[3] = enif_make_list(env, 0);

    enif_release_resource(d);

    opts = argv[1];
    if(!enif_is_list(env, opts)) {
        return enif_make_badarg(env);
    }

    while(enif_get_list_cell(env, opts, &val, &opts)) {

        if(get_bytes_per_iter(env, val, &(d->bytes_per_iter))) {
            continue;

        } else {
            return enif_make_badarg(env);
        }
    }

    return decode_iter(env, 4, tmp_argv);
}

ERL_NIF_TERM
decode_iter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Decoder* d;
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);

    ErlNifBinary bin;

    ERL_NIF_TERM objs;
    ERL_NIF_TERM curr;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    ERL_NIF_TERM ret;
    size_t start;

    ErlNifSInt64 header_len;
    unsigned char tag;

    ErlNifSInt64 test;
    int len;

    float myfloat;
    double mydouble;

    if( argc != 4 
     || !enif_inspect_binary(env, argv[0], &bin)
     || !enif_get_resource(env, argv[1], st->res_dec, (void**) &d)
     || !enif_is_list(env, argv[2])
     || !enif_is_list(env, argv[3])) {

        return enif_make_badarg(env);
    }

    dec_init(d, env, argv[0], &bin);
    objs = argv[2];
    curr = argv[3];

    start = d->i;
    debug_print("Starting parsing from pos: %zu\n", start);


    val = d->atoms->atom_null;

    if(start == 0) {
        debug_print("Parsing header from %zu\n", start);
    
        // crappy header reader
        if (bin.size < SRL_MAGIC_STRLEN + 3)
            return dec_error(d, "Invalid Sereal header");

        d->i += 5;
    
        header_len = srl_read_varint_int64_nocheck(d);
    
        // now d->i is at the start of the body
        d->body_pos = d->i;

        debug_print("header len: %lu, d->i = %d\n", header_len, d->i);
   }

    while(1) {

        debug_print("LOOP iter. state: %d, pos = %d\r\n", dec_curr(d), d->i);

        if(should_yield(d->i - start, d->bytes_per_iter)) {

            debug_print("YIELDING state: %d, d->i = %d\r\n", dec_curr(d), d->i);
            
            consume_timeslice(env, d->i - start, d->bytes_per_iter);
            
            return enif_make_tuple4(
                    env,
                    st->atom_iter,
                    argv[1],
                    objs,
                    curr
                );
        }

        if ( dec_curr(d) == st_done ) {
            
            debug_print("current state %s\r\n", "st_done");
            ret = make_ok(st, env, curr);

            goto done;
        }

        if ( dec_curr(d) == st_array_close ) {

            dec_pop(d, st_array_close);
            val = make_array(env, curr);

            if(!enif_get_list_cell(env, objs, &curr, &objs)) {
                ret = dec_error(d, "Internal_error 2");
                goto done;
            }
            curr = enif_make_list_cell(env, val, curr);

            continue;
        }

        if ( dec_curr(d) == st_hash_pair ) {
            dec_pop(d, st_hash_pair);
            
            enif_get_list_cell(env, curr, &val, &curr);
            enif_get_list_cell(env, curr, &key, &curr);
            
            val = enif_make_tuple2(env, key, val);
            curr = enif_make_list_cell(env, val, curr);

            continue;
        }

        if ( dec_curr(d) == st_hash_close ) {
            dec_pop(d, st_hash_close);

            /* val = make_hash(env, curr, list_len); */
            val = enif_make_tuple1(env, curr);
            if(!enif_get_list_cell(env, objs, &curr, &objs)) {
                ret = dec_error(d, "internal_error 3");
                goto done;
            }
            curr = enif_make_list_cell(env, val, curr);
            continue;
        }

        if (d->i >= bin.size) {
            ret = dec_error(d, "internal_error 4");
            goto done;
            break;
        }

        tag = d->p[d->i];
        if (tag & SRL_HDR_TRACK_FLAG) {
            tag = tag & ~SRL_HDR_TRACK_FLAG;
            /* track_sv(d); */
        }

        switch(dec_curr(d)) {
            case st_value:
                debug_print("current state %s\r\n", "st_value");

                if ( tag <= SRL_HDR_POS_HIGH ) {
                    debug_print("POSITIVE INTEGER tag %d, d->i = %d\r\n", (int)tag, d->i);
                    d->i++;
                    dec_pop(d, st_value);
                    val = enif_make_int(d->env, (int)tag);
                    curr = enif_make_list_cell(env, val, curr);
                }

                else if ( tag <= SRL_HDR_NEG_HIGH) {
                    debug_print("NEGATIVE INTEGER tag %d, d->i = %d\r\n", (int)tag, d->i);
                    d->i++;
                    dec_pop(d, st_value);
                    val = enif_make_int(d->env, (int)tag - 32);
                    curr = enif_make_list_cell(env, val, curr);
                }
                
                else if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {
                    len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
                    d->i++;
                    debug_print("SHORT_BINARY of len %d, d->i = %d\r\n", len, d->i);
                    dec_pop(d, st_value);
                    val = enif_make_sub_binary(d->env, d->arg, d->i, len);
                    curr = enif_make_list_cell(env, val, curr);
                    debug_print("SHORT_BINARY value = %*c\r\n", len, d->p[d->i]);
                    d->i += len;
                }
                else if ( IS_SRL_HDR_HASHREF(tag) ) {
                    len = tag & 15;
                    d->i++;
                    debug_print("SHORT HASHREF of len %d, d->i = %d\r\n", len, d->i);
                    dec_pop(d, st_value);

                    // create the temp list to store array elements in it
                    objs = enif_make_list_cell(env, curr, objs);
                    curr = enif_make_list(env, 0);

                    dec_push(d, st_hash_close);
                    while (len-- > 0) {
                        dec_push(d, st_hash_pair);
                        dec_push(d, st_value);
                        dec_push(d, st_value);
                    }
                }
                else if ( IS_SRL_HDR_ARRAYREF(tag) ) {
                    len = tag & 15;
                    d->i++;
                    debug_print("SHORT ARRAY of len %d, d->i = %d\r\n", len, d->i);
                    dec_pop(d, st_value);

                    // create the temp list to store array elements in it
                    objs = enif_make_list_cell(env, curr, objs);
                    curr = enif_make_list(env, 0);

                    dec_push(d, st_array_close);
                    while (len-- > 0) {
                        dec_push(d, st_value);
                    }
                }

                else {

                 switch(d->p[d->i]) {
                    case SRL_HDR_VARINT:
                        debug_print("VARINT, d->i = %d\r\n", d->i);
                        d->i++;
                        dec_pop(d, st_value);
                        /* enif_make_double(d->env, dval); */
                        test = srl_read_varint_int64_nocheck(d);
                        val = enif_make_int64(d->env, test);
                        curr = enif_make_list_cell(d->env, val, curr);
                        debug_print("VARINT value = %ld\r\n", test);
                        break;
                    case SRL_HDR_ZIGZAG:
                        debug_print("ZIGZAG (not supported), d->i = %d\r\n", d->i);
                        ret = dec_error(d, "zigzag not supported");
                        goto done;
                        break;
                    case SRL_HDR_FLOAT:
                        debug_print("FLOAT, d->i = %d\r\n", d->i);
                        dec_pop(d, st_value);
                        d->i++;
                        myfloat = *((float *) &(d->p[d->i]));
                        debug_print("FLOAT value = %f\r\n", myfloat);
                        d->i += sizeof(float);
                        val = enif_make_double(d->env, (double) myfloat);
                        curr = enif_make_list_cell(d->env, val, curr);
                        break;
                    case SRL_HDR_DOUBLE:
                        debug_print("FLOAT, d->i = %d\r\n", d->i);
                        dec_pop(d, st_value);
                        d->i++;
                        mydouble = *((double *) &(d->p[d->i]));
                        debug_print("DOUBLE value = %f\r\n", mydouble);
                        d->i += sizeof(double);
                        val = enif_make_double(d->env, (double) mydouble);
                        curr = enif_make_list_cell(d->env, val, curr);
                        break;
                    case SRL_HDR_LONG_DOUBLE:
                        debug_print("LONG DOUBLE (not supported), d->i = %d\r\n", d->i);
                        ret = dec_error(d, "long double not supported");
                        goto done;
                        break;
                    case SRL_HDR_UNDEF:
                        debug_print("UNDEF, d->i = %d\r\n", d->i);
                        dec_pop(d, st_value);
                        d->i++;
                        curr = enif_make_list_cell(d->env, d->atoms->atom_undefined, curr);
                        break;
                    case SRL_HDR_BINARY:
                        debug_print("BINARY, d->i = %d\r\n", d->i);
                        dec_pop(d, st_value);
                        d->i++;
                        test = srl_read_varint_int64_nocheck(d);
                        val = enif_make_sub_binary(d->env, d->arg, d->i, test);
                        curr = enif_make_list_cell(env, val, curr);
                        d->i += test;
                        break;
                    case SRL_HDR_STR_UTF8:
                        debug_print("STR_UTF8, d->i = %d\r\n", d->i);
                        dec_pop(d, st_value);
                        d->i++;
                        test = srl_read_varint_int64_nocheck(d);
                        val = enif_make_sub_binary(d->env, d->arg, d->i, test);
                        curr = enif_make_list_cell(env, val, curr);
                        d->i += test;
                        break;
                    case SRL_HDR_REFN:
                        debug_print("REFN, d->i = %d\r\n", d->i);
                        d->i++;
                        break;
                    case SRL_HDR_HASH:
                            debug_print("HASH d->i = %d\r\n", d->i);
                            d->i++;
                            dec_pop(d, st_value);

                            // create the temp list to store array elements in it
                            objs = enif_make_list_cell(env, curr, objs);
                            curr = enif_make_list(env, 0);

                            dec_push(d, st_hash_close);
                            // read the hash length
                            test = srl_read_varint_int64_nocheck(d);
                            debug_print("HASH: %ld pairs\r\n", test);
                            while (test-- > 0) {
                                dec_push(d, st_hash_pair);
                                dec_push(d, st_value);
                                dec_push(d, st_value);
                            }
                        break;
                    case SRL_HDR_ARRAY:
                            debug_print("ARRAY, d->i = %d\r\n", d->i);
                            d->i++;
                            dec_pop(d, st_value);

                            // create the temp list to store array elements in it
                            objs = enif_make_list_cell(env, curr, objs);
                            curr = enif_make_list(env, 0);

                            dec_push(d, st_array_close);
                            // read the array length
                            test = srl_read_varint_int64_nocheck(d);
                            while (test-- > 0) {
                                dec_push(d, st_value);
                            }
                        break;

                        
                        default:
                            ret = dec_error(d, "invalid_sereal");
                            goto done;
                    }
                }
                break;

        default:
            ret = dec_error(d, "Invalid internal state");
            goto done;
        }

    }

    if(dec_curr(d) != st_done) {
        ret = dec_error(d, "Truncated Sereal");

    } else if(d->is_partial) {
        ret = enif_make_tuple2(env, d->atoms->atom_partial, val);
    
    } else {
        ret = val;
    }

done:
    consume_timeslice(env, d->i - start, d->bytes_per_iter);
    return ret;
}

//TODO:  UInt64 isn't a good mapping for varint, should find another one
ErlNifUInt64 srl_read_varint_int64_nocheck(Decoder *d)
{
    ErlNifUInt64 int64 = 0;
    unsigned int lshift = 0;

    while (d->p[d->i] & 0x80) {
        int64 |= ((ErlNifUInt64)(d->p[d->i] & 0x7F) << lshift);
        lshift += 7;
        d->i++;
    }

    int64 |= ((ErlNifUInt64)(d->p[d->i]) << lshift);
    d->i++;
    
    return int64;
}
