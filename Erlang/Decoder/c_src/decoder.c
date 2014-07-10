
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"
#include "sereal_decoder.h"
#include "srl_protocol.h"


// -------------------------------------------------------





// -------------------------------------------------------

#define DEBUG 1
#define debug_print(fmt, ...)                                           \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,       \
                                __LINE__, __func__, __VA_ARGS__); } while (0)


#define ERROR(i, msg) make_error(st, env, msg)

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
    st_value=0,
    st_object,
    st_array,
    st_key,
    st_colon,
    st_comma,
    st_done,
    st_invalid,

    st_array_close,
    st_hash_pair,
    st_hash_close
} JsonState;

enum {
    nst_init=0,
    nst_sign,
    nst_mantissa,
    nst_frac0,
    nst_frac1,
    nst_frac,
    nst_esign,
    nst_edigit
} JsonNumState;

typedef struct {
    ErlNifEnv*      env;
    sereal_decoder_st*       atoms;

    ERL_NIF_TERM    arg;
    ErlNifBinary    bin;

    size_t          bytes_per_iter;
    int             is_partial;
    int             return_maps;

    char*           p;
    unsigned char*  u;
    int             i;
    int             len;

    char*           st_data;
    int             st_size;
    int             st_top;

    // -------

    int             body_pos;


} Decoder;

// -------------------------------------------------------

ErlNifSInt64 srl_read_varint_int64_nocheck(Decoder *d);

// -------------------------------------------------------




Decoder*
dec_new(ErlNifEnv* env)
{
    sereal_decoder_st* st = (sereal_decoder_st*) enif_priv_data(env);
    Decoder* d = enif_alloc_resource(st->res_dec, sizeof(Decoder));
    int i;

    if(d == NULL) {
        return NULL;
    }

    d->atoms = st;

    d->bytes_per_iter = DEFAULT_BYTES_PER_ITER;
    d->is_partial = 0;
    d->return_maps = 0;

    d->p = NULL;
    d->u = NULL;
    d->len = -1;
    d->i = -1;

    d->st_data = (char*) enif_alloc(STACK_SIZE_INC * sizeof(char));
    d->st_size = STACK_SIZE_INC;
    d->st_top = 0;

    for(i = 0; i < d->st_size; i++) {
        d->st_data[i] = st_invalid;
    }

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
    d->u = bin->data;
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
    return d->st_data[d->st_top-1];
}

int
dec_top(Decoder* d)
{
    return d->st_top;
}

void
dec_push(Decoder* d, char val)
{
    char* tmp;
    int new_sz;
    int i;

    if(d->st_top >= d->st_size) {
        new_sz = d->st_size + STACK_SIZE_INC;
        tmp = (char*) enif_alloc(new_sz * sizeof(char));
        memcpy(tmp, d->st_data, d->st_size * sizeof(char));
        enif_free(d->st_data);
        d->st_data = tmp;
        d->st_size = new_sz;
        for(i = d->st_top; i < d->st_size; i++) {
            d->st_data[i] = st_invalid;
        }
    }

    d->st_data[d->st_top++] = val;
}

void
dec_pop(Decoder* d, char val)
{
    assert(d->st_data[d->st_top-1] == val && "popped invalid state.");
    d->st_data[d->st_top-1] = st_invalid;
    d->st_top--;
}

/*int
dec_string(Decoder* d, ERL_NIF_TERM* value)
{
    int has_escape = 0;
    int num_escapes = 0;
    int st;
    int ulen;
    int ui;
    int hi;
    int lo;
    char* chrbuf;
    int chrpos;

    if(d->p[d->i] != '\"') {
        return 0;
    }
    d->i++;

    st = d->i;

    while(d->i < d->len) {
        if(d->u[d->i] < 0x20) {
            return 0;
        } else if(d->p[d->i] == '\"') {
            d->i++;
            goto parse;
        } else if(d->p[d->i] == '\\') {
            if(d->i+1 >= d->len) {
                return 0;
            }
            has_escape = 1;
            num_escapes += 1;
            d->i++;
            switch(d->p[d->i]) {
                case '\"':
                case '\\':
                case '/':
                case 'b':
                case 'f':
                case 'n':
                case 'r':
                case 't':
                    d->i++;
                    break;
                case 'u':
                    hi = 0;
                    lo = 0;
                    d->i++;
                    if(d->i + 4 >= d->len) {
                        return 0;
                    }
                    hi = int_from_hex(&(d->u[d->i]));
                    if(hi < 0) {
                        return 0;
                    }
                    d->i += 4;
                    if(hi >= 0xD800 && hi < 0xDC00) {
                        if(d->i + 6 >= d->len) {
                            return 0;
                        }
                        if(d->p[d->i++] != '\\') {
                            return 0;
                        } else if(d->p[d->i++] != 'u') {
                            return 0;
                        }
                        lo = int_from_hex(&(d->u[d->i]));
                        if(lo < 0) {
                            return 0;
                        }
                        hi = unicode_from_pair(hi, lo);
                        if(hi < 0) {
                            return 0;
                        }
                    }
                    hi = utf8_len(hi);
                    if(hi < 0) {
                        return 0;
                    }
                    if(lo == 0) {
                        num_escapes += 5 - hi;
                    } else {
                        num_escapes += 11 - hi;
                    }
                    break;
                default:
                    return 0;
            }
        } else if(d->u[d->i] < 0x80) {
            d->i++;
        } else {
            ulen = utf8_validate(&(d->u[d->i]), d->len - d->i);
            if(ulen < 0) {
                return 0;
            }
            d->i += ulen;
        }
    }

    // The goto above ensures that we only
    // hit this when a string is not terminated
    // correctly.
    return 0;

parse:
    if(!has_escape) {
        *value = enif_make_sub_binary(d->env, d->arg, st, (d->i - st - 1));
        return 1;
    }

    hi = 0;
    lo = 0;

    ulen = (d->i - 1) - st - num_escapes;
    chrbuf = (char*) enif_make_new_binary(d->env, ulen, value);
    chrpos = 0;
    ui = st;
    while(ui < d->i - 1) {
        if(d->p[ui] != '\\') {
            chrbuf[chrpos++] = d->p[ui++];
            continue;
        }
        ui++;
        switch(d->p[ui]) {
            case '\"':
            case '\\':
            case '/':
                chrbuf[chrpos++] = d->p[ui];
                ui++;
                break;
            case 'b':
                chrbuf[chrpos++] = '\b';
                ui++;
                break;
            case 'f':
                chrbuf[chrpos++] = '\f';
                ui++;
                break;
            case 'n':
                chrbuf[chrpos++] = '\n';
                ui++;
                break;
            case 'r':
                chrbuf[chrpos++] = '\r';
                ui++;
                break;
            case 't':
                chrbuf[chrpos++] = '\t';
                ui++;
                break;
            case 'u':
                ui++;
                hi = int_from_hex(&(d->u[ui]));
                if(hi < 0) {
                    return 0;
                }
                if(hi >= 0xD800 && hi < 0xDC00) {
                    lo = int_from_hex(&(d->u[ui+6]));
                    if(lo < 0) {
                        return 0;
                    }
                    hi = unicode_from_pair(hi, lo);
                    ui += 10;
                } else {
                    ui += 4;
                }
                hi = unicode_to_utf8(hi, (unsigned char*) chrbuf+chrpos);
                if(hi < 0) {
                    return 0;
                }
                chrpos += hi;
                break;
            default:
                return 0;
        }
    }

    return 1;
}*/

/*int
dec_number(Decoder* d, ERL_NIF_TERM* value)
{
    ERL_NIF_TERM num_type = d->atoms->atom_error;
    char state = nst_init;
    char nbuf[NUM_BUF_LEN];
    int st = d->i;
    int has_frac = 0;
    int has_exp = 0;
    double dval;
    long lval;

    while(d->i < d->len) {
        switch(state) {
            case nst_init:
                switch(d->p[d->i]) {
                    case '-':
                        state = nst_sign;
                        d->i++;
                        break;
                    case '0':
                        state = nst_frac0;
                        d->i++;
                        break;
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        state = nst_mantissa;
                        d->i++;
                        break;
                    default:
                        return 0;
                }
                break;

            case nst_sign:
                switch(d->p[d->i]) {
                    case '0':
                        state = nst_frac0;
                        d->i++;
                        break;
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        state = nst_mantissa;
                        d->i++;
                        break;
                    default:
                        return 0;
                }
                break;

            case nst_mantissa:
                switch(d->p[d->i]) {
                    case '.':
                        state = nst_frac1;
                        d->i++;
                        break;
                    case 'e':
                    case 'E':
                        state = nst_esign;
                        d->i++;
                        break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        d->i++;
                        break;
                    default:
                        goto parse;
                }
                break;

            case nst_frac0:
                switch(d->p[d->i]) {
                    case '.':
                        state = nst_frac1;
                        d->i++;
                        break;
                    case 'e':
                    case 'E':
                        state = nst_esign;
                        d->i++;
                        break;
                    default:
                        goto parse;
                }
                break;

            case nst_frac1:
                has_frac = 1;
                switch(d->p[d->i]) {
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        state = nst_frac;
                        d->i++;
                        break;
                    default:
                        goto parse;
                }
                break;

            case nst_frac:
                switch(d->p[d->i]) {
                    case 'e':
                    case 'E':
                        state = nst_esign;
                        d->i++;
                        break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        d->i++;
                        break;
                    default:
                        goto parse;
                }
                break;

            case nst_esign:
                has_exp = 1;
                switch(d->p[d->i]) {
                    case '-':
                    case '+':
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        state = nst_edigit;
                        d->i++;
                        break;
                    default:
                        return 0;
                }
                break;

            case nst_edigit:
                switch(d->p[d->i]) {
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        d->i++;
                        break;
                    default:
                        goto parse;
                }
                break;

            default:
                return 0;
        }
    }

parse:

    switch(state) {
        case nst_init:
        case nst_sign:
        case nst_frac1:
        case nst_esign:
            return 0;
        default:
            break;
    }

    errno = 0;

    if(d->i - st < NUM_BUF_LEN) {
        memset(nbuf, 0, NUM_BUF_LEN);
        memcpy(nbuf, &(d->p[st]), d->i - st);

        if(has_frac || has_exp) {
            dval = strtod(nbuf, NULL);
            if(errno != ERANGE) {
                *value = enif_make_double(d->env, dval);
                return 1;
            }
        } else {
            lval = strtol(nbuf, NULL, 10);
            if(errno != ERANGE) {
                *value = enif_make_int64(d->env, lval);
                return 1;
            }
        }
    }

    if(!has_frac && !has_exp) {
        num_type = d->atoms->atom_bignum;
    } else if(!has_frac && has_exp) {
        num_type = d->atoms->atom_bignum_e;
    } else {
        num_type = d->atoms->atom_bigdbl;
    }

    d->is_partial = 1;
    *value = enif_make_sub_binary(d->env, d->arg, st, d->i - st);
    *value = enif_make_tuple2(d->env, num_type, *value);
    return 1;
}*/

/*ERL_NIF_TERM
make_empty_object(ErlNifEnv* env, int ret_map)
{
#if MAP_TYPE_PRESENT
    if(ret_map) {
        return enif_make_new_map(env);
    }
#endif

    return enif_make_tuple1(env, enif_make_list(env, 0));
}
*/

/*int
make_object(ErlNifEnv* env, ERL_NIF_TERM pairs, ERL_NIF_TERM* out, int ret_map)
{
    ERL_NIF_TERM ret;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

#if MAP_TYPE_PRESENT
    if(ret_map) {
        ret = enif_make_new_map(env);
        while(enif_get_list_cell(env, pairs, &val, &pairs)) {
            if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
                assert(0 == 1 && "Unbalanced object pairs.");
            }
            if(!enif_make_map_put(env, ret, key, val, &ret)) {
                return 0;
            }
        }
        *out = ret;
        return 1;
    }
#endif

    ret = enif_make_list(env, 0);
    while(enif_get_list_cell(env, pairs, &val, &pairs)) {
        if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
            assert(0 == 1 && "Unbalanced object pairs.");
        }
        val = enif_make_tuple2(env, key, val);
        ret = enif_make_list_cell(env, val, ret);
    }
    *out = enif_make_tuple1(env, ret);

    return 1;
}
*/


ERL_NIF_TERM
make_array(ErlNifEnv* env, ERL_NIF_TERM list)
{
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    ERL_NIF_TERM item;

    while(enif_get_list_cell(env, list, &item, &list)) {
        ret = enif_make_list_cell(env, item, ret);
    }

    return ret;
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
        return make_error(st, env, "internal_error 1");
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
        } else if(enif_compare(val, d->atoms->atom_return_maps) == 0) {
#if MAP_TYPE_PRESENT
            d->return_maps = 1;
#else
            return enif_make_badarg(env);
#endif
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

    if(argc != 4) {
        return enif_make_badarg(env);
    } else if(!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    } else if(!enif_get_resource(env, argv[1], st->res_dec, (void**) &d)) {
        return enif_make_badarg(env);
    } else if(!enif_is_list(env, argv[2])) {
        return enif_make_badarg(env);
    } else if(!enif_is_list(env, argv[3])) {
        return enif_make_badarg(env);
    }

    dec_init(d, env, argv[0], &bin);
    objs = argv[2];
    curr = argv[3];

    start = d->i;
    debug_print("Starting parsing from pos: %zu\r\n", start);


    val = d->atoms->atom_null;

    if(start == 0) {
        debug_print("Parsing header from %zu \r\n", start);
        // crappy header reader
        if (bin.size < SRL_MAGIC_STRLEN + 3)
            return dec_error(d, "invalid_sereal_header");
        d->i += 5;
        header_len= srl_read_varint_int64_nocheck(d);
        // now d->i is at the start of the body
        d->body_pos = d->i;

        debug_print("header len: %lu, d->i = %d\r\n", header_len, d->i);
 
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
                ret = dec_error(d, "internal_error 2");
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

        /* else if ( IS_SRL_HDR_SHORT_BINARY(tag) ) { */
        /*     len= (STRLEN)SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag); */
        /*     ASSERT_BUF_SPACE(dec, len, " while reading ascii string"); */
        /*     sv_setpvn(into,(char*)dec->pos,len); */
        /*     dec->pos += len; */
        /* } else if ( IS_SRL_HDR_HASHREF(tag) ) { */
        /*     srl_read_hash(aTHX_ dec, into, tag); */
        /*     is_ref = 1; */
        /* } else if ( IS_SRL_HDR_ARRAYREF(tag) ) { */
        /* srl_read_array(aTHX_ dec, into, tag); */
        /* is_ref = 1; */
        /* } else { */

/* static void */
/* track_sv(Decoder *d, U8 *track_pos, ERL_NIF_TERM val) */
/* { */
/*     PTABLE_store(dec->ref_seenhash, (void *)(track_pos - dec->body_pos), (void *)sv); */
/* } */

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
                    /* switch(d->p[d->i]) { */
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

                    
    /*                 case ' ': */
    /*                 case '\n': */
    /*                 case '\r': */
    /*                 case '\t': */
    /*                     d->i++; */
    /*                     break; */
    /*                 case 'n': */
    /*                     if(d->i + 3 >= d->len) { */
    /*                         ret = dec_error(d, "invalid_literal"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(memcmp(&(d->p[d->i]), "null", 4) != 0) { */
    /*                         ret = dec_error(d, "invalid_literal"); */
    /*                         goto done; */
    /*                     } */
    /*                     val = d->atoms->atom_null; */
    /*                     dec_pop(d, st_value); */
    /*                     d->i += 4; */
    /*                     break; */
    /*                 case 't': */
    /*                     if(d->i + 3 >= d->len) { */
    /*                         ret = dec_error(d, "invalid_literal"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(memcmp(&(d->p[d->i]), "true", 4) != 0) { */
    /*                         ret = dec_error(d, "invalid_literal"); */
    /*                         goto done; */
    /*                     } */
    /*                     val = d->atoms->atom_true; */
    /*                     dec_pop(d, st_value); */
    /*                     d->i += 4; */
    /*                     break; */
    /*                 case 'f': */
    /*                     if(d->i + 4 >= bin.size) { */
    /*                         ret = dec_error(d, "invalid_literal"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(memcmp(&(d->p[d->i]), "false", 5) != 0) { */
    /*                         ret = dec_error(d, "invalid_literal"); */
    /*                         goto done; */
    /*                     } */
    /*                     val = d->atoms->atom_false; */
    /*                     dec_pop(d, st_value); */
    /*                     d->i += 5; */
    /*                     break; */
    /*                 case '\"': */
    /*                     if(!dec_string(d, &val)) { */
    /*                         ret = dec_error(d, "invalid_string"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_value); */
    /*                     break; */
    /*                 case '-': */
    /*                 case '0': */
    /*                 case '1': */
    /*                 case '2': */
    /*                 case '3': */
    /*                 case '4': */
    /*                 case '5': */
    /*                 case '6': */
    /*                 case '7': */
    /*                 case '8': */
    /*                 case '9': */
    /*                     if(!dec_number(d, &val)) { */
    /*                         ret = dec_error(d, "invalid_number"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_value); */
    /*                     break; */
    /*                 case '{': */
    /*                     dec_push(d, st_object); */
    /*                     dec_push(d, st_key); */
    /*                     objs = enif_make_list_cell(env, curr, objs); */
    /*                     curr = enif_make_list(env, 0); */
    /*                     d->i++; */
    /*                     break; */
    /*                 case '[': */
    /*                     dec_push(d, st_array); */
    /*                     dec_push(d, st_value); */
    /*                     objs = enif_make_list_cell(env, curr, objs); */
    /*                     curr = enif_make_list(env, 0); */
    /*                     d->i++; */
    /*                     break; */
    /*                 case ']': */
    /*                     if(!enif_is_empty_list(env, curr)) { */
    /*                         ret = dec_error(d, "invalid_json"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_value); */
    /*                     if(dec_curr(d) != st_array) { */
    /*                         ret = dec_error(d, "invalid_json"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_array); */
    /*                     dec_pop(d, st_value); */
    /*                     val = curr; // curr is [] */
    /*                     if(!enif_get_list_cell(env, objs, &curr, &objs)) { */
    /*                         ret = dec_error(d, "internal_error"); */
    /*                         goto done; */
    /*                     } */
    /*                     d->i++; */
    /*                     break; */
    /*                 default: */
    /*                     ret = dec_error(d, "invalid_json"); */
    /*                     goto done; */
    /*             } */
    /*             if(dec_top(d) == 0) { */
    /*                 dec_push(d, st_done); */
    /*             } else if(dec_curr(d) != st_value && dec_curr(d) != st_key) { */
    /*                 dec_push(d, st_comma); */
    /*                 curr = enif_make_list_cell(env, val, curr); */
    /*             } */
    /*             break; */

    /*         case st_key: */
    /*             switch(d->p[d->i]) { */
    /*                 case ' ': */
    /*                 case '\n': */
    /*                 case '\r': */
    /*                 case '\t': */
    /*                     d->i++; */
    /*                     break; */
    /*                 case '\"': */
    /*                     if(!dec_string(d, &val)) { */
    /*                         ret = dec_error(d, "invalid_string"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_key); */
    /*                     dec_push(d, st_colon); */
    /*                     curr = enif_make_list_cell(env, val, curr); */
    /*                     break; */
    /*                 case '}': */
    /*                     if(!enif_is_empty_list(env, curr)) { */
    /*                         ret = dec_error(d, "invalid_json"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_key); */
    /*                     dec_pop(d, st_object); */
    /*                     dec_pop(d, st_value); */
    /*                     val = make_empty_object(env, d->return_maps); */
    /*                     if(!enif_get_list_cell(env, objs, &curr, &objs)) { */
    /*                         ret = dec_error(d, "internal_error"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(dec_top(d) == 0) { */
    /*                         dec_push(d, st_done); */
    /*                     } else { */
    /*                         dec_push(d, st_comma); */
    /*                         curr = enif_make_list_cell(env, val, curr); */
    /*                     } */
    /*                     d->i++; */
    /*                     break; */
    /*                 default: */
    /*                     ret = dec_error(d, "invalid_json"); */
    /*                     goto done; */
    /*             } */
    /*             break; */

    /*         case st_colon: */
    /*             switch(d->p[d->i]) { */
    /*                 case ' ': */
    /*                 case '\n': */
    /*                 case '\r': */
    /*                 case '\t': */
    /*                     d->i++; */
    /*                     break; */
    /*                 case ':': */
    /*                     dec_pop(d, st_colon); */
    /*                     dec_push(d, st_value); */
    /*                     d->i++; */
    /*                     break; */
    /*                 default: */
    /*                     ret = dec_error(d, "invalid_json"); */
    /*                     goto done; */
    /*             } */
    /*             break; */

    /*         case st_comma: */
    /*             switch(d->p[d->i]) { */
    /*                 case ' ': */
    /*                 case '\n': */
    /*                 case '\r': */
    /*                 case '\t': */
    /*                     d->i++; */
    /*                     break; */
    /*                 case ',': */
    /*                     dec_pop(d, st_comma); */
    /*                     switch(dec_curr(d)) { */
    /*                         case st_object: */
    /*                             dec_push(d, st_key); */
    /*                             break; */
    /*                         case st_array: */
    /*                             dec_push(d, st_value); */
    /*                             break; */
    /*                         default: */
    /*                             ret = dec_error(d, "internal_error"); */
    /*                             goto done; */
    /*                     } */
    /*                     d->i++; */
    /*                     break; */
    /*                 case '}': */
    /*                     dec_pop(d, st_comma); */
    /*                     if(dec_curr(d) != st_object) { */
    /*                         ret = dec_error(d, "invalid_json"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_object); */
    /*                     dec_pop(d, st_value); */
    /*                     if(!make_object(env, curr, &val, d->return_maps)) { */
    /*                         ret = dec_error(d, "internal_object_error"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(!enif_get_list_cell(env, objs, &curr, &objs)) { */
    /*                         ret = dec_error(d, "internal_error"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(dec_top(d) > 0) { */
    /*                         dec_push(d, st_comma); */
    /*                         curr = enif_make_list_cell(env, val, curr); */
    /*                     } else { */
    /*                         dec_push(d, st_done); */
    /*                     } */
    /*                     d->i++; */
    /*                     break; */
    /*                 case ']': */
    /*                     dec_pop(d, st_comma); */
    /*                     if(dec_curr(d) != st_array) { */
    /*                         ret = dec_error(d, "invalid_json"); */
    /*                         goto done; */
    /*                     } */
    /*                     dec_pop(d, st_array); */
    /*                     dec_pop(d, st_value); */
    /*                     val = make_array(env, curr); */
    /*                     if(!enif_get_list_cell(env, objs, &curr, &objs)) { */
    /*                         ret = dec_error(d, "internal_error"); */
    /*                         goto done; */
    /*                     } */
    /*                     if(dec_top(d) > 0) { */
    /*                         dec_push(d, st_comma); */
    /*                         curr = enif_make_list_cell(env, val, curr); */
    /*                     } else { */
    /*                         dec_push(d, st_done); */
    /*                     } */
    /*                     d->i++; */
    /*                     break; */
                    default:
                        ret = dec_error(d, "invalid_sereal");
                        goto done;
                }
                }
                break;

    /*         case st_done: */
    /*             switch(d->p[d->i]) { */
    /*                 case ' ': */
    /*                 case '\n': */
    /*                 case '\r': */
    /*                 case '\t': */
    /*                     d->i++; */
    /*                     break; */
    /*                 default: */
    /*                     ret = dec_error(d, "invalid_trailing_data"); */
    /*                     goto done; */
    /*             } */
    /*             break; */

        default:
            ret = dec_error(d, "invalid_internal_state");
            goto done;
        }

    }

    if(dec_curr(d) != st_done) {
        ret = dec_error(d, "truncated_sereal");
    } else if(d->is_partial) {
        ret = enif_make_tuple2(env, d->atoms->atom_partial, val);
    } else {
        ret = val;
    }

done:
    consume_timeslice(env, d->i - start, d->bytes_per_iter);
    return ret;
}




ErlNifSInt64 srl_read_varint_int64_nocheck(Decoder *d)
{
    ErlNifSInt64 int64 = 0;
    unsigned int lshift = 0;

    while (d->p[d->i] & 0x80) {
        int64 |= ((ErlNifSInt64)(d->p[d->i] & 0x7F) << lshift);
        d->i++;
        lshift += 7;
    }
    int64 |= ((ErlNifSInt64)(d->p[d->i]) << lshift);
    d->i++;
    return int64;
}
