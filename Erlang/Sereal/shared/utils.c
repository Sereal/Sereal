// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#include "utils.h"
#include "erl_nif.h"

#define UNLIMITED (0)

void init_sereal_constants(ErlNifEnv *env, SerealConstants *st) {

    st->atom_ok               = make_atom(env, "ok");
    st->atom_error            = make_atom(env, "error");
    st->atom_true             = make_atom(env, "true");
    st->atom_false            = make_atom(env, "false");
    st->atom_bignum           = make_atom(env, "bignum");
    st->atom_bignum_e         = make_atom(env, "bignum_e");
    st->atom_bigdbl           = make_atom(env, "bigdbl");
    st->atom_undefined        = make_atom(env, "undefined");
    st->atom_partial          = make_atom(env, "partial");
    st->atom_convert          = make_atom(env, "convert");
    st->atom_zlib             = make_atom(env, "zlib");
    st->atom_snappy           = make_atom(env, "snappy");
    st->atom_bytes_per_iter   = make_atom(env, "bytes_per_iter");
    st->atom_arrayref_to_list = make_atom(env, "arrayref_to_list");

//    st->resource_encoder = enif_open_resource_type (
//                                env,
//                                NULL,
//                                "encoder",
//                                encoder_destroy,
//                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
//                                NULL
//                            );
//
}

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM result;

    if(!enif_make_existing_atom(env, name, &result, ERL_NIF_LATIN1)) {
        result = enif_make_atom(env, name);
    }

    return result;
}

ERL_NIF_TERM
make_ok(SerealConstants* st, ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, st->atom_ok, value);
}

ERL_NIF_TERM
make_error(SerealConstants* st, ErlNifEnv* env, const char* error)
{
    return enif_make_tuple2(env, st->atom_error, make_atom(env, error));
}

ERL_NIF_TERM
parse_error(SerealConstants* st, ErlNifEnv* env, const char* error, ERL_NIF_TERM term)
{
    return enif_make_tuple3(env, st->atom_error, make_atom(env, error), term);
}

int
should_yield(size_t used, size_t limit)
{
    return !(limit == UNLIMITED || used < limit);
}

int
consume_timeslice(ErlNifEnv* env, size_t used, size_t limit)
{
#if(ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 4)
    double u = (double) used;
    double l = (double) limit;
    int perc = (int) (100.0 * (u / l));

    if(perc < 1) {
        perc = 1;
    } else if(perc > 100) {
        perc = 100;
    }

    return enif_consume_timeslice(env, perc);
#else
    return 0;
#endif
}
