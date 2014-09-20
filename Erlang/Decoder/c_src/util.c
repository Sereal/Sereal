// This file is part of Jiffy released under the MIT license.
// See the LICENSE file for more information.

#ifndef UTILS_H
#define UTILS_H

#include "sereal.h"

#define UNLIMITED (0)

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
make_ok(sereal_st* st, ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, st->atom_ok, value);
}

ERL_NIF_TERM
make_error(sereal_st* st, ErlNifEnv* env, const char* error)
{
    return enif_make_tuple2(env, st->atom_error, make_atom(env, error));
}

int
get_bytes_per_iter(ErlNifEnv* env, ERL_NIF_TERM val, size_t* bpi)
{
    sereal_st* st = (sereal_st*) enif_priv_data(env);

    int arity;
    unsigned int bytes;
    const ERL_NIF_TERM* tuple;

    if(!enif_get_tuple(env, val, &arity, &tuple)) {
        return 0;
    }

    if(arity != 2) {
        return 0;
    }

    if(enif_compare(tuple[0], st->atom_bytes_per_iter) != 0) {
        return 0;
    }

    if(!enif_get_uint(env, tuple[1], &bytes)) {
        return 0;
    }

    *bpi = (size_t) bytes;

    return 1;
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

#endif
