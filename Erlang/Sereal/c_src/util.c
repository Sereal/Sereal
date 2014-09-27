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

#endif
