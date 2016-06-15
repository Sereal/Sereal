#ifndef UTILS_H
#define UTILS_H

#include "erl_nif.h"

// work around U8 being unknown here (see 94909904bdaf7357f515e7c3463636265fac8c64)
#define U8 unsigned char

#define debug_print(fmt, ...)                                           \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,       \
                                __LINE__, __func__, ##__VA_ARGS__); } while (0)

#define DEFAULT_BYTES_PER_ITERATION 4096

typedef struct {
    ERL_NIF_TERM  atom_ok;
    ERL_NIF_TERM  atom_error;
    ERL_NIF_TERM  atom_true;
    ERL_NIF_TERM  atom_false;
    ERL_NIF_TERM  atom_bignum;
    ERL_NIF_TERM  atom_bignum_e;
    ERL_NIF_TERM  atom_bigdbl;
    ERL_NIF_TERM  atom_undefined;
    ERL_NIF_TERM  atom_partial;
    ERL_NIF_TERM  atom_convert;
    ERL_NIF_TERM  atom_zlib;
    ERL_NIF_TERM  atom_snappy;
    ERL_NIF_TERM  atom_bytes_per_iter;
    ERL_NIF_TERM  atom_arrayref_to_list;

    ErlNifResourceType* resource_encoder;
    ErlNifResourceType* resource_decoder;

} SerealConstants;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_ok(SerealConstants* st, ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM make_error(SerealConstants* st, ErlNifEnv* env, const char* error);
ERL_NIF_TERM parse_error(SerealConstants* st, ErlNifEnv* env, const char* error, ERL_NIF_TERM term);

int should_yield(size_t used, size_t limit);
int consume_timeslice(ErlNifEnv* env, size_t used, size_t limit);

#endif // Included UTILS_H
