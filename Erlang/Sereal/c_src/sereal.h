#ifndef SEREAL_DECODER_H
#define SEREAL_DECODER_H

#include "erl_nif.h"

#define DEFAULT_BYTES_PER_ITER 2048

#define MAP_TYPE_PRESENT \
    ((ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 6) \
    || (ERL_NIF_MAJOR_VERSION > 2))

typedef struct {
    ERL_NIF_TERM  atom_ok;
    ERL_NIF_TERM  atom_error;
    ERL_NIF_TERM  atom_true;
    ERL_NIF_TERM  atom_false;
    ERL_NIF_TERM  atom_bignum;
    ERL_NIF_TERM  atom_bignum_e;
    ERL_NIF_TERM  atom_bigdbl;
    ERL_NIF_TERM  atom_undefined;
    ERL_NIF_TERM  atom_iter;
    ERL_NIF_TERM  atom_bytes_per_iter;

    ErlNifResourceType* resource_encoder;
    ErlNifResourceType* resource_decoder;

} SerealConstants;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_ok(SerealConstants* st, ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM make_error(SerealConstants* st, ErlNifEnv* env, const char* error);
ERL_NIF_TERM parse_error(SerealConstants* st, ErlNifEnv* env, const char* error, ERL_NIF_TERM term);

int should_yield(size_t used, size_t limit);
int get_bytes_per_iter(ErlNifEnv* env, ERL_NIF_TERM val, size_t* bpi);
int consume_timeslice(ErlNifEnv* env, size_t used, size_t limit);

ERL_NIF_TERM decoder_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM decoder_iterate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM encoder_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM encoder_iterate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

void encoder_destroy(ErlNifEnv* env, void* obj);
void decoder_destroy(ErlNifEnv* env, void* obj);

#endif // Included SEREAL_DECODER_H



