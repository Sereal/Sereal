#ifndef SEREAL_DECODER_H
#define SEREAL_DECODER_H

#include "erl_nif.h"

ERL_NIF_TERM decoder_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM decoder_iterate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

void decoder_destroy(ErlNifEnv* env, void* obj);

#endif // Included SEREAL_DECODER_H
