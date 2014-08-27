#include "sereal_decoder.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    sereal_decoder_st* st = enif_alloc(sizeof(sereal_decoder_st));
    if(st == NULL) {
        // no diagnostics?
        return 1;
    }

    st->atom_ok             = make_atom(env, "ok");
    st->atom_error          = make_atom(env, "error");
    st->atom_true           = make_atom(env, "true");
    st->atom_false          = make_atom(env, "false");
    st->atom_bignum         = make_atom(env, "bignum");
    st->atom_bignum_e       = make_atom(env, "bignum_e");
    st->atom_bigdbl         = make_atom(env, "bigdbl");
    st->atom_partial        = make_atom(env, "partial");
    st->atom_undefined      = make_atom(env, "undefined");
    st->atom_iter           = make_atom(env, "iter");
    st->atom_bytes_per_iter = make_atom(env, "bytes_per_iter");

    st->resource_decoder = enif_open_resource_type (
                                env,
                                NULL,
                                "decoder",
                                decoder_destroy,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                NULL
                            );

    *priv = (void*) st;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ErlNifFunc funcs[] =
{
    {"nif_decoder_init",    2, decoder_init},
    {"nif_decoder_iterate", 4, decoder_iterate},
};

ERL_NIF_INIT(sereal_decoder, funcs, &load, &reload, &upgrade, &unload);
