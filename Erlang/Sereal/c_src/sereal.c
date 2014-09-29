#include "sereal.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    SerealConstants* st = enif_alloc(sizeof(SerealConstants));
    if(st == NULL) {
        // no diagnostics?
        return 1;
    }

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

    st->resource_encoder = enif_open_resource_type (
                                env,
                                NULL,
                                "encoder",
                                encoder_destroy,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                NULL
                            );

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
    {"srl_encoder_setup",   2, srl_encoder_setup},
    {"srl_encoder_parse",   2, srl_encoder_parse},
};

ERL_NIF_INIT(sereal, funcs, &load, &reload, &upgrade, &unload);
