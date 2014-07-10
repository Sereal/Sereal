#include "sereal_decoder.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    sereal_decoder_st* st = enif_alloc(sizeof(sereal_decoder_st));
    if(st == NULL) {
        return 1;
    }

    st->atom_ok = make_atom(env, "ok");
    st->atom_error = make_atom(env, "error");
    st->atom_null = make_atom(env, "null");
    st->atom_true = make_atom(env, "true");
    st->atom_false = make_atom(env, "false");
    st->atom_bignum = make_atom(env, "bignum");
    st->atom_bignum_e = make_atom(env, "bignum_e");
    st->atom_bigdbl = make_atom(env, "bigdbl");
    st->atom_partial = make_atom(env, "partial");
    st->atom_uescape = make_atom(env, "uescape");
    st->atom_pretty = make_atom(env, "pretty");
    st->atom_force_utf8 = make_atom(env, "force_utf8");
    st->atom_iter = make_atom(env, "iter");
    st->atom_bytes_per_iter = make_atom(env, "bytes_per_iter");
    st->atom_return_maps = make_atom(env, "return_maps");

    st->atom_undefined = make_atom(env, "undefined");

    st->res_dec = enif_open_resource_type(
            env,
            NULL,
            "decoder",
            dec_destroy,
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

/* static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*     int x, ret; */
/*     if (!enif_get_string(env, argv[0], &x)) { */
/* 	return enif_make_badarg(env); */
/*     } */
/*     ret = foo(x); */
/*     return enif_make_int(env, ret); */
/* } */

/* static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*     int y, ret; */
/*     if (!enif_get_int(env, argv[0], &y)) { */
/* 	return enif_make_badarg(env); */
/*     } */
/*     ret = bar(y); */
/*     return enif_make_int(env, ret); */
/* } */

static ErlNifFunc funcs[] =
{
    {"nif_decode_init", 2, decode_init},
    {"nif_decode_iter", 4, decode_iter},
};

ERL_NIF_INIT(sereal_decoder, funcs, &load, &reload, &upgrade, &unload);


