/* c-basic-offset: 4;  indent-tabs-mode: nil */
#include <Python.h>
#include "srl_encoder.h"

const srl_encoder_ctor_args default_encoder_ctor_args = 
{
    .operational_flags = SRL_F_SHARED_HASHKEYS,
    .max_recursion_depth = 0,
};

srl_encoder *srl_encoder_new(const srl_encoder_ctor_args *args)
{
    if (!args)
        args = &default_encoder_ctor_args;
    
    /*
      allocate srl_encoder
      and buffer
     */

#define P(bitmask) printf( #bitmask ": %s\n", (args->operational_flags & bitmask) ? "1" : "0")
    
    P(SRL_F_SHARED_HASHKEYS);
    P(SRL_F_COMPRESS_SNAPPY);
    P(SRL_F_COMPRESS_SNAPPY_INCREMENTAL);
    P(SRL_F_SORT_KEYS);
    P(SRL_F_STRINGIFY_UNKNOWN);
    P(SRL_F_UNDEF_UNKNOWN);
    P(SRL_F_WARN_UNKNOWN);
    
    return 1;
}

void srl_encoder_delete(srl_encoder *enc)
{
}

int srl_encoder_ctor(srl_encoder *enc, const srl_encoder_ctor_args *args)
{
    return -1;
}

void srl_encoder_dtor(srl_encoder *enc)
{
}

PyObject *srl_encoder_dump(srl_encoder *enc, PyObject *obj) {
    Py_RETURN_NONE;
}
