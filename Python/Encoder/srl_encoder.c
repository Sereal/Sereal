/* c-basic-offset: 4;  indent-tabs-mode: nil */
#include <Python.h>
#include "srl_encoder.h"
#include "srl_protocol.h"
#include "srl_buffer.h"


const srl_encoder_ctor_args default_encoder_ctor_args = 
{
    SRL_F_SHARED_HASHKEYS,
    0
};

srl_encoder_t *srl_encoder_new(const srl_encoder_ctor_args *args)
{
    srl_encoder_t *enc;

    if (!args)
        args = &default_encoder_ctor_args;

    enc = PyMem_Malloc(sizeof(*enc));
    if (!enc)
        return NULL;

    if (-1 == srl_encoder_ctor(enc, args)) {
        PyMem_Free(enc);
        return (void*)PyErr_NoMemory();
    }

    return enc;
}

void srl_encoder_delete(srl_encoder_t *enc)
{
    assert(enc);

    srl_encoder_dtor(enc);
    PyMem_Free(enc);
}

int srl_encoder_ctor(srl_encoder_t *enc, const srl_encoder_ctor_args *args)
{
    assert(enc);

    enc->buf_start = PyMem_Malloc(INITIAL_BUFFER_SIZE);
    if (!enc->buf_start)
        return -1;

    enc->buf_end = enc->buf_start + INITIAL_BUFFER_SIZE - 1;
    enc->pos = enc->buf_start;

    enc->operational_flags = 0;

    enc->flags = args->flags;
    enc->max_recursion_depth = args->max_recursion_depth;

    enc->recursion_depth = 0;

    return 0;
}

void srl_encoder_dtor(srl_encoder_t *enc)
{
    assert(enc);

    PyMem_Free(enc->buf_start);
    enc->buf_start = enc->buf_end = enc->pos = NULL;
}

PyObject *srl_encoder_dump(srl_encoder_t *enc, PyObject *obj) {
    assert(enc);

    Py_RETURN_NONE;
}


void srl_write_header(srl_encoder_t *enc)
{
    /* 4th to 8th bit are flags. Using 4th for snappy flag. FIXME needs to go in spec. */
    const uint8_t version_and_flags = 
        SRL_PROTOCOL_VERSION | (
            SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY)
            ? SRL_PROTOCOL_ENCODING_SNAPPY
            : SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY_INCREMENTAL)
            ? SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL
            : SRL_PROTOCOL_ENCODING_RAW
            );

    /* 4 byte magic string + proto version
     * + potentially uncompressed size varint
     * +  1 byte varint that indicates zero-length header */
    BUF_SIZE_ASSERT(enc, sizeof(SRL_MAGIC_STRING) + 1 + 1);
    srl_buf_cat_str_s_nocheck(enc, SRL_MAGIC_STRING);
    srl_buf_cat_char_nocheck(enc, version_and_flags);
    srl_buf_cat_char_nocheck(enc, '\0'); /* variable header length (0 right now) */
}

