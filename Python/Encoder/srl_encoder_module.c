/* c-basic-offset: 4;  indent-tabs-mode: nil */
#include <Python.h>
#include "util.h"
#include "srl_inline.h"
#include "srl_encoder.h"

SRL_STATIC_INLINE int  encoder_args_from_dict(PyObject *dict, srl_encoder_ctor_args *out_args);
static PyObject *encode(PyObject *self, PyObject *args, PyObject *kwargs);

static PyMethodDef srl_encoder_methods[] = 
{
    {"encode",
     (PyCFunction)encode, 
     METH_VARARGS|METH_KEYWORDS,
     "Sereal encode data-structure."},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initsrlencoder(void)
{
    PyObject *m;
    m = Py_InitModule("srlencoder",srl_encoder_methods);
    if (!m)
        return;
    /*
      I could add a new exception for Encoding errors
    */
}

PyObject *encode(PyObject *self, PyObject *args, PyObject *kwargs)
{
    /*
      signature: encode(obj,**config)
    */
    PyObject *obj,*ret;
    srl_encoder_t *enc;
    srl_encoder_ctor_args enc_args;

    (void)self; //unused

    enc = NULL;
    ret = obj = NULL;

    if (!PyArg_ParseTuple(args, "O:encode", &obj)) 
        goto finally;

    Py_INCREF(obj);

    if (-1 == encoder_args_from_dict(kwargs, &enc_args))
        goto finally;

    enc = srl_encoder_new(&enc_args);
    if (!enc)
        goto finally;

    ret = srl_encoder_dump(enc, obj);

finally:
    Py_XDECREF(obj);
    if (enc)
        srl_encoder_delete(enc);
    return ret;
}

int encoder_args_from_dict(PyObject *dict, srl_encoder_ctor_args *out_args)
{
#ifdef NDEBUG
#  define ONKEY_DEBUG
#else
#  define ONKEY_DEBUG                                   \
    if (val)                                            \
        printf("%s = %s\n", key,                        \
               PyString_AsString(PyObject_Repr(val)));      
#endif
#define ONKEY(_key,BLOCK)                               \
    {                                                   \
        PyObject *val;                                  \
        char *key;                                      \
        key = (_key);                                   \
        val = PyMapping_GetItemString(dict,key);        \
        PyErr_Clear();                                  \
        ONKEY_DEBUG;                                    \
        if (val) {                                      \
            BLOCK;                                      \
        }                                               \
        Py_XDECREF(val);                                \
    }                                                   \

    srl_encoder_ctor_args args = default_encoder_ctor_args;

    if (dict) {
        ONKEY("no_shared_hashkeys",
              {
                  SET_IFF(!PyObject_IsTrue(val), args.
                          flags, SRL_F_SHARED_HASHKEYS);
              });
        ONKEY("max_recursion_depth",
              {
                  long depth;

                  depth = PyInt_AsLong(val);
                  if (depth == -1 && PyErr_Occurred()) {
                      PyErr_SetString(
                          PyErr_Occurred(), 
                          "max_recursion_depth requires an integer");
                      return -1;
                  }

                  args.max_recursion_depth = depth;
              });
        ONKEY("snappy",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.flags, SRL_F_COMPRESS_SNAPPY);
              });
        ONKEY("snappy_incr",
              {
                  SET_IFF(PyObject_IsTrue(val),
                          args.flags, 
                          SRL_F_COMPRESS_SNAPPY_INCREMENTAL);
              });
        ONKEY("snappy_threshold",
              {
                  long l;

                  l = PyInt_AsLong(val);
                  if (l == -1 && PyErr_Occurred()) {
                      PyErr_SetString(
                          PyErr_Occurred(), 
                          "snappy_threshold requires an integer");
                      return -1;
                  }
                  args.snappy_threshold = l;
              });
        ONKEY("sort_keys",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.flags, SRL_F_SORT_KEYS);
              });

        ONKEY("stringify_unknown",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.flags, SRL_F_STRINGIFY_UNKNOWN);
              });
        ONKEY("undef_unknown",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.flags, SRL_F_UNDEF_UNKNOWN);
              });
        ONKEY("warn_unknown",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.flags, SRL_F_WARN_UNKNOWN);
              });
        /*
          undef_unknown && stringify_unknown options are mutually exclusive
        */
        if ((args.flags & SRL_F_UNDEF_UNKNOWN) && 
            (args.flags & SRL_F_STRINGIFY_UNKNOWN))
        {
            PyErr_SetString(PyExc_TypeError, 
                            "'undef_unknown' and 'stringify_unknown' options"
                            " are mutually exclusive");
            return -1;
        }
    }
    
    if (out_args) 
        *out_args = args;

    return 0;

#undef ONKEY
#undef ONKEY_DEBUG
}


