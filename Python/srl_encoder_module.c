/* c-basic-offset: 4;  indent-tabs-mode: nil */
#include <Python.h>
#include "util.h"
#include "srl_encoder.h"
#include "srl_encoder_module.h"

static inline int  encoder_args_from_dict(PyObject *dict, srl_encoder_ctor_args *out_args);
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
    srl_encoder *enc;
    srl_encoder_ctor_args enc_args;
    PyObject *obj, *ret;

    if (!PyArg_ParseTuple(args, "O:encode", &obj)) 
        return NULL;

    if (-1 == encoder_args_from_dict(kwargs, &enc_args))
        return NULL;

    enc = srl_encoder_new(&enc_args);
    if (!enc)
        return NULL;

    ret = srl_encoder_dump(enc, obj);
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
                  SET_IFF(!PyObject_IsTrue(val), 
                          args.operational_flags, SRL_F_SHARED_HASHKEYS);
              });
        ONKEY("max_recursion_depth",
              {
                  PyObject *py_depth;
                  long depth;
                  int overflow;

                  py_depth = PyNumber_Long(val);
                  if (py_depth) {
                      PyErr_Clear(); /* To know if -1 from PyLong_AsLong
                                        represents an error or a number*/
                      depth = PyLong_AsLong(py_depth);
                      overflow = 
                          -1 == depth && 
                          PyErr_Occurred() &&
                          PyErr_ExceptionMatches(PyExc_OverflowError)
                          ? 1 : 0;
                      Py_DECREF(py_depth);

                      if (!overflow && depth >= 0)
                          args.max_recursion_depth = depth;
                  } 

                  if (!py_depth) {
                      PyErr_WarnEx(PyExc_RuntimeWarning, 
                                   "Non-number value given for max_recursion_depth,"
                                   " using default", 0);
                  } else if (overflow) {
                      PyErr_WarnEx(PyExc_RuntimeWarning, 
                                   "Value given for max_recursion_depth overflows,"
                                   " using default", 0);
                  } else if (depth < 0) {
                      PyErr_WarnEx(PyExc_RuntimeWarning, 
                                   "Negative value given for max_recursion_depth,"
                                   " using default", 0);
                  }
              });
        ONKEY("snappy",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.operational_flags, SRL_F_COMPRESS_SNAPPY);
              });
        ONKEY("snappy_incr",
              {
                  SET_IFF(PyObject_IsTrue(val),
                          args.operational_flags, 
                          SRL_F_COMPRESS_SNAPPY_INCREMENTAL);
              });
        ONKEY("snappy_threshold",
              {
                  /*
                    Read a number from Python, 
                    similar gymnastics as for max_recursion_depth.
                   */
              });
        ONKEY("sort_keys",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.operational_flags, SRL_F_SORT_KEYS);
              });

        ONKEY("stringify_unknown",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.operational_flags, SRL_F_STRINGIFY_UNKNOWN);
              });
        ONKEY("undef_unknown",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.operational_flags, SRL_F_UNDEF_UNKNOWN);
              });
        ONKEY("warn_unknown",
              {
                  SET_IFF(PyObject_IsTrue(val), 
                          args.operational_flags, SRL_F_WARN_UNKNOWN);
              });
        /*
          undef_unknown && stringify_unknown options are mutually exclusive
        */
        if ((args.operational_flags & SRL_F_UNDEF_UNKNOWN) && 
            (args.operational_flags & SRL_F_STRINGIFY_UNKNOWN))
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


