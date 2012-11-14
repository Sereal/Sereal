/*
 * pysereal - An implemetation of the Sereal serialization protocol
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#if PY_MAJOR_VERSION >= 3
#define PyInt_FromLong PyLong_FromLong
#define PyInt_Check PyLong_Check
#define PyInt_AsLong PyLong_AsLong
#endif

#include "srl_decoder.h"
#include <stdbool.h>

static PyObject * sereal_encode(PyObject *self, PyObject *args, PyObject *kw) {
    PyErr_SetString(PyExc_NotImplementedError, "Not yet implemented");
    return NULL;
}

static PyObject * sereal_decode(PyObject *self, PyObject *args, PyObject *kw) {
    srl_decoder_t *dec = srl_decoder_new();
    PyObject *ret;
    char *keywords[] = {
        "buffer",
        "refuse_snappy",
        "refuse_objects",
        "validate_utf8",
        NULL,
    };
    bool refuse_snappy=0, refuse_objects=0, validate_utf8 = 0;
    if(!PyArg_ParseTupleAndKeywords(args, kw, "s#|bbb", keywords, 
            &(dec->buf_start), &(dec->buf_len),
            &refuse_snappy, &refuse_objects, &validate_utf8
    )) {
        return NULL;
    }
    if(refuse_snappy)
        dec->flags |= SRL_F_DECODER_REFUSE_SNAPPY;
    if(refuse_objects)
        dec->flags |= SRL_F_DECODER_REFUSE_OBJECTS;
    if(validate_utf8)
        dec->flags |= SRL_F_DECODER_VALIDATE_UTF8;

    ret = srl_decode(dec);
    srl_decoder_free(dec);
    return ret;
}

static PyMethodDef SerealMethods[] = {
    {"encode", (PyCFunction)sereal_encode, METH_VARARGS|METH_KEYWORDS, "Encode objects using sereal"},
    {"decode", (PyCFunction)sereal_decode, METH_VARARGS|METH_KEYWORDS, "Decode a sereal-encoded bytestring"},
    {NULL, NULL, 0, NULL} /* Sentinel */
};

#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef serealmodule = {
    PyModuleDef_HEAD_INIT,
    "_sereal",
    NULL,
    -1,
   SerealMethods
};
#endif

PyMODINIT_FUNC
#if PY_MAJOR_VERSION < 3
init_sereal(void)
#else
PyInit__sereal(void)
#endif
{
#if PY_MAJOR_VERSION < 3
    /* PyObject *_sereal = */ Py_InitModule("_sereal", SerealMethods);
#else
    PyObject *_sereal = PyModule_Create(&serealmodule);
#endif
    PyObject *re = PyImport_ImportModule("re");
    srl_re_compile = PyObject_GetAttrString(re, "compile");
    /* All below use borrowed references */
    PyObject *re_dict = PyModule_GetDict(re);
    RE_IGNORECASE = PyInt_AS_LONG(PyDict_GetItemString(re_dict, "IGNORECASE"));
    RE_DOTALL = PyInt_AS_LONG(PyDict_GetItemString(re_dict, "DOTALL"));
    RE_VERBOSE = PyInt_AS_LONG(PyDict_GetItemString(re_dict, "VERBOSE"));
    RE_MULTILINE = PyInt_AS_LONG(PyDict_GetItemString(re_dict, "MULTILINE"));
    RE_UNICODE = PyInt_AS_LONG(PyDict_GetItemString(re_dict, "UNICODE"));
    RE_LOCALE = PyInt_AS_LONG(PyDict_GetItemString(re_dict, "LOCALE"));
#if PY_MAJOR_VERSION >= 3
    return _sereal;
#endif
}
