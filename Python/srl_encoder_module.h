/* c-basic-offset: 4;  indent-tabs-mode: nil */
#ifndef _SRL_ENCODER_MODULE_H_
#define _SRL_ENCODER_MODULE_H_

static inline  void print_description(PyObject *obj, char *name)
#ifdef NDEBUG
{}
#else
{
    if (obj)
        printf("%s: %p, %s, %s, %s, %s, %s\n"
               "%s: %s\n", 
               name, obj,
               (PyNumber_Check(obj) ? "number" : "non-number"),
               (PySequence_Check(obj) ? "sequence" : "non-sequence"),
               (PyMapping_Check(obj) ? "mapping" : "non-mapping"),
               (PyIter_Check(obj) ? "iterable" : "non-iterable"),
               (PyDict_Check(obj) ? "dict" : "non-dict"),
               name,
               PyString_AsString(PyObject_Repr(obj))
            );
    else 
        printf("%s: NULL\n", name);
}
#endif /* NDEBUG - print_description*/

#endif
