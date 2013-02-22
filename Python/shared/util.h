/* c-basic-offset: 4;  indent-tabs-mode: nil */
#ifndef _UTIL_H_
#define _UTIL_H_

#include "srl_inline.h"

/* Set If and Only If */
/* Be aware that flag and bitmask are evaluated twice */
#define SET_IFF(pred,flag,bitmask) ((flag) = (pred) ? (flag | (bitmask)) : (flag & ~(bitmask)))

SRL_STATIC_INLINE  void print_description(PyObject *obj, char *name)
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
