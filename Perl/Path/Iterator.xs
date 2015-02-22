#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_iterator.h"

typedef srl_iterator_t * Sereal__Path__Iterator;

MODULE = Sereal::Path::Iterator		PACKAGE = Sereal::Path::Iterator
PROTOTYPES: DISABLE

srl_iterator_t *
new(CLASS, src = NULL, opt = NULL)
    char *CLASS;
    SV *src;
    HV *opt;
  CODE:
    RETVAL = srl_build_iterator_struct(aTHX_ opt);
    if (src) srl_set_document(RETVAL, src); // XXX
  OUTPUT: RETVAL

void
DESTROY(iter)
    srl_iterator_t *iter;
  CODE:
    srl_destroy_iterator(aTHX_ iter);

void
set_document(iter, src)
    srl_iterator_t *iter;
    SV *src;
  CODE:
    srl_set_document(iter, src);

void
reset(iter)
    srl_iterator_t *iter;
  CODE:
    srl_reset(iter);

UV
eof(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_eof(iter);
  OUTPUT: RETVAL

UV
offset(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_offset(iter);
  OUTPUT: RETVAL

SV *
type(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_object_type(iter);
  OUTPUT: RETVAL

UV
next(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_next_n(iter, 1);
  OUTPUT: RETVAL

UV
step(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_step_n(iter, 1);
  OUTPUT: RETVAL

UV
next_n(iter, next)
    srl_iterator_t *iter;
    UV next;
  CODE:
    RETVAL = srl_next_n(iter, next);
  OUTPUT: RETVAL

UV
step_n(iter, step)
    srl_iterator_t *iter;
    UV step;
  CODE:
    RETVAL = srl_step_n(iter, step);
  OUTPUT: RETVAL

UV
find_key(iter, name)
    srl_iterator_t *iter;
    SV *name;
  CODE:
    RETVAL = srl_find_key(iter, name);
  OUTPUT: RETVAL

SV *
get_key(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_get_key(iter);
  OUTPUT: RETVAL
