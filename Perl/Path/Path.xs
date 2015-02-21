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
new(CLASS, opt = NULL)
    char *CLASS;
    HV *opt;
  CODE:
    RETVAL = srl_build_iterator_struct(aTHX_ opt);
  OUTPUT: RETVAL

void
DESTROY(iter)
    srl_iterator_t *iter;
  CODE:
    srl_destroy_iterator(aTHX_ iter);
