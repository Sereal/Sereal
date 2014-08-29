#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_merger.h"
#include "srl_buffer.h"

typedef srl_merger_t * Sereal__Merger;

MODULE = Sereal::Merger		PACKAGE = Sereal::Merger
PROTOTYPES: ENABLE

Sereal::Merger
new(CLASS, opt = NULL)
    HV *opt;
  CODE:
    RETVAL = srl_build_merger_struct(aTHX opt);
  OUTPUT: RETVAL

void
DESTROY(mrg)
    Sereal::Merger mrg;
  CODE:
    srl_destroy_merger(aTHX mrg);

void
finish(mrg)
    Sereal::Merger mrg
  PPCODE:
    srl_merger_finish(aTHX mrg);

void
append(mrg, srl_document)
    Sereal::Merger mrg
    char * srl_document
  PPCODE:
    srl_merger_append(aTHX mrg, srl_document);

void
test_me()

    PREINIT:

    PPCODE:

    PUSHs(sv_2mortal(newSVpv("0.042", 0)));
