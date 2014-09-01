#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_merger.h"

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
append(mrg, src)
    Sereal::Merger mrg;
    SV *src
  PPCODE:
    srl_merger_append(aTHX mrg, src);

char *
finish(mrg)
    Sereal::Merger mrg
  CODE:
    RETVAL = srl_merger_finish(aTHX mrg);
  OUTPUT: RETVAL

void
test_me()

    PREINIT:

    PPCODE:

    PUSHs(sv_2mortal(newSVpv("0.042", 0)));
