#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include <srl_merger.h>


typedef srl_merger_t * Sereal__Merger;


MODULE = Sereal::Merger		PACKAGE = Sereal::Merger

PROTOTYPES: ENABLE


Sereal::Merger
new(CLASS, opt = NULL)
    char *CLASS;
    HV *opt;
  CODE:
    RETVAL = srl_build_merger_struct(aTHX_ opt);
  OUTPUT: RETVAL


void
test_me()

    PREINIT:

    PPCODE:

    PUSHs(sv_2mortal(newSVpv("0.042", 0)));
















