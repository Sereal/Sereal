#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_merger.h"
#include "srl_buffer.h"

MODULE = Sereal::Merger		PACKAGE = Sereal::Merger
PROTOTYPES: DISABLE

srl_encoder_t *
new(CLASS, opt = NULL)
    char *CLASS;
    HV *opt;
  CODE:
    RETVAL = srl_build_merger_struct(aTHX_ opt);
  OUTPUT: RETVAL

void
DESTROY(mrg)
    srl_merger_t *mrg;
  CODE:
    srl_destroy_merger(aTHX_ mrg);

void
test_me()

    PREINIT:

    PPCODE:

    PUSHs(sv_2mortal(newSVpv("0.042", 0)));
