#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_common.h"
#include "srl_merger.h"
#include "srl_protocol.h"

/* Generated code for exposing C constants to Perl */
#include "const-c.inc"

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

void
append_all(mrg, src)
    Sereal::Merger mrg;
    AV *src
  PPCODE:
    srl_merger_append_all(aTHX mrg, src);

SV*
finish(mrg)
    Sereal::Merger mrg
  CODE:
    RETVAL = srl_merger_finish(aTHX mrg);
  OUTPUT: RETVAL

MODULE = Sereal::Merger        PACKAGE = Sereal::Merger::Constants
PROTOTYPES: DISABLE

INCLUDE: const-xs.inc
