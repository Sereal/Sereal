#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_common.h"
#include "srl_splitter.h"
#include "srl_protocol.h"


#include "const-c.inc"

typedef srl_splitter_t * Sereal__Splitter;

MODULE = Sereal::Splitter		PACKAGE = Sereal::Splitter
PROTOTYPES: DISABLE

Sereal::Splitter
new_xs(CLASS, opt)
    HV *opt;
  CODE:
    RETVAL = srl_build_splitter_struct(aTHX opt);
  OUTPUT: RETVAL

void
DESTROY(splitter)
    Sereal::Splitter splitter;
  PPCODE:
    srl_destroy_splitter(aTHX_ splitter);

SV*
next_chunk(splitter)
    Sereal::Splitter splitter;
  CODE:
    RETVAL = srl_splitter_next_chunk(splitter);
  OUTPUT: RETVAL



MODULE = Sereal::Splitter        PACKAGE = Sereal::Splitter::Constants
PROTOTYPES: DISABLE

INCLUDE: const-xs.inc

