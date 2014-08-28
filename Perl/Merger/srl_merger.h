#ifndef SRL_MERGER_H_
#define SRL_MERGER_H_

#include "EXTERN.h"
#include "perl.h"

/* the merger main struct */
typedef struct {
    SV * merged_sv;
    int status;
} srl_merger_t;

/* constructor */
srl_merger_t *srl_build_merger_struct(pTHX_ HV *opt);

#endif

