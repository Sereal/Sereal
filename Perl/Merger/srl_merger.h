#ifndef SRL_MERGER_H_
#define SRL_MERGER_H_

#include "EXTERN.h"
#include "perl.h"

/* General 'config' constants */
#ifdef MEMDEBUG
#   define INITIALIZATION_SIZE 1
#else
#   define INITIALIZATION_SIZE 64
#endif

#include "srl_inline.h"
#include "srl_buffer_types.h"

/* the merger main struct */
typedef struct {
    srl_buffer_t buf;
    U32 protocol_version;     /* The version of the Sereal protocol to emit. */
    //HV *string_deduper_hv;    /* track strings we have seen before, by content */
} srl_merger_t;

/* constructor from options */
srl_merger_t *srl_build_merger_struct(pTHX_ HV *opt);

/* Explicit destructor */
void srl_destroy_merger(pTHX_ srl_merger_t *mrg);

#endif

