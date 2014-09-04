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

#include "../Encoder/srl_buffer_types.h"

/* the merger main struct */
typedef struct {
    srl_buffer_t obuf;                   /* output buffer */
    srl_buffer_t ibuf;                   /* input buffer, MUST NOT be deallocated by srl_buf_free_buffer() */

    HV *string_deduper_hv;               /* track strings we have seen before, by content */
    HV *tracked_offsets_hv;              /* table to convert ibuf offsets to obuf offsets */
    AV *tracked_offsets_av;              /* list of sorted keys of tracked_offsets_hv */
    AV *parser_stack;

    U32 protocol_version;                /* the version of the Sereal protocol to emit. */
} srl_merger_t;

srl_merger_t *srl_build_merger_struct(pTHX_ HV *opt);       /* constructor from options */
void srl_destroy_merger(pTHX_ srl_merger_t *mrg);           /* explicit destructor */
void srl_merger_append(pTHX_ srl_merger_t *mrg, SV *src);   /* class methods */
SV * srl_merger_finish(pTHX_ srl_merger_t *mrg);

#endif

