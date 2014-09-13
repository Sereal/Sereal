#ifndef SRL_MERGER_H_
#define SRL_MERGER_H_

#include "EXTERN.h"
#include "perl.h"

/* General 'config' constants */
#ifdef MEMDEBUG
#   define INITIALIZATION_SIZE 8
#else
#   define INITIALIZATION_SIZE 64
#endif

#include "srl_stack.h"
#include "../Encoder/srl_buffer_types.h"

/* the merger main struct */
typedef struct {
    srl_buffer_t obuf;                    /* output buffer */
    srl_buffer_t ibuf;                    /* input buffer, MUST NOT be deallocated by srl_buf_free_buffer() */
    srl_stack_t     *tracked_offsets;     /* sorted list of offsets which should be tracked */
    struct PTABLE   *tracked_offsets_tbl; /* table to convert ibuf offsets to obuf offsets */
    struct STRTABLE *string_deduper_tbl;  /* track strings we have seen before, by content */

    U32 obuf_padding_bytes_offset;        /* pointer to start of SRL_MAX_VARINT_LENGTH padding bytes */
    U32 cnt_of_merged_elements;           /* total count of merged elements so far */
    U32 protocol_version;                 /* the version of the Sereal protocol to emit. */
} srl_merger_t;

srl_merger_t *srl_build_merger_struct(pTHX_ HV *opt);         /* constructor from options */
void srl_destroy_merger(pTHX_ srl_merger_t *mrg);             /* explicit destructor */
void srl_merger_append(pTHX_ srl_merger_t *mrg, SV *src);     /* merge one item */
void srl_merger_append_all(pTHX_ srl_merger_t *mrg, AV *src); /* merge all items from src */
SV * srl_merger_finish(pTHX_ srl_merger_t *mrg);

#endif
