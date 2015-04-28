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

#include "srl_reader_types.h"
#include "srl_buffer_types.h"

/* the merger main struct */
typedef struct {
    srl_buffer_t obuf;                    /* output buffer */
    srl_reader_buffer_t ibuf;             /* input buffer, MUST NOT be deallocated by srl_buf_free_buffer() */
    srl_reader_buffer_ptr pibuf;          /* pointer to ibuf */
    struct srl_stack *tracked_offsets;    /* sorted list of offsets from ibuf which
                                             reffered by COPY, OBJECTV or OBJECTV_FREEZE tag */

    struct PTABLE   *tracked_offsets_tbl; /* table to convert ibuf offsets to obuf offsets */
    struct STRTABLE *string_deduper_tbl;  /* track strings we have seen before, by content */
    struct STRTABLE *classname_deduper_tbl;  /* track classnames we have seen before, by content */

    UV obuf_last_successfull_offset;      /* pointer to last byte of last successfully merged Sereal document */
    UV obuf_padding_bytes_offset;         /* pointer to start of SRL_MAX_VARINT_LENGTH padding bytes */

    UV recursion_depth;                   /* recursion depth of current document */
    UV max_recursion_depth;               /* configurable limit on the number of recursive calls we're willing to make */

    U32 cnt_of_merged_elements;           /* total count of merged elements so far */
    U32 protocol_version;                 /* the version of the Sereal protocol to emit. */
    U32 flags;                            /* flag-like options: See SRL_F_* defines */

    void *snappy_workmem;                 /* lazily allocated if and only if using Snappy */
} srl_merger_t;

srl_merger_t *srl_build_merger_struct(pTHX_ HV *opt);         /* constructor from options */
void srl_destroy_merger(pTHX_ srl_merger_t *mrg);             /* explicit destructor */
void srl_merger_append(pTHX_ srl_merger_t *mrg, SV *src);     /* merge one item */
void srl_merger_append_all(pTHX_ srl_merger_t *mrg, AV *src); /* merge all items from src */
SV * srl_merger_finish(pTHX_ srl_merger_t *mrg, SV *user_header_src);

/* define option bits in srl_merger_t's flags member */

/* Define what top level tag will be used. Default is SRL_F_TOPLEVEL_KEY_ARRAY */
#define SRL_F_TOPLEVEL_KEY_SCALAR               0x00001UL
#define SRL_F_TOPLEVEL_KEY_ARRAY                0x00002UL
#define SRL_F_TOPLEVEL_KEY_HASH                 0x00004UL

/* WARNING: SRL_F_COMPRESS_SNAPPY               0x00040UL
 *          SRL_F_COMPRESS_SNAPPY_INCREMENTAL   0x00080UL
 *          SRL_F_COMPRESS_ZLIB                 0x00100UL
 *          are in srl_compress.h */

/* If set, use a hash to emit COPY() tags for all duplicated strings (including keys)
 * (slower, but great compression) */
#define SRL_F_DEDUPE_STRINGS                    0x00800UL

#endif
