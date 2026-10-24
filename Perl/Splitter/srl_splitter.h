#ifndef SRL_SPLITTER_H_
#define SRL_SPLITTER_H_

#include "EXTERN.h"
#include "perl.h"

#include "srl_reader_types.h"
#include "srl_buffer_types.h"

#define srl_stack_type_t UV
#include "srl_stack.h"

/* the splitter main struct */
typedef struct {
    /* input: buf.body_pos is the input body, buf.pos the parsing position */
    srl_reader_buffer_t buf;
    srl_reader_buffer_ptr pbuf;
    SV *input_sv;                       /* owns the input buffer, or its decompressed copy */
    U8 input_protocol_version;
    UV input_nb_elts;                   /* number of elements of the top level array */
    srl_checksum_config_t checksum;     /* how to treat a checksummed input */

    /* what is left to do, see the ST_* values below */
    srl_stack_t status_stack;
    UV deepness;
    UV max_recursion_depth;

    /* user header template, and where in it to store the element count */
    SV *header_sv;
    const char *header_str;
    STRLEN header_len;
    IV header_count_idx;

    /* the chunk being built */
    srl_buffer_t chunk;
    UV size_limit;                      /* approximate size of a chunk body */
    UV chunk_size;                      /* bytes of elements copied into the chunk */
    UV chunk_nb_elts;
    const unsigned char *chunk_iter_start; /* input bytes from here on are still to be copied */
    U32 compress_flags;                 /* SRL_F_COMPRESS_* */
    int compress_level;
    void *snappy_workmem;

    /* per chunk lookup tables */
    struct PTABLE *offset_tbl;          /* input body offset of a tracked tag -> its offset in the chunk */
    HV *dedupe_binary;                  /* string content -> offset of its tag in the chunk */
    HV *dedupe_utf8;

    bool tag_is_tracked;
    bool dont_check_for_duplicate;
} srl_splitter_t;

enum {
    ST_VALUE,
    ST_TRACK,
    ST_DEEPNESS_UP,
    ST_ABSOLUTE_JUMP,
    ST_TRACK_NEXT_VALUE,
    ST_DONT_CHECK_FOR_DUPLICATE
};

SV * srl_build_splitter_object(pTHX_ const char *class_name, HV *opt); /* new reference to a blessed splitter */
void srl_destroy_splitter(pTHX_ srl_splitter_t *splitter);
SV* srl_splitter_next_chunk(pTHX_ srl_splitter_t * splitter);

#endif
