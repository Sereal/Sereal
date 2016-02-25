#ifndef SRL_SPLITTER_H_
#define SRL_SPLITTER_H_

#include "EXTERN.h"
#include "perl.h"

typedef struct {
    UV * data;
    UV size;
    UV top;
} srl_splitter_stack_t;

/* the splitter main struct */
typedef struct {
    SV* input_sv;
    char * input_str;
    char * input_str_end;
    char * pos;
    char * input_body_pos;
    UV input_nb_elts;

    int deepness;

    STRLEN input_len;
    UV size_limit;
    U32 flags;
    srl_splitter_stack_t * status_stack;
    /* srl_splitter_stack_t * output_stack; */

    SV* header_sv;
    char* header_str;
    STRLEN header_len;
    IV header_count_idx;

    UV chunk_size;
    char* chunk_start;
    char* chunk_iter_start;
    char* chunk_body_pos;
    SV* chunk;
    UV chunk_nb_elts;
    IV chunk_offset_delta;

    /* The current position, from the chunk body point of view, + 1*/
    UV chunk_current_offset;

    IV compression_format;
    IV compression_level;

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

srl_splitter_t * srl_build_splitter_struct(pTHX_ HV *opt);
void srl_destroy_splitter(pTHX_ srl_splitter_t *splitter);
SV* srl_splitter_next_chunk(pTHX_ srl_splitter_t * splitter);

#endif
