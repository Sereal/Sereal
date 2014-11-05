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
    UV input_body_to_first_elt;

    int deepness;

    STRLEN input_len;
    UV size_limit;
    U32 flags;
    srl_splitter_stack_t * status_stack;
    /* srl_splitter_stack_t * output_stack; */

    UV chunk_size;
    char* chunk_start;
    char* chunk_iter_start;
    char* chunk_body_pos;
    SV* chunk;
    UV chunk_nb_elts;
    /* when we rewrite copy/refp tags in place, we add data( the data pointed
       to), and remove some (the tag + offset varint). This offset_delta stores
       by how much we have changed the counting */
    IV chunk_offset_delta;

    /* The current position where we are from the chunk body pos point of view */
    UV chunk_current_offset;

    /* SV* chunk_with_prefix; */

} srl_splitter_t;

enum {

    ST_VALUE,
    ST_TRACK,
    ST_DEEPNESS_UP,
    ST_ABSOLUTE_JUMP,


    /* ST_INVALID, */

    /* ST_ARRAY_CLOSE, */
    /* ST_HASH_PAIR, */
    /* ST_HASH_CLOSE, */

    /* ST_JUMP, */


};

srl_splitter_t * srl_build_splitter_struct(pTHX_ HV *opt);
void srl_destroy_splitter(pTHX_ srl_splitter_t *splitter);
SV* srl_splitter_next_chunk(pTHX_ srl_splitter_t * splitter);

#endif
