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
    char * body_pos;

    int cant_split_here;

    STRLEN input_len;
    UV chunk_size;
    UV cut_deepness;
    U32 flags;
    srl_splitter_stack_t * status_stack;
    /* srl_splitter_stack_t * output_stack; */

    UV current_chunk_size;
    char* current_chunk_start;
    char* current_chunk_iteration_start;
    SV* current_chunk;
    UV current_chunk_nb_elements;
    /* when we rewrite copy/refp tags in place, we add data( the data pointed
       to), and remove some (the tag + offset varint). This offset_delta stores
       by how much we have changed the counting */
    IV current_chunk_offset_delta;

    SV* current_chunk_with_prefix;

} srl_splitter_t;

enum {

    ST_VALUE,
    ST_TRACK,
    ST_CAN_SPLIT_AGAIN,
    ST_ABSOLUTE_JUMP,
    ST_ADD_DIFF_TO_OFFSET_DELTA


    /* ST_INVALID, */

    /* ST_ARRAY_CLOSE, */
    /* ST_HASH_PAIR, */
    /* ST_HASH_CLOSE, */

    /* ST_JUMP, */


};

srl_splitter_t * srl_build_splitter_struct(pTHX_ HV *opt);
void srl_destroy_splitter(pTHX_ srl_splitter_t *splitter);
SV* srl_splitter_next_chunk(pTHX_ srl_splitter_t * splitter);

/* Define what top level tag will be used. Default is SRL_F_TOPLEVEL_KEY_ARRAY */
#define SRL_F_TOPLEVEL_KEY_SCALAR               0x00001UL
#define SRL_F_TOPLEVEL_KEY_ARRAY                0x00002UL
#define SRL_F_TOPLEVEL_KEY_HASH                 0x00004UL

#endif
