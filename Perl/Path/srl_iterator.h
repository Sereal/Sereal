#ifndef SRL_ITERATOR_H_
#define SRL_ITERATOR_H_

#include "EXTERN.h"
#include "perl.h"
#include "srl_reader_types.h"

typedef struct srl_stack    * srl_stack_ptr;
typedef struct srl_iterator * srl_iterator_ptr;
typedef struct srl_iterator srl_iterator_t;

/* the iterator main struct */
struct srl_iterator {
    srl_reader_buffer_t buf;
    srl_reader_buffer_ptr pbuf;
    srl_stack_ptr stack;
    UV first_tag_offset;
    SV *tmp_buf_owner;
    void *dec; // srl_decoder object
};

/* constructor/destructor */
srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt);
void srl_destroy_iterator(pTHX_ srl_iterator_t *iter);

/* Sereal document */
void srl_set_document(pTHX_ srl_iterator_t *iter, SV *src);
void srl_reset(pTHX_ srl_iterator_t *iter);

/* navigation */
UV srl_eof(pTHX_ srl_iterator_t *iter);
void srl_next(pTHX_ srl_iterator_t *iter, UV n);
void srl_step_in(pTHX_ srl_iterator_t *iter, UV n);
void srl_step_out(pTHX_ srl_iterator_t *iter, UV n);
UV srl_next_at_depth(pTHX_ srl_iterator_t *iter, UV depth);
UV srl_offset(pTHX_ srl_iterator_t *iter);

/* expose stack status */
IV srl_stack_depth(pTHX_ srl_iterator_t *iter);
UV srl_stack_index(pTHX_ srl_iterator_t *iter);
UV srl_stack_info(pTHX_ srl_iterator_t *iter, UV *length_ptr);

/* information about current object */
UV srl_object_info(pTHX_ srl_iterator_t *iter, UV *length_ptr);

/* array parsing */
void srl_array_goto(pTHX_ srl_iterator_t *iter, I32 idx);

/* hash parsing */
SV * srl_hash_key(pTHX_ srl_iterator_t *iter);
IV srl_hash_exists(pTHX_ srl_iterator_t *iter, SV *name);

SV * srl_decode(pTHX_ srl_iterator_t *iter);

#define SRL_ITER_OBJ_IS_SCALAR  (1 << 1)
#define SRL_ITER_OBJ_IS_ARRAY   (1 << 2)
#define SRL_ITER_OBJ_IS_HASH    (1 << 3)

#endif
