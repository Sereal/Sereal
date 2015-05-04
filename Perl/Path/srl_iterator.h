#ifndef SRL_ITERATOR_H_
#define SRL_ITERATOR_H_

#include "EXTERN.h"
#include "perl.h"
#include "srl_reader_types.h"

typedef struct srl_iterator         * srl_iterator_ptr;
typedef struct srl_iterator         srl_iterator_t;
typedef struct srl_iterator_stack   srl_iterator_stack_t;
typedef struct srl_iterator_stack   * srl_iterator_stack_ptr;
typedef struct srl_stack            * srl_stack_ptr;

/* the iterator main struct */
struct srl_iterator {
    srl_reader_buffer_t buf;
    srl_reader_buffer_ptr pbuf;
    srl_stack_ptr stack;
    UV first_tag_offset;
    SV *tmp_buf_owner;
    SV *document;
    struct srl_decoder *dec;
};

struct srl_iterator_stack {
    UV offset;      // offset of the tag
    U32 count;      // number of child objects
    I32 idx;        // index of current object, in negative format
    U8 tag;
};

/* constructor/destructor */
srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt);
void srl_destroy_iterator(pTHX_ srl_iterator_t *iter);

/* Sereal document */
void srl_iterator_set_document(pTHX_ srl_iterator_t *iter, SV *src);
void srl_iterator_reset(pTHX_ srl_iterator_t *iter);

/* navigation */
void srl_iterator_next(pTHX_ srl_iterator_t *iter, UV n);
void srl_iterator_step_in(pTHX_ srl_iterator_t *iter, UV n);
void srl_iterator_step_out(pTHX_ srl_iterator_t *iter, UV n);
void srl_iterator_next_until_depth_and_idx(pTHX_ srl_iterator_t *iter, UV expected_depth, U32 expected_idx);
UV srl_iterator_offset(pTHX_ srl_iterator_t *iter);
UV srl_iterator_eof(pTHX_ srl_iterator_t *iter);

/* expose stack status */
srl_iterator_stack_ptr srl_iterator_stack(pTHX_ srl_iterator_t *iter);
IV srl_iterator_stack_depth(pTHX_ srl_iterator_t *iter);
UV srl_iterator_stack_index(pTHX_ srl_iterator_t *iter);
UV srl_iterator_stack_info(pTHX_ srl_iterator_t *iter, UV *length_ptr);

/* information about current object */
UV srl_iterator_object_info(pTHX_ srl_iterator_t *iter, UV *length_ptr);

/* array parsing */
void srl_iterator_array_goto(pTHX_ srl_iterator_t *iter, I32 idx);

/* hash parsing */
const char * srl_iterator_hash_key(pTHX_ srl_iterator_t *iter, STRLEN *len_out);
SV * srl_iterator_hash_key_sv(pTHX_ srl_iterator_t *iter);
IV srl_iterator_hash_exists(pTHX_ srl_iterator_t *iter, const char *name, STRLEN name_len);
IV srl_iterator_hash_exists_sv(pTHX_ srl_iterator_t *iter, SV *name);

SV * srl_iterator_decode(pTHX_ srl_iterator_t *iter);

#define SRL_ITERATOR_OBJ_IS_SCALAR  (1 << 1)
#define SRL_ITERATOR_OBJ_IS_ARRAY   (1 << 2)
#define SRL_ITERATOR_OBJ_IS_HASH    (1 << 3)

#endif
