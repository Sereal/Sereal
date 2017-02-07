#ifndef SRL_ITERATOR_H_
#define SRL_ITERATOR_H_

#include "EXTERN.h"
#include "perl.h"
#include "srl_reader_types.h"

typedef struct srl_iterator         * srl_iterator_ptr;
typedef struct srl_iterator         srl_iterator_t;
typedef struct srl_iterator_stack   srl_iterator_stack_t;
typedef struct srl_iterator_stack   * srl_iterator_stack_ptr;

struct srl_iterator_stack {
    I32 ridx;       // reverse index of current object, i.e. first element = length 
    U32 length;     // number of child objects
    UV offset;      // offset to first element
    UV prev_depth;  // offset at previous depth
    U8 tag;
};

#define srl_stack_type_t srl_iterator_stack_t
#include "srl_stack.h"

/* the iterator main struct */
struct srl_iterator {
    srl_reader_buffer_t buf;
    srl_reader_buffer_ptr pbuf;
    srl_stack_t stack;
    srl_stack_ptr pstack;
    SV *document;
    struct srl_decoder *dec;
};

/* constructor/destructor */
srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt);    // allocate structure and initalize
void srl_init_iterator(pTHX_ srl_iterator_t *iter, HV *opt); // initialize structure
void srl_deinit_iterator(pTHX_ srl_iterator_t *iter);        // deinitalize structure without freeing it
void srl_destroy_iterator(pTHX_ srl_iterator_t *iter);       // destroy structure and free it
void srl_shallow_copy_iterator(pTHX_ srl_iterator_t *from, srl_iterator_t *to);

/* Sereal document */
void srl_iterator_set(pTHX_ srl_iterator_t *iter, SV *src);
void srl_iterator_reset(pTHX_ srl_iterator_t *iter);

/* nested XXX */
IV srl_iterator_unite(pTHX_ srl_iterator_t *iter);
IV srl_iterator_disjoin(pTHX_ srl_iterator_t *iter);

/* navigation */
/* skip n next elements at current depth */
void srl_iterator_next(pTHX_ srl_iterator_t *iter, UV n);
/* do n steps */
void srl_iterator_step_in(pTHX_ srl_iterator_t *iter, UV n);
/* run until elements at current depth ends and go one level down; do this n times */
void srl_iterator_step_out(pTHX_ srl_iterator_t *iter, UV n);
/* run until depth and idx reached */
void srl_iterator_until(pTHX_ srl_iterator_t *iter, UV depth, U32 idx);
/* if n == 0, go to first element, otherwise pop stack n times and go to first element */
void srl_iterator_rewind(pTHX_ srl_iterator_t *iter, UV n);

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
IV srl_iterator_array_goto(pTHX_ srl_iterator_t *iter, I32 idx);
IV srl_iterator_array_exists(pTHX_ srl_iterator_t *iter, I32 idx);

SRL_STATIC_INLINE I32
srl_iterator_normalize_idx(pTHX_ I32 idx, UV length)
{
    return idx < 0 ? length + idx : idx;
}

/* hash parsing */
const char * srl_iterator_hash_key(pTHX_ srl_iterator_t *iter, STRLEN *len_out);
SV * srl_iterator_hash_key_sv(pTHX_ srl_iterator_t *iter); // return mortalized SV
IV srl_iterator_hash_exists(pTHX_ srl_iterator_t *iter, const char *name, STRLEN name_len);
IV srl_iterator_hash_exists_sv(pTHX_ srl_iterator_t *iter, SV *name);

SV * srl_iterator_decode(pTHX_ srl_iterator_t *iter); // return mortalized SV

#define SRL_ITER_NOT_FOUND (-1)

#define SRL_ITERATOR_OBJ_IS_SCALAR  (1 << 1)
#define SRL_ITERATOR_OBJ_IS_ARRAY   (1 << 2)
#define SRL_ITERATOR_OBJ_IS_HASH    (1 << 3)
#define SRL_ITERATOR_OBJ_IS_ROOT    (1 << 4)

#endif
