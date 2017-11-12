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
    U32 idx;        /* index of current object [0..length) */
    U32 length;     /* number of child objects */
    UV first;       /* offset to first element */
    UV end;         /* offset to end of this stack (i.e. offset after last */
                    /* element. Not always set. Used only in REFP/ALIAS case */
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
srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt);    /* allocate structure and initalize */
void srl_init_iterator(pTHX_ srl_iterator_t *iter, HV *opt); /* initialize structure */
void srl_deinit_iterator(pTHX_ srl_iterator_t *iter);        /* deinitalize structure without freeing it */
void srl_destroy_iterator(pTHX_ srl_iterator_t *iter);       /* destroy structure and free it */
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
/* if n == 0, go to first element, otherwise pop stack n times and go to first element */
void srl_iterator_rewind(pTHX_ srl_iterator_t *iter, UV n);

UV srl_iterator_eof(pTHX_ srl_iterator_t *iter);

/* expose stack status */
SRL_STATIC_INLINE IV
srl_iterator_stack_depth(pTHX_ srl_iterator_t *iter)
{
    return SRL_STACK_DEPTH(iter->pstack);
}

SRL_STATIC_INLINE U32
srl_iterator_stack_length(pTHX_ srl_iterator_t *iter)
{
    return iter->stack.ptr->length;
}

SRL_STATIC_INLINE U32
srl_iterator_stack_index(pTHX_ srl_iterator_t *iter)
{
    return iter->stack.ptr->idx;
}

/* information about current object */
U32 srl_iterator_info(pTHX_ srl_iterator_t *iter, UV *length_out, const char **classname_out, STRLEN *classname_lenght_out);

/* array parsing */
void srl_iterator_array_goto(pTHX_ srl_iterator_t *iter, I32 idx);
IV srl_iterator_array_exists(pTHX_ srl_iterator_t *iter, I32 idx);

SRL_STATIC_INLINE I32
srl_iterator_normalize_idx(pTHX_ I32 idx, UV length)
{
    return idx < 0 ? length + idx : idx;
}

/* hash parsing */
void srl_iterator_hash_key(pTHX_ srl_iterator_t *iter, const char **keyname, STRLEN *keyname_length_out);
IV srl_iterator_hash_exists(pTHX_ srl_iterator_t *iter, const char *name, STRLEN name_len);

SV * srl_iterator_decode(pTHX_ srl_iterator_t *iter); /* return mortalized SV */
SV * srl_iterator_decode_and_next(pTHX_ srl_iterator_t *iter); /* return mortalized SV */

#define SRL_ITER_NOT_FOUND (-1)

#define SRL_ITERATOR_INFO_TAG_MASK  (0xFF)
#define SRL_ITERATOR_INFO_ROOT      (1  << 8)
#define SRL_ITERATOR_INFO_REF       (2  << 8)
#define SRL_ITERATOR_INFO_HASH      (4  << 8)
#define SRL_ITERATOR_INFO_ARRAY     (8  << 8)
#define SRL_ITERATOR_INFO_REGEXP    (16 << 8)
#define SRL_ITERATOR_INFO_SCALAR    (32 << 8)
#define SRL_ITERATOR_INFO_BLESSED   (1  << 16)
#define SRL_ITERATOR_INFO_REF_TO    (2  << 16)

#endif
