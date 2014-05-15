#ifndef CSRL_HASH_H_
#define CSRL_HASH_H_

#include <stdlib.h>
#include <csrl_defines.h>


typedef struct CSRL_HASH_entry CSRL_HASH_ENTRY_t;
typedef struct CSRL_HASH       CSRL_HASH_t;
typedef struct CSRL_HASH_iter  CSRL_HASH_ITER_t;

struct CSRL_HASH_entry {
  struct CSRL_HASH_entry  *next;
  I8                      *key;
  size_t                  keylen;
  void                    *value;
};

/* private */
struct CSRL_HASH_iter {
  struct CSRL_HASH        *table;
  UV                      bucket_num;
  struct CSRL_HASH_entry  *cur_entry;
};

/* private */
struct CSRL_HASH {
  struct CSRL_HASH_entry  **tbl_ary;
  UV                      tbl_max;
  UV                      tbl_items;
  CSRL_HASH_ITER_t        *cur_iter; /* one iterator at a time can be auto-freed */
};

CSRL_HASH_t * CSRL_HASH_new();
CSRL_HASH_t * CSRL_HASH_new_size(const U8 size_base2_exponent);
CSRL_HASH_ENTRY_t * CSRL_HASH_find(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen);
void * CSRL_HASH_fetch(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen);
void CSRL_HASH_store(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen, void *value);
void CSRL_HASH_grow(CSRL_HASH_t *tbl);
void *CSRL_HASH_delete(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen);
void CSRL_HASH_clear(CSRL_HASH_t *tbl);
void CSRL_HASH_free(CSRL_HASH_t *tbl);

/* Instead of allocating a new iterator from the heap using
 * CSRL_HASH_iter_new or ..._flags, you can also initialize one
 * allocated from the stack using CSRL_HASH_iter_init_flags().
 * If you do so, no need to call CSRL_HASH_iter_free(), but
 * then you also shouldn't use CSRL_HASH_FLAG_AUTOCLEAN. */

#define CSRL_HASH_FLAG_AUTOCLEAN 1

CSRL_HASH_ITER_t * CSRL_HASH_iter_new(CSRL_HASH_t *tbl);
CSRL_HASH_ITER_t * CSRL_HASH_iter_new_flags(CSRL_HASH_t *tbl, int flags);
void CSRL_HASH_iter_init_flags(CSRL_HASH_t *tbl, CSRL_HASH_ITER_t *iter, int flags);
void CSRL_HASH_iter_free(CSRL_HASH_ITER_t *iter);
CSRL_HASH_ENTRY_t * CSRL_HASH_iter_next(CSRL_HASH_ITER_t *iter);
void *CSRL_HASH_iter_next_value(CSRL_HASH_ITER_t *iter);


#endif
