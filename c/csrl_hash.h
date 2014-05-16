#ifndef CSRL_HASH_H_
#define CSRL_HASH_H_

#include <stdlib.h>
#include <csrl_defines.h>

typedef struct CSRL_HASH_vtable CSRL_HASH_vtable_t;
typedef void *(*CSRL_HASH_free_cb_t)(void *);

typedef struct CSRL_HASH_entry  CSRL_HASH_entry_t;
typedef struct CSRL_HASH        CSRL_HASH_t;
typedef struct CSRL_HASH_iter   CSRL_HASH_iter_t;

struct CSRL_HASH_vtable {
  CSRL_HASH_free_cb_t free_cb;
};

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
  CSRL_HASH_iter_t        *cur_iter; /* one iterator at a time can be auto-freed */

  CSRL_HASH_vtable_t      *vtable;
};

extern CSRL_HASH_vtable_t CSRL_HASH_VTABLE_free;
extern CSRL_HASH_vtable_t CSRL_HASH_VTABLE_passthrough;

/* Allocate new hash */
CSRL_HASH_t *CSRL_HASH_new(CSRL_HASH_vtable_t *vtable);
/* Allocate new hash with a predefined power-of-2 bucket array size */
CSRL_HASH_t *CSRL_HASH_new_size(const U8 size_base2_exponent, CSRL_HASH_vtable_t *vtable);
/* Free a hash (includes clearing) */
void CSRL_HASH_free(CSRL_HASH_t *tbl);

/* Locate a hash entry by key */
CSRL_HASH_entry_t * CSRL_HASH_find(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen);
/* Fetch value from hash by key (NULL if not found) */
void *CSRL_HASH_fetch(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen);
/* Store value in hash by key */
void CSRL_HASH_store(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen, void *value);
/* Delete value from hash by key. Returns previous value (NULL if there was none) */
void *CSRL_HASH_delete(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen);
/* Clear all values from a hash */
void CSRL_HASH_clear(CSRL_HASH_t *tbl);

/* Instead of allocating a new iterator from the heap using
 * CSRL_HASH_iter_new or ..._flags, you can also initialize one
 * allocated from the stack using CSRL_HASH_iter_init_flags().
 * If you do so, no need to call CSRL_HASH_iter_free(), but
 * then you also shouldn't use CSRL_HASH_FLAG_AUTOCLEAN. */

#define CSRL_HASH_FLAG_AUTOCLEAN 1

/* Create new hash iterator for the given hash */
CSRL_HASH_iter_t *CSRL_HASH_iter_new(CSRL_HASH_t *tbl);
/* New hash iterator with options. Only option right now is "autoclean",
 * which means that the iterator will be owned and cleaned up by the hash.
 * A hash can own only one iterator at a time, so beware of memory leaks.
 * This might be changed in the future. */
CSRL_HASH_iter_t *CSRL_HASH_iter_new_flags(CSRL_HASH_t *tbl, int flags);
/* Initialize a hash iterator structure to be a new iterator for the given hash.
 * Useful for stack-allocated hash iterators. */
void CSRL_HASH_iter_init_flags(CSRL_HASH_t *tbl, CSRL_HASH_iter_t *iter, int flags);
/* Free a hash iterator. */
void CSRL_HASH_iter_free(CSRL_HASH_iter_t *iter);
/* Get the next hash entry from the hash iterator.
 * Returns NULL when no further entries in the hash. */
CSRL_HASH_entry_t *CSRL_HASH_iter_next(CSRL_HASH_iter_t *iter);
/* Get the next VALUE from the hash. Also returns NULL when done,
 * but values in the hash may also be NULL (depending on their usage)
 * so beware. */
void *CSRL_HASH_iter_next_value(CSRL_HASH_iter_t *iter);


/* pre-grow hash bucket list. This is an optimization only. */
void CSRL_HASH_grow(CSRL_HASH_t *tbl);

#endif
