#ifndef CSRL_HASH_H_
#define CSRL_HASH_H_

#include <stdlib.h>
#include <csrl_defines.h>

/* This implements a string hash table structure for CSereal.
 *
 * The keys are pairs of char* strings with size_t lengths,
 * the values void*'s. The hash tables support simple
 * hooks for executing actions on deletes so automatic
 * free() or refcounting decrements are easy to implement.
 *
 * A simple iteration interface is also provided, albeit
 * its behaviour becomes undefined if the hash is modified
 * during iteration.
 *
 * The main types involved are
 * CLH_t        => The main hash structure. Attributes are private.
 * CLH_entry_t  => A structure representing a single key/value pair.
 *                 Always owned by the hash! key/keylen/value attributes
 *                 are part of the public API for direct access.
 * CLH_iter_t   => A hash iterator state. Attributes are private.
 * CLH_vtable_t => A hash vtable (for implementing various value
 *                 delete strategies). These are NEVER owned
 *                 by a hash structure.
 *
 * The API is documented below, first a synopsis (C99 for convenience):
 *
 * typedef struct my_value { int a; int b } my_value_t;
 * myvalue_t *v;
 *
 * -- Construct new hash that frees the values on deletes.
 * CLH_t *h = CLH_new(CLH_VTABLE_free);
 * 
 * -- Create new value. Store in hash. Owned by hash
 * -- since we're using CLH_VTABLE_free.
 * v = calloc(1, sizeof(myvalue_t));
 * v->a = 1; v->b = 2;
 * CLH_store(h, "foo", 3, v);
 * 
 * -- One more value.
 * v = calloc(1, sizeof(myvalue_t));
 * v->a = 3; v->b = 4;
 * CLH_store(h, "bar", 3, v);
 *
 * -- Fetch a value (prints "a=1 b=2")
 * v = CLH_fetch(h, "foo", 3);
 * printf("a=%i b=%i\n", v->a, v->b);
 *
 * -- Iterate over hash values.
 * -- See API docs below on how to avoid the iterator allocation.
 * CLH_iter_t *it = CLH_iter_new(h);
 * while ( NULL != (v = CLH_iter_next_value(it)) ) {
 *   printf("a=%i b=%i\n", v->a, v->b);
 * }
 * -- Note that we're using value-iteration here. That is ok because
 * -- none of the values are NULL. Otherwise we would have to use the
 * -- full hash entry iteration instead (using CLH_iter_next).
 * 
 * CLH_delete(h, "bar", 3); -- bar is gone and its value free()'d!
 *
 * CLH_free(h); -- foo and its value struct are now also gone.
 */

/* The struct definitions and typedefs */
typedef void *(*CLH_free_cb_t)(void *);
typedef struct CLH_vtable {
  CLH_free_cb_t free_cb;
} CLH_vtable_t;

typedef struct CLH_entry {
  struct CLH_entry  *next;
  I8                *key;
  size_t            keylen;
  void              *value;
} CLH_entry_t;

struct CLH;
struct CLH_iter;

/* private */
typedef struct CLH_iter {
  struct CLH       *table;
  UV               bucket_num;
  struct CLH_entry *cur_entry;
} CLH_iter_t;

/* private */
typedef struct CLH {
  CLH_entry_t  **tbl_ary;
  UV           tbl_max;
  UV           tbl_items;
  CLH_iter_t   *cur_iter; /* one iterator at a time can be auto-freed */

  CLH_vtable_t *vtable;
} CLH_t;
typedef struct CLH        CLH_t;

/* These are two default vtables for common value free-ing
 * semantics: Doing nothing (CLH_VTABLE_passthrough which is
 * the same as a NULL vtable) and calling libc's free()
 * (CLH_VTABLE_free). */
extern CLH_vtable_t CLH_VTABLE_free;
extern CLH_vtable_t CLH_VTABLE_passthrough;

/* Allocate new hash */
CLH_t *CLH_new(CLH_vtable_t *vtable);
/* Allocate new hash with a predefined power-of-2 bucket array size */
CLH_t *CLH_new_size(const U8 size_base2_exponent, CLH_vtable_t *vtable);
/* Free a hash (includes clearing) */
void CLH_free(CLH_t *tbl);

/* Locate a hash entry by key */
CLH_entry_t * CLH_find(CLH_t *tbl, const I8 *key, const size_t keylen);
/* Fetch value from hash by key (NULL if not found) */
void *CLH_fetch(CLH_t *tbl, const I8 *key, const size_t keylen);
/* Store value in hash by key */
void CLH_store(CLH_t *tbl, const I8 *key, const size_t keylen, void *value);
/* Delete value from hash by key. Returns previous value (NULL if there was none) */
void *CLH_delete(CLH_t *tbl, const I8 *key, const size_t keylen);
/* Clear all values from a hash */
void CLH_clear(CLH_t *tbl);

/* Instead of allocating a new iterator from the heap using
 * CLH_iter_new or ..._flags, you can also initialize one
 * allocated from the stack using CLH_iter_init_flags().
 * If you do so, no need to call CLH_iter_free(), but
 * then you also shouldn't use CLH_FLAG_AUTOCLEAN. */

#define CLH_FLAG_AUTOCLEAN 1

/* Create new hash iterator for the given hash */
CLH_iter_t *CLH_iter_new(CLH_t *tbl);
/* New hash iterator with options. Only option right now is "autoclean",
 * which means that the iterator will be owned and cleaned up by the hash.
 * A hash can own only one iterator at a time, so beware of memory leaks.
 * This might be changed in the future. */
CLH_iter_t *CLH_iter_new_flags(CLH_t *tbl, int flags);
/* Initialize a hash iterator structure to be a new iterator for the given hash.
 * Useful for stack-allocated hash iterators. */
void CLH_iter_init_flags(CLH_t *tbl, CLH_iter_t *iter, int flags);
/* Free a hash iterator. */
void CLH_iter_free(CLH_iter_t *iter);
/* Get the next hash entry from the hash iterator.
 * Returns NULL when no further entries in the hash. */
CLH_entry_t *CLH_iter_next(CLH_iter_t *iter);
/* Get the next VALUE from the hash. Also returns NULL when done,
 * but values in the hash may also be NULL (depending on their usage)
 * so beware. */
void *CLH_iter_next_value(CLH_iter_t *iter);


/* pre-grow hash bucket list. This is an optimization only. */
void CLH_grow(CLH_t *tbl);

#endif
