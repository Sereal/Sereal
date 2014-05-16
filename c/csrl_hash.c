#include <csrl_hash.h>

#include <assert.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>

#include <csrl_util.h>

#include <PMurHash.h>

/* FIXME seed? */
/* MH_UINT32 PMurHash32(MH_UINT32 seed, const void *key, int len); */
typedef U32 CSRL_HVAL;
#define CSRL_HASH(str, len) (PMurHash32(0, str, len))

#define CLH_KEY_EQ(key1, keylen1, key2, keylen2) (keylen1 == keylen2 && !strncmp((const char *)key1, (const char *)key2, keylen1))

static void *
default_free(void *value)
{
  free(value);
  return NULL;
}

CLH_vtable_t CLH_VTABLE_free = {default_free};
CLH_vtable_t CLH_VTABLE_passthrough = {NULL};

/* create a new pointer => pointer table */
CLH_t *
CLH_new(CLH_vtable_t *vtable)
{
  return CLH_new_size(3, vtable);
}

CLH_t *
CLH_new_size(const U8 size_base2_exponent, CLH_vtable_t *vtable)
{
  CLH_t *tbl = calloc(1, sizeof(CLH_t));
  tbl->tbl_max = (1 << size_base2_exponent) - 1;
  tbl->tbl_items = 0;
  tbl->cur_iter = NULL;
  tbl->vtable = (vtable ? vtable : &CLH_VTABLE_passthrough);
  tbl->tbl_ary = calloc(tbl->tbl_max + 1, sizeof(CLH_entry_t*));
  return tbl;
}

/* map an existing pointer using a table */
CLH_entry_t *
CLH_find(CLH_t *tbl, const I8 *key, const size_t keylen) {
  CLH_entry_t *tblent;
  const CSRL_HVAL hash = CSRL_HASH(key, keylen);
  tblent = tbl->tbl_ary[hash & tbl->tbl_max];
  for (; tblent; tblent = tblent->next) {
    if (CLH_KEY_EQ(key, keylen, tblent->key, tblent->keylen))
      return tblent;
  }
  return NULL;
}

void *
CLH_fetch(CLH_t *tbl, const I8 *key, const size_t keylen)
{
  CLH_entry_t const *const tblent = CLH_find(tbl, key, keylen);
  return tblent ? tblent->value : NULL;
}


/* add a new entry to a pointer => pointer table */
void
CLH_store(CLH_t *tbl, const I8 *key, const size_t keylen, void *value)
{
  /* FIXME if not found, we're computing the hash value twice */
  CLH_entry_t *tblent = CLH_find(tbl, key, keylen);

  if (tblent) {
    tblent->value = value;
  } else {
    const CSRL_HVAL entry = CSRL_HASH(key, keylen) & tbl->tbl_max;
    tblent = malloc(sizeof(CLH_entry_t));

    tblent->key = (I8 *)strndup((const char *)key, keylen);
    tblent->keylen = keylen;
    tblent->value = value;
    tblent->next = tbl->tbl_ary[entry];
    tbl->tbl_ary[entry] = tblent;
    tbl->tbl_items++;
    if (tblent->next && (tbl->tbl_items > tbl->tbl_max))
      CLH_grow(tbl);
  }
}

/* double the hash bucket size of an existing hash table */
void
CLH_grow(CLH_t *tbl)
{
  CLH_entry_t **ary = tbl->tbl_ary;
  const UV oldsize = tbl->tbl_max + 1;
  UV newsize = oldsize * 2;
  UV i;

  ary = realloc(ary, newsize * sizeof(CLH_entry_t *));
  memset(&ary[oldsize], 0, (newsize - oldsize)*sizeof(CLH_entry_t *)); 
  tbl->tbl_max = --newsize;
  tbl->tbl_ary = ary;

  for (i = 0; i < oldsize; i++, ary++) {
    CLH_entry_t **curentp, **entp, *ent;
    if (!*ary)
      continue;
    curentp = ary + oldsize;
    for (entp = ary, ent = *ary; ent; ent = *entp) {
      if ((newsize & CSRL_HASH(ent->key, ent->keylen)) != i) {
        *entp = ent->next;
        ent->next = *curentp;
        *curentp = ent;
        continue;
      } else {
        entp = &ent->next;
      }
    }
  }
}

/* remove all the entries from a hash table */
void
CLH_clear(CLH_t *tbl)
{
  if (tbl && tbl->tbl_items) {
    register CLH_entry_t ** const array = tbl->tbl_ary;
    UV riter = tbl->tbl_max;

    do {
      CLH_entry_t *entry = array[riter];

      if (tbl->vtable->free_cb) {
        CLH_free_cb_t cb = tbl->vtable->free_cb;
        while (entry) {
          CLH_entry_t * const oentry = entry;
          entry = entry->next;
          (void)cb(oentry->value);
          free(oentry->key);
          free(oentry);
        }
      }
      else {
        while (entry) {
          CLH_entry_t * const oentry = entry;
          entry = entry->next;
          free(oentry->key);
          free(oentry);
        }
      }

      /* chocolateboy 2008-01-08
       *
       * make sure we clear the array entry, so that subsequent probes fail
       */

      array[riter] = NULL;
    } while (riter--);

    tbl->tbl_items = 0;
  }
}

/* remove one entry from a hash table */
void *
CLH_delete(CLH_t *tbl, const I8 *key, const size_t keylen)
{
  void *retval = NULL;
  CLH_entry_t *tblent;
  CLH_entry_t *tblent_prev;

  if (!tbl || !tbl->tbl_items) {
    return retval;
  } else {
    const CSRL_HVAL hash = CSRL_HASH(key, keylen);
    tblent_prev = NULL;
    tblent = tbl->tbl_ary[hash & tbl->tbl_max];

    for (; tblent; tblent_prev = tblent, tblent = tblent->next) {
      if (CLH_KEY_EQ(key, keylen, tblent->key, tblent->keylen)) {
        if (tblent_prev != NULL) {
          tblent_prev->next = tblent->next;
        }
        else {
          /* First entry in chain */
          tbl->tbl_ary[hash & tbl->tbl_max] = tblent->next;
        }

        if (tbl->vtable->free_cb)
          retval = tbl->vtable->free_cb(tblent->value);
        else
          retval = tblent->value;
        free(tblent->key);
        free(tblent);
        tbl->tbl_items--;
        break;
      }
    }
  }

  return retval;
}

/* clear and free a hash table */
void
CLH_free(CLH_t *tbl)
{
  if (!tbl)
    return;

  if (tbl->cur_iter != NULL) {
    CLH_iter_t *it = tbl->cur_iter;
    tbl->cur_iter = NULL; /* avoid circular checks */
    CLH_iter_free(it);
  }

  CLH_clear(tbl);
  free(tbl->tbl_ary);
  free(tbl);
}


/* NOT API */
#define CLH_ITER_NEXT_ELEM(iter, tbl)                       \
  STMT_START {                                                    \
    if ((iter)->cur_entry && (iter)->cur_entry->next) {           \
      (iter)->cur_entry = (iter)->cur_entry->next;                \
    }                                                             \
    else {                                                        \
      do {                                                        \
        if ((iter)->bucket_num > (tbl)->tbl_max) {                \
          (iter)->cur_entry = NULL;                               \
          break;                                                  \
        }                                                         \
        (iter)->cur_entry = (tbl)->tbl_ary[(iter)->bucket_num++]; \
      } while ((iter)->cur_entry == NULL);                        \
    }                                                             \
  } STMT_END


CLH_iter_t *
CLH_iter_new_flags(CLH_t *tbl, int flags)
{
  /* TODO can we sink this allocation in common cases? */
  CLH_iter_t *iter = malloc(sizeof(CLH_iter_t));
  CLH_iter_init_flags(tbl, iter, flags);
  return iter;
}

void
CLH_iter_init_flags(CLH_t *tbl, CLH_iter_t *iter, int flags)
{
  iter->table = tbl;
  iter->bucket_num = 0;
  iter->cur_entry = NULL;

  if (flags & CLH_FLAG_AUTOCLEAN) {
    tbl->cur_iter = iter;
  }
  if (tbl->tbl_items == 0) {
    /* Prevent hash bucket scanning.
     * This can be a significant optimization on large, empty hashes. */
    iter->bucket_num = INT_MAX;
  }
  else {
    CLH_ITER_NEXT_ELEM(iter, tbl);
    assert(iter->cur_entry != NULL);
  }
}

/* Free iterator object */
void
CLH_iter_free(CLH_iter_t *iter)
{
  /* If we're the iterator that can be auto-cleaned by the CSRL_HASH,
   * then unregister. */
  if (iter->table->cur_iter == iter)
    iter->table->cur_iter = NULL;
  free(iter);
}

void
CLH_debug_dump(CLH_t *tbl, void (*func)(CLH_entry_t *e))
{
  CLH_entry_t *e;
  CLH_iter_t *iter = CLH_iter_new(tbl);
  while (NULL != (e = CLH_iter_next(iter))) {
    func(e);
  }
  CLH_iter_free(iter);
}

/* Return next item from hash, NULL if at end */
CLH_entry_t *
CLH_iter_next(CLH_iter_t *iter)
{
  CLH_entry_t *retval = iter->cur_entry;
  CLH_t *tbl = iter->table;
  CLH_ITER_NEXT_ELEM(iter, tbl);
  return retval;
}

/* Return next value from hash, NULL if at end */
void *
CLH_iter_next_value(CLH_iter_t *iter)
{
  CLH_entry_t *e = iter->cur_entry;
  CLH_t *tbl = iter->table;
  CLH_ITER_NEXT_ELEM(iter, tbl);
  return e ? e->value : NULL;
}


/* Create new iterator object */
CLH_iter_t *
CLH_iter_new(CLH_t *tbl)
{
  return CLH_iter_new_flags(tbl, 0);
}


