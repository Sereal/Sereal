#include <csrl_hash.h>

#include <assert.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>

#include <csrl_util.h>

#include <PMurHash.h>

/* FIXME seed? */
/* MH_UINT32 PMurHash32(MH_UINT32 seed, const void *key, int len); */
#define CSRL_HVAL U32
#define CSRL_HASH(str, len) (PMurHash32(0, str, len))

#define CSRL_HASH_KEY_EQ(key1, keylen1, key2, keylen2) (keylen1 == keylen2 && !strncmp((const char *)key1, (const char *)key2, keylen1))

/* create a new pointer => pointer table */
CSRL_HASH_t *
CSRL_HASH_new(void)
{
  return CSRL_HASH_new_size(9);
}

CSRL_HASH_t *
CSRL_HASH_new_size(const U8 size_base2_exponent)
{
  CSRL_HASH_t *tbl = calloc(1, sizeof(CSRL_HASH_t));
  tbl->tbl_max = (1 << size_base2_exponent) - 1;
  tbl->tbl_items = 0;
  tbl->cur_iter = NULL;
  tbl->tbl_ary = calloc(tbl->tbl_max + 1, sizeof(CSRL_HASH_ENTRY_t*));
  return tbl;
}

/* map an existing pointer using a table */
CSRL_HASH_ENTRY_t *
CSRL_HASH_find(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen) {
  CSRL_HASH_ENTRY_t *tblent;
  const CSRL_HVAL hash = CSRL_HASH(key, keylen);
  tblent = tbl->tbl_ary[hash & tbl->tbl_max];
  for (; tblent; tblent = tblent->next) {
    if (CSRL_HASH_KEY_EQ(key, keylen, tblent->key, tblent->keylen))
      return tblent;
  }
  return NULL;
}

void *
CSRL_HASH_fetch(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen)
{
  CSRL_HASH_ENTRY_t const *const tblent = CSRL_HASH_find(tbl, key, keylen);
  return tblent ? tblent->value : NULL;
}


/* add a new entry to a pointer => pointer table */
void
CSRL_HASH_store(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen, void *value)
{
  /* FIXME if not found, we're computing the hash value twice */
  CSRL_HASH_ENTRY_t *tblent = CSRL_HASH_find(tbl, key, keylen);

  if (tblent) {
    tblent->value = value;
  } else {
    const CSRL_HVAL entry = CSRL_HASH(key, keylen) & tbl->tbl_max;
    tblent = malloc(sizeof(CSRL_HASH_ENTRY_t));

    tblent->key = (I8 *)strndup((const char *)key, keylen);
    tblent->keylen = keylen;
    tblent->value = value;
    tblent->next = tbl->tbl_ary[entry];
    tbl->tbl_ary[entry] = tblent;
    tbl->tbl_items++;
    if (tblent->next && (tbl->tbl_items > tbl->tbl_max))
      CSRL_HASH_grow(tbl);
  }
}

/* double the hash bucket size of an existing hash table */
void
CSRL_HASH_grow(CSRL_HASH_t *tbl)
{
  CSRL_HASH_ENTRY_t **ary = tbl->tbl_ary;
  const UV oldsize = tbl->tbl_max + 1;
  UV newsize = oldsize * 2;
  UV i;

  ary = realloc(ary, newsize * sizeof(CSRL_HASH_ENTRY_t *));
  memset(&ary[oldsize], 0, (newsize - oldsize)*sizeof(CSRL_HASH_ENTRY_t *)); 
  tbl->tbl_max = --newsize;
  tbl->tbl_ary = ary;

  for (i = 0; i < oldsize; i++, ary++) {
    CSRL_HASH_ENTRY_t **curentp, **entp, *ent;
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
CSRL_HASH_clear(CSRL_HASH_t *tbl)
{
  if (tbl && tbl->tbl_items) {
    register CSRL_HASH_ENTRY_t ** const array = tbl->tbl_ary;
    UV riter = tbl->tbl_max;

    do {
      CSRL_HASH_ENTRY_t *entry = array[riter];

      while (entry) {
        CSRL_HASH_ENTRY_t * const oentry = entry;
        entry = entry->next;
        free(oentry->key);
        free(oentry);
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
CSRL_HASH_delete(CSRL_HASH_t *tbl, const I8 *key, const size_t keylen)
{
  void *retval = NULL;
  CSRL_HASH_ENTRY_t *tblent;
  CSRL_HASH_ENTRY_t *tblent_prev;

  if (!tbl || !tbl->tbl_items) {
    return retval;
  } else {
    const CSRL_HVAL hash = CSRL_HASH(key, keylen);
    tblent_prev = NULL;
    tblent = tbl->tbl_ary[hash & tbl->tbl_max];

    for (; tblent; tblent_prev = tblent, tblent = tblent->next) {
      if (CSRL_HASH_KEY_EQ(key, keylen, tblent->key, tblent->keylen)) {
        if (tblent_prev != NULL) {
          tblent_prev->next = tblent->next;
        }
        else {
          /* First entry in chain */
          tbl->tbl_ary[hash & tbl->tbl_max] = tblent->next;
        }
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
CSRL_HASH_free(CSRL_HASH_t *tbl)
{
  if (!tbl)
    return;

  if (tbl->cur_iter != NULL) {
    CSRL_HASH_ITER_t *it = tbl->cur_iter;
    tbl->cur_iter = NULL; /* avoid circular checks */
    CSRL_HASH_iter_free(it);
  }

  CSRL_HASH_clear(tbl);
  free(tbl->tbl_ary);
  free(tbl);
}


/* NOT API */
#define CSRL_HASH_ITER_NEXT_ELEM(iter, tbl)                       \
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


CSRL_HASH_ITER_t *
CSRL_HASH_iter_new_flags(CSRL_HASH_t *tbl, int flags)
{
  /* TODO can we sink this allocation in common cases? */
  CSRL_HASH_ITER_t *iter = malloc(sizeof(CSRL_HASH_ITER_t));
  CSRL_HASH_iter_init_flags(tbl, iter, flags);
  return iter;
}

void
CSRL_HASH_iter_init_flags(CSRL_HASH_t *tbl, CSRL_HASH_ITER_t *iter, int flags)
{
  iter->table = tbl;
  iter->bucket_num = 0;
  iter->cur_entry = NULL;

  if (flags & CSRL_HASH_FLAG_AUTOCLEAN) {
    tbl->cur_iter = iter;
  }
  if (tbl->tbl_items == 0) {
    /* Prevent hash bucket scanning.
     * This can be a significant optimization on large, empty hashes. */
    iter->bucket_num = INT_MAX;
  }
  else {
    CSRL_HASH_ITER_NEXT_ELEM(iter, tbl);
    assert(iter->cur_entry != NULL);
  }
}

/* Free iterator object */
void
CSRL_HASH_iter_free(CSRL_HASH_ITER_t *iter)
{
  /* If we're the iterator that can be auto-cleaned by the CSRL_HASH,
   * then unregister. */
  if (iter->table->cur_iter == iter)
    iter->table->cur_iter = NULL;
  free(iter);
}

void
CSRL_HASH_debug_dump(CSRL_HASH_t *tbl, void (*func)(CSRL_HASH_ENTRY_t *e))
{
  CSRL_HASH_ENTRY_t *e;
  CSRL_HASH_ITER_t *iter = CSRL_HASH_iter_new(tbl);
  while (NULL != (e = CSRL_HASH_iter_next(iter))) {
    func(e);
  }
  CSRL_HASH_iter_free(iter);
}

/* Return next item from hash, NULL if at end */
CSRL_HASH_ENTRY_t *
CSRL_HASH_iter_next(CSRL_HASH_ITER_t *iter)
{
  CSRL_HASH_ENTRY_t *retval = iter->cur_entry;
  CSRL_HASH_t *tbl = iter->table;
  CSRL_HASH_ITER_NEXT_ELEM(iter, tbl);
  return retval;
}

/* Return next value from hash, NULL if at end */
void *
CSRL_HASH_iter_next_value(CSRL_HASH_ITER_t *iter)
{
  CSRL_HASH_ENTRY_t *e = iter->cur_entry;
  CSRL_HASH_t *tbl = iter->table;
  CSRL_HASH_ITER_NEXT_ELEM(iter, tbl);
  return e ? e->value : NULL;
}


/* Create new iterator object */
CSRL_HASH_ITER_t *
CSRL_HASH_iter_new(CSRL_HASH_t *tbl)
{
  return CSRL_HASH_iter_new_flags(tbl, 0);
}


