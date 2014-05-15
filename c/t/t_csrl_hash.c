#include "mytap.h"
#include <csrl_hash.h>

#include <stdlib.h>
#include <string.h>

void
test_alloc()
{
  CSRL_HASH_t *h;
  h = CSRL_HASH_new();
  ok_m(h != NULL, "CSRL_HASH_new");
  ok_m(h->tbl_items == 0, "n items");
  CSRL_HASH_free(h);
  pass();
}

void
test_basics()
{
  CSRL_HASH_t *h;
  h = CSRL_HASH_new();
  ok_m(h != NULL, "CSRL_HASH_new");

  CSRL_HASH_store(h, "foo", 3, (void *)42);
  CSRL_HASH_store(h, "barbaz", 6, (void *)43);
  ok_m(42 == (IV)CSRL_HASH_fetch(h, "foo", 3), "store/fetch");
  ok_m(43 == (UV)CSRL_HASH_fetch(h, "barbaz", 6), "store/fetch (2)");

  {
    CSRL_HASH_ENTRY_t *e = CSRL_HASH_find(h, "foo", 3);
    ok(e != NULL);
    ok_m(e->keylen == 3, "find: keylen");
    ok_m(!strncmp((char *)e->key, "foo", e->keylen), "find: key");
    ok_m(42 == (UV)e->value, "find: value");
  }

  ok_m(h->tbl_items == 2, "n items");
  ok_m(h->cur_iter == NULL, "no iterator");

  CSRL_HASH_store(h, "foo", 3, (void *)44);
  ok_m(44 == (IV)CSRL_HASH_fetch(h, "foo", 3), "store/fetch after overwrite");

  ok_m(h->tbl_items == 2, "n items (overwrite)");

  CSRL_HASH_free(h);
}

void
test_iter()
{
  IV i, j;
  CSRL_HASH_t *h;
  h = CSRL_HASH_new();
  ok_m(h != NULL, "CSRL_HASH_new");

  {
    /* empty iteration test, stack */
    CSRL_HASH_ITER_t it;
    CSRL_HASH_iter_init_flags(h, &it, 0);
    ok_m(CSRL_HASH_iter_next(&it) == NULL, "Nothing to iterate over (stack)");
  }

  {
    /* empty iteration test, heap*/
    CSRL_HASH_ITER_t *it = CSRL_HASH_iter_new(h);
    ok_m(CSRL_HASH_iter_next(it) == NULL, "Nothing to iterate over (heap)");
    CSRL_HASH_iter_free(it);
  }

  {
    /* empty iteration test, heap, autoclean*/
    CSRL_HASH_ITER_t *it = CSRL_HASH_iter_new_flags(h, CSRL_HASH_FLAG_AUTOCLEAN);
    ok_m(CSRL_HASH_iter_next(it) == NULL, "Nothing to iterate over (heap, autoclean)");
    ok_m(h->cur_iter != NULL, "iterator kept around");
  }

  CSRL_HASH_store(h, "foo", 3, (void *)12);
  CSRL_HASH_store(h, "bar", 3, (void *)13);

  {
    /* iteration test, stack */
    CSRL_HASH_ITER_t it;
    CSRL_HASH_iter_init_flags(h, &it, 0);
    i = (IV)CSRL_HASH_iter_next_value(&it);
    ok_m(i == 12 || i == 13, "first iter is valid (stack)");
    j = (IV)CSRL_HASH_iter_next_value(&it);
    ok_m(i-j == 1 || i-j == -1, "second iter is valid (stack)");
  }

  {
    /* empty iteration test, heap*/
    CSRL_HASH_ITER_t *it = CSRL_HASH_iter_new(h);
    i = (IV)CSRL_HASH_iter_next_value(it);
    ok_m(i == 12 || i == 13, "first iter is valid (heap)");
    j = (IV)CSRL_HASH_iter_next_value(it);
    ok_m(i-j == 1 || i-j == -1, "second iter is valid (heap)");
    CSRL_HASH_iter_free(it);
  }

  {
    /* empty iteration test, heap, autoclean*/
    CSRL_HASH_ITER_t *it = CSRL_HASH_iter_new_flags(h, CSRL_HASH_FLAG_AUTOCLEAN);
    i = (IV)CSRL_HASH_iter_next_value(it);
    ok_m(i == 12 || i == 13, "first iter is valid (heap, autoclean)");
    j = (IV)CSRL_HASH_iter_next_value(it);
    ok_m(i-j == 1 || i-j == -1, "second iter is valid (heap, autoclean)");
    ok_m(h->cur_iter != NULL, "iterator kept around");
  }

  CSRL_HASH_free(h);
}

int
main(int argc, char **argv)
{
  UNUSED(argc);
  UNUSED(argv);

  test_alloc();
  test_basics();
  test_iter();

  done_testing();
  return 0;
}

