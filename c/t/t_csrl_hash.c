#include "mytap.h"
#include <csrl_hash.h>

#include <stdlib.h>
#include <string.h>

void
test_alloc()
{
  CLH_t *h;

  note("Allocation tests");

  h = CLH_new(&CLH_VTABLE_free);
  ok_m(h != NULL, "CLH_new");
  ok_m(h->tbl_items == 0, "n items");
  CLH_free(h);
  pass();
}

void
test_basics()
{
  note("Basic tests");

  {
    CLH_t *h;

    h = CLH_new(&CLH_VTABLE_passthrough);
    ok_m(h != NULL, "CLH_new");

    CLH_store(h, "foo", 3, (void *)42);
    CLH_store(h, "barbaz", 6, (void *)43);
    ok_m(42 == (IV)CLH_fetch(h, "foo", 3), "store/fetch");
    ok_m(43 == (UV)CLH_fetch(h, "barbaz", 6), "store/fetch (2)");

    {
      CLH_entry_t *e = CLH_find(h, "foo", 3);
      ok(e != NULL);
      ok_m(e->keylen == 3, "find: keylen");
      ok_m(!strncmp((char *)e->key, "foo", e->keylen), "find: key");
      ok_m(42 == (UV)e->value, "find: value");
    }

    ok_m(h->tbl_items == 2, "n items");
    ok_m(h->cur_iter == NULL, "no iterator");

    CLH_store(h, "foo", 3, (void *)44);
    ok_m(44 == (IV)CLH_fetch(h, "foo", 3), "store/fetch after overwrite");

    ok_m(h->tbl_items == 2, "n items (overwrite)");

    ok_m(44 == (IV)CLH_delete(h, "foo", 3), "delete");
    ok_m(NULL == CLH_fetch(h, "foo", 3), "fetch after delete");

    ok_m(h->tbl_items == 1, "n items (delete)");

    CLH_free(h);
  }
}

static void *my_free_cb(void *value)
{
  char *retval = strdup((char *)value);
  free(value);
  retval[0] = 'Q';
  return retval;
}

void test_vtable()
{

  note("vtable tests");

  /* Test the CLH_VTABLE_free vtable - at least if running with valgrind */
  {
    CLH_t *h;
    char *str;

    h = CLH_new(&CLH_VTABLE_free);
    str = strdup("foo");
    CLH_store(h, "bar", 3, str);
    
    CLH_free(h);
  }

  /* Test our own vtable */
  {
    CLH_vtable_t vt = {my_free_cb};
    CLH_t *h;
    char *str;

    h = CLH_new(&vt);
    str = strdup("foo");
    CLH_store(h, "bar", 3, str);
    str = (char *)CLH_delete(h, "bar", 3);
    if (!ok_m(!strncmp(str, "Qoo", 3), "own vtable"))
      printf("# return value was '%s'\n", str);

    free(str);
    
    CLH_free(h);
  }
}

void
test_iter()
{
  IV i, j;
  CLH_t *h;

  note("Iterator tests");

  h = CLH_new(NULL);
  ok_m(h != NULL, "CLH_new");

  {
    /* empty iteration test, stack */
    CLH_iter_t it;
    CLH_iter_init_flags(h, &it, 0);
    ok_m(CLH_iter_next(&it) == NULL, "Nothing to iterate over (stack)");
  }

  {
    /* empty iteration test, heap*/
    CLH_iter_t *it = CLH_iter_new(h);
    ok_m(CLH_iter_next(it) == NULL, "Nothing to iterate over (heap)");
    CLH_iter_free(it);
  }

  {
    /* empty iteration test, heap, autoclean*/
    CLH_iter_t *it = CLH_iter_new_flags(h, CLH_FLAG_AUTOCLEAN);
    ok_m(CLH_iter_next(it) == NULL, "Nothing to iterate over (heap, autoclean)");
    ok_m(h->cur_iter != NULL, "iterator kept around");
  }

  CLH_free(h);

  h = CLH_new(&CLH_VTABLE_passthrough);
  CLH_store(h, "foo", 3, (void *)12);
  CLH_store(h, "bar", 3, (void *)13);

  {
    /* iteration test, stack */
    CLH_iter_t it;
    CLH_iter_init_flags(h, &it, 0);
    i = (IV)CLH_iter_next_value(&it);
    ok_m(i == 12 || i == 13, "first iter is valid (stack)");
    j = (IV)CLH_iter_next_value(&it);
    ok_m(i-j == 1 || i-j == -1, "second iter is valid (stack)");
  }

  {
    /* empty iteration test, heap*/
    CLH_iter_t *it = CLH_iter_new(h);
    i = (IV)CLH_iter_next_value(it);
    ok_m(i == 12 || i == 13, "first iter is valid (heap)");
    j = (IV)CLH_iter_next_value(it);
    ok_m(i-j == 1 || i-j == -1, "second iter is valid (heap)");
    CLH_iter_free(it);
  }

  {
    /* empty iteration test, heap, autoclean*/
    CLH_iter_t *it = CLH_iter_new_flags(h, CLH_FLAG_AUTOCLEAN);
    i = (IV)CLH_iter_next_value(it);
    ok_m(i == 12 || i == 13, "first iter is valid (heap, autoclean)");
    j = (IV)CLH_iter_next_value(it);
    ok_m(i-j == 1 || i-j == -1, "second iter is valid (heap, autoclean)");
    ok_m(h->cur_iter != NULL, "iterator kept around");
  }

  CLH_free(h);
}

int
main(int argc, char **argv)
{
  UNUSED(argc);
  UNUSED(argv);

  test_alloc();
  test_basics();
  test_vtable();
  test_iter();

  done_testing();
  return 0;
}

