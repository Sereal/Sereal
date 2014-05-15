#ifndef mytap_h_
#define mytap_h_

#include <stdio.h>

#ifndef UNUSED
#define UNUSED(v) ((void)(v));
#endif

static unsigned int ntests = 0;

unsigned int get_ntests() {
  return ntests;
}

void increment_ntests() {
  ntests++;
}

void done_testing() {
  printf("1..%u\n", ntests);
}

void plan(int expected_tests) {
  printf("1..%u\n", expected_tests);
}

void pass() {
  printf("ok %u\n", ++ntests);
}

void fail() {
  printf("not ok %u\n", ++ntests);
}

int ok(int i) {
  printf("%sok %u\n", (i ? "" : "not "), ++ntests);
  return i;
}

int ok_m(int i, char *msg) {
  printf("%sok %u - %s\n", (i ? "" : "not "), ++ntests, msg);
  return i;
}

int is_int(int a, int b) {
  int res = ok(a == b);
  if (res == 0)
    printf("# Input was: got: '%i' expected: '%i'\n", a, b);
  return res;
}

int is_int_m(int a, int b, char *msg) {
  int res = ok_m(a == b, msg);
  if (res == 0)
    printf("# Input was: got: '%i' expected: '%i'\n", a, b);
  return res;
}

int is_double(double eps, double a, double b) {
  int res = ok(a+eps > b && a-eps < b);
  if (res == 0)
    printf("# Input was: got: '%f' expected: '%f'\n", a, b);
  return res;
}

int is_double_m(double eps, double a, double b, char *msg) {
  int res = ok_m(a+eps > b && a-eps < b, msg);
  if (res == 0)
    printf("# Input was: got: '%f' expected: '%f'\n", a, b);
  return res;
}

void note(char *msg) {
  printf("# %s\n", msg);
}

#endif
