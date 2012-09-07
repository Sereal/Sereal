#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <math.h>

#include "EXTERN.h"
#include "perl.h"
#include "ppport.h"

static U32 len_table_u32[] = {
  9, 99, 999, 9999,
  99999, 999999, 9999999, 99999999,
  999999999, UINT_MAX
};

U8
uint32_length(U32 num)
{
  U8 i;
  for (i = 0;; ++i) {
    if (num < len_table_u32[i])
      return i+1;
  }
}

#ifndef U64_CONST
# define U64_CONST(x) ((uint64_t)x##UL)
#endif

static uint64_t len_table_u64[] = {
  9, 99, 999, 9999,
  99999, 999999, 9999999, 99999999,
  U64_CONST(999999999),
  U64_CONST(9999999999),
  U64_CONST(99999999999),
  U64_CONST(999999999999),
  U64_CONST(9999999999999),
  U64_CONST(99999999999999),
  U64_CONST(999999999999999),
  U64_CONST(9999999999999999),
  U64_CONST(99999999999999999),
  U64_CONST(999999999999999999),
  U64_CONST(9999999999999999999),
  U64_CONST(18446744073709551615)
};

U8
uint64_length(uint64_t num)
{
  U8 i;
  for (i = 0;; ++i) {
    if (num < len_table_u64[i])
      return i+1;
  }
}


U8
uint64_length_2(uint64_t num)
{
  U8 len = 0;
  unsigned int tmp = 1UL;
  while(tmp < num)
  {
      ++len;
      tmp = (tmp << 3) + (tmp << 1);
  }
  return len;
}

U8
uint64_length_3(uint64_t num)
{
  return (U8)floor( log10( num ) ) + 1;
}

#define CASE(n,i) if (num < (uint64_t)(n##UL)) return i
U8
uint64_length_4(uint64_t num)
{
  CASE(9, 1);
  CASE(99, 2);
  CASE(999, 3);
  CASE(9999, 4);
  CASE(99999, 5);
  CASE(999999, 6);
  CASE(9999999, 7);
  CASE(99999999, 8);
  CASE(999999999, 9);
  CASE(9999999999, 10);
  CASE(99999999999, 11);
  CASE(999999999999, 12);
  CASE(9999999999999, 13);
  CASE(99999999999999, 14);
  CASE(999999999999999, 15);
  CASE(9999999999999999, 16);
  CASE(99999999999999999, 17);
  CASE(999999999999999999, 18);
  CASE(9999999999999999999, 19);
  return 20;
}
#undef CASE

int
main(int argc, char** argv) {
  unsigned int i = 0;

  for (i = 0; i < 300000000; ++i) {
    uint64_length_4(i);
  }

  return 1;
}
