#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_common.h"
#include "srl_merger.h"
#include "srl_protocol.h"
#include "srl_buffer.h"
#include "strtable.h"

typedef srl_merger_t * Sereal__Merger;

MODULE = Sereal::Merger		PACKAGE = Sereal::Merger
PROTOTYPES: DISABLE

srl_merger_t *
new(CLASS, opt = NULL)
    char *CLASS;
    HV *opt;
  CODE:
    RETVAL = srl_build_merger_struct(aTHX_ opt);
  OUTPUT: RETVAL

void
DESTROY(mrg)
    srl_merger_t *mrg;
  CODE:
    srl_destroy_merger(aTHX_ mrg);

void
append(mrg, src)
    srl_merger_t *mrg;
    SV *src
  PPCODE:
    srl_merger_append(aTHX_ mrg, src);

void
append_all(mrg, src)
    srl_merger_t *mrg;
    AV *src
  PPCODE:
    srl_merger_append_all(aTHX_ mrg, src);

SV*
finish(mrg, user_header = NULL)
    srl_merger_t *mrg;
    SV *user_header;
  CODE:
    RETVAL = srl_merger_finish(aTHX_ mrg, user_header);
  OUTPUT: RETVAL

UV
elements_merged(mrg)
    srl_merger_t *mrg;
  CODE:
    RETVAL = (UV) mrg->cnt_of_merged_elements;
  OUTPUT: RETVAL

MODULE = Sereal::Merger        PACKAGE = Sereal::Merger::_strtabletest

void
test()
  PREINIT:
    STRTABLE_t *tbl;
    STRTABLE_ENTRY_t *ent;
    srl_buffer_t buf;
    UV i, len, n = 15;
    int found;

    char *testset[15] = {
        "S",
        "SH",
        "SHO",
        "SHOR",
        "SHORT",
        "SHORT_",
        "SHORT_B",
        "SHORT_BI",
        "SHORT_BIN",
        "SHORT_BINA",
        "SHORT_BINAR",
        "SHORT_BINARY",
        "SHORT_BINARY_",
        "SHORT_BINARY_1",
        "SHORT_BINARY_14",
    };
  CODE:
    srl_buf_init_buffer(aTHX_ &buf, 1024);
    tbl = STRTABLE_new(&buf);

    for (i = 0; i < n; ++i) {
      len = i + 1;
      *buf.pos++ = SRL_HDR_SHORT_BINARY_LOW + i;
      Copy(testset[i], buf.pos, len, char);
      buf.pos += len;

      ent = STRTABLE_insert(tbl, (unsigned char*) testset[i], len, &found);
      ent->offset = BODY_POS_OFS(&buf) - len;

      printf("%sok %u - insert %.*s\n", found ? "not " : "", (unsigned int)(1+i), (int)len, testset[i]);
      if (found) abort();
    }

    buf.pos = buf.start;
    for (i = n; i > 0; --i) {
      len = i;
      ent = STRTABLE_insert(tbl, (unsigned char*) testset[i - 1], len, &found);
      printf("%sok %u - fetch %.*s\n", found ? "" : "not ", (unsigned int)(n+n-i+1), (int)len, testset[i-1]);
      if (!found) abort();
    }

    STRTABLE_free(tbl);
    srl_buf_free_buffer(aTHX_ &buf);

#define POPULATE_STRTABLE(from, to)                     \
    for (i = from; i < to; ++i) {                       \
        len = sprintf((char*) b, "%d", (int) i);        \
        Copy(b, buf.pos, len, char);                    \
        buf.pos += len;                                 \
                                                        \
        ent = STRTABLE_insert(tbl, b, len, &found);     \
        ent->offset = i;                                \
        if (found) abort(); /* shoul never happen */    \
    }

void
test_purge()
  PREINIT:
    STRTABLE_t *tbl;
    STRTABLE_ENTRY_t *ent;
    srl_buffer_t buf;
    UV i, len;
    unsigned char b[128];
    int found;
  CODE:
    srl_buf_init_buffer(aTHX_ &buf, 1024 * 1024);
    tbl = STRTABLE_new(&buf);

    POPULATE_STRTABLE(0, 10);
    STRTABLE_purge(tbl, 0);
    printf("%sok - STRTABLE purged to 0\n", tbl->tbl_items == 0 ? "" : "not ");

    POPULATE_STRTABLE(0, 100);
    STRTABLE_purge(tbl, 0);
    printf("%sok - STRTABLE purged to 0\n", tbl->tbl_items == 0 ? "" : "not ");

    POPULATE_STRTABLE(0, 500);
    STRTABLE_purge(tbl, 0);
    printf("%sok - STRTABLE purged to 0\n", tbl->tbl_items == 0 ? "" : "not ");

    POPULATE_STRTABLE(0, 500);
    STRTABLE_purge(tbl, 100);
    printf("%sok - STRTABLE purged to 100\n", tbl->tbl_items == 100 ? "" : "not ");

    POPULATE_STRTABLE(100, 200);
    printf("%sok - STRTABLE filled to 200\n", tbl->tbl_items == 200 ? "" : "not ");

    STRTABLE_free(tbl);
    srl_buf_free_buffer(aTHX_ &buf);
