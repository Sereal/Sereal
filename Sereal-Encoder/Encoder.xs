#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include "srl_encoder.h"

/* Generated code for exposing C constants to Perl */
#include "srl_protocol.h"
#include "const-c.inc"

#include "ptable.h"

MODULE = Sereal::Encoder        PACKAGE = Sereal::Encoder
PROTOTYPES: DISABLE

void
encode_sereal(src, opt = newHV())
    SV *src;
    HV *opt;
  PREINIT:
    srl_encoder_t *enc;
  PPCODE:
    enc = build_encoder_struct(aTHX_ opt);
    assert(enc != NULL);
    srl_write_header(aTHX_ enc);
    srl_dump_sv(aTHX_ enc, src);
    /* FIXME optimization: avoid copy by stealing string buffer if
     *                     it is not too large. */
    assert(enc->pos > enc->buf_start);
    ST(0) = sv_2mortal(newSVpvn(enc->buf_start, (STRLEN)(enc->pos - enc->buf_start)));
    XSRETURN(1);


MODULE = Sereal::Encoder        PACKAGE = Sereal::Constants
PROTOTYPES: DISABLE

INCLUDE: const-xs.inc

MODULE = Sereal::Encoder        PACKAGE = Sereal::Encoder::_ptabletest

void
test()
  PREINIT:
    PTABLE_t *tbl;
    PTABLE_ITER_t *iter;
    PTABLE_ENTRY_t *ent;
    UV i, n = 20;
    char *check[20];
    char fail[5] = "not ";
    char noop[1] = "";
  CODE:
    tbl = PTABLE_new_size(10);
    for (i = 0; i < (UV)n; ++i) {
      PTABLE_store(tbl, (void *)(1000+i), (void *)(1000+i));
      check[i] = fail;
    }
    for (i = 0; i < (UV)n; ++i) {
      const UV res = (UV)PTABLE_fetch(tbl, (void *)(1000+i));
      printf("%sok %u - fetch %u\n", (res == (UV)(1000+i)) ? noop : fail, (unsigned int)(1+i), (unsigned int)(i+1));
    }
    iter = PTABLE_iter_new(tbl);
    while ( NULL != (ent = PTABLE_iter_next(iter)) ) {
      const UV res = ((UV)ent->value) - 1000;
      if (res < 20)
        check[res] = noop;
      else
        abort();
    }
    for (i = 0; i < (UV)n; ++i) {
      printf("%sok %u - iter %u\n", check[i], (unsigned int)(21+i), (unsigned int)(i+1));
    }
    PTABLE_iter_free(iter);
    PTABLE_free(tbl);
