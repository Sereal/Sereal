#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include "srl_decoder.h"

/* Generated code for exposing C constants to Perl */
#include "srl_protocol.h"
#include "const-c.inc"


MODULE = Sereal::Decoder        PACKAGE = Sereal::Decoder
PROTOTYPES: DISABLE

void
decode_sereal(src, opt = newHV())
    SV *src;
    HV *opt;
  PREINIT:
    srl_decoder_t *dec;
  PPCODE:
    dec = build_decoder_struct(aTHX_ opt, src);
    assert(dec != NULL);
    if (0 == srl_read_header(aTHX_ dec)) {
        ST(0)= srl_read_sv(aTHX_ dec);
    }
    /* FIXME optimization: avoid copy by stealing string buffer if
     *                     it is not too large. */
    assert(dec->pos == dec->buf_end);
    XSRETURN(1);


MODULE = Sereal::Decoder        PACKAGE = Sereal::Constants
PROTOTYPES: DISABLE

INCLUDE: const-xs.inc

