#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include "srl_decoder.h"
#include "srl_protocol.h"

/* Generated code for exposing C constants to Perl */
#include "const-c.inc"


MODULE = Sereal::Decoder        PACKAGE = Sereal::Decoder
PROTOTYPES: DISABLE

TYPEMAP: <<HERE
srl_decoder_t*	O_OBJECT
HERE

srl_decoder_t *
new(CLASS, opt = NULL)
    char *CLASS;
    HV *opt;
  CODE:
    RETVAL = srl_build_decoder_struct(aTHX_ opt);
    RETVAL->flags |= SRL_F_REUSE_DECODER;
  OUTPUT: RETVAL

void
DESTROY(dec)
    srl_decoder_t *dec;
  CODE:
    srl_destroy_decoder(aTHX_ dec);


void
decode(dec, src, into = NULL)
    srl_decoder_t *dec;
    SV *src;
    SV *into;
  PPCODE:
    assert(dec != NULL);
    srl_begin_decoding(aTHX_ dec, src);
    if (0 == srl_read_header(aTHX_ dec)) {
        if (!into) {
            into= sv_2mortal(newSV_type(SVt_NULL));
        }
        srl_read_single_value(aTHX_ dec, into);
    }
    if ( 0 == srl_finalize_structure(aTHX_ dec) ) {
        ST(0)= into;
    } else {
        srl_clear_decoder(aTHX_ dec);
        ERROR("finalize failed");
    }
    assert(dec->pos == dec->buf_end);
    srl_clear_decoder(aTHX_ dec);
    XSRETURN(1);


void
decode_sereal(src, opt = NULL, into = NULL)
    SV *src;
    SV *opt;
    SV *into;
  PREINIT:
    srl_decoder_t *dec= NULL;
  PPCODE:
    /* Support no opt at all, undef, hashref */
    if (opt != NULL) {
        SvGETMAGIC(opt);
        if (!SvOK(opt))
            opt = NULL;
        else if (SvROK(opt) && SvTYPE(SvRV(opt)) == SVt_PVHV)
            opt = (SV *)SvRV(opt);
        else
            croak("Options are neither undef nor hash reference");
    }
    dec = srl_build_decoder_struct(aTHX_ (HV *)opt);
    srl_begin_decoding(aTHX_ dec, src);
    assert(dec != NULL);
    if (0 == srl_read_header(aTHX_ dec)) {
        if (!into) {
            into= sv_2mortal(newSV_type(SVt_NULL));
        }
        srl_read_single_value(aTHX_ dec, into);
    }
    if ( 0 == srl_finalize_structure(aTHX_ dec) ) {
        ST(0)= into;
    } else {
        ERROR("finalize failed");
    }
    assert(dec->pos == dec->buf_end);
    XSRETURN(1);


MODULE = Sereal::Decoder        PACKAGE = Sereal::Decoder::Constants
PROTOTYPES: DISABLE

INCLUDE: const-xs.inc

