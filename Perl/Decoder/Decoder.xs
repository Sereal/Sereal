/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#define NEED_newSV_type
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
    ST(0)= srl_decode_into(aTHX_ dec, src, into, 0);
    XSRETURN(1);


void
decode_with_offset(dec, src, offset, into = NULL)
    srl_decoder_t *dec;
    SV *src;
    UV offset;
    SV *into;
  PPCODE:
    ST(0)= srl_decode_into(aTHX_ dec, src, into, offset);
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
    ST(0)= srl_decode_into(aTHX_ dec, src, into, 0);
    XSRETURN(1);

IV
looks_like_sereal(...)
  PREINIT:
    SV *data;
    char *strdata;
    STRLEN len;
    const STRLEN magic_len = strlen(SRL_MAGIC_STRING);
  CODE:
    RETVAL = 1;
    if (items > 2 || items == 0) {
        croak("Invalid number of parameters to looks_like_sereal: "
              "Need one data parameter, possibly preceded by an invocant.");
    }
    data = ST(items-1); /* 1 or two items, use the last parameter as data */
    if (!SvOK(data))
        RETVAL = 0;
    else {
        strdata = SvPV(data, len);
        if (len < magic_len+3 /* at least one version/flag byte, one byte for header len, one type byte (smallest payload) */
            || strnNE(strdata, SRL_MAGIC_STRING, magic_len)
            || strdata[magic_len] == (U8)0) /* FIXME this check could be much better using the proto versions and all*/
        {
            RETVAL = 0;
        }
    }
  OUTPUT: RETVAL

UV
bytes_consumed(dec)
    srl_decoder_t *dec;
  CODE:
    RETVAL = dec->bytes_consumed;
  OUTPUT: RETVAL


MODULE = Sereal::Decoder        PACKAGE = Sereal::Decoder::Constants
PROTOTYPES: DISABLE

INCLUDE: const-xs.inc

