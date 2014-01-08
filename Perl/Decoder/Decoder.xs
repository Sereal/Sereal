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

AV *
decode_with_header(dec, src, body_into = NULL, header_into = NULL)
    srl_decoder_t *dec;
    SV *src;
    SV *body_into;
    SV *header_into;
  CODE:
    if (header_into == NULL)
        header_into = sv_newmortal();
    if (body_into == NULL)
        body_into = sv_newmortal();
    srl_decode_all_into(aTHX_ dec, src, header_into, body_into, 0);
    RETVAL = newAV();
    sv_2mortal((SV *)RETVAL);
    av_extend(RETVAL, 1);
    av_store(RETVAL, 0, header_into);
    av_store(RETVAL, 1, body_into);
  OUTPUT: RETVAL

AV *
decode_with_header_and_offset(dec, src, offset, body_into = NULL, header_into = NULL)
    srl_decoder_t *dec;
    SV *src;
    UV offset;
    SV *body_into;
    SV *header_into;
  CODE:
    if (header_into == NULL)
        header_into = sv_newmortal();
    if (body_into == NULL)
        body_into = sv_newmortal();
    srl_decode_all_into(aTHX_ dec, src, header_into, body_into, offset);
    RETVAL = newAV();
    sv_2mortal((SV *)RETVAL);
    av_extend(RETVAL, 1);
    av_store(RETVAL, 0, header_into);
    av_store(RETVAL, 1, body_into);
  OUTPUT: RETVAL



void
decode_only_header(dec, src, header_into = NULL)
    srl_decoder_t *dec;
    SV *src;
    SV *header_into;
  PPCODE:
    ST(0)= srl_decode_header_into(aTHX_ dec, src, header_into, 0);
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
decode_only_header_with_offset(dec, src, offset, header_into = NULL)
    srl_decoder_t *dec;
    SV *src;
    UV offset;
    SV *header_into;
  PPCODE:
    ST(0)= srl_decode_header_into(aTHX_ dec, src, header_into, offset);
    XSRETURN(1);


void
decode_sereal(src, opt = NULL, into = NULL)
    SV *src;
    SV *opt;
    SV *into;
  PREINIT:
    srl_decoder_t *dec= NULL;
  PPCODE:
    if (SvROK(src))
        croak("We can't decode a reference as Sereal!");
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

AV *
decode_sereal_with_header_data(src, opt = NULL, body_into = NULL, header_into = NULL)
    SV *src;
    SV *opt;
    SV *body_into;
    SV *header_into;
  PREINIT:
    srl_decoder_t *dec= NULL;
  CODE:
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
    if (body_into == NULL)
      body_into = sv_newmortal();
    if (header_into == NULL)
      header_into = sv_newmortal();
    srl_decode_all_into(aTHX_ dec, src, header_into, body_into, 0);
    RETVAL = newAV();
    sv_2mortal((SV *)RETVAL);
    av_extend(RETVAL, 1);
    av_store(RETVAL, 0, header_into);
    av_store(RETVAL, 1, body_into);
  OUTPUT: RETVAL

IV
looks_like_sereal(...)
  PREINIT:
    SV *data;
    char *strdata;
    STRLEN len;
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
        if (len < SRL_MAGIC_STRLEN+3 /* at least one version/flag byte, one byte for header len, one type byte (smallest payload) */
            || strnNE(strdata, SRL_MAGIC_STRING, SRL_MAGIC_STRLEN)
            || strdata[SRL_MAGIC_STRLEN] == (U8)0) /* FIXME this check could be much better using the proto versions and all*/
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

