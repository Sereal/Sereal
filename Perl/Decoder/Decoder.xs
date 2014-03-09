/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#define NEED_newSV_type
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include "srl_common.h"
#include "srl_decoder.h"
#include "srl_protocol.h"

/* Generated code for exposing C constants to Perl */
#include "const-c.inc"

#if defined(cv_set_call_checker) && defined(XopENTRY_set)
# define USE_CUSTOM_OPS 1
#else
# define USE_CUSTOM_OPS 0
#endif

#define OPOPT_DO_BODY       (1<<0)
#define OPOPT_DO_HEADER     (1<<1)
#define OPOPT_OFFSET        (1<<2)
#define OPOPT_OUTARG_BODY   (1<<3)
#define OPOPT_OUTARG_HEADER (1<<4)

#define pp1_sereal_decode(opopt) THX_pp1_sereal_decode(aTHX_ opopt)
static void
THX_pp1_sereal_decode(pTHX_ U8 opopt)
{
    bool need_retvalue = GIMME_V != G_VOID;
    SV *decoder_ref_sv, *decoder_sv, *src_sv;
    UV offset;
    SV *body_into, *header_into;
    srl_decoder_t *decoder;
    char *stash_name;
    dSP;
    header_into = expect_false(opopt & OPOPT_OUTARG_HEADER) ? POPs :
            expect_false(opopt & OPOPT_DO_HEADER) ? sv_newmortal() : NULL;
    body_into = expect_false(opopt & OPOPT_OUTARG_BODY) ? POPs :
            expect_true(opopt & OPOPT_DO_BODY) ? sv_newmortal() : NULL;
    offset = expect_false(opopt & OPOPT_OFFSET) ? SvUVx(POPs) : 0;
    src_sv = POPs;
    decoder_ref_sv = POPs;
    PUTBACK;
    if(!(
                    decoder_ref_sv &&
                    SvROK(decoder_ref_sv) &&
                    (decoder_sv = SvRV(decoder_ref_sv)) &&
                    SvOBJECT(decoder_sv) &&
                    (stash_name = HvNAME(SvSTASH(decoder_sv))) &&
                    !strcmp(stash_name, "Sereal::Decoder")
    )) {
        croak("handle is not a Sereal::Decoder handle");
    }
    decoder = (srl_decoder_t *)SvIV(decoder_sv);
    if(expect_true(opopt & OPOPT_DO_BODY)) {
            if(opopt & OPOPT_DO_HEADER) {
                srl_decode_all_into(aTHX_ decoder, src_sv, header_into,
                    body_into, offset);
            } else {
                srl_decode_into(aTHX_ decoder, src_sv, body_into, offset);
            }
    } else {
        srl_decode_header_into(aTHX_ decoder, src_sv, header_into, offset);
    }
    if(expect_true(need_retvalue)) {
        SV *retvalue;
        if(expect_true(opopt & OPOPT_DO_BODY)) {
            if(opopt & OPOPT_DO_HEADER) {
                AV *retav = newAV();
                retvalue = newRV_noinc((SV*)retav);
                sv_2mortal(retvalue);
                av_extend(retav, 1);
                av_store(retav, 0, SvREFCNT_inc(header_into));
                av_store(retav, 1, SvREFCNT_inc(body_into));
            } else {
                retvalue = body_into;
            }
        } else {
            retvalue = header_into;
        }
        SPAGAIN;
        XPUSHs(retvalue);
        PUTBACK;
    }
}

#if USE_CUSTOM_OPS

static OP *
THX_pp_sereal_decode(pTHX)
{
    pp1_sereal_decode(PL_op->op_private);
    return NORMAL;
}

static OP *
THX_ck_entersub_args_sereal_decoder(pTHX_ OP *entersubop, GV *namegv, SV *ckobj)
{
    CV *cv = (CV*)ckobj;
    I32 cv_private = CvXSUBANY(cv).any_i32;
    U8 opopt = cv_private & 0xff;
    U8 min_arity = (cv_private >> 8) & 0xff;
    U8 max_arity = (cv_private >> 16) & 0xff;
    OP *pushop, *firstargop, *cvop, *lastargop, *argop, *newop;
    int arity;
    entersubop = ck_entersub_args_proto(entersubop, namegv, (SV*)cv);
    pushop = cUNOPx(entersubop)->op_first;
    if(!pushop->op_sibling) pushop = cUNOPx(pushop)->op_first;
    firstargop = pushop->op_sibling;
    for (cvop = firstargop; cvop->op_sibling; cvop = cvop->op_sibling) ;
    lastargop = pushop;
    for (arity = 0, lastargop = pushop, argop = firstargop; argop != cvop;
            lastargop = argop, argop = argop->op_sibling)
        arity++;
    if(expect_false(arity < min_arity || arity > max_arity)) return entersubop;
    if(arity > min_arity && (opopt & OPOPT_DO_BODY)) {
        opopt |= OPOPT_OUTARG_BODY;
        min_arity++;
    }
    if(arity > min_arity) opopt |= OPOPT_OUTARG_HEADER;
    pushop->op_sibling = cvop;
    lastargop->op_sibling = NULL;
    op_free(entersubop);
    newop = newUNOP(OP_CUSTOM, 0, firstargop);
    newop->op_private = opopt;
    newop->op_ppaddr = THX_pp_sereal_decode;
    return newop;
}

#endif /* USE_CUSTOM_OPS */

static void
THX_xsfunc_sereal_decode(pTHX_ CV *cv)
{
    dMARK;
    dSP;
    SSize_t arity = SP - MARK;
    I32 cv_private = CvXSUBANY(cv).any_i32;
    U8 opopt = cv_private & 0xff;
    U8 min_arity = (cv_private >> 8) & 0xff;
    U8 max_arity = (cv_private >> 16) & 0xff;
    if(arity < min_arity || arity > max_arity)
        croak("bad Sereal decoder usage");
    if(arity > min_arity && (opopt & OPOPT_DO_BODY)) {
        opopt |= OPOPT_OUTARG_BODY;
        min_arity++;
    }
    if(arity > min_arity) opopt |= OPOPT_OUTARG_HEADER;
    pp1_sereal_decode(opopt);
}

MODULE = Sereal::Decoder        PACKAGE = Sereal::Decoder
PROTOTYPES: DISABLE

BOOT:
{
    struct {
        char const *name_suffix;
        U8 opopt;
    } const funcs_to_install[] = {
        { "", OPOPT_DO_BODY },
        { "_only_header", OPOPT_DO_HEADER },
        { "_with_header", (OPOPT_DO_BODY|OPOPT_DO_HEADER) },
        { "_with_offset", (OPOPT_DO_BODY|OPOPT_OFFSET) },
        { "_only_header_with_offset", (OPOPT_DO_HEADER|OPOPT_OFFSET) },
        { "_with_header_and_offset",
            (OPOPT_DO_BODY|OPOPT_DO_HEADER|OPOPT_OFFSET) },
    }, *fti;
    int i;
#if USE_CUSTOM_OPS
    {
        XOP *xop;
        Newxz(xop, 1, XOP);
        XopENTRY_set(xop, xop_name, "sereal_decode");
        XopENTRY_set(xop, xop_desc, "sereal_decode");
        XopENTRY_set(xop, xop_class, OA_UNOP);
        Perl_custom_op_register(aTHX_ THX_pp_sereal_decode, xop);
    }
#endif /* USE_CUSTOM_OPS */
    for(i = sizeof(funcs_to_install)/sizeof(*fti); i--; ) {
        char name[55];
        char proto[7], *p = proto;
        U8 opopt;
        I32 cv_private;
        GV *gv;
        fti = &funcs_to_install[i];
        opopt = fti->opopt;
        cv_private = opopt | 0x020200;
        CV *cv;
        *p++ = '$';
        *p++ = '$';
        if(opopt & OPOPT_OFFSET) {
            *p++ = '$';
            cv_private += 0x010100;
        }
        *p++ = ';';
        if(opopt & OPOPT_DO_BODY) {
            *p++ = '$';
            cv_private += 0x010000;
        }
        if(opopt & OPOPT_DO_HEADER) {
            *p++ = '$';
            cv_private += 0x010000;
        }
        *p = 0;
        sprintf(name, "Sereal::Decoder::sereal_decode%s", fti->name_suffix);
        cv = newXSproto_portable(name, THX_xsfunc_sereal_decode, __FILE__,
                proto);
        CvXSUBANY(cv).any_i32 = cv_private;
#if USE_CUSTOM_OPS
        cv_set_call_checker(cv, THX_ck_entersub_args_sereal_decoder, (SV*)cv);
#endif /* USE_CUSTOM_OPS */
        sprintf(name, "Sereal::Decoder::decode%s", fti->name_suffix);
        gv = gv_fetchpv(name, GV_ADDMULTI, SVt_PVCV);
        GvCV_set(gv, cv);
    }
}

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
    av_store(RETVAL, 0, SvREFCNT_inc(header_into));
    av_store(RETVAL, 1, SvREFCNT_inc(body_into));
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

