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

#ifndef GvCV_set
# define GvCV_set(gv, cv) (GvCV(gv) = (cv))
#endif

#ifndef PERL_ARGS_ASSERT_CROAK_XS_USAGE
#define PERL_ARGS_ASSERT_CROAK_XS_USAGE assert(cv); assert(params)

/* prototype to pass -Wmissing-prototypes */
STATIC void
S_croak_xs_usage(pTHX_ const CV *const cv, const char *const params);

STATIC void
S_croak_xs_usage(pTHX_ const CV *const cv, const char *const params)
{
    const GV *const gv = CvGV(cv);

    PERL_ARGS_ASSERT_CROAK_XS_USAGE;

    if (gv) {
        const char *const gvname = GvNAME(gv);
        const HV *const stash = GvSTASH(gv);
        const char *const hvname = stash ? HvNAME(stash) : NULL;

        if (hvname)
            Perl_croak_nocontext("Usage: %s::%s(%s)", hvname, gvname, params);
        else
            Perl_croak_nocontext("Usage: %s(%s)", gvname, params);
    } else {
        /* Pants. I don't think that it should be possible to get here. */
        Perl_croak_nocontext("Usage: CODE(0x%"UVxf")(%s)", PTR2UV(cv), params);
    }
}

#ifdef PERL_IMPLICIT_CONTEXT
#define croak_xs_usage(a,b)     S_croak_xs_usage(aTHX_ a,b)
#else
#define croak_xs_usage          S_croak_xs_usage
#endif

#endif


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
#define OPOPT_LOOKS_LIKE    (1<<5)

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

    header_into = expect_false(opopt & OPOPT_OUTARG_HEADER)
                  ? POPs
                  : expect_false(opopt & OPOPT_DO_HEADER) ? sv_newmortal() : NULL;
    body_into = expect_false(opopt & OPOPT_OUTARG_BODY)
                ? POPs
                : expect_true(opopt & OPOPT_DO_BODY) ? sv_newmortal() : NULL;

    offset = expect_false(opopt & OPOPT_OFFSET) ? SvUVx(POPs) : 0;
    src_sv = POPs;
    decoder_ref_sv = POPs;
    PUTBACK;

    if (!expect_true(
          decoder_ref_sv &&
          SvROK(decoder_ref_sv) &&
          (decoder_sv = SvRV(decoder_ref_sv)) &&
          SvOBJECT(decoder_sv) &&
          (stash_name = HvNAME(SvSTASH(decoder_sv))) &&
          !strcmp(stash_name, "Sereal::Decoder")
       ))
    {
        croak("handle is not a Sereal::Decoder handle");
    }

    decoder = (srl_decoder_t *)SvIV(decoder_sv);
    if (expect_true(opopt & OPOPT_DO_BODY)) {
        if (opopt & OPOPT_DO_HEADER) {
             srl_decode_all_into(aTHX_ decoder, src_sv, header_into,
                                 body_into, offset);
        } else {
            srl_decode_into(aTHX_ decoder, src_sv, body_into, offset);
        }
    } else {
        srl_decode_header_into(aTHX_ decoder, src_sv, header_into, offset);
    }

    if (expect_true(need_retvalue)) {
        SV *retvalue;
        if (expect_true(opopt & OPOPT_DO_BODY)) {
            if (opopt & OPOPT_DO_HEADER) {
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

#define pp1_looks_like_sereal() THX_pp1_looks_like_sereal(aTHX)
static void
THX_pp1_looks_like_sereal(pTHX)
{
    dSP;
    SV *data= TOPs;
    /* Should this be SvPOK()? Or better yet, check if it's *really* a string pointer: SvPOKp(data). After all
       the serialization format is a string and anything otherwise would not look sereal. */
    if ( SvOK(data) ) {
        STRLEN len;
        char *strdata= SvPV(data, len);
        IV ret= srl_validate_header_version_pv_len(aTHX_ strdata, len);
        if ( ret < 0 ) {
            SETs(&PL_sv_no);
        } else {
            SETs(newSViv(ret & SRL_PROTOCOL_VERSION_MASK));
        }
    } else {
        SETs(&PL_sv_no);
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
THX_pp_looks_like_sereal(pTHX)
{
    pp1_looks_like_sereal();
    return NORMAL;
}

static OP *
THX_ck_entersub_args_sereal_decoder(pTHX_ OP *entersubop, GV *namegv, SV *ckobj)
{

   /* pull apart a standard entersub op tree */

    CV *cv = (CV*)ckobj;
    I32 cv_private = CvXSUBANY(cv).any_i32;
    U8 opopt = cv_private & 0xff;
    U8 min_arity = (cv_private >> 8) & 0xff;
    U8 max_arity = (cv_private >> 16) & 0xff;
    OP *pushop, *firstargop, *cvop, *lastargop, *argop, *newop;
    int arity;

    /* Walk the OP structure under the "entersub" to validate that we
     * can use the custom OP implementation. */

    entersubop = ck_entersub_args_proto(entersubop, namegv, (SV*)cv);
    pushop = cUNOPx(entersubop)->op_first;
    if ( ! OpHAS_SIBLING(pushop) )
        pushop = cUNOPx(pushop)->op_first;
    firstargop = OpSIBLING(pushop);

    for (cvop = firstargop; OpHAS_SIBLING(cvop); cvop = OpSIBLING(cvop)) ;

    lastargop = pushop;
    for (
        arity = 0, lastargop = pushop, argop = firstargop;
        argop != cvop;
        lastargop = argop, argop = OpSIBLING(argop)
    ){
        arity++;
    }

    if (expect_false(arity < min_arity || arity > max_arity))
        return entersubop;

    /* If we get here, we can replace the entersub with a suitable
     * custom OP. */

    if (arity > min_arity && (opopt & OPOPT_DO_BODY)) {
        opopt |= OPOPT_OUTARG_BODY;
        min_arity++;
    }

    if (arity > min_arity)
        opopt |= OPOPT_OUTARG_HEADER;

    OpMORESIB_set(pushop, cvop);
    OpLASTSIB_set(lastargop, op_parent(lastargop));
    op_free(entersubop);
    newop = newUNOP(OP_NULL, 0, firstargop);
    newop->op_type    = OP_CUSTOM;
    newop->op_private = opopt;
    newop->op_ppaddr = opopt & OPOPT_LOOKS_LIKE ? THX_pp_looks_like_sereal : THX_pp_sereal_decode;
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

    if (arity < min_arity || arity > max_arity)
        croak("bad Sereal decoder usage");
    if (arity > min_arity && (opopt & OPOPT_DO_BODY)) {
        opopt |= OPOPT_OUTARG_BODY;
        min_arity++;
    }
    if (arity > min_arity)
        opopt |= OPOPT_OUTARG_HEADER;

    pp1_sereal_decode(opopt);
}

static void
THX_xsfunc_looks_like_sereal(pTHX_ CV *cv)
{
    dMARK;
    dSP;
    SSize_t arity = SP - MARK;
    I32 cv_private = CvXSUBANY(cv).any_i32;
    U8 max_arity = (cv_private >> 16) & 0xff;

    if (arity < 1 || arity > max_arity)
        croak_xs_usage(cv, max_arity == 1 ? "data" : "[invocant,] data");
    if(arity == 2) {
        SV *data = POPs;
        SETs(data);
        PUTBACK;
    }
    pp1_looks_like_sereal();
}

#define MY_CXT_KEY "Sereal::Decoder::_stash" XS_VERSION

typedef struct {
    sv_with_hash options[SRL_DEC_OPT_COUNT];
} my_cxt_t;

START_MY_CXT


MODULE = Sereal::Decoder        PACKAGE = Sereal::Decoder
PROTOTYPES: DISABLE

BOOT:
{
    struct {
        char const *name_suffix;
        U8 opopt;
    } const funcs_to_install[] = {
        { "",                           OPOPT_DO_BODY },
        { "_only_header",               OPOPT_DO_HEADER },
        { "_with_header",               (OPOPT_DO_BODY|OPOPT_DO_HEADER) },
        { "_with_offset",               (OPOPT_DO_BODY|OPOPT_OFFSET) },
        { "_only_header_with_offset",   (OPOPT_DO_HEADER|OPOPT_OFFSET) },
        { "_with_header_and_offset",    (OPOPT_DO_BODY|OPOPT_DO_HEADER|OPOPT_OFFSET) },
         /*012345678901234567890123*/
    }, *fti;
    int i;
    {
        MY_CXT_INIT;
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_ALIAS_SMALLINT,             SRL_DEC_OPT_STR_ALIAS_SMALLINT             );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_ALIAS_VARINT_UNDER,         SRL_DEC_OPT_STR_ALIAS_VARINT_UNDER         );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_DESTRUCTIVE_INCREMENTAL,    SRL_DEC_OPT_STR_DESTRUCTIVE_INCREMENTAL    );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_MAX_NUM_HASH_ENTRIES,       SRL_DEC_OPT_STR_MAX_NUM_HASH_ENTRIES       );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_MAX_RECURSION_DEPTH,        SRL_DEC_OPT_STR_MAX_RECURSION_DEPTH        );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_NO_BLESS_OBJECTS,           SRL_DEC_OPT_STR_NO_BLESS_OBJECTS           );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_REFUSE_OBJECTS,             SRL_DEC_OPT_STR_REFUSE_OBJECTS             );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_REFUSE_SNAPPY,              SRL_DEC_OPT_STR_REFUSE_SNAPPY              );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_REFUSE_ZLIB,                SRL_DEC_OPT_STR_REFUSE_ZLIB                );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_SET_READONLY,               SRL_DEC_OPT_STR_SET_READONLY               );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_SET_READONLY_SCALARS,       SRL_DEC_OPT_STR_SET_READONLY_SCALARS       );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_USE_UNDEF,                  SRL_DEC_OPT_STR_USE_UNDEF                  );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_VALIDATE_UTF8,              SRL_DEC_OPT_STR_VALIDATE_UTF8              );
        SRL_INIT_OPTION( SRL_DEC_OPT_IDX_REFUSE_ZSTD,                SRL_DEC_OPT_STR_REFUSE_ZSTD                );
    }
#if USE_CUSTOM_OPS
    {
        XOP *xop;
        Newxz(xop, 1, XOP);
        XopENTRY_set(xop, xop_name, "sereal_decode_with_object");
        XopENTRY_set(xop, xop_desc, "sereal_decode_with_object");
        XopENTRY_set(xop, xop_class, OA_UNOP);
        Perl_custom_op_register(aTHX_ THX_pp_sereal_decode, xop);
    }
#endif /* USE_CUSTOM_OPS */
    for (i = sizeof(funcs_to_install)/sizeof(*fti); i--; ) {
#       define LONG_CLASS_FMT "Sereal::Decoder::sereal_decode%s_with_object"
        char name[sizeof(LONG_CLASS_FMT)+24];
        char proto[7], *p = proto;
        U8 opopt;
        I32 cv_private;
        GV *gv;
        CV *cv;

        fti = &funcs_to_install[i];
        opopt = fti->opopt;
        /*
         * The cv_private value incorporates flags describing the operation to be
         * performed by the sub and precomputed arity limits.  0x020200 corresponds
         * to min_arity=2 and max_arity=2.  The various additions to cv_private
         * increment one or both of these sub-values.

         * The six subs created there share a single C body function, and are
         * differentiated only by the option flags in cv_private.  The custom ops
         * likewise share one op_ppaddr function, and the operations they perform
         * are differentiated by the same flags, stored in op_private.
         */
        cv_private = opopt | 0x020200;

        /* Yes, the subs have prototypes.  The protoypes have no effect when the
         * subs are used as methods, so there's no break of compatibility for those
         * using the documented API.  There is a change that could be detected by
         * code such as "Sereal::Decoder::decode($dec, @v)", that uses the methods
         * directly in an undocumented way.
         *
         * The prototype, specifically the putting of argument expressions into
         * scalar context, is required in order to be able to resolve arity at
         * compile time.  If this wasn't done, there would have to be a pushmark
         * op preceding the argument ops, and pp_sereal_decode() would need the
         * same code as xsfunc_sereal_decode() to check arity and resolve the
         * optional-parameter flags.
         */
        *p++ = '$';
        *p++ = '$';

        if (opopt & OPOPT_OFFSET) {
            *p++ = '$';
            cv_private += 0x010100;
        }
        *p++ = ';';
        if (opopt & OPOPT_DO_BODY) {
            *p++ = '$';
            cv_private += 0x010000;
        }
        if (opopt & OPOPT_DO_HEADER) {
            *p++ = '$';
            cv_private += 0x010000;
        }
        *p = 0;
        /* setup the name of the sub */
        sprintf(name, LONG_CLASS_FMT, fti->name_suffix);
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

BOOT:
{
#if USE_CUSTOM_OPS
    {
        XOP *xop;
        Newxz(xop, 1, XOP);
        XopENTRY_set(xop, xop_name, "scalar_looks_like_sereal");
        XopENTRY_set(xop, xop_desc, "scalar_looks_like_sereal");
        XopENTRY_set(xop, xop_class, OA_UNOP);
        Perl_custom_op_register(aTHX_ THX_pp_looks_like_sereal, xop);
    }
#endif /* USE_CUSTOM_OPS */
    {
        CV *cv;
        cv = newXSproto_portable("Sereal::Decoder::scalar_looks_like_sereal", THX_xsfunc_looks_like_sereal, __FILE__, "$");
        CvXSUBANY(cv).any_i32 = 0x010100 | OPOPT_LOOKS_LIKE;
#if USE_CUSTOM_OPS
        cv_set_call_checker(cv, THX_ck_entersub_args_sereal_decoder, (SV*)cv);
#endif /* USE_CUSTOM_OPS */
        cv = newXS("Sereal::Decoder::looks_like_sereal", THX_xsfunc_looks_like_sereal, __FILE__);
        CvXSUBANY(cv).any_i32 = 0x020100 | OPOPT_LOOKS_LIKE;
    }
}

srl_decoder_t *
new(CLASS, opt = NULL)
    char *CLASS;
    HV *opt;
  PREINIT:
    dMY_CXT;
  CODE:
    RETVAL = srl_build_decoder_struct(aTHX_ opt, MY_CXT.options);
    RETVAL->flags |= SRL_F_DECODER_REUSE;
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
    dMY_CXT;
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
    dec = srl_build_decoder_struct(aTHX_ (HV *)opt, MY_CXT.options);
    ST(0)= srl_decode_into(aTHX_ dec, src, into, 0);
    XSRETURN(1);

AV *
decode_sereal_with_header_data(src, opt = NULL, body_into = NULL, header_into = NULL)
    SV *src;
    SV *opt;
    SV *body_into;
    SV *header_into;
  PREINIT:
    dMY_CXT;
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
    dec = srl_build_decoder_struct(aTHX_ (HV *)opt, MY_CXT.options);
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

UV
bytes_consumed(dec)
    srl_decoder_t *dec;
  CODE:
    RETVAL = dec->bytes_consumed;
  OUTPUT: RETVAL

U32
flags(dec)
    srl_decoder_t *dec;
  CODE:
    RETVAL = dec->flags;
  OUTPUT: RETVAL

