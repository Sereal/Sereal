/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"
#ifdef __cplusplus
}
#endif

#ifndef PERL_VERSION
#    include <patchlevel.h>
#    if !(defined(PERL_VERSION) || (PERL_SUBVERSION > 0 && defined(PATCHLEVEL)))
#        include <could_not_find_Perl_patchlevel.h>
#    endif
#    define PERL_REVISION       5
#    define PERL_VERSION        PATCHLEVEL
#    define PERL_SUBVERSION     PERL_SUBVERSION
#endif
#if PERL_VERSION < 8
#   define PERL_MAGIC_qr                  'r' /* precompiled qr// regex */
#   define BFD_Svs_SMG_OR_RMG SVs_RMG
#elif ((PERL_VERSION==8) && (PERL_SUBVERSION >= 1) || (PERL_VERSION>8))
#   define BFD_Svs_SMG_OR_RMG SVs_SMG
#   define MY_PLACEHOLDER PL_sv_placeholder
#else
#   define BFD_Svs_SMG_OR_RMG SVs_RMG
#   define MY_PLACEHOLDER PL_sv_undef
#endif
#if (((PERL_VERSION == 9) && (PERL_SUBVERSION >= 4)) || (PERL_VERSION > 9))
#   define NEW_REGEX_ENGINE 1
#endif
#if (((PERL_VERSION == 8) && (PERL_SUBVERSION >= 1)) || (PERL_VERSION > 8))
#define MY_CAN_FIND_PLACEHOLDERS
#define HAS_SV2OBJ
#endif

#include "srl_encoder.h"

#include "srl_common.h"
#include "ptable.h"
#include "srl_buffer.h"
#include "srl_protocol.h"

/* The ENABLE_DANGEROUS_HACKS (passed through from ENV via Makefile.PL) enables
 * optimizations that may make the code so cozy with a particular version of the
 * Perl core that the code is no longer portable and/or compatible.
 * It would be great to determine where these hacks are safe and enable them
 * where possible. Gut feeling as for portability is that most things will be
 * ok on Unixes, but fail on the stricter Win32. As for compatibility with old
 * versions of perl, all bets are off.
 */
#ifdef ENABLE_DANGEROUS_HACKS
    /* It's unclear why DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK doesn't
     * help much. It basically means breaking perl's encapsulation to
     * check whether a HE (hash entry) that is shared has a refcount > 1
     * and only bothers inserting key into our ptr table if that's the
     * case. Benchmarks don't show much of a difference and it's a high
     * price to pay to break encapsulation for something that's not
     * measureable.
     */
    /* DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK only works on 5.10 and better */
#   define DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK 1
#else
#   define DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK 0
#endif

#define MAX_DEPTH 10000

#define DEBUGHACK 0

/* some static function declarations */
static void srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src);
static void srl_dump_pv(pTHX_ srl_encoder_t *enc, const char* src, STRLEN src_len, int is_utf8);
static SRL_INLINE void srl_dump_alias(pTHX_ srl_encoder_t *enc, const ptrdiff_t oldoffset);
static SRL_INLINE void srl_fixup_weakrefs(pTHX_ srl_encoder_t *enc);
static SRL_INLINE void srl_dump_rv(pTHX_ srl_encoder_t *enc, SV *rv);
static SRL_INLINE void srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src);
static SRL_INLINE void srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src);
static SRL_INLINE void srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src, const int share_keys);
static SRL_INLINE void srl_dump_nv(pTHX_ srl_encoder_t *enc, SV *src);
static SRL_INLINE void srl_dump_ivuv(pTHX_ srl_encoder_t *enc, SV *src);
static SRL_INLINE void srl_dump_classname(pTHX_ srl_encoder_t *enc, SV *src);
static SRL_INLINE PTABLE_t *srl_init_string_hash(srl_encoder_t *enc);
static SRL_INLINE PTABLE_t *srl_init_ref_hash(srl_encoder_t *enc);
static SRL_INLINE PTABLE_t *srl_init_weak_hash(srl_encoder_t *enc);

#define SRL_GET_STR_SEENHASH(enc) ( (enc)->str_seenhash == NULL     \
                                    ? srl_init_string_hash(enc)     \
                                   : (enc)->str_seenhash )

#define SRL_GET_REF_SEENHASH(enc) ( (enc)->ref_seenhash == NULL     \
                                    ? srl_init_ref_hash(enc)        \
                                   : (enc)->ref_seenhash )

#define SRL_GET_WEAK_SEENHASH(enc) ( (enc)->weak_seenhash == NULL   \
                                    ? srl_init_weak_hash(enc)       \
                                   : (enc)->weak_seenhash )

#define PULL_WEAK_REFCOUNT_IS_WEAK(refcnt, is_weak, sv)     \
    STMT_START {                                            \
        MAGIC *mg = NULL;                                   \
        if( SvMAGICAL(sv)                                   \
            && (mg = mg_find(sv, PERL_MAGIC_backref) ) )    \
        {                                                   \
            is_weak = 1;                                    \
            SV **svp = (SV**)mg->mg_obj;                    \
            if (svp && *svp) {                              \
                (refcnt) += SvTYPE(*svp) == SVt_PVAV        \
                          ? av_len((AV*)*svp)+1             \
                          : 1;                              \
                }                                           \
        }                                                   \
    } STMT_END

#define PULL_WEAK_REFCOUNT(refcnt, sv)                      \
    STMT_START {                                            \
        MAGIC *mg = NULL;                                   \
        if( SvMAGICAL(sv)                                   \
            && (mg = mg_find(sv, PERL_MAGIC_backref) ) )    \
        {                                                   \
            SV **svp = (SV**)mg->mg_obj;                    \
            if (svp && *svp) {                              \
                (refcnt) += SvTYPE(*svp) == SVt_PVAV        \
                          ? av_len((AV*)*svp)+1             \
                          : 1;                              \
                }                                           \
        }                                                   \
    } STMT_END

/* This is fired when we exit the Perl pseudo-block.
 * It frees our encoder and all. Put encoder-level cleanup
 * logic here so that we can simply use croak/longjmp for
 * exception handling. Makes life vastly easier!
 */
void srl_destructor_hook(void *p)
{
    srl_encoder_t *enc = (srl_encoder_t *)p;
    /* Do not auto-destroy encoder if set to be re-used */
    if (!SRL_ENC_HAVE_OPTION(enc, SRL_F_REUSE_ENCODER)) {
        /* Exception cleanup. Under normal operation, we should have
         * assigned NULL to buf_start after we're done. */
        Safefree(enc->buf_start);
        if (enc->ref_seenhash != NULL)
            PTABLE_free(enc->ref_seenhash);
        if (enc->str_seenhash != NULL)
            PTABLE_free(enc->str_seenhash);
        if (enc->weak_seenhash != NULL)
            PTABLE_free(enc->weak_seenhash);
        Safefree(enc);
    }
    else {
        srl_clear_encoder(enc);
    }
}

void
srl_clear_encoder(srl_encoder_t *enc)
{
    if (enc->pos > enc->buf_start) {
        enc->depth = 0;
        if (enc->ref_seenhash != NULL)
            PTABLE_clear(enc->ref_seenhash);
        if (enc->str_seenhash != NULL)
            PTABLE_clear(enc->str_seenhash);
        if (enc->weak_seenhash != NULL)
            PTABLE_clear(enc->weak_seenhash);
        enc->pos = enc->buf_start;
    }
}

void
srl_destroy_encoder(pTHX_ srl_encoder_t *enc)
{
    Safefree(enc->buf_start);
    if (enc->ref_seenhash != NULL)
        PTABLE_free(enc->ref_seenhash);
    if (enc->str_seenhash != NULL)
        PTABLE_free(enc->str_seenhash);
    if (enc->weak_seenhash != NULL)
        PTABLE_free(enc->weak_seenhash);
    Safefree(enc);
}

/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_encoder_t *
srl_build_encoder_struct(pTHX_ HV *opt)
{
    srl_encoder_t *enc;
    SV **svp;

    Newx(enc, 1, srl_encoder_t);
    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR(&srl_destructor_hook, (void *)enc);

    /* Init struct */
    Newx(enc->buf_start, INITIALIZATION_SIZE, char);
    enc->buf_end = enc->buf_start + INITIALIZATION_SIZE - 1;
    enc->pos = enc->buf_start;
    enc->depth = 0;
    enc->flags = 0;

    enc->weak_seenhash = NULL;
    enc->str_seenhash = NULL;
    enc->ref_seenhash = NULL;

    /* load options */
    if (opt != NULL) {
        /* SRL_F_SHARED_HASHKEYS on by default */
        svp = hv_fetchs(opt, "no_shared_hashkeys", 0);
        if ( !svp || !SvTRUE(*svp) )
          enc->flags |= SRL_F_SHARED_HASHKEYS;
    }
    else {
        /* SRL_F_SHARED_HASHKEYS on by default */
        enc->flags |= SRL_F_SHARED_HASHKEYS;
    }
    DEBUG_ASSERT_BUF_SANE(enc);

    return enc;
}

static SRL_INLINE PTABLE_t *
srl_init_string_hash(srl_encoder_t *enc)
{
    enc->str_seenhash = PTABLE_new_size(4);
    return enc->str_seenhash;
}

static SRL_INLINE PTABLE_t *
srl_init_ref_hash(srl_encoder_t *enc)
{
    enc->ref_seenhash = PTABLE_new_size(4);
    return enc->ref_seenhash;
}

static SRL_INLINE PTABLE_t *
srl_init_weak_hash(srl_encoder_t *enc)
{
    enc->weak_seenhash = PTABLE_new_size(3);
    return enc->weak_seenhash;
}


void
srl_write_header(pTHX_ srl_encoder_t *enc)
{
    /* works for now: 3 byte magic string + proto version + 1 byte varint that indicates zero-length header */
    DEBUG_ASSERT_BUF_SANE(enc);
    srl_buf_cat_str_s(enc, SRL_MAGIC_STRING "\x01");
    srl_buf_cat_char(enc,'\0');
}


/* Code for serializing floats */
static SRL_INLINE void
srl_dump_nv(pTHX_ srl_encoder_t *enc, SV *src)
{
    NV nv= SvNV(src);
    float f= nv;
    double d= nv;
    if ( f == nv || nv != nv ) {
        BUF_SIZE_ASSERT(enc, 1 + sizeof(f)); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc,SRL_HDR_FLOAT);
        Copy((char *)&f, enc->pos, sizeof(f), char);
        enc->pos += sizeof(f);
    } else if (d == nv) {
        BUF_SIZE_ASSERT(enc, 1 + sizeof(d)); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc,SRL_HDR_DOUBLE);
        Copy((char *)&d, enc->pos, sizeof(d), char);
        enc->pos += sizeof(d);
    } else {
        BUF_SIZE_ASSERT(enc, 1 + sizeof(nv)); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc,SRL_HDR_LONG_DOUBLE);
        Copy((char *)&nv, enc->pos, sizeof(nv), char);
        enc->pos += sizeof(nv);
    }
}


/* Code for serializing any SINGLE integer type */
static SRL_INLINE void
srl_dump_ivuv(pTHX_ srl_encoder_t *enc, SV *src)
{
    char hdr;
    /* TODO for the time being, we just won't ever use NUMLIST types because that's
     *      a fair amount of extra implementation work. The decoders won't care and
     *      we're just wasting some space. */
    /* TODO optimize! */

    if (SvIOK_UV(src) || SvIV(src) >= 0) { /* FIXME find a way to express this without repeated SvIV/SvUV */
        const UV num = SvUV(src); /* FIXME is SvUV_nomg good enough because of the GET magic in dump_sv? SvUVX after having checked the flags? */
        if (num < 16) {
            /* encodable as POS */
            hdr = SRL_HDR_POS_LOW | (unsigned char)num;
            srl_buf_cat_char(enc, hdr);
        }
        else {
            srl_buf_cat_varint(aTHX_ enc, SRL_HDR_VARINT, num);
        }
    }
    else {
        const IV num = SvIV(src);
        if (num > -17) {
            /* encodable as NEG */
            hdr = SRL_HDR_NEG_HIGH | ((unsigned char)abs(num) - 1);
            srl_buf_cat_char(enc, hdr);
        }
        else {
            /* Needs ZIGZAG */
            srl_buf_cat_zigzag(aTHX_ enc, SRL_HDR_ZIGZAG, num);
        }
    }
}


/* Outputs a bless header and the class name (as some form of string or COPY).
 * Caller then has to output the actual reference payload. */
static SRL_INLINE void
srl_dump_classname(pTHX_ srl_encoder_t *enc, SV *src)
{
    const HV *stash = SvSTASH(src);
    PTABLE_t *string_seenhash = SRL_GET_STR_SEENHASH(enc);
    const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(string_seenhash, (SV *)stash);

    if (oldoffset != 0) {
        /* Issue COPY instead of literal class name string */
        srl_buf_cat_varint(aTHX_ enc, SRL_HDR_COPY, (UV)oldoffset);
    }
    else {
        const char *class_name = HvNAME_get(stash);
        const size_t len = HvNAMELEN_get(stash);

        /* First save this new string (well, the HV * that it is represented by) into the string
         * dedupe table.
         * By saving the ptr to the HV, we only dedupe class names with class names, though
         * this seems a small price to pay for not having to keep a full string table.
         * At least, we can safely use the same PTABLE to store the ptrs to hashkeys since
         * the set of pointers will never collide.
         * /me bows to Yves for the delightfully evil hack. */

        /* remember current offset before advancing it */
        PTABLE_store(string_seenhash, (void *)stash, (void *)(enc->pos - enc->buf_start));

        /* HvNAMEUTF8 not in older perls and it would be 0 for those anyway */
#if PERL_VERSION >= 16
        srl_dump_pv(aTHX_ enc, class_name, len, HvNAMEUTF8(stash));
#else
        srl_dump_pv(aTHX_ enc, class_name, len, 0);
#endif
    }
}


void
srl_dump_data_structure(pTHX_ srl_encoder_t *enc, SV *src)
{
    if (DEBUGHACK) warn("== start dump");
    srl_write_header(aTHX_ enc);
    srl_dump_sv(aTHX_ enc, src);
    srl_fixup_weakrefs(aTHX_ enc);
    if (DEBUGHACK) warn("== end dump");
}

static SRL_INLINE void
srl_fixup_weakrefs(pTHX_ srl_encoder_t *enc)
{
    PTABLE_t *weak_seenhash = SRL_GET_WEAK_SEENHASH(enc);
    PTABLE_ITER_t *it = PTABLE_iter_new(weak_seenhash);
    PTABLE_ENTRY_t *ent;

    /* we now walk the weak_seenhash and set any tags it points
     * at to the PAD opcode, this basically turns the first weakref
     * we encountered into a normal ref when there is only a weakref
     * pointing at the structure. */
    while ( NULL != (ent = PTABLE_iter_next(it)) ) {
        const ptrdiff_t offset = (ptrdiff_t)ent->value;
        if ( offset ) {
            char *pos = enc->buf_start + offset;
            assert(*pos == SRL_HDR_WEAKEN);
            if (DEBUGHACK) warn("setting %lu to PAD", offset);
            *pos = SRL_HDR_PAD;
        }
    }

    PTABLE_iter_free(it);
}

#ifndef MAX_CHARSET_NAME_LENGTH
#define MAX_CHARSET_NAME_LENGTH 2
#endif

#ifdef SvRX
#define MODERN_REGEXP
#endif

static inline void
srl_dump_regexp(pTHX_ srl_encoder_t *enc, SV *sv)
{
    STRLEN left = 0;
    const char *fptr;
    char ch;
    U16 match_flags;
#ifdef MODERN_REGEXP
    REGEXP *re= SvRX(sv);
#else
#define INT_PAT_MODS "msix"
#define RXf_PMf_STD_PMMOD_SHIFT 12
#define RX_PRECOMP(re) ((re)->precomp)
#define RX_PRELEN(re) ((re)->prelen)
#define RX_UTF8(re) ((re)->reganch & ROPT_UTF8)
#define RX_EXTFLAGS(re) ((re)->reganch)
#define RXf_PMf_COMPILETIME  PMf_COMPILETIME
    regexp *re = (regexp *)(((MAGIC*)sv)->mg_obj);
#endif
    char reflags[sizeof(INT_PAT_MODS) + MAX_CHARSET_NAME_LENGTH];

    /*
       we are in list context so stringify
       the modifiers that apply. We ignore "negative
       modifiers" in this scenario, and the default character set
    */

#ifdef REGEXP_DEPENDS_CHARSET
    if (get_regex_charset(RX_EXTFLAGS(re)) != REGEX_DEPENDS_CHARSET) {
        STRLEN len;
        const char* const name = get_regex_charset_name(RX_EXTFLAGS(re),
                                                        &len);
        Copy(name, reflags + left, len, char);
        left += len;
    }
#endif
    fptr = INT_PAT_MODS;
    match_flags = (U16)((RX_EXTFLAGS(re) & RXf_PMf_COMPILETIME)
                            >> RXf_PMf_STD_PMMOD_SHIFT);

    while((ch = *fptr++)) {
        if(match_flags & 1) {
            reflags[left++] = ch;
        }
        match_flags >>= 1;
    }

    srl_buf_cat_char(enc, SRL_HDR_REGEXP);
    srl_dump_pv(aTHX_ enc, RX_PRECOMP(re),RX_PRELEN(re), (RX_UTF8(re) ? SVf_UTF8 : 0));
    srl_dump_pv(aTHX_ enc, reflags, left, 0);
    return;
}

#define TRACK_REFCOUNT(enc, src, refcount)                      \
STMT_START {                                                    \
    if (refcount>1) {                                           \
        const ptrdiff_t newoffset = enc->pos - enc->buf_start;  \
        PTABLE_t *ref_seenhash = SRL_GET_REF_SEENHASH(enc);     \
        PTABLE_store(ref_seenhash, src, (void *)newoffset);     \
    }                                                           \
} STMT_END

/* Dumps generic SVs and delegates
 * to more specialized functions for RVs, etc. */
static void
srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src)
{
    UV refcount;
    U8 value_is_weak_referent = 0;

    SvGETMAGIC(src);

    /* TODO decide when to use the IV, when to use the PV, and when
     *      to use the NV slots of the SV.
     *      Safest simple solution seems "prefer string" (fuck dualvars).
     *      Potentially better but slower: If we would choose the string,
     *      then try int-to-string (respective float-to-string) conversion
     *      and strcmp. If same, then use int or float.
     */
    refcount = SvREFCNT(src);
    PULL_WEAK_REFCOUNT_IS_WEAK(refcount, value_is_weak_referent, src);

    /* check if we have seen this scalar before, and track it so
     * if we see it again we recognize it */
    if (refcount > 1) {
        PTABLE_t *ref_seenhash = SRL_GET_REF_SEENHASH(enc);
        PTABLE_t *weak_seenhash = SRL_GET_WEAK_SEENHASH(enc);
        const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(ref_seenhash, src);
        PTABLE_ENTRY_t *pe = PTABLE_find(weak_seenhash, src);
        if (!oldoffset) {
            if (DEBUGHACK) warn("storing %p as %lu", src, BUF_POS_OFS(enc));
            PTABLE_store(ref_seenhash, src, (void *)BUF_POS_OFS(enc));
            if (value_is_weak_referent) {
                /* we have never seen it before at all (or oldoffset would tell us so)
                 * so if we have seen it before in the weak_seenhash it is because
                 * we are reaching it for the first time via a (weak)ref. On the other
                 * hand if we havent seen it before we might find a weakref to it later,
                 * and since the item is referenced implicitly, either by being our return
                 * value, or because we are a value in a composite, we dont need to worry
                 * about any subsequent weakres.
                 */
                if (!pe) {
                    if (DEBUGHACK) warn("scalar %p - is weak referent, storing 0", src);
                    PTABLE_store(weak_seenhash, src, NULL);
                } else {
                    if (DEBUGHACK) warn("scalar %p - is weak referent, seen before", src);
                }
            }
        } else {
            /* We have seen it before, and now we have seen it again as an sv. The only
             * way this can happen is if we have an alias (either explicit or implicit)
             * in a composite structure. This means that the composite has a "refcount"
             * on the item, so any weakrefs to it are "safe". */
            if (pe) {
                if (DEBUGHACK) warn("scalar %p - is weak referent and we have seen it before storing 0", src);
                pe->value= NULL;
            } else {
                /* I am pretty sure this should never happen */
                if (DEBUGHACK) warn("scalar %p - is weak referent, but we havent seen any refs to it yet, storing 0", src);
                PTABLE_store(weak_seenhash, src, NULL);
            }
            /* it must be an alias */
            srl_dump_alias(aTHX_ enc, oldoffset);
            SRL_SET_FBIT(*(enc->buf_start + oldoffset));
            return;
        }
    }
    /* if we got here we have not seen this scalar before */

    /* dump strings */
    if (SvPOKp(src)) {
        STRLEN len;
        char *str = SvPV(src, len);
        srl_dump_pv(aTHX_ enc, str, len, SvUTF8(src));
    }
    /* dump floats */
    else if (SvNOKp(src))
        srl_dump_nv(aTHX_ enc, src);
    /* dump ints */
    else if (SvIOKp(src))
        srl_dump_ivuv(aTHX_ enc, src);
    /* undef */
    else if (!SvOK(src))
        srl_buf_cat_char(enc, SRL_HDR_UNDEF);
    /* dump references */
    else if (SvROK(src)) 
        srl_dump_rv(aTHX_ enc, src);
    else {
        croak("Attempting to dump unsupported or invalid SV");
    }
    /* TODO what else do we need to support in this main if/else? */
}

/* Dump references, delegates to more specialized functions for
 * arrays, hashes, etc. */
static SRL_INLINE void
srl_dump_rv(pTHX_ srl_encoder_t *enc, SV *rv)
{
    /* Warning: This function has a second return path:
     *          It short-circuits for REUSE references. */
    unsigned int refcount;
    U8 value_is_weak_referent = 0;
    ptrdiff_t oldoffset= 0;
    svtype svt;
    SV *src = SvRV(rv);
    if (0) sv_dump(rv); /* enabled this line to see a dump of each item as we go */

    if (++enc->depth > MAX_DEPTH)
        croak("Reached maximum recursion depth of %u. Aborting", MAX_DEPTH);

    SvGETMAGIC(src);
    svt = SvTYPE(src);
    refcount = SvREFCNT(src);
    PULL_WEAK_REFCOUNT_IS_WEAK(refcount, value_is_weak_referent, src);


    if (refcount > 1){
        PTABLE_t *ref_seenhash = SRL_GET_REF_SEENHASH(enc);
        oldoffset = (ptrdiff_t)PTABLE_fetch(ref_seenhash, src);

        /* it is possible rv is a weakref, or that src is the target of a weakref
         * so we have to do some extra bookkeeping */
        if (value_is_weak_referent) {
            PTABLE_t *weak_seenhash = SRL_GET_WEAK_SEENHASH(enc);
            PTABLE_ENTRY_t *pe = PTABLE_find(weak_seenhash, src);
            /* If we have not see this item before then it is possible the only
             * reference to it is a weakref, so we track the first one we see.
             * If we later see a real ref we will set the value to 0. */
            if (SvWEAKREF(rv)) {
                if (!pe)  {
                    if (DEBUGHACK) warn("weakref %p - storing %lu", src, BUF_POS_OFS(enc));
                    PTABLE_store(weak_seenhash, src, (void *)BUF_POS_OFS(enc));
                } else {
                    if (DEBUGHACK) warn("weakref %p - previous weakref seen", src);
                }
                /* FIXME: what happens if this is the only reference? we need
                 * to track it and update it later if there isnt a ref to the
                 * value later */
                srl_buf_cat_char(enc, SRL_HDR_WEAKEN);
            } else {
                /* good - we are dumping at least one "real" ref to this object.
                 * so we can clear the pe value - we do not delete, as we want to
                 * track it so later weakrefs "know" the item is "safe". */
                if (pe) {
                    if (DEBUGHACK) warn("ref %p - seen weakref before, setting to 0", src);
                    pe->value = NULL;
                } else {
                    if (DEBUGHACK) warn("ref %p - to weak referent, storing 0", src);
                    PTABLE_store(weak_seenhash, src, NULL);
                }
            }
        }
    }

    if (oldoffset == 0 && SvOBJECT(src)) {
        /* Write bless operator with class name */
        /* FIXME reuse/ref/... should INCLUDE the bless stuff. */
        srl_buf_cat_char(enc, SRL_HDR_BLESS);
    }
    if (oldoffset) {
        srl_buf_cat_varint(aTHX_ enc, SRL_HDR_REFP, (UV)oldoffset);
        SRL_SET_FBIT(*(enc->buf_start + oldoffset));
    } else {
    /* fallthrough for value*/
    /* see sv_reftype in sv.c */
        srl_buf_cat_char(enc, SRL_HDR_REFN);
        switch (svt) {
            case SVt_PVMG:
#ifndef MODERN_REGEXP
            {
                MAGIC *mg;
                if ( ((SvFLAGS(src) &
                       (SVs_OBJECT|SVf_OK|SVs_GMG|SVs_SMG|SVs_RMG))
                      == (SVs_OBJECT|BFD_Svs_SMG_OR_RMG))
                     && (mg = mg_find(src, PERL_MAGIC_qr)))
                {
                    /* Housten, we have a regex! */
                    TRACK_REFCOUNT(enc, src, refcount);
                    srl_dump_regexp(aTHX_ enc, (SV*)mg); /* yes the SV* cast makes me feel dirty too */
                    break;

                }
            }
            /* fallthrough */
            case SVt_RV:
#endif
            case SVt_NULL:
            case SVt_IV:
            case SVt_NV:
            case SVt_PV:
            case SVt_PVIV:
            case SVt_PVNV:
            case SVt_PVLV:
                srl_dump_sv(aTHX_ enc, src);
                break;
#ifdef MODERN_REGEXP
            case SVt_REGEXP:
                TRACK_REFCOUNT(enc, src, refcount);
                srl_dump_regexp(aTHX_ enc, src);
                break;
#endif
            case SVt_PVAV:
                TRACK_REFCOUNT(enc, src, refcount);
                srl_dump_av(aTHX_ enc, (AV *)src);
                break;
            case SVt_PVHV:
                TRACK_REFCOUNT(enc, src, refcount);
                srl_dump_hv(aTHX_ enc, (HV *)src);
                break;
            case SVt_PVCV:
            case SVt_PVGV:
            case SVt_PVFM:
            case SVt_PVIO:
#if PERL_VERSION >= 10
            case SVt_BIND:
#endif
            default:
                croak("found type %u %s(0x%p), but it is not representable by the Sereal encoding format", svt, sv_reftype(src,0),src);
        }
    }
    if (!oldoffset && SvOBJECT(src)) {
        srl_dump_classname(aTHX_ enc, src);
    }
}


static SRL_INLINE void
srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src)
{
    UV i, n;
    SV **svp;

    n = av_len(src)+1;

    /* heuristic: n is virtually the min. size of any element */
    BUF_SIZE_ASSERT(enc, 2 + SRL_MAX_VARINT_LENGTH + n);

    /* header and num. elements */
    srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_ARRAY, n);

    if (n == 0) {
        return;
    }

    svp = av_fetch(src, 0, 0);
    if (svp != NULL)
        srl_dump_sv(aTHX_ enc, *svp);
    else
        srl_buf_cat_char(enc, SRL_HDR_UNDEF);

    for (i = 1; i < n; ++i) {
        svp = av_fetch(src, i, 0);
        if (svp != NULL)
            srl_dump_sv(aTHX_ enc, *svp);
        else
            srl_buf_cat_char(enc, SRL_HDR_UNDEF);
    }
}


static SRL_INLINE void
srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src)
{
    HE *he;
    UV n = hv_iterinit(src);
    const int do_share_keys = HvSHAREKEYS((SV *)src);

    /* heuristic: n = ~min size of n values;
     *            + 2*n = very conservative min size of n hashkeys if all COPY */
    BUF_SIZE_ASSERT(enc, 2 + SRL_MAX_VARINT_LENGTH + 3*n);
    srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_HASH, n);

    if (n > 0 || SvMAGICAL(src)) {
        while ((he = hv_iternext(src))) {
            /* note we dump the values first, this makes deserializing a little easier
             * given how perls hash api works */
            srl_dump_sv(aTHX_ enc, SvMAGICAL(src) ? hv_iterval(src, he) : HeVAL(he));
            srl_dump_hk(aTHX_ enc, he, do_share_keys);
        }
    }

}


static SRL_INLINE void
srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src, const int share_keys)
{
    if (HeKLEN(src) == HEf_SVKEY) {
        SV *sv = HeSVKEY(src);
        STRLEN len;
        char *str;

        SvGETMAGIC(sv);
        str = SvPV(sv, len);

        srl_dump_pv(aTHX_ enc, str, len, SvUTF8(sv));
    }
    else if (share_keys) {
        /* This logic is an optimization for output space: We keep track of
         * all seen hash key strings that are in perl's shared string storage.
         * If we see one again, we just emit a COPY instruction.
         * This means that we only need to keep a ptr table since the strings
         * don't move in the shared key storage -- otherwise, we'd have to
         * compare strings / keep a full string hash table. */
        const char *keystr = HeKEY(src);
        if ( SRL_ENC_HAVE_OPTION(enc, SRL_F_SHARED_HASHKEYS) /* only enter branch if shared hk's enabled */
#if PERL_VERSION >= 10
             && (!DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK
                || src->he_valu.hent_refcount > 1)
#endif
            )
        {
            PTABLE_t *string_seenhash = SRL_GET_STR_SEENHASH(enc);
            const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(string_seenhash, keystr);
            if (oldoffset != 0) {
                /* Issue COPY instead of literal hash key string */
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_COPY, (UV)oldoffset);
            }
            else {
                /* remember current offset before advancing it */
                const ptrdiff_t newoffset = enc->pos - enc->buf_start;
                PTABLE_store(string_seenhash, (void *)keystr, (void *)newoffset);
                srl_dump_pv(aTHX_ enc, keystr, HeKLEN(src), HeKUTF8(src));
            }
        }
        else
            srl_dump_pv(aTHX_ enc, keystr, HeKLEN(src), HeKUTF8(src));
    }
    else
        srl_dump_pv(aTHX_ enc, HeKEY(src), HeKLEN(src), HeKUTF8(src));
}


static void
srl_dump_pv(pTHX_ srl_encoder_t *enc, const char* src, STRLEN src_len, int is_utf8)
{
    BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH + src_len); /* overallocate a bit sometimes */
    if (is_utf8) {
        srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_STRING_UTF8, src_len);
    } else if (src_len <= SRL_MAX_ASCII_LENGTH) {
        srl_buf_cat_char_nocheck(enc, SRL_HDR_ASCII | (char)src_len);
    } else {
        srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_STRING, src_len);
    }
    Copy(src, enc->pos, src_len, char);
    enc->pos += src_len;
}


static SRL_INLINE void
srl_dump_alias(pTHX_ srl_encoder_t *enc, const ptrdiff_t oldoffset)
{
    srl_buf_cat_varint(aTHX_ enc, SRL_HDR_ALIAS, (UV)oldoffset);
}

