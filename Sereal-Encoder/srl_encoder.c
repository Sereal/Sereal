#include "srl_encoder.h"

#include "ppport.h"

#define PERL_NO_GET_CONTEXT

#include "ptable.h"
#include "srl_inline.h"
#include "srl_buffer.h"
#include "srl_protocol.h"

/* General 'config' constants */
#ifdef MEMDEBUG
#   define INITIALIZATION_SIZE 1
#else
#   define INITIALIZATION_SIZE 16384
#endif

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
#   define DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK 1
#else
#   define DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK 0
#endif

#define MAX_DEPTH 10000

/* define option bits in srl_encoder_t's flags member */
#define SRL_F_SHARED_HASHKEYS                1UL
#define SRL_HAVE_OPTION(enc, flag_num) ((enc)->flags & flag_num)

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
static SRL_INLINE void srl_dump_bless(pTHX_ srl_encoder_t *enc, SV *src);

#define SRL_SET_FBIT(where) (where |= 0b10000000)

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
    /* Exception cleanup. Under normal operation, we should have
     * assigned NULL to buf_start after we're done. */
    Safefree(enc->buf_start);
    PTABLE_free(enc->ref_seenhash);
    PTABLE_free(enc->str_seenhash);
    PTABLE_free(enc->weak_seenhash);
    Safefree(enc);
}

/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_encoder_t *
build_encoder_struct(pTHX_ HV *opt)
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

    /* prealloc'ing large ref and str tables saves reallocs at the
     * expense of a larger malloc. prealloc'ing a large weak ref table
     * costs O(n) at the end of the serialization since we need to check
     * all buckets for weakrefs. */
    enc->ref_seenhash = PTABLE_new_size(8); /* size in powers of 2 */
    enc->str_seenhash = PTABLE_new_size(8);
    enc->weak_seenhash = PTABLE_new_size(3);

    /* load options */
    if (opt != NULL) {
        /* SRL_F_SHARED_HASHKEYS on by default */
        if ( !((svp = hv_fetchs(opt, "no_shared_hashkeys", 0)) && SvTRUE(*svp)) )
          enc->flags |= SRL_F_SHARED_HASHKEYS;
    }
    DEBUG_ASSERT_BUF_SANE(enc);

    return enc;
}


void
srl_write_header(pTHX_ srl_encoder_t *enc)
{
    /* works for now: 3 byte magic string + proto version + 1 byte varint that indicates zero-length header */
    DEBUG_ASSERT_BUF_SANE(enc);
    srl_buf_cat_str_s(enc, SRL_MAGIC_STRING "\x01");
    srl_buf_cat_char(enc, '\0');
}


/* Code for serializing floats */
static SRL_INLINE void
srl_dump_nv(pTHX_ srl_encoder_t *enc, SV *src)
{
    NV nv= SvNV(src);
    float f= nv;
    double d= nv;
    long double ld= nv;
    if (f == nv) {
        BUF_SIZE_ASSERT(enc, 1 + sizeof(f)); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc,SRL_HDR_FLOAT);
        Copy((char *)&f, enc->pos, sizeof(f), char);
        enc->pos += sizeof(f);
    } else if (d == nv) {
        BUF_SIZE_ASSERT(enc, 1 + sizeof(d)); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc,SRL_HDR_DOUBLE);
        Copy((char *)&d, enc->pos, sizeof(d), char);
        enc->pos += sizeof(d);

    } else if (ld == nv) {
        BUF_SIZE_ASSERT(enc, 1 + sizeof(ld)); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc,SRL_HDR_LONG_DOUBLE);
        Copy((char *)&ld, enc->pos, sizeof(ld), char);
        enc->pos += sizeof(ld);
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
srl_dump_bless(pTHX_ srl_encoder_t *enc, SV *src)
{
    const HV *stash = SvSTASH(src);
    const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(enc->str_seenhash, (SV *)stash);

    if (oldoffset != 0) {
        char *oldpos = (char *)(enc->buf_start + oldoffset);
        /* Issue COPY instead of literal class name string */
        srl_buf_cat_varint(aTHX_ enc, SRL_HDR_COPY, (UV)(enc->pos - enc->buf_start));

        /* Now, make sure that the "this is referenced", F bit of the original
         * string object in the output is set */
        SRL_SET_FBIT(*oldpos);
    }
    else {
        const char *class_name = HvNAME_get(stash);
        const size_t len = HvNAMELEN_get(stash);
        const ptrdiff_t offset = enc->pos - enc->buf_start;

        /* First save this new string (well, the HV * that it is represented by) into the string
         * dedupe table.
         * By saving the ptr to the HV, we only dedupe class names with class names, though
         * this seems a small price to pay for not having to keep a full string table.
         * At least, we can safely use the same PTABLE to store the ptrs to hashkeys since
         * the set of pointers will never collide.
         * /me bows to Yves for the delightfully evil hack. */

        /* remember current offset before advancing it */
        PTABLE_store(enc->str_seenhash, (void *)stash, (void *)offset);

        BUF_SIZE_ASSERT(enc, 1 + 2 + len + 2); /* heuristic: header + string + simple value */
        srl_buf_cat_char_nocheck(enc, SRL_HDR_BLESS);

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
    srl_write_header(aTHX_ enc);
    srl_dump_sv(aTHX_ enc, src);
    srl_fixup_weakrefs(aTHX_ enc);
    /* FIXME weak-ref hash traversal and fixup missing here! */
}

static SRL_INLINE void
srl_fixup_weakrefs(pTHX_ srl_encoder_t *enc)
{
    PTABLE_ITER_t *it = PTABLE_iter_new(enc->weak_seenhash);
    PTABLE_ENTRY_t *ent;

    /* FIXME it's arguable whether this should issue PTABLE_delete's
     * for each processed case of a weakref. But since this function can
     * really only be run once anyway, that's somewhat wasteful.
     */
    while ( NULL != (ent = PTABLE_iter_next(it)) ) {
        if ( (UV)ent->value < 2 ) {
            const ptrdiff_t offset = (ptrdiff_t)PTABLE_fetch(enc->ref_seenhash, ent->key);
            /* -1 is because we need to munge the byte BEFORE the
             * original offset (so, we need to replace the WEAKEN tag) */
            char *pos = enc->buf_start + offset - 1;
            assert(offset != 0);
            assert(pos >= enc->buf_start);
            assert(pos <= enc->buf_end);
            assert(*pos == SRL_HDR_PAD);
            *pos = SRL_HDR_PAD;
        }
    }

    PTABLE_iter_free(it);
}

/* Dumps generic SVs and delegates
 * to more specialized functions for RVs, etc. */
static void
srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src)
{
    UV refcount;

    SvGETMAGIC(src);

    /* TODO decide when to use the IV, when to use the PV, and when
     *      to use the NV slots of the SV.
     *      Safest simple solution seems "prefer string" (fuck dualvars).
     *      Potentially better but slower: If we would choose the string,
     *      then try int-to-string (respective float-to-string) conversion
     *      and strcmp. If same, then use int or float.
     */
    refcount = SvREFCNT(src);
    PULL_WEAK_REFCOUNT(refcount, src);

    /* check if we have seen this scalar before, and track it so
     * if we see it again we recognize it */
    if (refcount > 1) {
        const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(enc->ref_seenhash, src);
        if (!oldoffset) {
            PTABLE_store(enc->ref_seenhash, src, (void *)BUF_POS_OFS(enc));
        } else {
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


    /* Have to check the seen hash if high refcount or a weak ref */
    if (refcount > 1) {
        /* FIXME is the actual sv location the right thing to use? */
        oldoffset = (ptrdiff_t)PTABLE_fetch(enc->ref_seenhash, src);

        if (value_is_weak_referent) {
            /* output WEAKEN prefix before the actual item if the current
             * referrer is one of the weakrefs */
            if (SvWEAKREF(rv))
                srl_buf_cat_char(enc, SRL_HDR_WEAKEN);

            /* set or increment this weakref's refcount */
            PTABLE_ENTRY_t *pe;
            if ( (pe = PTABLE_find(enc->weak_seenhash, src)) == NULL)
                PTABLE_store(enc->weak_seenhash, src, (void *)1UL);
            else
                pe->value = (void *)((unsigned long)pe->value + 1UL);
        }
    }
    if (SvOBJECT(src)) {
        /* Write bless operator with class name */
        /* FIXME reuse/ref/... should INCLUDE the bless stuff. */
        srl_dump_bless(aTHX_ enc, src);
        /* fallthrough for value*/
    }

    /* see sv_reftype in sv.c */
    switch (svt) {
        case SVt_NULL:
        case SVt_IV:
        case SVt_NV:
        case SVt_PV:
        case SVt_PVIV:
        case SVt_PVNV:
        case SVt_PVMG:
        case SVt_PVLV:
        case SVt_REGEXP:
            srl_buf_cat_varint(aTHX_ enc, SRL_HDR_REF, (UV)oldoffset);
            if (oldoffset) {
                SRL_SET_FBIT(*(enc->buf_start + oldoffset));
            } else {
                srl_dump_sv(aTHX_ enc, src);
            }
            break;
        case SVt_PVAV:
            if (oldoffset) {
                SRL_SET_FBIT(*(enc->buf_start + oldoffset));
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_REUSE, (UV)oldoffset);
            } else {
                if (refcount>1) {
                    const ptrdiff_t newoffset = enc->pos - enc->buf_start;
                    PTABLE_store(enc->ref_seenhash, src, (void *)newoffset);
                }
                srl_dump_av(aTHX_ enc, (AV *)src);
            }
            break;
        case SVt_PVHV:
            if (oldoffset) {
                SRL_SET_FBIT(*(enc->buf_start + oldoffset));
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_REUSE, (UV)oldoffset);
            } else {
                if (refcount>1) {
                    const ptrdiff_t newoffset = enc->pos - enc->buf_start;
                    PTABLE_store(enc->ref_seenhash, src, (void *)newoffset);
                }
                srl_dump_hv(aTHX_ enc, (HV *)src);
            }
            break;
        case SVt_PVCV:
        case SVt_PVGV:
        case SVt_PVFM:
        case SVt_PVIO:
        case SVt_BIND:
        default:
            croak("found %s(0x%p), but it is not representable by the Sereal encoding format", sv_reftype(src,0),src);
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
        srl_buf_cat_char_nocheck(enc, SRL_HDR_TAIL);
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
    /* add on a tail element so that we can sanity check things during
     * deserialization */
    srl_buf_cat_char_nocheck(enc, SRL_HDR_TAIL);
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

    /* add on a tail element so that we can sanity check things during
     * deserialization */
    srl_buf_cat_char_nocheck(enc, SRL_HDR_TAIL);
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
        if ( SRL_HAVE_OPTION(enc, SRL_F_SHARED_HASHKEYS) /* only enter branch if shared hk's enabled */
             && (!DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK
                || src->he_valu.hent_refcount > 1) )
        {
            const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(enc->str_seenhash, keystr);
            if (oldoffset != 0) {
                /* Issue COPY instead of literal hash key string */
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_COPY, (UV)oldoffset);
            }
            else {
                /* remember current offset before advancing it */
                const ptrdiff_t newoffset = enc->pos - enc->buf_start;
                PTABLE_store(enc->str_seenhash, (void *)keystr, (void *)newoffset);
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

