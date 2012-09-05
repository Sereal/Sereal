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

#include "snappy/csnappy_compress.c"

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
static SRL_INLINE void srl_fixup_weakrefs(pTHX_ srl_encoder_t *enc);
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

#define CALL_SRL_DUMP_SV(enc, src) STMT_START {                         \
    if (!(src)) {                                                       \
        srl_buf_cat_char((enc), SRL_HDR_UNDEF);                         \
    }                                                                   \
    else                                                                \
    if (SvTYPE((src)) < SVt_PVMG &&                                     \
        SvREFCNT((src)) == 1 &&                                         \
        !SvROK((src))                                                   \
    ) {                                                                 \
        if (SvPOKp((src))) {                                            \
            STRLEN len;                                                 \
            char *str = SvPV((src), len);                               \
            srl_dump_pv(aTHX_ (enc), str, len, SvUTF8((src)));          \
        }                                                               \
        else                                                            \
        if (SvNOKp((src))) {                                            \
            /* dump floats */                                           \
            srl_dump_nv(aTHX_ (enc), (src));                            \
        }                                                               \
        else                                                            \
        if (SvIOKp((src))) {                                            \
            /* dump ints */                                             \
            srl_dump_ivuv(aTHX_ (enc), (src));                          \
        }                                                               \
        else {                                                          \
            srl_dump_sv(aTHX_ (enc), (src));                            \
        }                                                               \
    } else {                                                            \
        srl_dump_sv(aTHX_ (enc), (src));                                \
    }                                                                   \
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
        Safefree(enc->snappy_workmem);
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
    Safefree(enc->snappy_workmem);
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
    enc->snappy_workmem = NULL;

    /* load options */
    if (opt != NULL) {
        /* SRL_F_SHARED_HASHKEYS on by default */
        svp = hv_fetchs(opt, "no_shared_hashkeys", 0);
        if ( !svp || !SvTRUE(*svp) )
          enc->flags |= SRL_F_SHARED_HASHKEYS;

        svp = hv_fetchs(opt, "croak_on_bless", 0);
        if ( svp && SvTRUE(*svp) )
          enc->flags |= SRL_F_CROAK_ON_BLESS;

        svp = hv_fetchs(opt, "snappy", 0);
        if ( svp && SvTRUE(*svp) )
          enc->flags |= SRL_F_COMPRESS_SNAPPY;
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
    /* 4th to 8th bit are flags. Using 4th for snappy flag. FIXME needs to go in spec. */
    const U8 version_and_flags = SRL_PROTOCOL_VERSION
                                 | (
                                    SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY)
                                    ? SRL_F_SNAPPY : 0
                                 );

    /* 4 byte magic string + proto version
     * + potentially uncompressed size varint
     * +  1 byte varint that indicates zero-length header */
    BUF_SIZE_ASSERT(enc, sizeof(SRL_MAGIC_STRING) + 1 + 1);
    srl_buf_cat_str_s_nocheck(enc, SRL_MAGIC_STRING);
    srl_buf_cat_char_nocheck(enc, version_and_flags);
    srl_buf_cat_char_nocheck(enc, '\0'); /* variable header length (0 right now) */
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
            hdr = SRL_HDR_NEG_LOW | ((unsigned char)abs(num) - 1);
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
    if (!SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY)) {
        srl_write_header(aTHX_ enc);
        srl_dump_sv(aTHX_ enc, src);
        srl_fixup_weakrefs(aTHX_ enc);
    }
    else {
        /* sizeof includes \0 in count */
        const STRLEN max_header_len = sizeof(SRL_MAGIC_STRING)-1 + 1 + SRL_MAX_VARINT_LENGTH + 1;
        STRLEN uncompressed_length;
        char *old_buf;
        uint32_t dest_len;

        srl_dump_sv(aTHX_ enc, src);
        srl_fixup_weakrefs(aTHX_ enc);

        uncompressed_length = enc->pos - enc->buf_start;
        dest_len = csnappy_max_compressed_length(uncompressed_length) + max_header_len + 1;

        if (expect_false( enc->snappy_workmem == NULL )) {
            /* Cleaned up automatically by the cleanup handler */
            Newx(enc->snappy_workmem, CSNAPPY_WORKMEM_BYTES, char);
            if (enc->snappy_workmem == NULL)
                croak("Out of memory!");
        }

        old_buf = enc->buf_start;
        Newx(enc->buf_start, dest_len, char);
        if (!enc->buf_start) {
            enc->buf_start = old_buf;
            croak("Out of memory!");
        }
        enc->pos = enc->buf_start;
        enc->buf_end = enc->buf_start + dest_len;

        srl_write_header(aTHX_ enc);
        csnappy_compress(old_buf, (uint32_t)uncompressed_length, enc->pos, &dest_len,
                         enc->snappy_workmem, CSNAPPY_WORKMEM_BYTES_POWER_OF_TWO);
        Safefree(old_buf);
        /* FIXME could check whether output larger than uncompressed and revert... */
        enc->pos += dest_len;
    }
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

static SRL_INLINE void
srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src)
{
    UV n;
    SV **svp;

    n = av_len(src)+1;

    /* heuristic: n is virtually the min. size of any element */
    BUF_SIZE_ASSERT(enc, 2 + SRL_MAX_VARINT_LENGTH + n);

    /* header and num. elements */
    srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_ARRAY, n);

    if (n == 0) {
        return;
    }
    /* I can't decide if this should make me feel dirty */
    if (SvMAGICAL(src)) {
        UV i;
        for (i = 0; i < n; ++i) {
            svp = av_fetch(src, i, 0);
            CALL_SRL_DUMP_SV(enc, *svp);
        }
    } else {
        SV **end;
        svp= AvARRAY(src);
        end= svp + n;
        for ( ; svp < end ; svp++) {
            CALL_SRL_DUMP_SV(enc, *svp);
        }
    }
}


static SRL_INLINE void
srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src)
{
    HE *he;
    const int do_share_keys = HvSHAREKEYS((SV *)src);

    /* heuristic: n = ~min size of n values;
     *            + 2*n = very conservative min size of n hashkeys if all COPY */

    if ( SvMAGICAL(src) ) {
        UV n= hv_iterinit(src);
        BUF_SIZE_ASSERT(enc, 2 + SRL_MAX_VARINT_LENGTH + 3*n);
        srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_HASH, n);
        while ((he = hv_iternext(src))) {
            SV *v= HeVAL(he);
            srl_dump_hk(aTHX_ enc, he, do_share_keys);
            CALL_SRL_DUMP_SV(enc, v);
        }
    } else {
        UV n= HvUSEDKEYS(src);
        BUF_SIZE_ASSERT(enc, 2 + SRL_MAX_VARINT_LENGTH + 3*n);
        srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_HASH, n);
        if (n) {
            HE **he_ptr= HvARRAY(src);
            HE **he_end= he_ptr + HvMAX(src) + 1;
            do {
                for (he= *he_ptr++; he; he= HeNEXT(he) ) {
                    SV *v= HeVAL(he);
                    if (v != &PL_sv_placeholder) {
                        srl_dump_hk(aTHX_ enc, he, do_share_keys);
                        CALL_SRL_DUMP_SV(enc, v);
                        if (--n == 0) {
                            he_ptr= he_end;
                            break;
                        }
                    }
                }
            } while ( he_ptr < he_end );
        }
    }
}


static SRL_INLINE void
srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src, const int share_keys)
{
    char *str;
    STRLEN len;
    char mode;

    if (HeKLEN(src) == HEf_SVKEY) {
        SV *sv = HeSVKEY(src);

        SvGETMAGIC(sv);
        str = SvPV(sv, len);
        mode= SvUTF8(sv) ? 1 : 0;

    }
    else {
        str = HeKEY(src);
        /* This logic is an optimization for output space: We keep track of
         * all seen hash key strings that are in perl's shared string storage.
         * If we see one again, we just emit a COPY instruction.
         * This means that we only need to keep a ptr table since the strings
         * don't move in the shared key storage -- otherwise, we'd have to
         * compare strings / keep a full string hash table. */
        if ( share_keys && SRL_ENC_HAVE_OPTION(enc, SRL_F_SHARED_HASHKEYS) /* only enter branch if shared hk's enabled */
#if PERL_VERSION >= 10
             && (!DO_SHARED_HASH_ENTRY_REFCOUNT_CHECK
                || src->he_valu.hent_refcount > 1)
#endif
            )
        {
            PTABLE_t *string_seenhash = SRL_GET_STR_SEENHASH(enc);
            const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(string_seenhash, str);
            if (oldoffset != 0) {
                /* Issue COPY instead of literal hash key string */
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_COPY, (UV)oldoffset);
                return;
            }
            else {
                /* remember current offset before advancing it */
                const ptrdiff_t newoffset = enc->pos - enc->buf_start;
                PTABLE_store(string_seenhash, (void *)str, (void *)newoffset);
            }
        }
        len= HeKLEN(src);
        mode= HeKWASUTF8(src) ? 2 :  HeKUTF8(src) ? 1 : 0;
    }
    if (mode == 2) { /* must convert back to utf8 */
        char* utf8= (char *)Perl_bytes_to_utf8(aTHX_ (U8 *)str, &len);
        srl_dump_pv(aTHX_ enc, utf8, len, 1);
        Safefree(utf8);
    } else {
        srl_dump_pv(aTHX_ enc, str, len, mode);
    }
}


static void
srl_dump_pv(pTHX_ srl_encoder_t *enc, const char* src, STRLEN src_len, int is_utf8)
{
    BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH + src_len); /* overallocate a bit sometimes */
    if (is_utf8) {
        srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_STRING_UTF8, src_len);
    } else if (src_len <= SRL_MASK_ASCII_LEN) {
        srl_buf_cat_char_nocheck(enc, SRL_HDR_ASCII_LOW | (char)src_len);
    } else {
        srl_buf_cat_varint_nocheck(aTHX_ enc, SRL_HDR_STRING, src_len);
    }
    Copy(src, enc->pos, src_len, char);
    enc->pos += src_len;
}




/* Dumps generic SVs and delegates
 * to more specialized functions for RVs, etc. */
/* TODO decide when to use the IV, when to use the PV, and when
 *      to use the NV slots of the SV.
 *      Safest simple solution seems "prefer string" (fuck dualvars).
 *      Potentially better but slower: If we would choose the string,
 *      then try int-to-string (respective float-to-string) conversion
 *      and strcmp. If same, then use int or float.
 */
static void
srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src)
{
    UV refcount;
    svtype svt;
    MAGIC *mg;
    UV weakref_ofs= 0;              /* preserved between loops */
    char *ref_rewrite_pos= NULL;    /* preserved between loops */
    assert(src);

redo_dump:
    svt = SvTYPE(src);
    refcount = SvREFCNT(src);

    if ( SvMAGICAL(src) ) {
        SvGETMAGIC(src);
        if ( ( mg = mg_find(src, PERL_MAGIC_backref) ) ) {
            PTABLE_t *weak_seenhash= SRL_GET_WEAK_SEENHASH(enc);
            PTABLE_ENTRY_t *pe= PTABLE_find(weak_seenhash, src);
            if (!pe) {
                /* not seen it before */
                if (DEBUGHACK) warn("scalar %p - is weak referent, storing %lu", src, weakref_ofs);
                /* if weakref_ofs is false we got here some way that holds a refcount on this item */
                PTABLE_store(weak_seenhash, src, (void *)weakref_ofs);
            } else {
                if (DEBUGHACK) warn("scalar %p - is weak referent, seen before value:%lu weakref_ofs:%lu",
                        src, (UV)pe->value, (UV)weakref_ofs);
                if (pe->value)
                    pe->value= (void *)weakref_ofs;
            }
            refcount++;
            weakref_ofs= 0;
        }
    }

    /* check if we have seen this scalar before, and track it so
     * if we see it again we recognize it */
    if ( expect_false( refcount > 1 ) ) {
        PTABLE_t *ref_seenhash= SRL_GET_REF_SEENHASH(enc);
        const ptrdiff_t oldoffset = (ptrdiff_t)PTABLE_fetch(ref_seenhash, src);
        if (expect_false(oldoffset)) {
            if (ref_rewrite_pos) {
                if (DEBUGHACK) warn("ref to %p as %lu", src, oldoffset);
                enc->pos= ref_rewrite_pos;
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_REFP, (UV)oldoffset);
            } else {
                if (DEBUGHACK) warn("alias to %p as %lu", src, oldoffset);
                srl_buf_cat_varint(aTHX_ enc, SRL_HDR_ALIAS, (UV)oldoffset);
            }
            SRL_SET_FBIT(*(enc->buf_start + oldoffset));
            return;
        }
        ref_rewrite_pos= NULL;
        if (DEBUGHACK) warn("storing %p as %lu", src, BUF_POS_OFS(enc));
        PTABLE_store(ref_seenhash, src, (void *)BUF_POS_OFS(enc));
    }
    assert(weakref_ofs == 0);
    assert(is_ref == 0);

    if (SvPOKp(src)) {
#ifdef MODERN_REGEXP
        if (expect_false( svt == SVt_REGEXP ) ) {
            srl_dump_regexp(aTHX_ enc, src);
        }
        else
#endif
        {
            STRLEN len;
            char *str = SvPV(src, len);
            srl_dump_pv(aTHX_ enc, str, len, SvUTF8(src));
        }
    }
    else
    if (SvNOKp(src)) {
        /* dump floats */
        srl_dump_nv(aTHX_ enc, src);
    }
    else
    if (SvIOKp(src)) {
        /* dump ints */
        srl_dump_ivuv(aTHX_ enc, src);
    }
    else
    if (SvROK(src)) {
        /* dump references */
        SV *referent= SvRV(src);
        if (SvWEAKREF(src)) {
            weakref_ofs= BUF_POS_OFS(enc);
            srl_buf_cat_char(enc, SRL_HDR_WEAKEN);
        }
        ref_rewrite_pos= enc->pos;
        if (sv_isobject(src)) {
            /* Check that we actually want to support objects */
            if (expect_false( SRL_ENC_HAVE_OPTION(enc, SRL_F_CROAK_ON_BLESS)) ) {
                croak("Attempted to serialize blessed reference. Serializing objects "
                      "using Sereal::Encoder was explicitly disabled using the "
                      "'croak_on_bless' option.");
            }
            /* FIXME reuse/ref/... should INCLUDE the bless stuff. */
            /* Write bless operator with class name */
            srl_buf_cat_char(enc, SRL_HDR_BLESS);
            srl_dump_classname(aTHX_ enc, referent);
        }
        srl_buf_cat_char(enc, SRL_HDR_REFN);
        src= referent;
        goto redo_dump;
    }
    else
#ifndef MODERN_REGEXP
    if (
        svt == SVt_PVMG &&
        ((SvFLAGS(src) & (SVs_OBJECT|SVf_OK|SVs_GMG|SVs_SMG|SVs_RMG)) == (SVs_OBJECT|BFD_Svs_SMG_OR_RMG)) &&
        (mg = mg_find(src, PERL_MAGIC_qr))
    ) {
            /* Housten, we have a regex! */
        srl_dump_regexp(aTHX_ enc, (SV*)mg); /* yes the SV* cast makes me feel dirty too */
    }
    else
#endif
    if (svt == SVt_PVHV) {
        srl_dump_hv(aTHX_ enc, (HV *)src);
    }
    else
    if (svt == SVt_PVAV) {
        srl_dump_av(aTHX_ enc, (AV *)src);
    }
    else
    if (!SvOK(src)) {
        /* undef */
        srl_buf_cat_char(enc, SRL_HDR_UNDEF);
    }
    else
    {
        croak("found type %u %s(0x%p), but it is not representable by the Sereal encoding format", svt, sv_reftype(src,0),src);
    }
}



