/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#define NEED_newSV_type
#define NEED_newSVpvn_flags
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

#include "srl_decoder.h"

#include "srl_common.h"
#include "ptable.h"
#include "srl_protocol.h"

#include "snappy/csnappy_decompress.c"

/* predeclare all our subs so we have one definitive authority for their signatures */
static SRL_INLINE UV srl_read_varint_uv_safe(pTHX_ srl_decoder_t *dec);
static SRL_INLINE UV srl_read_varint_uv_nocheck(pTHX_ srl_decoder_t *dec);
static SRL_INLINE UV srl_read_varint_uv(pTHX_ srl_decoder_t *dec);
static SRL_INLINE UV srl_read_varint_uv_offset(pTHX_ srl_decoder_t *dec, const char * const errstr);
static SRL_INLINE UV srl_read_varint_uv_length(pTHX_ srl_decoder_t *dec, const char * const errstr);

static SRL_INLINE SV *srl_fetch_item(pTHX_ srl_decoder_t *dec, UV item, const char const *tag_name);

/* these three are "Public" */
srl_decoder_t *srl_build_decoder_struct(pTHX_ HV *opt);             /* constructor - called from ->new() */
void srl_destroy_decoder(pTHX_ srl_decoder_t *dec);                 /* destructor  - called from ->DESTROY() */
void srl_decoder_destructor_hook(pTHX_ void *p);                    /* destructor hook - called automagically */

/* the top level components of the decode process - called by srl_decode_into() */
static SRL_INLINE void srl_begin_decoding(pTHX_ srl_decoder_t *dec, SV *src, UV start_offset);       /* set up the decoder to handle a given var */
static SRL_INLINE void srl_read_header(pTHX_ srl_decoder_t *dec);                    /* validate header */
static SRL_INLINE void srl_read_single_value(pTHX_ srl_decoder_t *dec, SV* into);   /* main recursive dump routine */
static SRL_INLINE void srl_finalize_structure(pTHX_ srl_decoder_t *dec);             /* optional finalize structure logic */
static SRL_INLINE void srl_clear_decoder(pTHX_ srl_decoder_t *dec);                 /* clean up decoder after a dump */

/* the internal routines to handle each kind of object we have to deserialize */
static SRL_INLINE SV *srl_read_alias(pTHX_ srl_decoder_t *dec);
static SRL_INLINE void srl_read_copy(pTHX_ srl_decoder_t *dec, SV* into);

static SRL_INLINE void srl_read_hash(pTHX_ srl_decoder_t *dec, SV* into, U8 tag);
static SRL_INLINE void srl_read_array(pTHX_ srl_decoder_t *dec, SV* into, U8 tag);
static SRL_INLINE void srl_read_regexp(pTHX_ srl_decoder_t *dec, SV* into);

static SRL_INLINE void srl_read_refp(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_refn(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_weaken(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_long_double(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_double(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_float(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8, SV* into);
static SRL_INLINE void srl_read_varint(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_zigzag(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE void srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag, SV* into);
static SRL_INLINE void srl_read_object(pTHX_ srl_decoder_t *dec, SV* into);

/* FIXME unimplemented!!! */
static SRL_INLINE void srl_read_objectv(pTHX_ srl_decoder_t *dec, SV* into);
static SRL_INLINE SV *srl_read_extend(pTHX_ srl_decoder_t *dec, SV* into);

#define ASSERT_BUF_SPACE(dec,len,msg) STMT_START {              \
    if (expect_false( (UV)BUF_SPACE((dec)) < (UV)(len) )) { \
        MYCROAK("Unexpected termination of packet%s, want %lu bytes, only have %lu available", (msg), (UV)(len), (UV)BUF_SPACE((dec)));  \
    }                                                       \
} STMT_END

#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)

/* Macro to assert that the type of an SV is complex enough to
 * be an RV. Differs on old perls since there used to be an RV type.
 */
#if PERL_VERSION < 12
#   define SRL_ASSERT_TYPE_FOR_RV(sv) STMT_START {  \
            if (SvTYPE(sv) < SVt_PV)                \
                sv_upgrade(into, SVt_RV);           \
        } STMT_END
#else
#   define SRL_ASSERT_TYPE_FOR_RV(sv) STMT_START {  \
            if (SvTYPE(sv) < SVt_PV)                \
                sv_upgrade(into, SVt_IV);           \
        } STMT_END
#endif


/* PUBLIC ROUTINES ROUTINES */

/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_decoder_t *
srl_build_decoder_struct(pTHX_ HV *opt)
{
    srl_decoder_t *dec;
    SV **svp;

    Newxz(dec, 1, srl_decoder_t);

    dec->ref_seenhash = PTABLE_new();
    /* load options */
    if (opt != NULL) {
        if ( (svp = hv_fetchs(opt, "refuse_snappy", 0)) && SvTRUE(*svp))
          dec->flags |= SRL_F_DECODER_REFUSE_SNAPPY;
    }

    return dec;
}

/* Explicit destructor */
void
srl_destroy_decoder(pTHX_ srl_decoder_t *dec)
{
    PTABLE_free(dec->ref_seenhash);
    if (dec->ref_stashes) {
        PTABLE_free(dec->ref_stashes);
        PTABLE_free(dec->ref_bless_av);
    }
    if (dec->weakref_av) {
        SvREFCNT_dec(dec->weakref_av);
        dec->weakref_av = NULL;
    }
    Safefree(dec);
}

/* This is fired when we exit the Perl pseudo-block.
 * It frees our decoder and all. Put decoder-level cleanup
 * logic here so that we can simply use croak/longjmp for
 * exception handling. Makes life vastly easier!
 */
void
srl_decoder_destructor_hook(pTHX_ void *p)
{
    srl_decoder_t *dec = (srl_decoder_t *)p;

    assert(SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DESTRUCTOR_OK));
    SRL_DEC_UNSET_OPTION(dec, SRL_F_DECODER_DESTRUCTOR_OK);

    /* Only free decoder if not for reuse */
    if (!SRL_DEC_HAVE_OPTION(dec, SRL_F_REUSE_DECODER)) {
        srl_destroy_decoder(aTHX_ dec);
    }
    else {
        /* Clear instead - decoder reused */
        srl_clear_decoder(aTHX_ dec);
    }
}

/* This is the main routine to deserialize a structure.
 * It rolls up all the other "top level" routines into one
 */
SV *
srl_decode_into(pTHX_ srl_decoder_t *dec, SV *src, SV* into, UV start_offset)
{
    assert(dec != NULL);
    srl_begin_decoding(aTHX_ dec, src, start_offset);
    srl_read_header(aTHX_ dec);
    if (SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DECOMPRESS_SNAPPY)) {
        /* uncompress */
        uint32_t dest_len;
        SV *buf_sv;
        unsigned char *buf;
        unsigned char *old_pos;
        const ptrdiff_t compressed_packet_len = dec->buf_end - dec->pos;
        const ptrdiff_t sereal_header_len = dec->pos - dec->buf_start;
        int decompress_ok;

        int header_len = csnappy_get_uncompressed_length(
                            (char *)dec->pos,
                            dec->buf_end - dec->pos,
                            &dest_len
                         );
        if (header_len == CSNAPPY_E_HEADER_BAD)
            ERROR("Invalid Snappy header in Snappy-compressed Sereal packet");

        /* Let perl clean this up. Yes, it's not the most efficient thing
         * ever, but it's just one mortal per full decompression, so not
         * a bottle-neck. */
        buf_sv = sv_2mortal( newSV(sereal_header_len + dest_len + 1 ));
        buf = (unsigned char *)SvPVX(buf_sv);

        /* FIXME probably unnecessary to copy the Sereal header! */
        Copy(dec->buf_start, buf, sereal_header_len, unsigned char);

        old_pos = dec->pos;
        dec->buf_start = buf;
        dec->pos = buf + sereal_header_len;
        dec->buf_end = dec->pos + dest_len;
        dec->buf_len = dest_len + sereal_header_len;

        decompress_ok = csnappy_decompress_noheader((char *)(old_pos + header_len),
                                                    compressed_packet_len - header_len,
                                                    (char *)dec->pos,
                                                    &dest_len);
        if (expect_false( decompress_ok != 0 ))
        {
            ERRORf1("Snappy decompression of Sereal packet payload failed with error %i!", decompress_ok);
        }
    }

    if (expect_true(!into)) {
        into= sv_2mortal(newSV_type(SVt_NULL));
    }
    srl_read_single_value(aTHX_ dec, into);
    /* assert(dec->pos == dec->buf_end); For now we disable this */
    if (expect_false(SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE))) {
        srl_finalize_structure(aTHX_ dec);
    }

    /* Remember the number of bytes used for the user to query */
    dec->bytes_consumed = dec->pos - dec->buf_start;

    srl_clear_decoder(aTHX_ dec);
    return into;
}

/* TOP LEVEL PRIVATE ROUTINES */

static SRL_INLINE void
srl_clear_decoder(pTHX_ srl_decoder_t *dec)
{
    if (dec->buf_start == dec->buf_end)
        return;

    SRL_DEC_RESET_VOLATILE_FLAGS(dec);
    dec->buf_start = dec->buf_end = dec->pos = dec->save_pos = NULL;
    if (dec->weakref_av)
        av_clear(dec->weakref_av);

    PTABLE_clear(dec->ref_seenhash);
    if (dec->ref_stashes) {
        PTABLE_clear(dec->ref_stashes);
        PTABLE_clear(dec->ref_bless_av);
    }
}

static SRL_INLINE void
srl_begin_decoding(pTHX_ srl_decoder_t *dec, SV *src, UV start_offset)
{
    STRLEN len;
    unsigned char *tmp;

    /* Assert that we did not push a destructor before */
    assert(!SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DESTRUCTOR_OK));
    /* Push destructor, set destructor-is-pushed flag */
    SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_DESTRUCTOR_OK);
    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR_X(&srl_decoder_destructor_hook, (void *)dec);

    SRL_DEC_RESET_VOLATILE_FLAGS(dec);
    tmp = (unsigned char*)SvPV(src, len);
    if (expect_false( start_offset > len )) {
        ERROR("Start offset is beyond input string length");
    }
    dec->buf_start= dec->pos= tmp + start_offset;
    dec->buf_end= dec->buf_start + len - start_offset;
    dec->buf_len= len - start_offset;
    dec->bytes_consumed = 0;
}

static SRL_INLINE void
srl_read_header(pTHX_ srl_decoder_t *dec)
{
    UV header_len;
    U8 proto_version_and_flags;

    /* 4 byte magic string + version/flags + hdr len at least */
    ASSERT_BUF_SPACE(dec, 4 + 1 + 1," while reading header");
    if (strnEQ((char*)dec->pos, SRL_MAGIC_STRING, 4)) {
        dec->pos += 4;
        proto_version_and_flags = *dec->pos++;
        if (expect_false( (proto_version_and_flags & SRL_PROTOCOL_VERSION_MASK) != 1 ))
            ERRORf1("Unsupported Sereal protocol version %u",
                    proto_version_and_flags & SRL_PROTOCOL_VERSION_MASK);
        if ((proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_RAW) {
            /* no op */
        }
        else
        if (( proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) == SRL_PROTOCOL_ENCODING_SNAPPY) {
            dec->flags |= SRL_F_DECODER_DECOMPRESS_SNAPPY;
            if (expect_false( SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_REFUSE_SNAPPY) )) {
                ERROR("Sereal document is compressed with Snappy, "
                      "but this decoder is configured to refuse Snappy-compressed input.");
            }
        }
        else
        {
            ERRORf1("Serial document encoded in an unknown format '%d'", ( proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) >> 4 );
        }
        header_len= srl_read_varint_uv_length(aTHX_ dec," while reading header"); /* must do this via a temporary as it modifes dec->pos itself */
        dec->pos += header_len;
    } else {
        ERROR("bad header");
    }
}

static SRL_INLINE void
srl_finalize_structure(pTHX_ srl_decoder_t *dec)
{
    if (dec->weakref_av)
        av_clear(dec->weakref_av);
    if (dec->ref_stashes) {
        PTABLE_ITER_t *it = PTABLE_iter_new(dec->ref_stashes);
        PTABLE_ENTRY_t *ent;

        /* we now walk the weak_seenhash and set any tags it points
         * at to the PAD opcode, this basically turns the first weakref
         * we encountered into a normal ref when there is only a weakref
         * pointing at the structure. */
        while ( NULL != (ent = PTABLE_iter_next(it)) ) {
            HV *stash = (HV* )ent->value;
            AV *ref_bless_av  = PTABLE_fetch(dec->ref_bless_av, ent->key);
            I32 len;
            if (expect_false( !stash || !ref_bless_av )) {
                ERROR("missing stash or ref_bless_av!");
            }
            for( len= av_len(ref_bless_av) + 1 ; len > 0 ; len-- ) {
                SV* obj= av_pop(ref_bless_av); /*note that av_pop does NOT refcnt dec the sv*/
                if (SvREFCNT(obj)>1) {
                    /* It is possible that someone handcrafts a hash with a key collision,
                     * which could trick us into effectively blessing an object and then
                     * calling DESTROY on it. So we track the refcount of the objects
                     * popped off the ref_bless_av, and only bless if their refcount *before*
                     * we refcount dec is higher than 1. If it is 1 then we just destroy the 
                     * object.
                     * */
                    if (expect_true( obj )) {
                        sv_bless(obj, stash);
                    } else {
                        ERROR("object missing from ref_bless_av array?");
                    }
                } else {
                    warn("serialization contains a duplicated key, ignoring");
                }
                SvREFCNT_dec(obj);
            }
        }
        PTABLE_iter_free(it);
    }
}

/* PRIVATE UTILITY FUNCTIONS */

static SRL_INLINE UV
srl_read_varint_uv_safe(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (BUF_NOT_DONE(dec) && *dec->pos & 0x80) {
        uv |= ((UV)(*dec->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            ERROR("varint too big");
    }
    if (expect_true( BUF_NOT_DONE(dec) )) {
        uv |= ((UV)*dec->pos++ << lshift);
    } else {
        ERROR("varint terminated prematurely");
    }
    return uv;
}

#define SET_UV_FROM_VARINT(uv, from) STMT_START {      \
    if (*from < 0x80) {                                             \
        uv= (UV)*from++;                                            \
    } else {                                                        \
        unsigned int lshift= 7;                                     \
        uv= (UV)(*from++ & 0x7f);                                   \
        while (*from & 0x80){                                       \
            uv |= ((UV)(*from++ & 0x7F) << lshift);                 \
            lshift += 7;                                            \
        }                                                           \
        uv |= ((UV)(*from++) << lshift);                            \
    }                                                               \
} STMT_END

static SRL_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*dec->pos & 0x80) {
        uv |= ((UV)(*dec->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (expect_false( lshift > (sizeof(UV) * 8) ))
            ERROR("varint too big");
    }
    uv |= ((UV)(*dec->pos++) << lshift);
    return uv;
}

static SRL_INLINE UV
srl_read_varint_uv(pTHX_ srl_decoder_t *dec)
{
    if (expect_true( dec->buf_end - dec->pos > 10 ))
        return srl_read_varint_uv_nocheck(aTHX_ dec);
    else
        return srl_read_varint_uv_safe(aTHX_ dec);
}

static SRL_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_decoder_t *dec, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ dec);

    if (dec->buf_start + len >= dec->pos) {
        ERRORf4("Corrupted packet%s. Offset %lu points past current position %lu in packet with length of %lu bytes long",
                errstr, len, BUF_POS_OFS(dec), dec->buf_len);
    }
    return len;
}

static SRL_INLINE UV
srl_read_varint_uv_length(pTHX_ srl_decoder_t *dec, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ dec);

    ASSERT_BUF_SPACE(dec, len, errstr);

    return len;
}

static SRL_INLINE UV
srl_read_varint_uv_count(pTHX_ srl_decoder_t *dec, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ dec);
    if (len > I32_MAX) {
        ERRORf3("Corrupterd packet%s. Count %lu exceeds I32_MAX (%li), which is impossible.",
                errstr, len, I32_MAX);
    }
    return len;
}


static SRL_INLINE void
srl_track_sv(pTHX_ srl_decoder_t *dec, U8 *track_pos, SV *sv) {
    PTABLE_store(dec->ref_seenhash, (void *)(track_pos - dec->buf_start), (void *)sv);
}

static SRL_INLINE SV *
srl_fetch_item(pTHX_ srl_decoder_t *dec, UV item, const char const *tag_name) {
    SV *sv= (SV *)PTABLE_fetch(dec->ref_seenhash, (void *)item);
    if (expect_false( !sv ))
        ERRORf2("%s(%d) references an unknown item", tag_name, item);
    return sv;
}

/****************************************************************************
 * PRIVATE WORKER SUBS FOR DEPARSING                                        *
 ****************************************************************************/

static SRL_INLINE void
srl_read_varint(pTHX_ srl_decoder_t *dec, SV* into)
{
    UV uv= srl_read_varint_uv(aTHX_ dec);
    if (uv <= (UV)IV_MAX) {
        sv_setiv(into, (IV)uv);
    } else {
        /* grr, this is ridiculous! */
        sv_setiv(into, 0);
        SvIsUV_on(into);
        SvUV_set(into, uv);
    }
}


static SRL_INLINE void
srl_read_zigzag(pTHX_ srl_decoder_t *dec, SV* into)
{
    UV uv= srl_read_varint_uv(aTHX_ dec);
    if (uv & 1) {
        sv_setiv(into, (IV)( -( 1 + (uv >> 1) ) ) );
    } else {
        uv = uv >> 1;
        if (uv <= (UV)IV_MAX) {
            sv_setiv(into, (IV)uv);
        } else {
            /* grr, this is ridiculous! */
            sv_setiv(into, 0);
            SvIsUV_on(into);
            SvUV_set(into, uv);
        }
    }
}


static SRL_INLINE void
srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8, SV* into)
{
    UV len= srl_read_varint_uv_length(aTHX_ dec, " while reading string");
    sv_setpvn(into,(char *)dec->pos,len);
    if (is_utf8) {
        SvUTF8_on(into);
    } else {
        SvUTF8_off(into);
    }
    dec->pos+= len;
}


static SRL_INLINE void
srl_read_float(pTHX_ srl_decoder_t *dec, SV* into)
{
    ASSERT_BUF_SPACE(dec, sizeof(float), " while reading FLOAT");
    sv_setnv(into, (NV)*((float *)dec->pos));
    dec->pos+= sizeof(float);
}


static SRL_INLINE void
srl_read_double(pTHX_ srl_decoder_t *dec, SV* into)
{
    ASSERT_BUF_SPACE(dec, sizeof(double)," while reading DOUBLE");
    sv_setnv(into, (NV)*((double *)dec->pos));
    dec->pos+= sizeof(double);
}


static SRL_INLINE void
srl_read_long_double(pTHX_ srl_decoder_t *dec, SV* into)
{
    ASSERT_BUF_SPACE(dec, sizeof(long double)," while reading LONG_DOUBLE");
    sv_setnv(into, (NV)*((long double *)dec->pos));
    dec->pos+= sizeof(long double);
}


static SRL_INLINE void
srl_read_array(pTHX_ srl_decoder_t *dec, SV *into, U8 tag) {
    UV len;
    if (tag) {
        SV *referent= (SV *)newAV();
        len= tag & 15;
        SRL_ASSERT_TYPE_FOR_RV(into);
        SvTEMP_off(referent);
        SvRV_set(into, referent);
        SvROK_on(into);
        into= referent;
    } else {
        len= srl_read_varint_uv_count(aTHX_ dec," while reading ARRAY");
        (void)SvUPGRADE(into, SVt_PVAV);
    }

    if (len) {
        SV **av_array;
        SV **av_end;

        /* we cheat and store undef in the array - we will overwrite it later */
        av_store((AV*)into, len-1, &PL_sv_undef);
        av_array= AvARRAY((AV*)into);
        av_end= av_array + len;

        for ( ; av_array < av_end ; av_array++) {
            if ( expect_false( *dec->pos == SRL_HDR_ALIAS ) ) {
                dec->pos++;
                *av_array= srl_read_alias(aTHX_ dec);
            } else {
                *av_array= newSV_type(SVt_NULL);
                srl_read_single_value(aTHX_ dec, *av_array);
            }
        }
    }
}

#ifndef HV_FETCH_LVALUE
#define OLDHASH
#define IS_LVALUE 1
#endif

static SRL_INLINE void
srl_read_hash(pTHX_ srl_decoder_t *dec, SV* into, U8 tag) {
    IV num_keys;
    if (tag) {
        SV *referent= (SV *)newHV();
        num_keys= tag & 15;
        SRL_ASSERT_TYPE_FOR_RV(into);
        SvTEMP_off(referent);
        SvRV_set(into, referent);
        SvROK_on(into);
        into= referent;
    } else {
        num_keys= srl_read_varint_uv_count(aTHX_ dec," while reading HASH");
        (void)SvUPGRADE(into, SVt_PVHV);
    }

    HvSHAREKEYS_on(into); /* apparently required on older perls */

    hv_ksplit((HV *)into, num_keys); /* make sure we have enough room */
    /* NOTE: contents of hash are stored VALUE/KEY, reverse from normal perl
     * storage, this is because it simplifies the hash storage logic somewhat */
    for (; num_keys > 0 ; num_keys--) {
        U8 *from;
        U8 tag;
        SV **fetched_sv;
        I32 key_len;
#ifndef OLDHASH
        U32 flags= 0;
#endif
        ASSERT_BUF_SPACE(dec,1," while reading key tag byte for HASH");
        tag= *dec->pos++;
        if (IS_SRL_HDR_SHORT_BINARY(tag)) {
            key_len= (IV)SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            ASSERT_BUF_SPACE(dec,key_len," while reading string/SHORT_BINARY key");
            from= dec->pos;
            dec->pos += key_len;
        } else if (tag == SRL_HDR_BINARY) {
            key_len= (IV)srl_read_varint_uv_length(aTHX_ dec, " while reading string/BINARY key");
            from= dec->pos;
            dec->pos += key_len;
        } else if (tag == SRL_HDR_STR_UTF8) {
            key_len= (IV)srl_read_varint_uv_length(aTHX_ dec, " while reading UTF8 key");
            from= dec->pos;
            dec->pos += key_len;
#ifdef OLDHASH
            key_len= -key_len;
#else
            flags= HVhek_UTF8;
#endif
        } else if (tag == SRL_HDR_COPY) {
            UV ofs= srl_read_varint_uv_offset(aTHX_ dec, " while reading COPY tag");
            from= dec->buf_start + ofs;
            tag= *from++;
            /* note we do NOT validate these items, as we have alread read them
             * and if they were a problem we would not be here to process them! */
            if (IS_SRL_HDR_SHORT_BINARY(tag)) {
                key_len= SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            }
            else
            if (tag == SRL_HDR_BINARY) {
                SET_UV_FROM_VARINT(key_len, from);
            }
            else
            if (tag == SRL_HDR_STR_UTF8) {
                SET_UV_FROM_VARINT(key_len, from);
#ifdef OLDHASH
                key_len= -key_len;
#else
                flags= HVhek_UTF8;
#endif
            }
            else {
                ERROR_BAD_COPY(dec, SRL_HDR_HASH);
            }
        } else {
            ERROR_UNEXPECTED(dec,tag,"a stringish type");
        }
#ifdef OLDHASH
        fetched_sv= hv_fetch((HV *)into, (char *)from, key_len, IS_LVALUE);
#else
        fetched_sv= hv_common((HV *)into, NULL, (char *)from, key_len, flags, HV_FETCH_LVALUE|HV_FETCH_JUST_SV, NULL, 0);
#endif
        if (!fetched_sv) {
            ERROR_PANIC(dec,"failed to hv_store");
        }
        if ( expect_false( *dec->pos == SRL_HDR_ALIAS ) ) {
            dec->pos++;
            SvREFCNT_dec(*fetched_sv);
            *fetched_sv= srl_read_alias(aTHX_ dec);
        } else {
            srl_read_single_value(aTHX_ dec, *fetched_sv);
        }
    }
}


static SRL_INLINE void
srl_read_refn(pTHX_ srl_decoder_t *dec, SV* into)
{
    SV *referent;
    ASSERT_BUF_SPACE(dec, 1, " while reading REFN referent");
    referent= newSV(SVt_NULL);

    SRL_ASSERT_TYPE_FOR_RV(into);
    SvTEMP_off(referent);
    SvRV_set(into, referent);
    SvROK_on(into);
    srl_read_single_value(aTHX_ dec, referent);
}

static SRL_INLINE void
srl_read_refp(pTHX_ srl_decoder_t *dec, SV* into)
{
    /* something we did before */
    UV item= srl_read_varint_uv_offset(aTHX_ dec, " while reading REFP tag");
    SV *referent= srl_fetch_item(aTHX_ dec, item, "REFP");
    SvREFCNT_inc(referent);

    SRL_ASSERT_TYPE_FOR_RV(into);
    SvTEMP_off(referent);
    SvRV_set(into, referent);
    SvROK_on(into);
}


static SRL_INLINE void
srl_read_weaken(pTHX_ srl_decoder_t *dec, SV* into)
{
    SV* referent;
    srl_read_single_value(aTHX_ dec, into);
    if (expect_false( !SvROK(into) ))
        ERROR("WEAKEN op");
    referent= SvRV(into);
    /* we have to be careful to not allow the referent's refcount
     * to go to zero in the process of us weakening the the ref.
     * For instance this may be aliased or reused later by a non-weakref
     * which will "fix" the refcount, however we need to be able to deserialize
     * in the opposite order, so if the referent's refcount is 1
     * we increment it and stuff it in the weakref_av before we call
     * sv_rvweaken(), right before we exit we clear any items from
     * that array, which does the REFCNT_dec for us, and everything
     * works out ok. */
    if (expect_true( SvREFCNT(referent)==1 )) {
        if (expect_false( !dec->weakref_av ))
            dec->weakref_av= newAV();
        SvREFCNT_inc(referent);
        av_push(dec->weakref_av, referent);
        SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE);
    }
    sv_rvweaken(into);
}

static SRL_INLINE void
srl_read_objectv(pTHX_ srl_decoder_t *dec, SV* into)
{
    AV *av= NULL;
    STRLEN ofs= srl_read_varint_uv_offset(aTHX_ dec," while reading OBJECTV classname");

    if ( !dec->ref_bless_av)
        ERROR("Corrupt packet. OBJECTV used without preceding OBJECTV of any kind");
    av= (AV *)PTABLE_fetch(dec->ref_bless_av, (void *)ofs);
    if (NULL == av) {
        ERRORf1("Corrupt packet. OBJECTV references unknown classname offset: %lu", ofs);
    }
    /* now deparse the thing we are going to bless */
    srl_read_single_value(aTHX_ dec, into);

    /* and also stuff it into the av - we dont have to do any more book-keeping */
    SvREFCNT_inc(into);
    av_push(av, into);
}

static SRL_INLINE void
srl_read_object(pTHX_ srl_decoder_t *dec, SV* into)
{
    HV *stash= NULL;
    AV *av= NULL;
    STRLEN storepos= 0;
    UV ofs= 0;
    U32 key_len;
    I32 flags= GV_ADD;
    U8 tag;
    U8 *from;
    /* now find the class name - first check if this is a copy op
     * this is bit tricky, as we could have a copy of a raw string
     * we could also have a copy of a previously mentioned class
     * name. We have to handle both, which leads to some non-linear
     * code flow in the below code */
    ASSERT_BUF_SPACE(dec,1," while reading classname tag");

    storepos= BUF_POS_OFS(dec);
    tag= *dec->pos++;
    if (IS_SRL_HDR_SHORT_BINARY(tag)) {
        key_len= SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        from= dec->pos;
        dec->pos += key_len;
    }
    else
    if (tag == SRL_HDR_STR_UTF8) {
        key_len= srl_read_varint_uv_length(aTHX_ dec, " while reading UTF8 class name");
        flags = flags | SVf_UTF8;
        from= dec->pos;
        dec->pos += key_len;
    }
    else
    if (tag == SRL_HDR_BINARY) {
        key_len= srl_read_varint_uv_length(aTHX_ dec, " while reading string/BINARY class name");
        from= dec->pos;
        dec->pos += key_len;
    }
    else
    if (tag == SRL_HDR_COPY) {
        ofs= srl_read_varint_uv_offset(aTHX_ dec, " while reading COPY class name");
        storepos= ofs;
        if (dec->ref_seenhash) {
            stash= PTABLE_fetch(dec->ref_seenhash, (void *)ofs);
        }
        if (!stash) {
            from= dec->buf_start + ofs;
            tag= *from++;
            /* note we do NOT validate these items, as we have alread read them
             * and if they were a problem we would not be here to process them! */
            if (IS_SRL_HDR_SHORT_BINARY(tag)) {
                key_len= SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
            }
            else
            if (tag == SRL_HDR_BINARY) {
                SET_UV_FROM_VARINT(key_len, from);
            }
            else
            if (tag == SRL_HDR_STR_UTF8) {
                SET_UV_FROM_VARINT(key_len, from);
                flags = flags | SVf_UTF8;
            }
            else {
                ERROR_BAD_COPY(dec, SRL_HDR_OBJECT);
            }
        }
        /* NOTREACHED */
    } else {
        ERROR_UNEXPECTED(dec,tag, "a class name");
    }
    if (!stash) {
        if (expect_false( !dec->ref_stashes )) {
            dec->ref_stashes = PTABLE_new();
            dec->ref_bless_av = PTABLE_new();
        }
        stash= gv_stashpvn((char *)from, key_len, flags);
        PTABLE_store(dec->ref_stashes, (void *)storepos, (void *)stash);
        av= newAV();
        sv_2mortal((SV*)av);
        PTABLE_store(dec->ref_bless_av, (void *)storepos, (void *)av);
    } else if (NULL == (av= (AV *)PTABLE_fetch(dec->ref_bless_av, (void *)storepos)) ) {
        ERRORf1("Panic, no ref_bless_av for %lu",storepos);
    }

    /* we now have a stash so we /could/ bless... except that
     * we don't actually want to do so right now. We want to defer blessing
     * until the full packet has been read. Yes it is more overhead, but
     * we really dont want to trigger DESTROY methods from a partial
     * deparse. So we insert the item into an array to be blessed later */
    SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE);
    SvREFCNT_inc(into);
    av_push(av, into);

    /* now deparse the thing we are going to bless */
    srl_read_single_value(aTHX_ dec, into);
}


static SRL_INLINE void
srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag, SV* into)
{
    (void)tag; /* unused as of now */
    const UV len = srl_read_varint_uv_length(aTHX_ dec, " while reading reserved");
    dec->pos += len; /* discard */
    sv_setsv(into, &PL_sv_undef);
}

#ifdef SvRX
#define MODERN_REGEXP
#endif

static SRL_INLINE void
srl_read_regexp(pTHX_ srl_decoder_t *dec, SV* into)
{
    SV *sv_pat= newSV_type(SVt_NULL);
    srl_read_single_value(aTHX_ dec, sv_pat);
    ASSERT_BUF_SPACE(dec, 1, " while reading regexp modifer tag");
    /* For now we will serialize the flags as ascii strings. Maybe we should use
     * something else but this is easy to debug and understand - since the modifiers
     * are tagged it doesn't matter much, we can add other tags later */
    if ( expect_true( IS_SRL_HDR_SHORT_BINARY(*dec->pos) ) ) {
        U8 mod_len= SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(*dec->pos++);
        U32 flags= 0;
        ASSERT_BUF_SPACE(dec, mod_len, " while reading regexp modifiers");
        while (mod_len > 0) {
            mod_len--;
            switch (*dec->pos++) {
                case 'm':
                    flags= flags | PMf_MULTILINE;
                    break;
                case 's':
                    flags= flags | PMf_SINGLELINE;
                    break;
                case 'i':
                    flags= flags | PMf_FOLD;
                    break;
                case 'x':
                    flags= flags | PMf_EXTENDED;
                    break;
#ifdef MODERN_REGEXP
                case 'p':
                    flags = flags | PMf_KEEPCOPY;
                    break;
#endif
                default:
                    ERROR("bad modifier");
                    break;
            }
        }
#ifdef SvRX
        {
            /* This is ugly. We have to swap out the insides of our SV
             * with the one we get back from CALLREGCOMP, as there is no
             * way to get it to fill our SV.
             *
             * As far as I understand this works because of how the SV
             * is laid out. Needs to be verified with someone who knows
             * better.
             */

            /* compile the regex */
            SV *referent= (SV*)CALLREGCOMP(sv_pat, flags);
            SV tmp;

            /* make sure the SV came from us (it should) and
             * is bodyless */
            assert( SvTYPE(into) == SVt_NULL );

#define SWAP_DEBUG 0
            if (SWAP_DEBUG) { warn("before swap:"); sv_dump(into); sv_dump(referent); }

            /* Swap the contents of the two heads. */
            Copy(into, &tmp, 1, SV);
            Copy((SV*)referent, into, 1, SV);
            Copy(&tmp, (SV*)referent, 1, SV);
            SvREFCNT(referent)= SvREFCNT(into);
            SvREFCNT(into)= SvREFCNT(&tmp);

            if (SWAP_DEBUG) { warn("after swap:"); sv_dump(into); sv_dump(referent); }

            /* and now throw away the head we got from the regexp engine. */
            SvREFCNT_dec(referent);
        }
#else
        {
            PMOP pm; /* grr */
            STRLEN pat_len;
            REGEXP *re;
            char *pat= SvPV(sv_pat, pat_len);

            Zero(&pm,1,PMOP);
            pm.op_pmdynflags= SvUTF8(sv_pat) ? PMdf_CMP_UTF8 : 0;
            pm.op_pmflags= flags;

            re= CALLREGCOMP(aTHX_ pat, pat + pat_len, &pm);
            SvREFCNT_dec(sv_pat);
            sv_magic( into, (SV*)re, PERL_MAGIC_qr, 0, 0);
            SvFLAGS( into ) |= SVs_SMG;
        }
#endif
    }
    else {
        ERROR("Expecting SRL_HDR_SHORT_BINARY for modifiers of regexp");
    }
}




static SRL_INLINE SV *
srl_read_extend(pTHX_ srl_decoder_t *dec, SV* into)
{
    /* FIXME unimplemented!!! */
    ERROR_UNIMPLEMENTED(dec,SRL_HDR_EXTEND,"EXTEND");
    return into;
}

/* these are all special */

static SRL_INLINE SV *
srl_read_alias(pTHX_ srl_decoder_t *dec)
{
    UV item= srl_read_varint_uv_offset(aTHX_ dec," while reading ALIAS tag");
    SV *referent= srl_fetch_item(aTHX_ dec, item, "ALIAS");
    SvREFCNT_inc(referent);
    return referent;
}

static SRL_INLINE void
srl_read_copy(pTHX_ srl_decoder_t *dec, SV* into)
{
    UV item= srl_read_varint_uv_offset(aTHX_ dec, " while reading COPY tag");
    if (expect_false( dec->save_pos )) {
        ERRORf1("COPY(%d) called during parse", item);
    }
    if (expect_false( (IV)item > dec->buf_end - dec->buf_start )) {
        ERRORf1("COPY(%d) points out of packet",item);
    }
    dec->save_pos= dec->pos;
    dec->pos= dec->buf_start + item;
    srl_read_single_value(aTHX_ dec, into);
    dec->pos= dec->save_pos;
    dec->save_pos= 0;
}

/****************************************************************************
 * MAIN DISPATCH SUB - ALL ROADS LEAD HERE                                  *
 ****************************************************************************/

static SRL_INLINE void
srl_read_single_value(pTHX_ srl_decoder_t *dec, SV* into)
{
    STRLEN len;
    U8 tag;

  read_again:
    if (expect_false( BUF_DONE(dec) ))
        ERROR("unexpected end of input stream while expecting a single value");

    tag= *dec->pos;
    if (expect_false(tag & SRL_HDR_TRACK_FLAG)) {
        tag= tag & ~SRL_HDR_TRACK_FLAG;
        srl_track_sv(aTHX_ dec, dec->pos, into);
    }
    dec->pos++;

    if ( tag <= SRL_HDR_POS_HIGH ) {
        sv_setiv(into, tag); /* it will fit in an iv and they are faster */
    }
    else
    if ( tag <= SRL_HDR_NEG_HIGH) {
        sv_setiv(into, (IV)(tag - 32));
    }
    else
    if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {
        len= (STRLEN)SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        ASSERT_BUF_SPACE(dec, len, " while reading ascii string");
        sv_setpvn(into,(char*)dec->pos,len);
        dec->pos += len;
    }
    else
    if ( IS_SRL_HDR_HASHREF(tag) ) {
        srl_read_hash(aTHX_ dec, into, tag);
    }
    else
    if ( IS_SRL_HDR_ARRAYREF(tag) ) {
        srl_read_array(aTHX_ dec, into, tag);
    }
    else {
        switch (tag) {
            case SRL_HDR_VARINT:        srl_read_varint(aTHX_ dec, into);           break;
            case SRL_HDR_ZIGZAG:        srl_read_zigzag(aTHX_ dec, into);           break;

            case SRL_HDR_FLOAT:         srl_read_float(aTHX_ dec, into);            break;
            case SRL_HDR_DOUBLE:        srl_read_double(aTHX_ dec, into);           break;
            case SRL_HDR_LONG_DOUBLE:   srl_read_long_double(aTHX_ dec, into);      break;

            case SRL_HDR_TRUE:          sv_setsv(into, &PL_sv_yes);                 break;
            case SRL_HDR_FALSE:         sv_setsv(into, &PL_sv_no);                  break;
            case SRL_HDR_UNDEF:         sv_setsv(into, &PL_sv_undef);               break;
            case SRL_HDR_BINARY:        srl_read_string(aTHX_ dec, 0, into);        break;
            case SRL_HDR_STR_UTF8:      srl_read_string(aTHX_ dec, 1, into);        break;

            case SRL_HDR_WEAKEN:        srl_read_weaken(aTHX_ dec, into);           break;
            case SRL_HDR_REFN:          srl_read_refn(aTHX_ dec, into);             break;
            case SRL_HDR_REFP:          srl_read_refp(aTHX_ dec, into);             break;
            case SRL_HDR_OBJECT:        srl_read_object(aTHX_ dec, into);           break;
            case SRL_HDR_OBJECTV:       srl_read_objectv(aTHX_ dec, into);          break;
            case SRL_HDR_COPY:          srl_read_copy(aTHX_ dec, into);             break;
            case SRL_HDR_EXTEND:        srl_read_extend(aTHX_ dec, into);           break;
            case SRL_HDR_HASH:          srl_read_hash(aTHX_ dec, into, 0);          break;
            case SRL_HDR_ARRAY:         srl_read_array(aTHX_ dec, into, 0);         break;
            case SRL_HDR_REGEXP:        srl_read_regexp(aTHX_ dec, into);           break;

            case SRL_HDR_PAD:           /* no op */
                while (BUF_NOT_DONE(dec) && *dec->pos == SRL_HDR_PAD)
                    dec->pos++;
                goto read_again;
            break;
            default:
                ERROR_UNEXPECTED(dec,tag, " single value");
            break;
        }
    }
}
