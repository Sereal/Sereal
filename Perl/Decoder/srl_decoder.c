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
#if (PERL_VERSION < 10)
#   define FIXUP_RITER 1
#endif
#define DEFAULT_MAX_RECUR_DEPTH 10000

#include "srl_decoder.h"

#include "srl_common.h"
#include "ptable.h"
#include "srl_protocol.h"

#include "snappy/csnappy_decompress.c"

/* 5.8.8 and earlier have a nasty bug in their handling of overloading:
 * The overload-flag is set on the referer of the blessed object instead of
 * the referent. That means that our late-bless logic breaks for
 * multiply-occurring objects.
 * So for 5.8.8 and earlier, the easiest workaround is to bless as we go
 * instead of blessing at the end of a decode run. Additionally, on repeatedly
 * encountered objects (REFP), we have to check the stash of the referent for
 * overloadedness and set the OVERLOAD flag (AMAGIC_on) on the NEW referer.
 *
 * Details on the perl bug in perl589delta.pod,
 * see "Reblessing overloaded objects now works".
 *
 * This is potentially a security problem (destructors!), but we really need
 * this to work on 5.8.5 for now, so let's make it work.
 * Another way of making it work might be to keep track of all occurrences
 * of objects and fix them up afterwards. That seems even more intrusive.
 * Please prove us wrong, though, since it's semantically a better fix.
 *
 * --Eric and Steffen
 */
#if ((PERL_VERSION == 8) && (PERL_SUBVERSION >= 9) || (PERL_VERSION > 8))
#  define USE_588_WORKAROUND 0
#else
#  define USE_588_WORKAROUND 1
#endif

/* predeclare all our subs so we have one definitive authority for their signatures */
SRL_STATIC_INLINE UV srl_read_varint_uv_safe(pTHX_ srl_decoder_t *dec);
SRL_STATIC_INLINE UV srl_read_varint_uv_nocheck(pTHX_ srl_decoder_t *dec);
SRL_STATIC_INLINE UV srl_read_varint_uv(pTHX_ srl_decoder_t *dec);
SRL_STATIC_INLINE UV srl_read_varint_uv_offset(pTHX_ srl_decoder_t *dec, const char * const errstr);
SRL_STATIC_INLINE UV srl_read_varint_uv_length(pTHX_ srl_decoder_t *dec, const char * const errstr);

SRL_STATIC_INLINE SV *srl_fetch_item(pTHX_ srl_decoder_t *dec, UV item, const char * const tag_name);

/* these three are "Public" */
srl_decoder_t *srl_build_decoder_struct(pTHX_ HV *opt);             /* constructor - called from ->new() */
void srl_destroy_decoder(pTHX_ srl_decoder_t *dec);                 /* destructor  - called from ->DESTROY() */
void srl_decoder_destructor_hook(pTHX_ void *p);                    /* destructor hook - called automagically */

/* the top level components of the decode process - called by srl_decode_into() */
/* srl_begin_decoding: set up the decoder to handle a given var */
SRL_STATIC_INLINE srl_decoder_t *srl_begin_decoding(pTHX_ srl_decoder_t *dec, SV *src, UV start_offset);
SRL_STATIC_INLINE void srl_read_header(pTHX_ srl_decoder_t *dec, SV *header_user_data); /* read/validate header */
SRL_STATIC_INLINE void srl_read_single_value(pTHX_ srl_decoder_t *dec, SV* into);   /* main recursive dump routine */
SRL_STATIC_INLINE void srl_finalize_structure(pTHX_ srl_decoder_t *dec);             /* optional finalize structure logic */
SRL_STATIC_INLINE void srl_clear_decoder(pTHX_ srl_decoder_t *dec);                 /* clean up decoder after a dump */
SRL_STATIC_INLINE void srl_clear_decoder_body_state(pTHX_ srl_decoder_t *dec);      /* clean up after each document body */

/* the internal routines to handle each kind of object we have to deserialize */
SRL_STATIC_INLINE SV *srl_read_alias(pTHX_ srl_decoder_t *dec);
SRL_STATIC_INLINE void srl_read_copy(pTHX_ srl_decoder_t *dec, SV* into);

SRL_STATIC_INLINE void srl_read_hash(pTHX_ srl_decoder_t *dec, SV* into, U8 tag);
SRL_STATIC_INLINE void srl_read_array(pTHX_ srl_decoder_t *dec, SV* into, U8 tag);
SRL_STATIC_INLINE void srl_read_regexp(pTHX_ srl_decoder_t *dec, SV* into);

SRL_STATIC_INLINE void srl_read_refp(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_refn(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_weaken(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_long_double(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_double(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_float(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8, SV* into);
SRL_STATIC_INLINE void srl_read_varint(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_zigzag(pTHX_ srl_decoder_t *dec, SV* into);
SRL_STATIC_INLINE void srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag, SV* into);
SRL_STATIC_INLINE void srl_read_object(pTHX_ srl_decoder_t *dec, SV* into, U8 obj_tag);
SRL_STATIC_INLINE void srl_read_objectv(pTHX_ srl_decoder_t *dec, SV* into, U8 obj_tag);

SRL_STATIC_INLINE void srl_track_sv(pTHX_ srl_decoder_t *dec, U8 *track_pos, SV *sv);
SRL_STATIC_INLINE void srl_read_frozen_object(pTHX_ srl_decoder_t *dec, HV *class_stash, SV *into);

/* FIXME unimplemented!!! */
SRL_STATIC_INLINE SV *srl_read_extend(pTHX_ srl_decoder_t *dec, SV* into);


#define ASSERT_BUF_SPACE(dec,len,msg) STMT_START {              \
    if (expect_false( (UV)BUF_SPACE((dec)) < (UV)(len) )) { \
        SRL_ERRORf3("Unexpected termination of packet%s, want %lu bytes, only have %lu available", (msg), (UV)(len), (UV)BUF_SPACE((dec)));  \
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
#   define SVt_RV_FAKE SVt_RV
#else
#   define SVt_RV_FAKE SVt_IV
#endif

#define SRL_ASSERT_REF_PTR_TABLES(dec) STMT_START {     \
            if (expect_false( !(dec)->ref_stashes )) {  \
                (dec)->ref_stashes = PTABLE_new();      \
                (dec)->ref_bless_av = PTABLE_new();     \
            }                                           \
        } STMT_END


STATIC void
srl_ptable_debug_callback(PTABLE_ENTRY_t *e)
{
    dTHX;
    printf("KEY=%lu\nVALUE:\n", (unsigned long)e->key);
    sv_dump((SV *)e->value);
    printf("\n");
}

STATIC void
srl_ptable_debug_dump(pTHX_ PTABLE_t *tbl)
{
    PTABLE_debug_dump(tbl, srl_ptable_debug_callback);
}


/* PUBLIC ROUTINES */

/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_decoder_t *
srl_build_decoder_struct(pTHX_ HV *opt)
{
    srl_decoder_t *dec;
    SV **svp;

    Newxz(dec, 1, srl_decoder_t);

    dec->ref_seenhash = PTABLE_new();
    dec->max_recursion_depth = DEFAULT_MAX_RECUR_DEPTH;
    dec->max_num_hash_entries = 0; /* 0 == any number */

    /* load options */
    if (opt != NULL) {
        if ( (svp = hv_fetchs(opt, "refuse_snappy", 0)) && SvTRUE(*svp))
            SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_REFUSE_SNAPPY);

        if ( (svp = hv_fetchs(opt, "refuse_objects", 0)) && SvTRUE(*svp))
            SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_REFUSE_OBJECTS);

        if ( (svp = hv_fetchs(opt, "no_bless_objects", 0)) && SvTRUE(*svp))
            SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_NO_BLESS_OBJECTS);

        if ( (svp = hv_fetchs(opt, "validate_utf8", 0)) && SvTRUE(*svp))
            SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_VALIDATE_UTF8);

        if ( (svp = hv_fetchs(opt, "max_recursion_depth", 0)) && SvTRUE(*svp))
            dec->max_recursion_depth = SvUV(*svp);

        if ( (svp = hv_fetchs(opt, "max_num_hash_entries", 0)) && SvTRUE(*svp))
            dec->max_num_hash_entries = SvUV(*svp);

        if ( (svp = hv_fetchs(opt, "incremental", 0)) && SvTRUE(*svp))
            SRL_DEC_SET_OPTION(dec,SRL_F_DECODER_DESTRUCTIVE_INCREMENTAL);
    }

    return dec;
}

/* Clone a decoder whilst resetting ephemeral state on the clone. */
SRL_STATIC_INLINE srl_decoder_t *
srl_build_decoder_struct_alike(pTHX_ srl_decoder_t *proto)
{
    srl_decoder_t *dec;

    Newxz(dec, 1, srl_decoder_t);

    dec->ref_seenhash = PTABLE_new();
    dec->max_recursion_depth = proto->max_recursion_depth;
    dec->max_num_hash_entries = proto->max_num_hash_entries;

    dec->flags = proto->flags;
    SRL_DEC_RESET_VOLATILE_FLAGS(dec);

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
    if (dec->ref_thawhash)
        PTABLE_free(dec->ref_thawhash);
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

    /* Only free decoder if not for reuse */
    if (!SRL_DEC_HAVE_OPTION(dec, SRL_F_REUSE_DECODER)) {
        srl_destroy_decoder(aTHX_ dec);
    }
    else {
        /* Clear instead - decoder reused */
        srl_clear_decoder(aTHX_ dec);
    }
}

/* Logic shared by the various decoder entry points. */
SRL_STATIC_INLINE void
srl_decode_into_internal(pTHX_ srl_decoder_t *origdec, SV *src, SV *header_into, SV *body_into, UV start_offset)
{
    srl_decoder_t *dec;

    assert(origdec != NULL);
    dec = srl_begin_decoding(aTHX_ origdec, src, start_offset);
    srl_read_header(aTHX_ dec, header_into);
    SRL_UPDATE_BODY_POS(dec);
    if (SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DECOMPRESS_SNAPPY)) {
        /* uncompress */
        uint32_t dest_len;
        SV *buf_sv;
        unsigned char *buf;
        unsigned char *old_pos;
        const ptrdiff_t sereal_header_len = dec->pos - dec->buf_start;
        const STRLEN compressed_packet_len =
                ( dec->proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL
                ? (STRLEN)srl_read_varint_uv_length(aTHX_ dec, " while reading compressed packet size")
                : (STRLEN)(dec->buf_end - dec->pos);
        int decompress_ok;
        int header_len;

        /* all decl's above here, or we break C89 compilers */

        dec->bytes_consumed= compressed_packet_len + (dec->pos - dec->buf_start);
        origdec->bytes_consumed = dec->bytes_consumed;

        header_len = csnappy_get_uncompressed_length(
                            (char *)dec->pos,
                            compressed_packet_len,
                            &dest_len
                         );
        if (header_len == CSNAPPY_E_HEADER_BAD)
            SRL_ERROR("Invalid Snappy header in Snappy-compressed Sereal packet");

        /* Let perl clean this up. Yes, it's not the most efficient thing
         * ever, but it's just one mortal per full decompression, so not
         * a bottle-neck. */
        buf_sv = sv_2mortal( newSV(sereal_header_len + dest_len + 1 ));
        buf = (unsigned char *)SvPVX(buf_sv);

        /* not necessary to copy the Sereal header! */
        /* Copy(dec->buf_start, buf, sereal_header_len, unsigned char); */

        old_pos = dec->pos;
        dec->buf_start = buf;
        dec->pos = buf + sereal_header_len;
        SRL_UPDATE_BODY_POS(dec);
        dec->buf_end = dec->pos + dest_len;
        dec->buf_len = dest_len + sereal_header_len;

        decompress_ok = csnappy_decompress_noheader((char *)(old_pos + header_len),
                                                    compressed_packet_len - header_len,
                                                    (char *)dec->pos,
                                                    &dest_len);
        if (expect_false( decompress_ok != 0 ))
        {
            SRL_ERRORf1("Snappy decompression of Sereal packet payload failed with error %i!", decompress_ok);
        }
    }

    /* The actual document body deserialization: */
    srl_read_single_value(aTHX_ dec, body_into);
    if (expect_false(SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE))) {
        srl_finalize_structure(aTHX_ dec);
    }

    /* If we aren't reading from a decompressed buffer we have to remember the number
     * of bytes used for the user to query. */
    if (dec->bytes_consumed == 0) {
        dec->bytes_consumed = dec->pos - dec->buf_start;
        origdec->bytes_consumed = dec->bytes_consumed;
    }

    if (SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DESTRUCTIVE_INCREMENTAL)) {
        STRLEN len;
        char *pv= SvPV(src,len);
        /* check the length here? do something different if the string is now exhausted? */
        sv_chop(src, pv + dec->bytes_consumed);
    }

    srl_clear_decoder(aTHX_ dec);
}

/* This is the main routine to deserialize just the header of a document. */
SV *
srl_decode_header_into(pTHX_ srl_decoder_t *origdec, SV *src, SV* header_into, UV start_offset)
{
    srl_decoder_t *dec;
    assert(origdec != NULL);
    dec = srl_begin_decoding(aTHX_ origdec, src, start_offset);
    if (header_into == NULL)
        header_into = sv_newmortal();
    srl_read_header(aTHX_ dec, header_into);
    return header_into;
}

/* This is the main routine to deserialize a Sereal document
 * w/o data in header. */
SV *
srl_decode_into(pTHX_ srl_decoder_t *dec, SV *src, SV* body_into, UV start_offset)
{
    if (expect_true(!body_into))
        body_into= sv_2mortal(newSV_type(SVt_NULL));
    srl_decode_into_internal(aTHX_ dec, src, NULL, body_into, start_offset);
    return body_into;
}

/* This is the main routine to deserialize Sereal document body
 * and header all at once. */
void
srl_decode_all_into(pTHX_ srl_decoder_t *dec, SV *src, SV *header_into, SV *body_into, UV start_offset)
{
    assert(header_into != NULL);
    assert(body_into != NULL);
    (void)srl_decode_into_internal(aTHX_ dec, src, header_into, body_into, start_offset);
}


/* TOP LEVEL PRIVATE ROUTINES */

SRL_STATIC_INLINE void
srl_clear_decoder(pTHX_ srl_decoder_t *dec)
{
    if (dec->buf_start == dec->buf_end)
        return;

    srl_clear_decoder_body_state(aTHX_ dec);
    SRL_DEC_RESET_VOLATILE_FLAGS(dec);
    dec->body_pos = dec->buf_start = dec->buf_end = dec->pos = dec->save_pos = NULL;
}

SRL_STATIC_INLINE void
srl_clear_decoder_body_state(pTHX_ srl_decoder_t *dec)
{
    SRL_DEC_UNSET_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE);

    if (dec->weakref_av)
        av_clear(dec->weakref_av);

    PTABLE_clear(dec->ref_seenhash);
    if (dec->ref_stashes) {
        PTABLE_clear(dec->ref_stashes);
        PTABLE_clear(dec->ref_bless_av);
    }

    dec->recursion_depth = 0;
}

SRL_STATIC_INLINE srl_decoder_t *
srl_begin_decoding(pTHX_ srl_decoder_t *dec, SV *src, UV start_offset)
{
    STRLEN len;
    unsigned char *tmp;

    /* Check whether decoder is in use and create a new one on the
     * fly if necessary. Should only happen in edge cases such as
     * a THAW hook calling back into the same decoder. */
    if (SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DIRTY)) {
        srl_decoder_t * const proto = dec;
        dec = srl_build_decoder_struct_alike(aTHX_ proto);
        SRL_DEC_UNSET_OPTION(dec, SRL_F_REUSE_DECODER);
    }

    /* Needs to be before setting DIRTY because DIRTY is volatile. */
    SRL_DEC_RESET_VOLATILE_FLAGS(dec);

    /* Set to being in use. */;
    SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_DIRTY);

    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR_X(&srl_decoder_destructor_hook, (void *)dec);

    if (SvUTF8(src)) {
        /* If we are being asked to decode a utf8-on string then we
         * make a mortal copy, and then try to downgrade the copy.
         * The downgrade will croak if it cannot successfully downgrade
         * the buffer. If it is sucessful then decode the downgraded
         * copy. */
        src= sv_mortalcopy(src);
        sv_utf8_downgrade(src, 0);
    }

    tmp = (unsigned char*)SvPV(src, len);
    if (expect_false( start_offset > len )) {
        SRL_ERROR("Start offset is beyond input string length");
    }
    dec->buf_start= dec->pos= tmp + start_offset;
    dec->buf_end= dec->buf_start + len - start_offset;
    dec->buf_len= len - start_offset;
    SRL_SET_BODY_POS(dec, dec->buf_start);
    dec->bytes_consumed = 0;

    return dec;
}

SRL_STATIC_INLINE void
srl_read_header(pTHX_ srl_decoder_t *dec, SV *header_user_data)
{
    UV header_len;

    /* 4 byte magic string + version/flags + hdr len at least */
    ASSERT_BUF_SPACE(dec, 4 + 1 + 1," while reading header");
    if (strnEQ((char*)dec->pos, SRL_MAGIC_STRING, 4)) {
        unsigned int proto_version;

        dec->pos += 4;
        dec->proto_version_and_flags = *dec->pos++;

        proto_version = dec->proto_version_and_flags & SRL_PROTOCOL_VERSION_MASK;
        if (expect_false( proto_version == 1 ))
            SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_PROTOCOL_V1); /* compat mode */
        else if (expect_false( proto_version != 2 ))
            SRL_ERRORf1("Unsupported Sereal protocol version %u",
                    dec->proto_version_and_flags & SRL_PROTOCOL_VERSION_MASK);

        if ((dec->proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_RAW) {
            /* no op */
        }
        else
        if (
                ( dec->proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) == SRL_PROTOCOL_ENCODING_SNAPPY
                ||
                ( dec->proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL
        ) {
            if (expect_false( SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_REFUSE_SNAPPY) )) {
                SRL_ERROR("Sereal document is compressed with Snappy, "
                      "but this decoder is configured to refuse Snappy-compressed input.");
            }
            dec->flags |= SRL_F_DECODER_DECOMPRESS_SNAPPY;
        }
        else
        {
            SRL_ERRORf1( "Sereal document encoded in an unknown format '%d'",
                     (dec->proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK)
                      >> SRL_PROTOCOL_VERSION_BITS);
        }

        /* Must do this via a temporary as it modifes dec->pos itself */
        header_len= srl_read_varint_uv_length(aTHX_ dec, " while reading header");

        if (proto_version > 1 && header_len) {
            /* We have a protocol V2 extensible header:
             *  - 8bit bitfield
             *  - if lowest bit set, we have custom-header-user-data after the bitfield
             *  => Only read header user data if an SV* was passed in to fill. */
            const U8 bitfield = *(dec->pos++);
            if (bitfield & SRL_PROTOCOL_HDR_USER_DATA && header_user_data != NULL) {
                /* Do an actual document body deserialization for the user data: */
                SRL_UPDATE_BODY_POS(dec);
                srl_read_single_value(aTHX_ dec, header_user_data);
                if (expect_false(SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE))) {
                    srl_finalize_structure(aTHX_ dec);
                }
                srl_clear_decoder_body_state(aTHX_ dec); /* clean up for the main body decode */
            }
            else {
                /* Either off in bitfield or no user data wanted, skip to end of header */
                dec->pos += header_len - 1; /* header_len includes bitfield */
            }
        }
        else {
            /* Skip header since we don't have any defined header-content in this
             * protocol version. */
            dec->pos += header_len;
        }
    } else {
        SRL_ERROR("Bad Sereal header: Does not start with Sereal magic");
    }
}

SRL_STATIC_INLINE void
srl_finalize_structure(pTHX_ srl_decoder_t *dec)
{
    int nobless = SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_NO_BLESS_OBJECTS);
    if (dec->weakref_av)
        av_clear(dec->weakref_av);
    if (dec->ref_stashes) {
        /* The iterator could be leaked on exceptions if not for PTABLE_FLAG_AUTOCLEAN. */
        PTABLE_ITER_t *it = PTABLE_iter_new_flags(dec->ref_stashes, PTABLE_FLAG_AUTOCLEAN);
        PTABLE_ENTRY_t *ent;

        /* We have gotten here without error, so bless all the objects.
         * We defer to the end like this so that we only bless data structures
         * if the entire deserialization completes. */
        while ( NULL != (ent = PTABLE_iter_next(it)) ) {
            HV *stash = (HV* )ent->value;
            AV *ref_bless_av  = PTABLE_fetch(dec->ref_bless_av, ent->key);
            I32 len;
            if (expect_false( !stash || !ref_bless_av )) {
                PTABLE_iter_free(it);
                SRL_ERROR("missing stash or ref_bless_av!");
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
#if USE_588_WORKAROUND
                        /* was blessed early, don't rebless */
#else
                        if (!nobless)
                            sv_bless(obj, stash);
#endif
                    } else {
                        PTABLE_iter_free(it);
                        SRL_ERROR("object missing from ref_bless_av array?");
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

SRL_STATIC_INLINE UV
srl_read_varint_uv_safe(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (BUF_NOT_DONE(dec) && *dec->pos & 0x80) {
        uv |= ((UV)(*dec->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            SRL_ERROR("varint too big");
    }
    if (expect_true( BUF_NOT_DONE(dec) )) {
        uv |= ((UV)*dec->pos++ << lshift);
    } else {
        SRL_ERROR("varint terminated prematurely");
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

SRL_STATIC_INLINE UV
srl_read_varint_uv_nocheck(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*dec->pos & 0x80) {
        uv |= ((UV)(*dec->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (expect_false( lshift > (sizeof(UV) * 8) ))
            SRL_ERROR("varint too big");
    }
    uv |= ((UV)(*dec->pos++) << lshift);
    return uv;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv(pTHX_ srl_decoder_t *dec)
{
    if (expect_true( dec->buf_end - dec->pos > 10 ))
        return srl_read_varint_uv_nocheck(aTHX_ dec);
    else
        return srl_read_varint_uv_safe(aTHX_ dec);
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_offset(pTHX_ srl_decoder_t *dec, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ dec);

    if (dec->body_pos + len >= dec->pos) {
        SRL_ERRORf4("Corrupted packet%s. Offset %lu points past current position %lu in packet with length of %lu bytes long",
                errstr, (unsigned long)len, (unsigned long)BUF_POS_OFS(dec), (unsigned long)dec->buf_len);
    }
    return len;
}

SRL_STATIC_INLINE UV
srl_read_varint_uv_length(pTHX_ srl_decoder_t *dec, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ dec);

    ASSERT_BUF_SPACE(dec, len, errstr);

    return len;
}


SRL_STATIC_INLINE UV
srl_read_varint_uv_count(pTHX_ srl_decoder_t *dec, const char * const errstr)
{
    UV len= srl_read_varint_uv(aTHX_ dec);
    if (len > I32_MAX) {
        SRL_ERRORf3("Corrupted packet%s. Count %lu exceeds I32_MAX (%i), which is impossible.",
                errstr, len, I32_MAX);
    }
    return len;
}

SRL_STATIC_INLINE void
srl_track_thawed(srl_decoder_t *dec, U8 *track_pos, SV *sv)
{
    if (!dec->ref_thawhash)
        dec->ref_thawhash = PTABLE_new();
    PTABLE_store(dec->ref_thawhash, (void *)(track_pos - dec->body_pos), (void *)sv);
}


SRL_STATIC_INLINE SV *
srl_fetch_thawed(srl_decoder_t *dec, UV item)
{
    if (dec->ref_thawhash) {
        SV *sv= (SV *)PTABLE_fetch(dec->ref_thawhash, (void *)item);
        return sv;
    } else {
        return NULL;
    }
}

SRL_STATIC_INLINE void
srl_track_sv(pTHX_ srl_decoder_t *dec, U8 *track_pos, SV *sv)
{
    PTABLE_store(dec->ref_seenhash, (void *)(track_pos - dec->body_pos), (void *)sv);
}


SRL_STATIC_INLINE SV *
srl_fetch_item(pTHX_ srl_decoder_t *dec, UV item, const char * const tag_name)
{
    SV *sv= (SV *)PTABLE_fetch(dec->ref_seenhash, (void *)item);
    if (expect_false( !sv )) {
        /*srl_ptable_debug_dump(aTHX_ dec->ref_seenhash);*/
        SRL_ERRORf2("%s(%d) references an unknown item", tag_name, (int)item);
    }
    return sv;
}

/****************************************************************************
 * PRIVATE WORKER SUBS FOR DEPARSING                                        *
 ****************************************************************************/

SRL_STATIC_INLINE void
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


SRL_STATIC_INLINE void
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


SRL_STATIC_INLINE void
srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8, SV* into)
{
    UV len= srl_read_varint_uv_length(aTHX_ dec, " while reading string");
    if (is_utf8 && SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_VALIDATE_UTF8)) {
        /* checks for invalid byte sequences. */
        if (expect_false( !is_utf8_string((U8*)dec->pos, len) )) {
            SRL_ERROR("Invalid UTF8 byte sequence");
        }
    }
    sv_setpvn(into,(char *)dec->pos,len);
    if (is_utf8) {
        SvUTF8_on(into);
    } else {
        SvUTF8_off(into);
    }
    dec->pos+= len;
}


SRL_STATIC_INLINE void
srl_read_float(pTHX_ srl_decoder_t *dec, SV* into)
{
    ASSERT_BUF_SPACE(dec, sizeof(float), " while reading FLOAT");
    sv_setnv(into, (NV)*((float *)dec->pos));
    dec->pos+= sizeof(float);
}


SRL_STATIC_INLINE void
srl_read_double(pTHX_ srl_decoder_t *dec, SV* into)
{
    ASSERT_BUF_SPACE(dec, sizeof(double)," while reading DOUBLE");
    sv_setnv(into, (NV)*((double *)dec->pos));
    dec->pos+= sizeof(double);
}


SRL_STATIC_INLINE void
srl_read_long_double(pTHX_ srl_decoder_t *dec, SV* into)
{
    ASSERT_BUF_SPACE(dec, sizeof(long double)," while reading LONG_DOUBLE");
    sv_setnv(into, (NV)*((long double *)dec->pos));
    dec->pos+= sizeof(long double);
}


SRL_STATIC_INLINE void
srl_read_array(pTHX_ srl_decoder_t *dec, SV *into, U8 tag) {
    UV len;
    if (tag) {
        SV *referent= (SV *)newAV();
        len= tag & 15;
        (void)SvUPGRADE(into, SVt_RV_FAKE);
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

        ASSERT_BUF_SPACE(dec,len,"while reading array contents, insuffienct remaining tags for specified array size");

        /* we cheat and store undef in the array - we will overwrite it later */
        av_store((AV*)into, len-1, &PL_sv_undef);
        av_array= AvARRAY((AV*)into);
        av_end= av_array + len;

        for ( ; av_array != av_end ; av_array++) {
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

SRL_STATIC_INLINE void
srl_read_hash(pTHX_ srl_decoder_t *dec, SV* into, U8 tag) {
    UV num_keys;
    if (tag) {
        SV *referent= (SV *)newHV();
        num_keys= tag & 15;
        (void)SvUPGRADE(into,SVt_RV_FAKE);
        SvTEMP_off(referent);
        SvRV_set(into, referent);
        SvROK_on(into);
        into= referent;
    } else {
        num_keys= srl_read_varint_uv_count(aTHX_ dec," while reading HASH");
        (void)SvUPGRADE(into, SVt_PVHV);
    }
#ifdef FIXUP_RITER
    HvRITER_set(into,-1);
#endif

    /* Limit the maximum number of hash keys that we accept to whetever was configured */
    if (expect_false( dec->max_num_hash_entries != 0 && num_keys > dec->max_num_hash_entries )) {
        SRL_ERRORf2("Got input hash with %u entries, but the configured maximum is just %u",
                (int)num_keys, (int)dec->max_num_hash_entries);
    }

    ASSERT_BUF_SPACE(dec,num_keys*2,"while reading hash contents, insuffienct remaining tags for number of keys specified");

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
        tag= (*dec->pos++)&127;
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
            from= dec->body_pos + ofs;
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
                SRL_ERROR_BAD_COPY(dec, SRL_HDR_HASH);
            }
        } else {
            SRL_ERROR_UNEXPECTED(dec,tag,"a stringish type");
        }
#ifdef OLDHASH
        fetched_sv= hv_fetch((HV *)into, (char *)from, key_len, IS_LVALUE);
#else
        fetched_sv= hv_common((HV *)into, NULL, (char *)from, key_len, flags, HV_FETCH_LVALUE|HV_FETCH_JUST_SV, NULL, 0);
#endif
        if (expect_false( !fetched_sv )) {
            SRL_ERROR_PANIC(dec,"failed to hv_store");
        }
        if (expect_false( *dec->pos == SRL_HDR_ALIAS )) {
            dec->pos++;
            *fetched_sv= srl_read_alias(aTHX_ dec);
        } else {
            srl_read_single_value(aTHX_ dec, *fetched_sv);
        }
    }
}


SRL_STATIC_INLINE void
srl_read_refn(pTHX_ srl_decoder_t *dec, SV* into)
{
    SV *referent;
    ASSERT_BUF_SPACE(dec, 1, " while reading REFN referent");
    U8 tag= *(dec->pos); /* Look ahead for special vars. */
    if (tag == SRL_HDR_TRUE) {
        dec->pos++;
        referent= &PL_sv_yes;
    }
    else if (tag == SRL_HDR_FALSE) {
        dec->pos++;
        referent= &PL_sv_no;
    }
    /*
     * We cant do the below, as we have use SRL_HDR_UNDEF also
     * to represent "any SV which is undef". We need a different
     * tag for true perl undef.
     *
     */
    /*
    else if (tag == SRL_HDR_UNDEF) {
        dec->pos++;
        referent= &PL_sv_undef;
    }
    */
    else {
        referent= newSV(SVt_NULL);
        SvTEMP_off(referent);
        tag = 0;
    }
    (void)SvUPGRADE(into, SVt_RV_FAKE);
    SvRV_set(into, referent);
    SvROK_on(into);
    if (!tag)
        srl_read_single_value(aTHX_ dec, referent);
}

SRL_STATIC_INLINE void
srl_read_refp(pTHX_ srl_decoder_t *dec, SV* into)
{
    /* something we did before */
    UV item= srl_read_varint_uv_offset(aTHX_ dec, " while reading REFP tag");
    SV *thawed= srl_fetch_thawed(dec, item);
    SV *referent;
    if (thawed) {
        sv_setsv(into, thawed);
        return;
    }
    referent= srl_fetch_item(aTHX_ dec, item, "REFP");
    (void)SvREFCNT_inc(referent);

    (void)SvUPGRADE(into, SVt_RV_FAKE);
    SvTEMP_off(referent);
    SvRV_set(into, referent);
    SvROK_on(into);

#if USE_588_WORKAROUND
    /* See 'define USE_588_WORKAROUND' above for a discussion of what this does. */
    if (SvOBJECT(referent)) {
        HV *stash = SvSTASH(referent);
        if (Gv_AMG(stash))
            SvAMAGIC_on(into);
    }
#endif
}


SRL_STATIC_INLINE void
srl_read_weaken(pTHX_ srl_decoder_t *dec, SV* into)
{
    SV* referent;
    /* TODO This really just wants a subset of the states that srl_read_single_value covers, right?
     *      Optimization opportunity? Or robustness against invalid packets issue? */
    srl_read_single_value(aTHX_ dec, into);
    if (expect_false( !SvROK(into) ))
        SRL_ERROR("WEAKEN op");
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
        av_push(dec->weakref_av, SvREFCNT_inc(referent));
        SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE);
    }
    sv_rvweaken(into);
}

SRL_STATIC_INLINE void
srl_read_objectv(pTHX_ srl_decoder_t *dec, SV* into, U8 obj_tag)
{
    AV *av= NULL;
    STRLEN ofs;

    if (expect_false( SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_REFUSE_OBJECTS) ))
        SRL_ERROR_REFUSE_OBJECT();

    ofs= srl_read_varint_uv_offset(aTHX_ dec," while reading OBJECTV(_FREEZE) classname");

    if (expect_false( !dec->ref_bless_av ))
        SRL_ERROR("Corrupted packet. OBJECTV(_FREEZE) used without "
                  "preceding OBJECT(_FREEZE) to define classname");
    av= (AV *)PTABLE_fetch(dec->ref_bless_av, (void *)ofs);
    if (expect_false( NULL == av )) {
        SRL_ERRORf1("Corrupted packet. OBJECTV(_FREEZE) references unknown classname offset: %lu", (unsigned long)ofs);
    }

    /* checking tag: SRL_HDR_OBJECTV_FREEZE or SRL_HDR_OBJECTV? */
    if (expect_false( obj_tag == SRL_HDR_OBJECTV_FREEZE )) {
        HV *class_stash= PTABLE_fetch(dec->ref_stashes, (void *)ofs);
        if (expect_false( class_stash == NULL ))
            SRL_ERROR("Corrupted packet. OBJECTV(_FREEZE) used without "
                      "preceding OBJECT(_FREEZE) to define classname");
        srl_read_frozen_object(aTHX_ dec, class_stash, into);
    }  else {
        /* SRL_HDR_OBJECTV, not SRL_HDR_OBJECTV_FREEZE */
        /* now deparse the thing we are going to bless */
        srl_read_single_value(aTHX_ dec, into);

        /* and also stuff it into the av - we dont have to do any more book-keeping */
        av_push(av, SvREFCNT_inc(into));

#if USE_588_WORKAROUND
        {
            /* See 'define USE_588_WORKAROUND' above for a discussion of what this does. */
            HV *class_stash= PTABLE_fetch(dec->ref_stashes, (void *)ofs);
            if (expect_false( class_stash == NULL ))
                SRL_ERROR("Corrupted packet. OBJECTV(_FREEZE) used without "
                          "preceding OBJECT(_FREEZE) to define classname");
            if (!SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_NO_BLESS_OBJECTS))
                sv_bless(into, class_stash);
        }
#endif
    }
}

SRL_STATIC_INLINE void
srl_read_object(pTHX_ srl_decoder_t *dec, SV* into, U8 obj_tag)
{
    HV *class_stash= NULL;
    AV *av= NULL;
    STRLEN storepos= 0;
    UV ofs= 0;
    I32 flags= GV_ADD;
    U8 tag;
    U32 key_len = 0;
    U8 *from = NULL;

    if (expect_false( SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_REFUSE_OBJECTS) ))
        SRL_ERROR_REFUSE_OBJECT();

    /* Now find the class name - first check if this is a copy op
     * this is bit tricky, as we could have a copy of a raw string.
     * We could also have a copy of a previously mentioned class
     * name. We have to handle both, which leads to some non-linear
     * code flow in the below code. */
    ASSERT_BUF_SPACE(dec,1," while reading classname tag");

    /* Now read the class name and cache it */
    storepos= BODY_POS_OFS(dec);
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
        /* if this string was seen before as part of a classname then we expect
         * a stash available below. However it might have been serialized as a key
         * or something like that, which would mean we dont have an entry in ref_stashes
         * anymore. So first we check if we have a stash. If we do, then we can avoid
         * some work. */
        if (expect_true( dec->ref_stashes != NULL )) {
            class_stash= PTABLE_fetch(dec->ref_stashes, (void *)ofs);
        }
        /* Check if we actually got a class_stash back. If we didn't then we need
         * to deserialize the class name */
        if (!class_stash) {
            from= dec->body_pos + ofs;
            tag= *from++;
            /* Note we do NOT validate these items, as we have already read them
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
                SRL_ERROR_BAD_COPY(dec, SRL_HDR_OBJECT);
            }
        }
    } else {
        SRL_ERROR_UNEXPECTED(dec,tag, "a class name");
    }

    /* At this point we may or may not have a class stash. If they used a Copy there
     * is a decent chance we do. */
    SRL_ASSERT_REF_PTR_TABLES(dec);
    if (!class_stash) {
        /* no class stash - so we need to look it up and then store it away for future use */
        class_stash= gv_stashpvn((char *)from, key_len, flags);
        PTABLE_store(dec->ref_stashes, (void *)storepos, (void *)class_stash);
        /* Since this is the first time we have seen this stash then it is the first time
         * that we have stored an item in the ref_bless_av hash as well. So create a new one
         * and store it away. */
        av= newAV();
        sv_2mortal((SV*)av);
        PTABLE_store(dec->ref_bless_av, (void *)storepos, (void *)av);
    } else {
        /* we have a class stash so we should have a ref_bless_av as well. */
        av= (AV *)PTABLE_fetch(dec->ref_bless_av, (void *)storepos);
        if ( !av )
            SRL_ERRORf1("Panic, no ref_bless_av for %lu", (unsigned long)storepos);
    }

    if (expect_false( obj_tag == SRL_HDR_OBJECT_FREEZE )) {
        srl_read_frozen_object(aTHX_ dec, class_stash, into);
    }  else {
        /* We now have a stash so we /could/ bless... except that
         * we don't actually want to do so right now. We want to defer blessing
         * until the full packet has been read. Yes it is more overhead, but
         * we really dont want to trigger DESTROY methods from a partial
         * deparse. So we insert the item into an array to be blessed later. */
        SRL_DEC_SET_OPTION(dec, SRL_F_DECODER_NEEDS_FINALIZE);
        av_push(av, SvREFCNT_inc(into));

        /* now deparse the thing we are going to bless */
        srl_read_single_value(aTHX_ dec, into);

#if USE_588_WORKAROUND
        /* See 'define USE_588_WORKAROUND' above for a discussion of what this does. */
        if (!SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_NO_BLESS_OBJECTS))
            sv_bless(into, class_stash);
#endif

    }
}

/* Invoke a THAW callback on the given class. Pass in the next item in the
 * decoder stream. This is implementing the FREEZE/THAW part of
 * SRL_HDR_OBJECT_FREEZE and SRL_HDR_OBJECTV_FREEZE. */

SRL_STATIC_INLINE void
srl_read_frozen_object(pTHX_ srl_decoder_t *dec, HV *class_stash, SV *into)
{
    GV *method = gv_fetchmethod_autoload(class_stash, "THAW", 0);
    char *classname = HvNAME(class_stash);
    SV* referent;
    SV *replacement;

    /* At this point in the input stream we should have REFN WHATEVER. The WHATEVER
     * may be referenced from multiple RV's in the data structure, which means that
     * srl_read_single_value() will cache the *unthawed* representation when we finally
     * process it. So we need to do some special bookkeeping here and then overwrite
     * that representation in the refs hash.
     */

    unsigned char *fixup_pos= dec->pos + 1; /* get the tag for the WHATEVER */

    if (expect_false( method == NULL ))
        SRL_ERRORf1("No THAW method defined for class '%s'", HvNAME(class_stash));

    srl_read_single_value(aTHX_ dec, into);

    /* Assert that we got a top level array ref as the spec requires.
     * Not throwing an exception here violates expectations down the line and
     * can lead to segfaults. */
    if (expect_false( !SvROK(into) || SvTYPE(SvRV(into)) != SVt_PVAV ))
        SRL_ERROR("Corrupted packet. OBJECT(V)_FREEZE used without "
                  "being followed by an array reference");

    {
        int count;
        AV *arg_av= (AV*)SvRV(into);
        int arg_av_len = av_len(arg_av)+1;
        dSP;

        ENTER;
        SAVETMPS;
        PUSHMARK(SP);

        EXTEND(SP, 3);
        /* TODO Consider more caching for some of this */
        PUSHs(sv_2mortal(newSVpvn(classname, strlen(classname))));
        /* FIXME do not recreate the following SV. That's dumb and wasteful! - so long as it doesnt get modified! */
        PUSHs(sv_2mortal(newSVpvs("Sereal")));
        /* Push the args into the stack */
        for (count=0 ; count < arg_av_len; count++) {
            PUSHs((SV*)*av_fetch(arg_av, count, 0));
        }

        PUTBACK;
        count = call_sv((SV *)GvCV(method), G_SCALAR);
        /* TODO explore method lookup caching */
        SPAGAIN;

        if (expect_true( count == 1 )) {
            replacement = POPs;
            SvREFCNT_inc(replacement);
        } else {
            replacement = &PL_sv_undef;
        }
        /* If count is not 1, then it's 0. Then into is already undef. */

        PUTBACK;
        FREETMPS;
        LEAVE;
    }

    /* At this point "into" is an SvRV pointing at the *unthawed* representation.
     * This means we need to a) remove the old unthawed item and dispose of it
     * and b) make "into" point at the replacement, and c) if necessary store the
     * replacement in the sv tracking hash so that future references to this item
     * point at the *thawed* version. */
    if (SvROK(replacement)) {
        SV *tmpsv= replacement;
        replacement= SvRV(tmpsv);
        SvREFCNT_inc(replacement);
        SvREFCNT_dec(tmpsv);
        referent= SvRV(into);
        SvRV_set(into, replacement);
        SvREFCNT_dec(referent);
        if (*fixup_pos & SRL_HDR_TRACK_FLAG)
            srl_track_sv(aTHX_ dec, fixup_pos, replacement);
    } else if (*fixup_pos & SRL_HDR_TRACK_FLAG) {
        srl_track_thawed(dec, fixup_pos, replacement);
        sv_setsv(into, replacement);
    }
}


SRL_STATIC_INLINE void
srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag, SV* into)
{
    const UV len = srl_read_varint_uv_length(aTHX_ dec, " while reading reserved");
    (void)tag; /* unused as of now */

    dec->pos += len; /* discard */
    sv_setsv(into, &PL_sv_undef);
}

#if ((PERL_VERSION > 10) || (PERL_VERSION == 10 && PERL_SUBVERSION > 1 ))
#   define MODERN_REGEXP
#   define REGEXP_HAS_P_MODIFIER
#   pragma message ( "MODERN_REGEXP" )
#elif PERL_VERSION == 10
#   define TRANSITION_REGEXP
#   define REGEXP_HAS_P_MODIFIER
#   pragma message ( "TRANSITION_REGEXP" )
#else
#   define OLD_REGEXP
#   pragma message ( "OLD_REGEXP" )
#endif

SRL_STATIC_INLINE void
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
#ifdef REGEXP_HAS_P_MODIFIER
                case 'p':
                    flags = flags | PMf_KEEPCOPY;
                    break;
#endif
                default:
                    SRL_ERROR("bad modifier");
                    break;
            }
        }
#ifdef MODERN_REGEXP
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

            SvREFCNT_dec(sv_pat); /* I think we need this or we leak */
            /* and now throw away the head we got from the regexp engine. */
            SvREFCNT_dec(referent);
        }
#elif defined( TRANSITION_REGEXP )
        {
            REGEXP *referent = CALLREGCOMP(aTHX_ sv_pat, flags);
            SvREFCNT_dec(sv_pat);
            sv_magic( into, (SV*)referent, PERL_MAGIC_qr, 0, 0);
            SvFLAGS( into ) |= SVs_SMG;
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
        SRL_ERROR("Expecting SRL_HDR_SHORT_BINARY for modifiers of regexp");
    }
}




SRL_STATIC_INLINE SV *
srl_read_extend(pTHX_ srl_decoder_t *dec, SV* into)
{
    /* FIXME unimplemented!!! */
    SRL_ERROR_UNIMPLEMENTED(dec,SRL_HDR_EXTEND,"EXTEND");
    return into;
}

/* these are all special */

SRL_STATIC_INLINE SV *
srl_read_alias(pTHX_ srl_decoder_t *dec)
{
    UV item= srl_read_varint_uv_offset(aTHX_ dec," while reading ALIAS tag");
    SV *referent= srl_fetch_item(aTHX_ dec, item, "ALIAS");
    return SvREFCNT_inc(referent);
}

SRL_STATIC_INLINE void
srl_read_copy(pTHX_ srl_decoder_t *dec, SV* into)
{
    UV item= srl_read_varint_uv_offset(aTHX_ dec, " while reading COPY tag");
    if (expect_false( dec->save_pos )) {
        SRL_ERRORf1("COPY(%d) called during parse", (int)item);
    }
    if (expect_false( (IV)item > dec->buf_end - dec->buf_start )) {
        SRL_ERRORf1("COPY(%d) points out of packet", (int)item);
    }
    dec->save_pos= dec->pos;
    dec->pos= dec->body_pos + item;
    srl_read_single_value(aTHX_ dec, into);
    dec->pos= dec->save_pos;
    dec->save_pos= 0;
}

/****************************************************************************
 * MAIN DISPATCH SUB - ALL ROADS LEAD HERE                                  *
 ****************************************************************************/

SRL_STATIC_INLINE void
srl_read_single_value(pTHX_ srl_decoder_t *dec, SV* into)
{
    STRLEN len;
    U8 tag;
    if (++dec->recursion_depth > dec->max_recursion_depth) {
        SRL_ERRORf1("Reached recursion limit (%lu) during deserialization",
                (unsigned long)dec->max_recursion_depth);
    }

  read_again:
    if (expect_false( BUF_DONE(dec) ))
        SRL_ERROR("unexpected end of input stream while expecting a single value");

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
            case SRL_HDR_OBJECT_FREEZE:
            case SRL_HDR_OBJECT:        srl_read_object(aTHX_ dec, into, tag);      break;
            case SRL_HDR_OBJECTV_FREEZE:
            case SRL_HDR_OBJECTV:       srl_read_objectv(aTHX_ dec, into, tag);     break;
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
                SRL_ERROR_UNEXPECTED(dec,tag, " single value");
            break;
        }
    }

    dec->recursion_depth--;
}
