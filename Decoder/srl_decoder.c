#define PERL_NO_GET_CONTEXT

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
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

/* declare some of the in-file functions to avoid ordering issues */
static SRL_INLINE UV srl_read_varint_uv(pTHX_ srl_decoder_t *dec);

SV *srl_read_single_value(pTHX_ srl_decoder_t *dec, U8 *track_pos);
static SRL_INLINE SV *srl_read_hash(pTHX_ srl_decoder_t *dec, U8 *track_pos);
static SRL_INLINE SV *srl_read_array(pTHX_ srl_decoder_t *dec, U8 *track_pos);
static SRL_INLINE SV *srl_read_ref(pTHX_ srl_decoder_t *dec, U8 *track_pos);
static SRL_INLINE SV *srl_read_weaken(pTHX_ srl_decoder_t *dec, U8 *track_pos);

static SRL_INLINE SV *srl_read_long_double(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_double(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_float(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8);
static SRL_INLINE SV *srl_read_varint(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_zigzag(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_copy(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_reuse(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_alias(pTHX_ srl_decoder_t *dec);

/* FIXME unimplemented!!! */
static SRL_INLINE SV *srl_read_bless(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_blessv(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag);
static SRL_INLINE SV *srl_read_regexp(pTHX_ srl_decoder_t *dec);
static SRL_INLINE SV *srl_read_extend(pTHX_ srl_decoder_t *dec);

#define ASSERT_BUF_SPACE(dec,len) STMT_START {              \
    if (expect_false( (UV)BUF_SPACE((dec)) < (UV)(len) )) { \
        MYCROAK("Unexpected termination of packet, want %lu bytes, only have %lu available", (UV)(len), (UV)BUF_SPACE((dec)));  \
    }                                                       \
} STMT_END


/* Explicit destructor */
void
srl_destroy_decoder(pTHX_ srl_decoder_t *dec)
{
    PTABLE_free(dec->ref_seenhash);
    if (dec->ref_stashes) {
        PTABLE_free(dec->ref_stashes);
        PTABLE_free(dec->ref_bless_av);
    }
    if (dec->weakref_av)
        SvREFCNT_dec(dec->weakref_av);
    Safefree(dec);
}


void
srl_clear_decoder(pTHX_ srl_decoder_t *dec)
{
    dec->depth = 0;
    dec->buf_start = dec->buf_end = dec->pos = dec->save_pos = NULL;
    if (dec->weakref_av)
        av_clear(dec->weakref_av);

    PTABLE_clear(dec->ref_seenhash);
    if (dec->ref_stashes) {
        PTABLE_clear(dec->ref_stashes);
        PTABLE_clear(dec->ref_bless_av);
    }
}


/* This is fired when we exit the Perl pseudo-block.
 * It frees our decoder and all. Put decoder-level cleanup
 * logic here so that we can simply use croak/longjmp for
 * exception handling. Makes life vastly easier!
 */
void srl_decoder_destructor_hook(pTHX_ void *p)
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


void
srl_begin_decoding(pTHX_ srl_decoder_t *dec, SV *src)
{
    STRLEN len;
    dec->buf_start= dec->pos= (unsigned char*)SvPV(src, len);
    dec->buf_end= dec->buf_start + len;
}


/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_decoder_t *
srl_build_decoder_struct(pTHX_ HV *opt)
{
    srl_decoder_t *dec;
    /* SV **svp; */

    Newxz(dec, 1, srl_decoder_t);

    dec->buf_start = NULL;
    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR_X(&srl_decoder_destructor_hook, (void *)dec);

    dec->ref_seenhash = PTABLE_new();
    /* load options */
    if (opt != NULL) {
        /* if ( (svp = hv_fetchs(opt, "undef_blessed", 0)) && SvTRUE(*svp))
          dec->flags |= F_UNDEF_BLESSED;
        */
    }

    return dec;
}


int
srl_read_header(pTHX_ srl_decoder_t *dec)
{
    UV len;
    /* works for now: 3 byte magic string + proto version + 1 byte varint that indicates zero-length header */
    ASSERT_BUF_SPACE(dec, sizeof(SRL_MAGIC_STRING "\x01") ); /* sizeof returns the size for the 0, so we dont need to add 1 for the varint */
    if (expect_true( strEQ((char*)dec->pos, SRL_MAGIC_STRING "\x01") )) {
        dec->pos += sizeof(SRL_MAGIC_STRING "\x01") - 1;
        len= srl_read_varint_uv(aTHX_ dec); /* must do this via a temporary as it modifes dec->pos itself */
        dec->pos += len;
    } else {
        ERROR("bad header");
    }
    return 0;
}

int srl_finalize_structure(pTHX_ srl_decoder_t *dec)
{
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
                SV* obj= av_pop(ref_bless_av);
                if (expect_true( obj )) {
                    sv_bless(obj, stash);
                } else {
                    ERROR("object missing from ref_bless_av array?");
                }
            }
        }
        PTABLE_iter_free(it);
    }
    return 0;
}

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

static SRL_INLINE void
srl_track_sv(pTHX_ srl_decoder_t *dec, U8 *track_pos, SV *sv) {
    PTABLE_store(dec->ref_seenhash, (void *)(track_pos - dec->buf_start), (void *)sv);
}

static SV *srl_fetch_item(pTHX_ srl_decoder_t *dec, UV item, const char const *tag_name) {
    SV *sv= (SV *)PTABLE_fetch(dec->ref_seenhash, (void *)item);
    if (expect_false( !sv ))
        ERRORf2("%s(%d) references an unknown item", tag_name, item);
    return sv;
}

/****************************************************************************
 * implementation of various opcodes                                        *
 ****************************************************************************/


static SRL_INLINE SV *
srl_read_varint(pTHX_ srl_decoder_t *dec)
{
    return newSVuv(srl_read_varint_uv(aTHX_ dec));
}

static SRL_INLINE SV *
srl_read_zigzag(pTHX_ srl_decoder_t *dec)
{
    const UV uv= srl_read_varint_uv(aTHX_ dec);
    return uv & 1 ? newSViv((IV)-( 1 + (uv >> 1) )) : newSVuv(uv >> 1);
}

static SRL_INLINE SV *
srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8)
{
    UV len= srl_read_varint_uv(aTHX_ dec);
    SV *ret;
    ASSERT_BUF_SPACE(dec, len);
    ret= newSVpvn_utf8((char *)dec->pos,len,is_utf8);
    dec->pos+= len;
    return ret;
}

static SRL_INLINE SV *
srl_read_float(pTHX_ srl_decoder_t *dec)
{
    SV *ret;
    ASSERT_BUF_SPACE(dec, sizeof(float));
    ret= newSVnv((NV)*((float *)dec->pos));
    dec->pos+= sizeof(float);
    return ret;
}

static SRL_INLINE SV *
srl_read_double(pTHX_ srl_decoder_t *dec)
{
    SV *ret;
    ASSERT_BUF_SPACE(dec, sizeof(double));
    ret= newSVnv((NV)*((double *)dec->pos));
    dec->pos+= sizeof(double);
    return ret;
}

static SRL_INLINE SV *
srl_read_long_double(pTHX_ srl_decoder_t *dec)
{
    SV *ret;
    ASSERT_BUF_SPACE(dec, sizeof(long double));
    ret= newSVnv((NV)*((long double *)dec->pos));
    dec->pos+= sizeof(long double);
    return ret;
}


static SRL_INLINE SV *
srl_read_array(pTHX_ srl_decoder_t *dec, U8 *track_pos) {
    UV len= srl_read_varint_uv(aTHX_ dec);
    AV *av= newAV();
    SV *rv= newRV_noinc((SV *)av);
    if (expect_false( track_pos ))
        srl_track_sv(aTHX_ dec, track_pos, (SV*)rv);
    if (expect_false( len > 8 ))
        av_extend(av, len+1);
    while ( len-- > 0) {
        if (expect_false( *dec->pos == SRL_HDR_LIST )) {
            ERROR_UNIMPLEMENTED(dec, SRL_HDR_LIST, "LIST");
        }
        SV *got= srl_read_single_value(aTHX_ dec, NULL);
        av_push(av, got);
    }
    ASSERT_BUF_SPACE(dec,1);
    if (expect_true( *dec->pos == SRL_HDR_TAIL )) {
        dec->pos++;
    } else {
        ERROR_UNTERMINATED(dec,SRL_HDR_ARRAY,"ARRAY");
    }
    return rv;
}

static SRL_INLINE SV *
srl_read_hash(pTHX_ srl_decoder_t *dec, U8 *track_pos) {
    IV num_keys= srl_read_varint_uv(aTHX_ dec);
    HV *hv= newHV();
    SV *rv= newRV_noinc((SV *)hv);
    hv_ksplit(hv, num_keys); /* make sure we have enough room */
    /* NOTE: contents of hash are stored VALUE/KEY, reverse from normal perl
     * storage, this is because it simplifies the hash storage logic somewhat */
    if (expect_false( track_pos ))
        srl_track_sv(aTHX_ dec, track_pos, (SV*)rv);
    for (; num_keys > 0 ; num_keys--) {
        STRLEN key_len;
        SV *key_sv;
        SV *got_sv= srl_read_single_value(aTHX_ dec, NULL);
        U8 tag;

      read_key:
        ASSERT_BUF_SPACE(dec,1);
        tag= *dec->pos++;
        if (tag == SRL_HDR_STRING_UTF8) {
            key_len= srl_read_varint_uv(aTHX_ dec);
            ASSERT_BUF_SPACE(dec,key_len);
            key_sv= newSVpvn_flags((char*)dec->pos,key_len,1);
            if (expect_false( !hv_store_ent(hv,key_sv,got_sv,0))) {
                SvREFCNT_dec(key_sv); /* throw away the key */
                ERROR_PANIC(dec,"failed to hv_store_ent");
            } else {
                SvREFCNT_dec(key_sv);
            }
        } else {
            if (tag & SRL_HDR_ASCII) {
                key_len= tag & SRL_HDR_ASCII_LEN_MASK;
            } else if (tag == SRL_HDR_STRING) {
                key_len= srl_read_varint_uv(aTHX_ dec);
            } else if (tag == SRL_HDR_COPY) {
                UV ofs= srl_read_varint_uv(aTHX_ dec);
                if (expect_false( dec->save_pos )) {
                    ERROR_BAD_COPY(dec, SRL_HDR_HASH);
                } else {
                    dec->save_pos= dec->pos;
                    dec->pos= dec->buf_start + ofs;
                    goto read_key;
                }
            } else {
                ERROR_UNEXPECTED(dec,"a stringish type");
            }
            ASSERT_BUF_SPACE(dec,key_len);
            if (expect_false( !hv_store(hv,(char *)dec->pos,key_len,got_sv,0) )) {
                ERROR_PANIC(dec,"failed to hv_store");
            }
        }
        if (dec->save_pos) {
            dec->pos= dec->save_pos;
            dec->save_pos= NULL;
        } else {
            dec->pos += key_len;
        }
    }
    ASSERT_BUF_SPACE(dec,1);
    if (expect_true( *dec->pos == SRL_HDR_TAIL )) {
        dec->pos++;
    } else {
        ERROR_UNTERMINATED(dec,SRL_HDR_HASH,"HASH");
    }
    return rv;
}


static SRL_INLINE SV *
srl_read_ref(pTHX_ srl_decoder_t *dec, U8 *track_pos)
{
    UV item= srl_read_varint_uv(aTHX_ dec);
    SV *ret= NULL;
    SV *referent= NULL;
    /* This is a little tricky - If we are not referencing something
     * already dumped and we have to track the scalar holding this ref
     * then we have to track the item *before* we deserialize whatever
     * comes next as that thing might refer right back at us.
     * OTOH, When we deserialize something that we have seen before things
     * are simple */
    if (item) {
        /* something we did before */
        referent= srl_fetch_item(aTHX_ dec, item, "REF");
        ret= newRV_inc(referent);
    } else {
        ret= newRV_noinc(newSV(0));
    }
    if (track_pos)
        srl_track_sv(aTHX_ dec, track_pos, ret);
    if (!referent) {
        referent= srl_read_single_value(aTHX_ dec, NULL);
        SvRV_set(ret, referent);
    }
    return ret;
}

static SRL_INLINE SV *
srl_read_reuse(pTHX_ srl_decoder_t *dec)
{
    UV item= srl_read_varint_uv(aTHX_ dec);
    SV *referent= srl_fetch_item(aTHX_ dec, item, "REUSE");
    return newSVsv(referent);
}

static SRL_INLINE SV *
srl_read_alias(pTHX_ srl_decoder_t *dec)
{
    UV item= srl_read_varint_uv(aTHX_ dec);
    SV *referent= srl_fetch_item(aTHX_ dec, item, "ALIAS");
    SvREFCNT_inc(referent);
    return referent;
}

static SRL_INLINE SV *
srl_read_copy(pTHX_ srl_decoder_t *dec)
{
    UV item= srl_read_varint_uv(aTHX_ dec);
    SV *ret;
    if (expect_false( dec->save_pos )) {
        ERRORf1("COPY(%d) called during parse", item);
    }
    if (expect_false( (IV)item > dec->buf_end - dec->buf_start )) {
        ERRORf1("COPY(%d) points out of packet",item);
    }
    dec->save_pos= dec->pos;
    dec->pos= dec->buf_start + item;
    ret= srl_read_single_value(aTHX_ dec, NULL);
    dec->pos= dec->save_pos;
    dec->save_pos= 0;
    return ret;
}

static SRL_INLINE SV *
srl_read_weaken(pTHX_ srl_decoder_t *dec, U8 *track_pos)
{
    SV* ret= srl_read_single_value(aTHX_ dec, track_pos);
    if (expect_false( !SvROK(ret) ))
        ERROR("WEAKEN op");
    if (expect_true( SvREFCNT(ret)==1 )) {
        if (expect_false( !dec->weakref_av ))
            dec->weakref_av= newAV();
        SvREFCNT_inc(ret);
        av_push(dec->weakref_av, ret);
    }
    sv_rvweaken(ret);
    return ret;
}

static SRL_INLINE SV *
srl_read_bless(pTHX_ srl_decoder_t *dec)
{
    HV *stash= NULL;
    AV *av= NULL;
    STRLEN storepos= 0;
    UV ofs= 0;
    /* first deparse the thing we are going to bless */
    SV* ret= srl_read_single_value(aTHX_ dec, NULL);
    /* now find the class name - first check if this is a copy op
     * this is bit tricky, as we could have a copy of a raw string
     * we could also have a copy of a previously mentioned class
     * name. We have to handle both, which leads to some non-linear
     * code flow in the below code */
    ASSERT_BUF_SPACE(dec,1);
    if (*dec->pos == SRL_HDR_COPY) {
        dec->pos++;
        ofs= srl_read_varint_uv(aTHX_ dec);
        if (dec->ref_stashes) {
            stash= PTABLE_fetch(dec->ref_seenhash, (void *)ofs);
        }
        if (!stash)
            goto read_copy;
        else
            storepos= ofs;
        /* we should have seen this item before as a class name */
    } else {
        U32 key_len;
        I32 flags= GV_ADD;
        U8 tag;
        /* we now expect either a STRING type or a COPY type */
      read_class:
        storepos= BUF_POS_OFS(dec);
        tag= *dec->pos++;

        if (tag == SRL_HDR_STRING_UTF8) {
            flags = flags | SVf_UTF8;
            key_len= srl_read_varint_uv(aTHX_ dec);
        } else if (tag & SRL_HDR_ASCII) {
            key_len= tag & SRL_HDR_ASCII_LEN_MASK;
        } else if (tag == SRL_HDR_STRING) {
            key_len= srl_read_varint_uv(aTHX_ dec);
        } else if (tag == SRL_HDR_COPY) {
            ofs= srl_read_varint_uv(aTHX_ dec);
          read_copy:
            if (expect_false( dec->save_pos )) {
                ERROR_BAD_COPY(dec, SRL_HDR_HASH);
            } else {
                if (expect_false( dec->buf_end - dec->buf_start < (IV)ofs ) )
                    ERRORf1("copy command points at tag outside of buffer, offset=%"UVuf, ofs);
                dec->save_pos= dec->pos;
                dec->pos= dec->buf_start + ofs;
                goto read_class;
            }
            /* NOTREACHED */
        } else {
            ERROR_UNEXPECTED(dec,"a class name");
        }
        ASSERT_BUF_SPACE(dec, key_len);
        if (expect_false( !dec->ref_stashes )) {
            dec->ref_stashes = PTABLE_new();
            dec->ref_bless_av = PTABLE_new();
        }
        stash= gv_stashpvn((char *)dec->pos, key_len, flags);
        if (dec->save_pos) {
            dec->pos= dec->save_pos;
            dec->save_pos= NULL;
        } else {
            dec->pos += key_len;
        }
    }
    if (expect_false( !storepos ))
        ERROR("Bad bless: no storepos");
    if (expect_false( !stash ))
        ERROR("Bad bless: no stash");

    PTABLE_store(dec->ref_stashes, (void *)storepos, (void *)stash);
    if (NULL == (av= (AV *)PTABLE_fetch(dec->ref_bless_av, (void *)ofs)) ) {
        av= newAV();
        sv_2mortal((SV*)av);
        PTABLE_store(dec->ref_bless_av, (void *)storepos, (void *)av);
    }
    av_push(av, ret);
    /* we now have a stash and a value, so we can bless... except that
     * we dont actually want to do so right now. We want to defer blessing
     * until the full packet has been read. Yes it is more overhead, but
     * we really dont want to trigger DESTROY methods from a partial
     * deparse. */
    return ret;
}



SV *
srl_read_single_value(pTHX_ srl_decoder_t *dec, U8 *track_pos)
{
    STRLEN len;
    SV *ret;
    U8 tag;

  read_again:
    if (expect_false( BUF_DONE(dec) ))
        ERROR("unexpected end of input stream while expecting a single value");

    tag= *dec->pos++;
    if (tag & SRL_HDR_TRACK_FLAG) {
        if (expect_true( track_pos == 0 ))
            track_pos= dec->pos - 1;
        else
            ERROR("bad tracking");
        tag= tag & ~SRL_HDR_TRACK_FLAG;
    }
    if ( tag <= SRL_HDR_POS_HIGH ) {
        ret= newSVuv(tag);
    } else if ( tag <= SRL_HDR_NEG_LOW) {
        ret= newSViv( -tag + 15);
    } else if (tag & SRL_HDR_ASCII) {
        len= (STRLEN)(tag & SRL_HDR_ASCII_LEN_MASK);
        ASSERT_BUF_SPACE(dec,len);
        ret= newSVpvn((char*)dec->pos,len);
        dec->pos += len;
    }
    else {
        switch (tag) {
            case SRL_HDR_VARINT:        ret= srl_read_varint(aTHX_ dec);        break;
            case SRL_HDR_ZIGZAG:        ret= srl_read_zigzag(aTHX_ dec);        break;

            case SRL_HDR_FLOAT:         ret= srl_read_float(aTHX_ dec);         break;
            case SRL_HDR_DOUBLE:        ret= srl_read_double(aTHX_ dec);        break;
            case SRL_HDR_LONG_DOUBLE:   ret= srl_read_long_double(aTHX_ dec);   break;

            case SRL_HDR_UNDEF:         ret= newSVsv(&PL_sv_undef);             break;
            case SRL_HDR_STRING:        ret= srl_read_string(aTHX_ dec, 0);     break;
            case SRL_HDR_STRING_UTF8:   ret= srl_read_string(aTHX_ dec, 1);     break;
            case SRL_HDR_REUSE:         ret= srl_read_reuse(aTHX_ dec);         break;

            case SRL_HDR_REF:           ret= srl_read_ref(aTHX_ dec, track_pos);   break;
            case SRL_HDR_HASH:          ret= srl_read_hash(aTHX_ dec, track_pos);  break;
            case SRL_HDR_ARRAY:         ret= srl_read_array(aTHX_ dec, track_pos); break;

            case SRL_HDR_BLESS:         ret= srl_read_bless(aTHX_ dec);         break;
            case SRL_HDR_BLESSV:        ret= srl_read_blessv(aTHX_ dec);        break;
            case SRL_HDR_ALIAS:         ret= srl_read_alias(aTHX_ dec);         break;
            case SRL_HDR_COPY:          ret= srl_read_copy(aTHX_ dec);          break;

            case SRL_HDR_EXTEND:        ret= srl_read_extend(aTHX_ dec);        break;
            case SRL_HDR_LIST:          ERROR_UNEXPECTED(dec,tag);              break;

            case SRL_HDR_WEAKEN:        ret= srl_read_weaken(aTHX_ dec, track_pos); break;
            case SRL_HDR_REGEXP:        ret= srl_read_regexp(aTHX_ dec);        break;

            case SRL_HDR_TAIL:          ERROR_UNEXPECTED(dec,tag);              break;
            case SRL_HDR_PAD:           /* no op */                             
                while (BUF_NOT_DONE(dec) && *dec->pos == SRL_HDR_PAD)
                    dec->pos++;
                goto read_again;
            break;
            default:
                if (expect_true( SRL_HDR_RESERVED_LOW <= tag && tag <= SRL_HDR_RESERVED_HIGH )) {
                    ret= srl_read_reserved(aTHX_ dec, tag);
                } else {
                    ERROR_PANIC(dec,tag);
                }
            break;
        }
    }
    if (expect_false( track_pos ))
        srl_track_sv(aTHX_ dec, track_pos, ret);
    return ret;
}

/* FIXME unimplemented!!! */
static SRL_INLINE SV *
srl_read_blessv(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED(dec,SRL_HDR_BLESSV,"BLESSV");
}

static SRL_INLINE SV *
srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag)
{
    (void)tag; /* unused as of now */
    const UV len = srl_read_varint_uv(aTHX_ dec);
    ASSERT_BUF_SPACE(dec, len);
    dec->pos += len; /* discard */
    return &PL_sv_undef;
}

#ifdef SvRX
#define MODERN_REGEXP
#endif

static SRL_INLINE SV *
srl_read_regexp(pTHX_ srl_decoder_t *dec)
{
    SV *sv_pat= srl_read_single_value(aTHX_ dec, NULL);
    SV *ret= NULL;
    ASSERT_BUF_SPACE(dec, 1);
    /* For now we will serialize the flags as ascii strings. Maybe we should use
     * something else but this is easy to debug and understand - since the modifiers
     * are tagged it doesn't matter much, we can add other tags later */
    if (expect_true( *dec->pos & SRL_HDR_ASCII )) {
        U8 mod_len= *dec->pos++ & SRL_HDR_ASCII_LEN_MASK;
        U32 flags= 0;
        ASSERT_BUF_SPACE(dec, mod_len);
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
            REGEXP *re= CALLREGCOMP(sv_pat, flags);
            ret= newRV_noinc((SV*)re);
        }
#else
        {
            PMOP pm; /* grr */
            STRLEN pat_len;
            REGEXP *re;
            SV *sv= newSV(0);
            char *pat= SvPV(sv_pat, pat_len);

            Zero(&pm,1,PMOP);
            pm.op_pmdynflags= SvUTF8(sv_pat) ? PMdf_CMP_UTF8 : 0;
            pm.op_pmflags= flags;

            re= CALLREGCOMP(aTHX_ pat, pat + pat_len, &pm);
            sv_magic( sv, (SV*)re, PERL_MAGIC_qr, 0, 0);
            SvFLAGS(sv) |= SVs_SMG;
            ret= newRV_noinc(sv);
            SvREFCNT_dec(sv_pat);
        }
#endif
    }
    else {
        ERROR("Expecting SRL_HDR_ASCII for modifiers of regexp");
    }
    return ret;
}

static SRL_INLINE SV *
srl_read_extend(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED(dec,SRL_HDR_EXTEND,"EXTEND");
}


