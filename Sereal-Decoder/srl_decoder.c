#include "srl_decoder.h"

#include "ppport.h"

#define PERL_NO_GET_CONTEXT

#include "srl_inline.h"
#include "ptable.h"
#include "srl_protocol.h"

/* declare some of the in-file functions to avoid ordering issues */
static UV srl_read_varint_uv(pTHX_ srl_decoder_t *dec);

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

/* This is fired when we exit the Perl pseudo-block.
 * It frees our decoder and all. Put decoder-level cleanup
 * logic here so that we can simply use croak/longjmp for
 * exception handling. Makes life vastly easier!
 */
void srl_decoder_destructor_hook(void *p)
{
    srl_decoder_t *dec = (srl_decoder_t *)p;
    /* Exception cleanup. Under normal operation, we should have
     * assigned NULL to buf_start after we're done. */
    PTABLE_free(dec->ref_seenhash);
    if (dec->ref_stashes) {
        PTABLE_free(dec->ref_stashes);
        PTABLE_free(dec->ref_bless_av);
    }
    Safefree(dec);
}

/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_decoder_t *
build_decoder_struct(pTHX_ HV *opt, SV *src)
{
    STRLEN len;
    srl_decoder_t *dec;
    /* SV **svp; */

    Newxz(dec, 1, srl_decoder_t);

    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR(&srl_decoder_destructor_hook, (void *)dec);

    dec->ref_seenhash = PTABLE_new();
    dec->buf_start= dec->pos= (unsigned char*)SvPV(src, len);
    dec->buf_end= dec->buf_start + len;

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
    if (strEQ((char*)dec->pos, SRL_MAGIC_STRING "\x01")) {
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
            if (!stash || !ref_bless_av) {
                ERROR("missing stash or ref_bless_av!");
            }
            for( len= av_len(ref_bless_av) + 1 ; len > 0 ; len-- ) {
                SV* obj= av_pop(ref_bless_av);
                if (obj)
                    sv_bless(obj, stash);
                else
                    ERROR("object missing from ref_bless_av array?");
            }
        }

        PTABLE_iter_free(it);
    }
    return 0;
}

static inline UV
srl_read_varint_uv_safe(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (BUF_NOT_DONE(dec) && *dec->pos & 0x80) {
        uv += (*dec->pos++ & 0x7F) << lshift;
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            ERROR("varint too big");
    }
    if (BUF_NOT_DONE(dec)) {
        uv += (*dec->pos++) << lshift;
    } else {
        ERROR("varint terminated prematurely");
    }
    return uv;
}

static inline UV
srl_read_varint_uv_nocheck(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    unsigned int lshift= 0;

    while (*dec->pos & 0x80) {
        uv += (*dec->pos++ & 0x7F) << lshift;
        lshift += 7;
        if (lshift > (sizeof(UV) * 8))
            ERROR("varint too big");
    }
    uv += (*dec->pos++) << lshift;
    return uv;
}

static inline UV
srl_read_varint_uv(pTHX_ srl_decoder_t *dec)
{
    if (dec->buf_end - dec->pos > 10)
        return srl_read_varint_uv_nocheck(aTHX_ dec);
    else
        return srl_read_varint_uv_safe(aTHX_ dec);
}

static inline void
srl_track_sv(pTHX_ srl_decoder_t *dec, U8 *track_pos, SV *sv) {
    PTABLE_store(dec->ref_seenhash, (void *)(track_pos - dec->buf_start), (void *)sv);
}

static SV *srl_fetch_item(pTHX_ srl_decoder_t *dec, UV item, const char const *tag_name) {
    SV *sv= (SV *)PTABLE_fetch(dec->ref_seenhash, (void *)item);
    if (!sv)
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
    if (track_pos)
        srl_track_sv(aTHX_ dec, track_pos, (SV*)rv);
    if (len > 8)
        av_extend(av, len+1);
    while ( len-- > 0) {
        if ( *dec->pos == SRL_HDR_LIST ) {
            ERROR_UNIMPLEMENTED(dec, SRL_HDR_LIST, "LIST");
        }
        SV *got= srl_read_single_value(aTHX_ dec, NULL);
        av_push(av, got);
    }
    ASSERT_BUF_SPACE(dec,1);
    if (*dec->pos == SRL_HDR_TAIL) {
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
    if (track_pos)
        srl_track_sv(aTHX_ dec, track_pos, (SV*)rv);
    for (; num_keys > 0 ; num_keys--) {
        STRLEN key_len;
        SV *key_sv;
        SV *got_sv= srl_read_single_value(aTHX_ dec, NULL);

      read_key:
        ASSERT_BUF_SPACE(dec,1);
        if (*dec->pos == SRL_HDR_STRING_UTF8) {
            key_len= srl_read_varint_uv(aTHX_ dec);
            ASSERT_BUF_SPACE(dec,key_len);
            key_sv= newSVpvn_flags((char*)dec->pos,key_len,1);
            if (!hv_store_ent(hv,key_sv,got_sv,0)) {
                SvREFCNT_dec(key_sv); /* throw away the key */
                ERROR_PANIC(dec,"failed to hv_store_ent");
            } else {
                SvREFCNT_dec(key_sv);
            }
        } else {
            if (*dec->pos & SRL_HDR_ASCII) {
                key_len= (*dec->pos++) & SRL_HDR_ASCII_LEN_MASK;
            } else if (*dec->pos == SRL_HDR_STRING) {
                key_len= srl_read_varint_uv(aTHX_ dec);
            } else if (*dec->pos == SRL_HDR_COPY) {
                UV ofs= srl_read_varint_uv(aTHX_ dec);
                if (dec->save_pos) {
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
            if (!hv_store(hv,(char *)dec->pos,key_len,got_sv,0)) {
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
    if (*dec->pos == SRL_HDR_TAIL) {
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

static SRL_INLINE SV *srl_read_copy(pTHX_ srl_decoder_t *dec)
{
    UV item= srl_read_varint_uv(aTHX_ dec);
    SV *ret;
    if (dec->save_pos) {
        ERRORf1("COPY(%d) called during parse", item);
    }
    if ((IV)item > dec->buf_end - dec->buf_start) {
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
    if (!SvROK(ret))
        ERROR("WEAKEN op");
    if (SvREFCNT(ret)==1) {
        if (!dec->weakref_av) {
            dec->weakref_av= newAV();
            SAVEFREESV(dec->weakref_av);
        }
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
            if (dec->save_pos) {
                ERROR_BAD_COPY(dec, SRL_HDR_HASH);
            } else {
                if (dec->buf_end - dec->buf_start < (IV)ofs)
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
        if (!dec->ref_stashes) {
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
    if (!storepos)
        ERROR("Bad bless: no storepos");
    if (!stash)
        ERROR("Bad bless: no stash");

    PTABLE_store(dec->ref_stashes, (void *)storepos, (void *)stash);
    if (NULL == (av= (AV *)PTABLE_fetch(dec->ref_bless_av, (void *)ofs)) ) {
        av= newAV();
        SAVEFREESV((SV*)av);
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
    if (BUF_DONE(dec))
        ERROR("unexpected end of input stream while expecting a single value");

    tag= *dec->pos++;
    if (tag & SRL_HDR_TRACK_FLAG) {
        if (track_pos == 0 )
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
    else{
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
                if (SRL_HDR_RESERVED_LOW <= tag && tag <= SRL_HDR_RESERVED_HIGH) {
                    ret= srl_read_reserved(aTHX_ dec, tag);
                } else {
                    ERROR_PANIC(dec,tag);
                }
            break;
        }
    }
    if (track_pos)
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
    if (dec->buf_end - dec->pos < (ptrdiff_t)len) { /* FIXME double-check for off-by-one error */
        dec->pos += len; /* discard */
    }
    else {
        ERROR("unexpected end of buffer");
    }
    return &PL_sv_undef;
}

static SRL_INLINE SV *
srl_read_regexp(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED(dec,SRL_HDR_REGEXP,"REGEXP");
}
static SRL_INLINE SV *
srl_read_extend(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED(dec,SRL_HDR_EXTEND,"EXTEND");
}


