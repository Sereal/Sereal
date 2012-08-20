#include "srl_decoder.h"

#include "ppport.h"

#define PERL_NO_GET_CONTEXT

#include "ptable.h"
#include "srl_protocol.h"

#define SRL_SET_FBIT(ptr) (*ptr |= 0b10000000)

/* declare some of the in-file functions to avoid ordering issues */
static SV *srl_read_single_value(pTHX_ srl_decoder_t *dec);
static HV *srl_read_hash(pTHX_ srl_decoder_t *dec);
static AV *srl_read_array(pTHX_ srl_decoder_t *dec);
static SV *srl_read_long_double(pTHX_ srl_decoder_t *dec);
static SV *srl_read_double(pTHX_ srl_decoder_t *dec);
static SV *srl_read_float(pTHX_ srl_decoder_t *dec);
static SV *srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8);
static SV *srl_read_varint(pTHX_ srl_decoder_t *dec);
static SV *srl_read_zigzag(pTHX_ srl_decoder_t *dec);
static UV srl_read_varint_uv(pTHX_ srl_decoder_t *dec);

/* FIXME unimplemented!!! */
static SV *srl_read_ref(pTHX_ srl_decoder_t *dec);
static SV *srl_read_reuse(pTHX_ srl_decoder_t *dec);
static SV *srl_read_bless(pTHX_ srl_decoder_t *dec);
static SV *srl_read_blessv(pTHX_ srl_decoder_t *dec);
static SV *srl_read_alias(pTHX_ srl_decoder_t *dec);
static SV *srl_read_copy(pTHX_ srl_decoder_t *dec);
static SV *srl_read_weaken(pTHX_ srl_decoder_t *dec);
static SV *srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag);
static SV *srl_read_regexp(pTHX_ srl_decoder_t *dec);
static SV *srl_read_extend(pTHX_ srl_decoder_t *dec);

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
    PTABLE_free(dec->str_seenhash);
    Safefree(dec->val_stack);
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

    Newx(dec, 1, srl_decoder_t);
    dec->val_stack_size= 1000;
    Newx(dec->val_stack, dec->val_stack_size, SV *);
    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR(&srl_decoder_destructor_hook, (void *)dec);
    dec->depth = 0;
    dec->flags = 0;
    dec->save_pos= NULL;

    dec->ref_seenhash = PTABLE_new();
    dec->str_seenhash = PTABLE_new();
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
    /* TODO implement */
    /* works for now: 3 byte magic string + proto version + 1 byte varint that indicates zero-length header 
    DEBUG_ASSERT_BUF_SANE(dec);
    srl_buf_cat_str_s(dec, SRL_MAGIC_STRING "\x01");
    srl_buf_cat_char(dec, '\0');
    */
    return 0;
}

static UV
srl_read_varint_uv(pTHX_ srl_decoder_t *dec)
{
    UV uv= 0;
    while (BUF_NOT_DONE(dec) && *pos->dec & 0x80) {
        uv+= *pos->dec++ & 0x7F;
    }
    if (BUF_NOT_DONE) {
        uv+= *pos->dec++;
    } else {
        ERROR("varint terminated prematurely");
    }
    return uv;
}

static SV *
srl_read_varint(pTHX_ srl_decoder_t *dec)
{
    return newSVuv(srl_read_varint_uv(dec));
}

static SV *
srl_read_zigzag(pTHX_ srl_decoder_t *dec)
{
    UV uv= srl_read_varint_uv(dec);
    return uv & 1 ? newSViv((IV)-( 1 + (uv >> 1) )) : newSVuv(uv >> 1);
}

static SV *
srl_read_string(pTHX_ srl_decoder_t *dec, int is_utf8)
{
    UV len= srl_read_varint_uv(dec);
    SV *ret= newSVpvn_utf8(dec->pos,len,is_utf8);
    dec->pos+= len;
    return ret;
}

static SV *
srl_read_float(pTHX_ srl_decoder_t *dec)
{
    SV *ret= newSVnv((NV)*((float *)dec->pos));
    dec->pos+= sizeof(float);
    return ret;
}

static SV *
srl_read_double(pTHX_ srl_decoder_t *dec)
{
    SV *ret= newSVnv((NV)*((double *)dec->pos));
    dec->pos+= sizeof(double);
    return ret;
}

static SV *
srl_read_long_double(pTHX_ srl_decoder_t *dec)
{
    SV *ret= newSVnv((NV)*((long double *)dec->pos));
    dec->pos+= sizeof(long double);
    return ret;
}

static AV *
srl_read_array(pTHX_ srl_decoder_t *dec) {
    UV len= srl_read_varint_uv(aTHX_ dec);
    AV *av= newAV();
    UV idx;
    av_extend(av, len+1);
    for (idx = 0; idx <= len; len++) {
        if (*dec->pos = SRL_HDR_LIST) {
            ERROR_UNIMPLEMENTED(dec, SRL_HDR_LIST);
        }
        SV *got= srl_read_single_value(aTHX_ dec);
        av_push(av, got);
    }
    ASSERT_BUF_SPACE(1);
    if (*dec->pos == SRL_HDR_TAIL) {
        dec->pos++;
    } else {
        ERROR_UNTERMINATED(dec,SRL_HDR_ARRAY);
    }
    return av;
}

static HV *
srl_read_hash(pTHX_ srl_decoder_t *dec) {
    UV keys= srl_read_varint_uv(dec);
    HV *hv= newHV();
    hv_ksplit(hv, len+1); /* make sure we have enough room */
    /* NOTE: contents of hash are stored VALUE/KEY, reverse from normal perl
     * storage, this is because it simplifies the hash storage logic somewhat */
    for (idx = 0; idx <= len; len++) {
        STRLEN key_len;
        SV *key_sv;
        SV *got_sv= srl_read_single_value(aTHX_ dec);

        ASSERT_BUF_SPACE(1);
      read_key:
        if (*dec->pos == SRL_HDR_STRING_UTF8) {
            key_len= srl_read_varint_uv(aTHX_ dec);
            key_sv= newSVpvn_flags((char*)dec->pos,key_len,1);
            if (!hv_store_ent(hv,key_sv,got_sv,0)) {
                SvREFCNT_dec(key_sv); /* throw away the key */
                ERROR_PANIC(dec);
            } else {
                SvREFCNT_dec(key_sv);
            }
        } else {
            if (*dec->pos & SRL_HDR_ASCII) {
                key_len= (dec->pos++) & SRL_HDR_ASCII_LEN_MASK;
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
                ERROR_UNEXPECTED(dec);
            }
            if (!hv_store(hv,(char *)dec->pos,key_len,got_sv,0)) {
                ERROR_PANIC(dec);
            }
        }
        if (dec->save_pos) {
            dec->pos= dec->save_pos;
            dec->save_pos= NULL;
        } else {
            dec->pos += key_len;
        }
    }
    ASSERT_BUF_SPACE(1);
    if (*dec->pos == SRL_HDR_TAIL) {
        dec->pos++;
    } else {
        ERROR_UNTERMINATED(dec,SRL_HDR_HASH);
    }
    return hv;
}

#define ERROR_UNIMPLEMENTED() STMT_START {      \
    croak("unimplemented");                     \
    return NULL;                                \
} STMT_END 

/* FIXME unimplemented!!! */
static SV *srl_read_ref(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_reuse(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_bless(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_blessv(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_alias(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_copy(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_weaken(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_reserved(pTHX_ srl_decoder_t *dec, U8 tag)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_regexp(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}
static SV *srl_read_extend(pTHX_ srl_decoder_t *dec)
{
    ERROR_UNIMPLEMENTED();
}


static SV *
srl_read_single_value(pTHX_ srl_decoder_t *dec)
{
    STRLEN len;
    SV *ret= NULL;

    while (BUF_NOT_DONE(dec) && ret == NULL) {
        U8 tag= *dec->pos++;
        U8 track= tag & SRL_HDR_TRACK_FLAG;
        if (track)
            tag= tag & ~SRL_HDR_TRACK_FLAG;

        if ( tag <= SRL_HDR_POS_HIGH ) {
            ret= newSVuv(tag);
        } else if ( tag <= SRL_HDR_NEG_LOW) {
            ret= newSViv( -tag + 15);
        } else if (tag & SRL_HDR_ASCII) {
            len= (STRLEN)(tag & SRL_HDR_ASCII_LEN_MASK);
            BUF_READ_ASSERT(len);
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
                case SRL_HDR_REF:           ret= srl_read_ref(aTHX_ dec);           break;
                case SRL_HDR_REUSE:         ret= srl_read_reuse(aTHX_ dec);         break;

                case SRL_HDR_HASH:          ret= (SV*)srl_read_hash(aTHX_ dec);     break;
                case SRL_HDR_ARRAY:         ret= (SV*)srl_read_array(aTHX_ dec);    break;
                case SRL_HDR_BLESS:         ret= srl_read_bless(aTHX_ dec);         break;
                case SRL_HDR_BLESSV:        ret= srl_read_blessv(aTHX_ dec);        break;
                case SRL_HDR_ALIAS:         ret= srl_read_alias(aTHX_ dec);         break;
                case SRL_HDR_COPY:          ret= srl_read_copy(aTHX_ dec);          break;

                case SRL_HDR_EXTEND:        ret= srl_read_extend(aTHX_ dec);        break;
                case SRL_HDR_LIST:          ERROR_UNEXPECTED(dec,tag);              break;

                case SRL_HDR_WEAKEN:        ret= srl_read_weaken(aTHX_ dec);        break;
                case SRL_HDR_REGEXP:        ret= srl_read_regexp(aTHX_ dec);        break;

                case SRL_HDR_TAIL:          ERROR_UNEXPECTED(dec,tag);              break;
                case SRL_HDR_PAD:           NO_OP(dec,tag);                         break;

                default:
                    if (SRL_HDR_RESERVED_LOW <= tag && tag <= SRL_HDR_RESERVED_HIGH) {
                        ret= srl_read_reserved(aTHX_ dec, tag);
                    } else {
                        ERROR_PANIC(dec,tag);
                    }
                break;
            }
        }
    }
    return ret;
}

