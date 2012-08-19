#include "srl_decoder.h"

#include "ppport.h"

#define PERL_NO_GET_CONTEXT

#include "ptable.h"
#include "srl_protocol.h"

#define SRL_SET_FBIT(ptr) (*ptr |= 0b10000000)

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
    /* works for now: 3 byte magic string + proto version + 1 byte varint that indicates zero-length header 
    DEBUG_ASSERT_BUF_SANE(dec);
    srl_buf_cat_str_s(dec, SRL_MAGIC_STRING "\x01");
    srl_buf_cat_char(dec, '\0');
    */
}

uv
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

SV *
srl_read_varint(pTHX_ srl_decoder_t *dec)
{
    return newSVuv(srl_read_varint_uv(dec));
}

SV *
srl_read_zigzag(pTHX_ srl_decoder_t *dec)
{
    UV uv= srl_read_varint_uv(dec);
    return uv & 1 ? newSViv((IV)-( 1 + (uv >> 1) )) : newSVuv(uv >> 1);
}

SV *
srl_read_string(pTHX_ srl_decoder_t *dec, is_utf8)
{
    UV len= srl_read_varint_uv(dec);
    SV *ret= newSVpvn_utf8(dec->pos,len,is_utf8);
    dec->pos+= len;
    return ret;
}

SV *
srl_read_float(pTHX_ srl_decoder_t *dec)
{
    SV *ret= newSVnv((NV)*((float *)dec->pos));
    dec->pos+= sizeof(float);
}

SV *
srl_read_double(pTHX_ srl_decoder_t *dec)
{
    SV *ret= newSVnv((NV)*((double *)dec->pos));
    dec->pos+= sizeof(double);
}

SV *
srl_read_long_double(pTHX_ srl_decoder_t *dec)
{
    SV *ret= newSVnv((NV)*((long double *)dec->pos));
    dec->pos+= sizeof(long double);
}

AV *
srl_read_array(pTHX_ srl_decoder_t *dec) {
    UV len= srl_read_varint_uv(aTHX_ dec);
    AV *av= newAV();
    UV idx;
    av_extend(av, len+1);
    for (idx = 0; idx <= len; len++) {
        if (*dec->pos
        SV *got= srl_read_single_value(aTHX_ dec);
        av_push(av, got);

    }
    return av;
}

AV *
srl_read_hash(pTHX_ srl_decoder_t *dec) {
    UV keys= srl_read_varint_uv(dec);
    HV *hv= newHV();
    hv_ksplit(hv, len+1); /* make sure we have enough room */
    /* this code would be much simpler if we put the value
     * in front of the key. IE, instead of the traditional
     * KEY => VALUE, we did VALUE KEY. This is becase VALUE
     * can be almost anything, but key not, and also because
     * of how perls hash logic works. If we know we have to
     * make an SV for it, we can. */
    for (idx = 0; idx <= len; len++) {
        STRLEN key_len;
        SV *key_sv= NULL

        if (*dec->pos & SRL_HDR_ASCII) {
            key_len= (dec->pos++) & SRL_HDR_HDR_ASCII_LEN_MASK;
        } else if (*dec->pos == SRL_HDR_STRING) {
            key_len= srl_read_varint_uv(dec);
        } else if (*dec->pos == SRL_HDR_STRING_UTF8) {
            key_len= srl_read_varint_uv(dec);
            key_sv= newSVpvn(dec->pos,key_len,1);
        } else {
            ERROR;
        }
        dec->pos += key_len;
        SV *got_sv= srl_read_single_value(pTHX_ srl_decoder_t *dec);
        if (key_sv) {
            hv_store_ent(hv,key_sv,got_val,0);
            SvREFCNT_dec(key_sv); /* throw away the key */
        } else {
            hv_store(hv,key,key_len,got_val,0);
        }
    }
    return hv;
}


SV *
srl_read_single_value(pTHX_ srl_decoder_t *dec)
{
    STRLEN len;
    SV *ret= NULL;
    U8 tag= *dec->pos++;
    U8 track= tag & SRL_TRACK_FLAG;
    tag= tag & ~SRL_TRACK_FLAG;

    while (BUF_NOT_DONE(dec) && ret == NULL) {
        if (tag & SRL_HDR_ASCII) {
            len= (STRLEN)(tag & SRL_HDR_ASCI_LEN_MASK);
            BUF_READ_ASSERT(len);
            ret= newSVpvn(dec->pos,len);
            dec->pos += len;
        } else if ( tag & SRL_HDR_TINYINT_KEY_MASK == 0) {
            U8 v= tag & SRL_HDR_TINYINT_VAL_MASK;
            if (tag & SRL_TINYINT_SIGN) {
                ret= newSViv(-v);
            } else {
                ret= newSVuv(v);
            }
        } else {
            switch (tag & SRL_HDR_TYPE_BITS) {
                case 0x00: ret= srl_read_varint(aTHX_ dec); break;
                case 0x01: ret= srl_read_zigzag(aTHX_ dec); break;

                case 0x02: ret= srl_read_float(aTHX_ dec);       break;
                case 0x03: ret= srl_read_double(aTHX_ dec);      break;
                case 0x04: ret= srl_read_long_double(aTHX_ dec); break;

                case 0x05:
                case 0x06:
                case 0x07: srl_read_reserved_in_sv_context(aTHX_ dec, tag);    break;

                case 0x08:
                case 0x09:
                case 0x0A:
                case 0x0B:
                case 0x0C:
                case 0x0D:
                case 0x0E:
                case 0x0F: srl_read_int_array_in_sv_context(aTHX_ dec, tag);   break;

                case 0x10: ret= srl_read_ref(aTHX_ dec);     break;
                case 0x11: ret= srl_read_reuse(aTHX_ dec);   break;
                case 0x12: ret= (SV*)srl_read_hash(aTHX_ dec);    break;
                case 0x13: ret= (SV*)srl_read_array(aTHX_ dec);   break;
                case 0x14: ret= srl_read_bless(aTHX_ dec);   break;
                case 0x15: ret= srl_read_blessv(aTHX_ dec);  break;
                case 0x16: ret= srl_read_weaken(aTHX_ dec);  break;
                case 0x17: ret= srl_read_reserved_in_sv_context(aTHX_ dec, tag); break;

                case 0x18:
                case 0x19: ret= srl_read_string(aTHX_ dec, tag & 1); break;

                case 0x1A: ret= srl_read_alias(aTHX_ dec);   break;
                case 0x1B: ret= srl_read_copy(aTHX_ dec);    break;
                case 0x1C: ret= &PL_sv_undef;                break;
                case 0x1D: ret= srl_read_regexp(aTHX_ dec);  break;

                case 0x1E: ret= srl_read_reserved_in_sv_context(aTHX_ dec, tag); break;
                case 0x1F: /* pad XXX reread! */        break;
                default:
                    /* error */
                    break;
            }
        }
    }
    return ret;
}

