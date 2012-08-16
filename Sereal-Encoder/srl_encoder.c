#include "srl_encoder.h"

#include "ppport.h"

#define PERL_NO_GET_CONTEXT

#include "ptable.h"
#include "srl_buffer.h"
#include "srl_protocol.h"

/* General 'config' constants */
#define INITIALIZATION_SIZE 16384
#define MAX_DEPTH 10000

/* define option bits in srl_encoder_t's flags member */
/* example: #define F_UNDEF_BLESSED                 1UL */

/* some static function declarations */
static void srl_dump_rv(pTHX_ srl_encoder_t *enc, SV *src);
static void srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src);
static void srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src);
static void srl_dump_pv(pTHX_ srl_encoder_t *enc, const char* src, STRLEN src_len, int is_utf8);
static inline void srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src);
static inline void srl_dump_nv(pTHX_ srl_encoder_t *enc, SV *src);
static inline void srl_dump_ivuv(pTHX_ srl_encoder_t *enc, SV *src);

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
    PTABLE_free(enc->seenhash);
    Safefree(enc);
}

/* Builds the C-level configuration and state struct.
 * Automatically freed at scope boundary. */
srl_encoder_t *
build_encoder_struct(pTHX_ HV *opt)
{
    srl_encoder_t *enc;
    /* SV **svp; */

    Newx(enc, 1, srl_encoder_t);
    /* Register our structure for destruction on scope exit */
    SAVEDESTRUCTOR(&srl_destructor_hook, (void *)enc);

    /* Init struct */
    Newx(enc->buf_start, INITIALIZATION_SIZE, char);
    enc->buf_end = enc->buf_start + INITIALIZATION_SIZE;
    enc->pos = enc->buf_start;
    enc->depth = 0;
    enc->flags = 0;

    /* TODO: We could do this lazily: Only if there's references with high refcount/weakrefs */
    enc->seenhash = PTABLE_new();

    /* load options */
    if (opt != NULL) {
        /* if ( (svp = hv_fetchs(opt, "undef_blessed", 0)) && SvTRUE(*svp))
          enc->flags |= F_UNDEF_BLESSED;
        */
    }

    return enc;
}


void
srl_write_header(pTHX_ srl_encoder_t *enc)
{
    /* works for now: 4 byte magic string + 1 byte varint that indicates zero-length header */
    srl_buf_cat_str_s(enc, SRL_MAGIC_STRING "\x00");
}


/* Code for serializing floats */
static inline void
srl_dump_nv(pTHX_ srl_encoder_t *enc, SV *src)
{
    /* TODO memcpy on little endian 64bit, otherwise manual diddling? */
}


/* Code for serializing any SINGLE integer type */
static inline void
srl_dump_ivuv(pTHX_ srl_encoder_t *enc, SV *src)
{
    char hdr;
    /* TODO for the time being, we just won't ever use NUMLIST types because that's
     *      a fair amount of extra implementation work. The decoders won't care and
     *      we're just wasting some space. */
    /* TODO optimize! */

    if (SvIOK_UV(src) || SvIV(src) > 0) { /* FIXME find a way to express this without repeated SvIV/SvUV */
        UV num = SvUV(src); /* FIXME is SvUV_nomg good enough because of the GET magic in dump_sv? SvUVX after having checked the flags? */
        if (num < 16) {
            /* encodable as POS */
            hdr = SRL_HDR_POS_LOW | (unsigned char)num;
            srl_buf_cat_char(enc, hdr);
        }
        else {
            /* Needs VARINT */
            BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH);
            srl_buf_cat_char_nocheck(enc, SRL_HDR_VARINT);
            srl_buf_cat_varint_nocheck(aTHX_ enc, num);
        }
    }
    else {
        IV num = SvIV(src);
        if (num > -17) {
            /* encodable as NEG */
            hdr = SRL_HDR_NEG_HIGH | (unsigned char)num;
            srl_buf_cat_char(enc, hdr);
        }
        else {
            /* Needs ZIGZAG */
            BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_ZIGZAG_LENGTH);
            srl_buf_cat_char_nocheck(enc, SRL_HDR_ZIGZAG);
            srl_buf_cat_zigzag_nocheck(aTHX_ enc, num);
        }
    }
}


/* Entry point for serialization AFTER header. Dumps generic SVs and delegates
 * to more specialized functions for RVs, etc. */
void
srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src)
{
    SvGETMAGIC(src);

    /* TODO decide when to use the IV, when to use the PV, and when
     *      to use the NV slots of the SV.
     *      Safest simple solution seems "prefer string" (fuck dualvars).
     *      Potentially better but slower: If we would choose the string,
     *      then try int-to-string (respective float-to-string) conversion
     *      and strcmp. If same, then use int or float.
     */

    /* dump strings */
    if (SvPOKp(src)) {
        STRLEN len;
        char *str = SvPV(src, len);
        BUF_SIZE_ASSERT(enc, 2 + len);
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
        srl_dump_rv(aTHX_ enc, SvRV(src));
    else {
        croak("Attempting to dump unsupported or invalid SV");
    }
    /* TODO what else do we need to support in this many if/else? */
}


/* Dump references, delegates to more specialized functions for
 * arrays, hashes, etc. */
static void
srl_dump_rv(pTHX_ srl_encoder_t *enc, SV *src)
{
    svtype svt;
    int blessed_object = 0;

    if (++enc->depth > MAX_DEPTH) {
        croak("Reached maximum recursion depth of %u. Aborting", MAX_DEPTH);
    }

    SvGETMAGIC(src);
    svt = SvTYPE(src);

    /* Have to check the seen hash if high refcount or a weak ref */
    if (SvREFCNT(src) > 1 || SvWEAKREF(src)) {
        /* FIXME is the actual sv location the right thing to use? */
        PTABLE_ENTRY_t *entry = PTABLE_find(enc->seenhash, src);
        if (entry != NULL)
            croak("Encountered reference multiple times: '%s'",
                  SvPV_nolen(sv_2mortal(newRV_inc(src))));
        else
            PTABLE_store(enc->seenhash, src, NULL);

        /* output WEAKEN prefix before the actual item */
        if (SvWEAKREF(src))
            srl_buf_cat_char(enc, SRL_HDR_WEAKEN);
    }

    if (SvOBJECT(src)) {
        croak("Encountered object '%s', but objects are not implemented yet",
              SvPV_nolen(sv_2mortal(newRV_inc(src))));
    }

    if (svt == SVt_PVHV)
        srl_dump_hv(aTHX_ enc, (HV *)src);
    else if (svt == SVt_PVAV)
        srl_dump_av(aTHX_ enc, (AV *)src);
    else if (svt < SVt_PVAV) {
        srl_buf_cat_char(enc, '\\');
        srl_dump_sv(aTHX_ enc, src);
    }
    /* else if (enc->json.flags & F_ALLOW_UNKNOWN)
     *    srl_dump_pv(aTHX_ enc, "null", 4, 0);
     */
    else {
        croak("found %s, but it is not representable by Data::Dumper::Limited serialization",
               SvPV_nolen(sv_2mortal(newRV_inc(src))));
    }

    /* finish writing the bless(XXX,"classname") call */
    if (blessed_object) {
        /* FIXME this should probably do ' escaping! */
        const char *class_name = HvNAME(SvSTASH(src));
        const size_t len = strlen(class_name);
        BUF_SIZE_ASSERT(enc, len + 4);
        srl_buf_cat_str_s_nocheck(enc, ",");
        srl_dump_pv(aTHX_ enc,class_name,len,0);
        srl_buf_cat_str_s_nocheck(enc, ")");
    }

    /* If we DO allow multiple occurrence of the same ref (default), then
     * we need to drop its seenhash entry as soon as it cannot be a cyclic
     * ref any more. */
    /*
     * if (!(enc->flags & F_DISALLOW_MULTI_OCCURRENCE)) {
     *   PTABLE_delete(enc->seenhash, src);
     * }
     */
}


static void
srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src)
{
    UV i, n;
    SV **svp;

    n = av_len(src)+1;

    /* heuristic: n is virtually the min. size of any element */
    BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH + n);

    /* header and num. elements */
    srl_buf_cat_char_nocheck(enc, SRL_HDR_ARRAY);
    srl_buf_cat_varint_nocheck(aTHX_ enc, n);

    if (n == 0)
        return;

    svp = av_fetch(src, 0, 0);
    if (svp != NULL)
        srl_dump_sv(aTHX_ enc, *svp);
    else
        srl_buf_cat_char(enc, SRL_HDR_UNDEF);

    for (i = 1; i < n; ++i) {
        srl_buf_cat_char(enc, ',');
        svp = av_fetch(src, i, 0);
        if (svp != NULL)
            srl_dump_sv(aTHX_ enc, *svp);
        else
            srl_buf_cat_char(enc, SRL_HDR_UNDEF);
    }
}


static void
srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src)
{
    HE *he;
    UV n = hv_iterinit(src);

    /* heuristic: n = ~min size of n values;
     *            + 2*n = very conservative min size of n hashkeys if all COPY */
    BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH + 3*n);
    srl_buf_cat_char_nocheck(enc, SRL_HDR_HASH);
    srl_buf_cat_varint_nocheck(aTHX_ enc, n);

    if (n == 0 && !SvMAGICAL(src))
        return;

    if ((he = hv_iternext(src))) {
        for (;;) {
            srl_dump_hk(aTHX_ enc, he);
            srl_dump_sv(aTHX_ enc, SvMAGICAL(src) ? hv_iterval(src, he) : HeVAL(he));

            if (!(he = hv_iternext(src)))
                break;
        }
    }
}


static void
srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src)
{
    if (HeKLEN(src) == HEf_SVKEY) {
        SV *sv = HeSVKEY(src);
        STRLEN len;
        char *str;

        SvGETMAGIC(sv);
        str = SvPV(sv, len);

        srl_dump_pv(aTHX_ enc, str, len, SvUTF8(sv));
    }
    else
        srl_dump_pv(aTHX_ enc, HeKEY(src), HeKLEN(src), HeKUTF8(src));
}

static void
srl_dump_pv(pTHX_ srl_encoder_t *enc, const char* src, STRLEN src_len, int is_utf8)
{
    const U8 *scan= (U8*)src;
    const U8 *scan_end= (U8*)src + src_len;
    const U8 *plain_start= 0;
    const U8 *plain_end= 0;
    UV cp;
    STRLEN ulen;
    int has_escapes= 0;
    STRLEN quote_ofs= BUF_POS_OFS(enc);
#define CLEAR_PLAIN_START(enc, plain_start, plain_end)                                          \
    STMT_START {                                                                                \
        if (plain_start) {                                                                      \
            srl_buf_cat_str(enc, (const char *)plain_start, plain_end - plain_start);     \
            plain_start= plain_end= 0;                                                          \
        }                                                                                       \
    } STMT_END

    BUF_SIZE_ASSERT(enc,src_len);

    srl_buf_cat_char(enc,'"');
    while (scan < scan_end) {
        cp= *scan;
        switch ((U8)cp) {
        case 0:   /* 0 */
            cp= '0';
            goto simple_esc;
        case '\a': /* 7 */
            cp= 'a';
            goto simple_esc;
        case '\b': /* 8 */
            cp= 'b';
            goto simple_esc;
        case '\t': /* 9 */
            cp= 't';
            goto simple_esc;
        case '\n': /* 10 */
            cp= 'n';
            goto simple_esc;
        case '\f': /* 12 */
            cp= 'f';
            goto simple_esc;
        case '\r': /* 13 */
            cp= 'r';
            goto simple_esc;
        case 27:
            cp= 'e';
            goto simple_esc;
            /* fallthrough */
        case '"':
            /* fallthrough */
        case '\\':
            /* fallthrough */
        case '$':
            /* fallthrough */
        case '@':
            /* fallthrough */
            /* handle simple escapes */
        simple_esc:
            CLEAR_PLAIN_START(enc,plain_start,plain_end);
            BUF_SIZE_ASSERT(enc,2);         /* max size of a special escape including null*/
            *enc->pos++= '\\';
            *enc->pos++= cp;
            scan++;
            has_escapes= 1;
            break;
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 11:
        case 14:
        case 15:
        case 16:
        case 17:
        case 18:
        case 19:
        case 20:
        case 21:
        case 22:
        case 23:
        case 24:
        case 25:
        case 26:
        case 28:
        case 29:
        case 30:
        case 31:
            goto octal;
            break; /* not reached */
        case ' ':
        case '!':
        case '#':
        case '%':
        case '&':
        case '\'':
        case '(':
        case ')':
        case '*':
        case '+':
        case ',':
        case '-':
        case '.':
        case '/':
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case ':':
        case ';':
        case '<':
        case '=':
        case '>':
        case '?':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
        case '[':
        case ']':
        case '^':
        case '_':
        case '`':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case '{':
        case '|':
        case '}':
        case '~':
        case 127:
            if (!plain_start)
                plain_start= scan;
            plain_end= ++scan;
            break;
        default:
            if ( is_utf8 ) {
                has_escapes= 1;
                CLEAR_PLAIN_START(enc,plain_start,plain_end);
                // cp=  Perl_utf8_to_uvchr_buf(aTHX_ scan, scan_end, &ulen);
                cp= Perl_utf8_to_uvchr(aTHX_ (U8 *)scan, &ulen);
                scan += ulen;
                BUF_SIZE_ASSERT(enc,21); /* max size of a hex value of an escape (assume \x{FEDCBA9876543210} is possible) including null*/
                ulen= sprintf(enc->pos,"\\x{%"UVxf"}",cp); /* no need for snprintf here IMO, if the the size assert is right */
                enc->pos += ulen;
            } else {
              octal:
                has_escapes= 1;
                CLEAR_PLAIN_START(enc,plain_start,plain_end);
                scan++;
                BUF_SIZE_ASSERT(enc,5); /* max size of an octal value (\001) including null*/
                if (scan >= scan_end || *scan < '0' || *scan > '7') {
                    ulen= sprintf(enc->pos,"\\%"UVof,cp);
                    enc->pos += ulen;
                } else {
                    ulen= sprintf(enc->pos,"\\%03"UVof,cp);
                    enc->pos += ulen;
                }
            }
        }
    }
    CLEAR_PLAIN_START(enc,plain_start,plain_end);
    if (has_escapes) {
        srl_buf_cat_char(enc,'"');
    } else {
        srl_buf_cat_char(enc,'\'');
        enc->buf_start[quote_ofs]='\'';
    }
}


