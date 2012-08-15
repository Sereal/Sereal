#include "srl_encoder.h"

#include "ppport.h"

#define PERL_NO_GET_CONTEXT

#include "ptable.h"
#include "srl_buffer.h"

/* General 'config' constants */
#define INITIALIZATION_SIZE 16384
#define MAX_DEPTH 10000

/* three extra for rounding, sign, and end of string */
#define IVUV_MAXCHARS (sizeof (UV) * CHAR_BIT * 28 / 93 + 3)

/* define options */
#define F_UNDEF_BLESSED                 1UL
#define F_DISALLOW_MULTI_OCCURRENCE     2UL
#define F_DUMP_OBJECTS_AS_UNBLESSED     4UL
#define F_DUMP_OBJECTS_AS_BLESSED       8UL

/* some static function declarations */
static void srl_dump_rv(pTHX_ srl_encoder_t *enc, SV *src);
static void srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src);
static void srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src);
static void srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src);
static void srl_dump_pv(pTHX_ srl_encoder_t *enc, const char* src, STRLEN src_len, int is_utf8);

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
  SV **svp;

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
    if ( (svp = hv_fetchs(opt, "undef_blessed", 0)) && SvTRUE(*svp))
      enc->flags |= F_UNDEF_BLESSED;
    if ( (svp = hv_fetchs(opt, "disallow_multi", 0)) && SvTRUE(*svp))
      enc->flags |= F_DISALLOW_MULTI_OCCURRENCE;
    if ( (svp = hv_fetchs(opt, "objects_as_unblessed", 0)) && SvTRUE(*svp))
      enc->flags |= F_DUMP_OBJECTS_AS_UNBLESSED;
    if ( (svp = hv_fetchs(opt, "dump_objects", 0)) && SvTRUE(*svp))
      enc->flags |= F_DUMP_OBJECTS_AS_BLESSED;
  }
  /* option vlaidation */
  /* FIXME my bit field fu is weak, apparently. Needs replacing with proper idiom */
  if (   (enc->flags & F_UNDEF_BLESSED ? 1 : 0)
       + (enc->flags & F_DUMP_OBJECTS_AS_UNBLESSED ? 1 : 0)
       + (enc->flags & F_DUMP_OBJECTS_AS_BLESSED ? 1 : 0)
       > 1)
  {
    croak("Can only have one of 'undef_blessed', "
          "'objects_as_unblessed', and 'dump_objects' options at a time.");
  }

  return enc;
}



/* Entry point for serialization. Dumps generic SVs and delegates
 * to more specialized functions for RVs, etc. */
void
srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src)
{
  SvGETMAGIC(src);

  /* dump strings */
  if (SvPOKp(src)) {
    STRLEN len;
    char *str = SvPV(src, len);
    BUF_SIZE_ASSERT(enc, 2 + len);
    srl_dump_pv(aTHX_ enc, str, len, SvUTF8(src));
  }
  /* dump floats */
  else if (SvNOKp(src)) {
    BUF_SIZE_ASSERT(enc, NV_DIG + 32);
    Gconvert(SvNVX(src), NV_DIG, 0, enc->pos);
    enc->pos += strlen(enc->pos);
  }
  /* dump ints */
  else if (SvIOKp(src)) {
    /* we assume we can always read an IV as a UV and vice versa
     * we assume two's complement
     * we assume no aliasing issues in the union */
    if (SvIsUV(src) ? SvUVX(src) <= 59000
                    : SvIVX(src) <= 59000 && SvIVX(src) >= -59000)
    {
      /* optimise the "small number case"
       * code will likely be branchless and use only a single multiplication
       * works for numbers up to 59074 */
      I32 i = SvIVX(src);
      U32 u;
      char digit, nz = 0;

      BUF_SIZE_ASSERT(enc, 6);

      *enc->pos = '-'; enc->pos += i < 0 ? 1 : 0;
      u = i < 0 ? -i : i;

      /* convert to 4.28 fixed-point representation */
      u *= ((0xfffffff + 10000) / 10000); /* 10**5, 5 fractional digits */

      /* now output digit by digit, each time masking out the integer part
       * and multiplying by 5 while moving the decimal point one to the right,
       * resulting in a net multiplication by 10.
       * we always write the digit to memory but conditionally increment
       * the pointer, to enable the use of conditional move instructions. */
      digit = u >> 28; *enc->pos = digit + '0'; enc->pos += (nz = nz || digit); u = (u & 0xfffffffUL) * 5;
      digit = u >> 27; *enc->pos = digit + '0'; enc->pos += (nz = nz || digit); u = (u & 0x7ffffffUL) * 5;
      digit = u >> 26; *enc->pos = digit + '0'; enc->pos += (nz = nz || digit); u = (u & 0x3ffffffUL) * 5;
      digit = u >> 25; *enc->pos = digit + '0'; enc->pos += (nz = nz || digit); u = (u & 0x1ffffffUL) * 5;
      digit = u >> 24; *enc->pos = digit + '0'; enc->pos += 1; /* correctly generate '0' */
    }
    else {
      /* large integer, use the (rather slow) snprintf way. */
      BUF_SIZE_ASSERT(enc, IVUV_MAXCHARS);
      enc->pos +=
         SvIsUV(src)
            ? snprintf(enc->pos, IVUV_MAXCHARS, "%"UVuf, (UV)SvUVX(src))
            : snprintf(enc->pos, IVUV_MAXCHARS, "%"IVdf, (IV)SvIVX(src));
    }
  } /* end is an integer */
  /* undef */
  else if (!SvOK(src)) {
    srl_buf_cat_str_s(enc, "undef");
  }
  /* dump references */
  else if (SvROK(src))
    srl_dump_rv(aTHX_ enc, SvRV(src));
  else {
    croak("Attempting to dump unsupported or invalid SV");
  }
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
  }

  if (SvOBJECT(src) && (0 == (enc->flags & F_DUMP_OBJECTS_AS_UNBLESSED))) {
    if (enc->flags & F_UNDEF_BLESSED) {
      srl_buf_cat_str_s(enc, "undef");
      goto done;
    }
    else if (enc->flags & F_DUMP_OBJECTS_AS_BLESSED) {
      srl_buf_cat_str_s(enc, "bless(");
      blessed_object = 1;
    }
    else
      croak("Encountered object '%s', but undef_blessed setting is not enabled",
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
   *  srl_dump_pv(aTHX_ enc, "null", 4, 0);
   */
  else {
    croak("found %s, but it is not representable by Data::Dumper::Limited serialization",
           SvPV_nolen(sv_2mortal(newRV_inc(src))));
  }

done:
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
  if (!(enc->flags & F_DISALLOW_MULTI_OCCURRENCE)) {
    PTABLE_delete(enc->seenhash, src);
  }
}


static void
srl_dump_av(pTHX_ srl_encoder_t *enc, AV *src)
{
  UV i, n;
  SV **svp;

  n = av_len(src)+1;
  if (n == 0) {
    srl_buf_cat_str_s(enc, "[]");
    return;
  }

  srl_buf_cat_char(enc, '[');

  svp = av_fetch(src, 0, 0);
  if (svp == NULL)
    croak("Got NULL SV from av_fetch");
  srl_dump_sv(aTHX_ enc, *svp);

  for (i = 1; i < n; ++i) {
    srl_buf_cat_char(enc, ',');
    svp = av_fetch(src, i, 0);
    if (svp != NULL)
      srl_dump_sv(aTHX_ enc, *svp);
    else
      srl_buf_cat_str_s(enc, "undef");
  }

  srl_buf_cat_char(enc, ']');
}


static void
srl_dump_hv(pTHX_ srl_encoder_t *enc, HV *src)
{
  HE *he;
  srl_buf_cat_char(enc, '{');
  if (hv_iterinit(src) || SvMAGICAL(src)) {
    if ((he = hv_iternext(src))) {
      for (;;) {
        srl_dump_hk(aTHX_ enc, he);
        srl_buf_cat_char(enc, ','); /* see comments in srl_dump_hk */
        srl_dump_sv(aTHX_ enc, SvMAGICAL(src) ? hv_iterval(src, he) : HeVAL(he));

        if (!(he = hv_iternext(src)))
          break;

        srl_buf_cat_char(enc, ',');
      }
    }
  }
  srl_buf_cat_char(enc, '}');
}


static void
srl_dump_hk(pTHX_ srl_encoder_t *enc, HE *src)
{
  /* FIXME we could scan the string to see whether we could
   *       skip quoting the string and instead using a fat comma.
   *       But that's a lot of extra coding work, potentially slow,
   *       and a small gain.
   *       Even if that's not done, we can always use the fat comma
   *       for readability. Maybe make that configurable later? */
  if (HeKLEN(src) == HEf_SVKEY) {
    SV *sv = HeSVKEY(src);
    STRLEN len;
    char *str;

    SvGETMAGIC(sv);
    str = SvPV(sv, len);

    srl_dump_pv(aTHX_ enc, str, len, SvUTF8(sv));
  }
  else {
    srl_dump_pv(aTHX_ enc, HeKEY(src), HeKLEN(src), HeKUTF8(src));
  }
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

