#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include <assert.h>
#include "srl_common.h"
#include "srl_iterator.h"

// TODO use autofifivy off

/* this SHOULD be newSV_type(SVt_NULL) but newSV(0) is faster :-( */
#if 1
#   define FRESH_SV() newSV(0)
#else
#   define FRESH_SV() newSV_type(SVt_NULL);
#endif

typedef struct sereal_iterator_tied sereal_iterator_tied_t;
typedef struct sereal_iterator_tied_hash sereal_iterator_tied_hash_t;
typedef struct sereal_iterator_tied_array sereal_iterator_tied_array_t;
typedef struct sereal_iterator_tied_scalar sereal_iterator_tied_scalar_t;

typedef srl_iterator_t * Sereal__Path__Iterator;
typedef struct sereal_iterator_tied_hash *Sereal__Path__Tie__Hash;
typedef struct sereal_iterator_tied_array *Sereal__Path__Tie__Array;
typedef struct sereal_iterator_tied_scalar *Sereal__Path__Tie__Scalar;

struct sereal_iterator_tied {
    srl_iterator_t *iter;   // it's assumed that iter_sv owns iter
    IV depth;
    U32 count;
};

// same memory layout as in sereal_iterator_tied
struct sereal_iterator_tied_scalar {
    srl_iterator_t *iter;
    IV depth;
    U32 count;
    SV *store; // internal storage to workaround autovivification
};

// same memory layout as in sereal_iterator_tied
struct sereal_iterator_tied_array {
    srl_iterator_t *iter;
    IV depth;
    U32 count;
    AV *store; // internal storage to workaround autovivification
};

// same memory layout as in sereal_iterator_tied
struct sereal_iterator_tied_hash {
    srl_iterator_t *iter;
    IV depth;
    U32 count;
    I32 cur_idx;
    HV *store; // internal storage to workaround autovivification
};

SRL_STATIC_INLINE SV *
srl_tie_new_tied_sv(pTHX_ srl_iterator_t *iter)
{
    int how;
    IV depth;
    UV count;
    SV *tie, *result;
    const char* tied_class_name;
    sereal_iterator_tied_t *tied;
    U32 type = srl_iterator_info(aTHX_ iter, &count, NULL, NULL);

    if ((type & SRL_ITERATOR_INFO_REF_TO) == 0) {
        return srl_iterator_decode(aTHX_ iter);
    }

    if ((type & SRL_ITERATOR_INFO_HASH) == SRL_ITERATOR_INFO_HASH) {
        sereal_iterator_tied_hash_t *hash = NULL;
        Newx(hash, 1, sereal_iterator_tied_hash_t);
        if (!hash) croak("Out of memory");

        hash->store = NULL;
        tied = (sereal_iterator_tied_t*) hash;
        tied_class_name = "Sereal::Path::Tie::Hash";
        tied->count = count * 2; // for proper iterating
        result = sv_2mortal(newRV_noinc((SV*) newHV()));
        how = PERL_MAGIC_tied;
    } else if ((type & SRL_ITERATOR_INFO_ARRAY) == SRL_ITERATOR_INFO_ARRAY) {
        sereal_iterator_tied_array_t *array = NULL;
        Newx(array, 1, sereal_iterator_tied_array_t);
        if (!array) croak("Out of memory");
        array->store = NULL;

        tied = (sereal_iterator_tied_t*) array;
        tied_class_name = "Sereal::Path::Tie::Array";
        tied->count = count;
        result = sv_2mortal(newRV_noinc((SV*) newAV()));
        how = PERL_MAGIC_tied;
    } else if ((type & SRL_ITERATOR_INFO_REF_TO) == SRL_ITERATOR_INFO_REF_TO) {
        sereal_iterator_tied_scalar_t *scalar = NULL;
        Newx(scalar, 1, sereal_iterator_tied_scalar_t);
        if (!scalar) croak("Out of memory");
        scalar->store = NULL;

        tied = (sereal_iterator_tied_t*) scalar;
        tied_class_name = "Sereal::Path::Tie::Scalar";
        tied->count = count;
        result = sv_2mortal(newRV_noinc(FRESH_SV()));
        how = PERL_MAGIC_tiedscalar;
    } else {
        return srl_iterator_decode(aTHX_ iter);
    }

    {
        // copy iterator logic
        // TODO fix potential memleaks, how to free memory if srl_shallow_copy_iterator() croaks????
        // TODO get remove of unnesseccary copying
        tied->iter = NULL;
        Newx(tied->iter, 1, srl_iterator_t);
        if (tied->iter == NULL) croak("Out of memory");
        srl_shallow_copy_iterator(aTHX_ iter, tied->iter);
    }

    /* 
     * (1) creates new RV pointing to SV
     * (2) blesses SV to tied_class_name and store srl_iterator_tied_t* into SV
     * (3) mortalizes SV becase sv_magic() increas counter:
     * http://perldoc.perl.org/perlguts.html#Assigning-Magic
     * The obj argument is stored in the mg_obj field of the MAGIC structure.
     * If it is not the same as the sv argument, the reference count of the obj
     * object is incremented. If it is the same, or if the how argument is
     * PERL_MAGIC_arylen , or if it is a NULL pointer, then obj is merely
     * stored, without the reference count being incremented.
     * (4) adds magic referenced by tie to result SV
     */

    tie = newRV_noinc(FRESH_SV());                  // (1)
    tie = sv_setref_pv(tie, tied_class_name, tied); // (2)
    tie = sv_2mortal(tie);                          // (3)
    sv_magic(SvRV(result), tie, how, NULL, 0);      // (4)

    srl_iterator_step_in(aTHX_ tied->iter, 1);
    tied->depth = srl_iterator_stack_depth(aTHX_ tied->iter);
    return result;
}

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie
PROTOTYPES: DISABLE

SV *
new(CLASS, src)
    const char *CLASS;
    SV *src
  PREINIT:
    srl_iterator_t *iter;
  PPCODE:
    if (   !sv_isobject(src)
        || !sv_isa(src, "Sereal::Path::Iterator")
        || SvTYPE(SvRV(src)) != SVt_PVMG)
    {
        warn("Sereal::Path::Iterator::new() -- src is not "
             "a blessed 'Sereal::Path::Iterator' SV reference");
        XSRETURN_UNDEF;
    }

    iter = INT2PTR(srl_iterator_t*, SvIV((SV*) SvRV(src)));
    ST(0) = srl_tie_new_tied_sv(aTHX_ iter);
    XSRETURN(1);

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie::Scalar
PROTOTYPES: DISABLE

void
DESTROY(this)
    sereal_iterator_tied_scalar_t *this;
  CODE:
    if (this->store != NULL)
        SvREFCNT_dec(this->store);
    if (this->iter != NULL)
        srl_destroy_iterator(aTHX_ this->iter);
    Safefree(this);

void
FETCH(this)
    sereal_iterator_tied_scalar_t *this;
  PPCODE:
    if (this->store == NULL) {
        ST(0) = srl_tie_new_tied_sv(aTHX_ this->iter);
    } else {
        ST(0) = sv_2mortal(SvREFCNT_inc(this->store));
    }

    XSRETURN(1);

void
STORE(this, value)
    sereal_iterator_tied_scalar_t *this;
    SV *value;
  CODE:
    if (this->store != NULL)
        SvREFCNT_dec(this->store);

    this->store = value;
    SvREFCNT_inc(value);

void
UNTIE(this)
  CODE:
    croak("UNTIE is not supported");

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie::Array
PROTOTYPES: DISABLE

void
DESTROY(this)
    sereal_iterator_tied_array_t *this;
  CODE:
    if (this->store != NULL)
        SvREFCNT_dec((SV*) this->store);
    if (this->iter != NULL)
        srl_destroy_iterator(aTHX_ this->iter);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_array_t *this;
    I32 key;
  PREINIT:
    IV idx;
    SV **svptr;
  PPCODE:
    if (this->store != NULL && (svptr = av_fetch(this->store, key, 0)) != NULL) {
        ST(0) = sv_2mortal(SvREFCNT_inc(*svptr));
        XSRETURN(1);
    }

    idx = srl_iterator_array_exists(aTHX_ this->iter, key);
    if (idx == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        srl_iterator_array_goto(aTHX_ this->iter, key);
        ST(0) = srl_tie_new_tied_sv(aTHX_ this->iter);
    }

    XSRETURN(1);

void
FETCHSIZE(this)
    sereal_iterator_tied_array_t *this;
  PREINIT:
    U32 len;
    U32 avlen;
  PPCODE:
    avlen = (U32) (this->store != NULL ? av_len(this->store) + 1 : 0);
    len = (avlen > this->count ? avlen : this->count);
    ST(0) = sv_2mortal(newSVuv(len));
    XSRETURN(1);

void
EXISTS(this, key)
    sereal_iterator_tied_array_t *this;
    I32 key;
  PREINIT:
    IV result;
  PPCODE:
    if (this->store != NULL && av_exists(this->store, (SSize_t) key) != 0) {
        ST(0) = &PL_sv_yes;
        XSRETURN(1);
    }

    result = srl_iterator_array_exists(aTHX_ this->iter, key);
    ST(0) = (result == SRL_ITER_NOT_FOUND ? &PL_sv_undef : &PL_sv_yes);
    XSRETURN(1);

void
STORE(this, key, value)
    sereal_iterator_tied_array_t *this;
    I32 key;
    SV *value;
  CODE:
    if (this->store == NULL)
        this->store = newAV();

    av_store(this->store, key, value);
    SvREFCNT_inc(value);

void
STORESIZE(this, count)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
EXTEND(this, count)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
DELETE(this, key)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
CLEAR(this)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
PUSH(this, ...)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
POP(this)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
SHIFT(this)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
UNSHIFT(this, ...)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
SPLICE(this, offset, length, LIST)
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

void
UNTIE(this)
  CODE:
    croak("UNTIE is not supported");

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie::Hash
PROTOTYPES: DISABLE

void
DESTROY(this)
    sereal_iterator_tied_hash_t *this;
  CODE:
    if (this->store != NULL)
        SvREFCNT_dec((SV*) this->store);
    if (this->iter != NULL)
        srl_destroy_iterator(aTHX_ this->iter);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PREINIT:
    HE *he;
    const char *keyname;
    STRLEN keyname_length;
  PPCODE:
    if (this->store != NULL) {
        if ((he = hv_fetch_ent(this->store, key, 0, 0)) != NULL) {
            ST(0) = sv_2mortal(SvREFCNT_inc(HeVAL(he)));
            XSRETURN(1);
        }
    }

    keyname = SvPV(key, keyname_length);
    if (srl_iterator_hash_exists(aTHX_ this->iter, keyname, keyname_length) == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        ST(0) = srl_tie_new_tied_sv(aTHX_ this->iter);
    }

    XSRETURN(1);

void
EXISTS(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PREINIT:
    const char *keyname;
    STRLEN keyname_length;
  PPCODE:
    if (this->store != NULL && hv_exists_ent(this->store, key, 0)) {
        ST(0) = &PL_sv_yes;
        XSRETURN(1);
    }

    keyname = SvPV(key, keyname_length);
    ST(0) = srl_iterator_hash_exists(aTHX_ this->iter, keyname, keyname_length) == SRL_ITER_NOT_FOUND
          ? &PL_sv_undef
          : &PL_sv_yes;

    XSRETURN(1);

void
FIRSTKEY(this)
    sereal_iterator_tied_hash_t *this;
  PREINIT:
    const char *keyname;
    STRLEN keyname_length;
  PPCODE:
    if (this->store != NULL && HvUSEDKEYS(this->store) > 0) {
        this->cur_idx = -1; //indication that we should try to fetch next key from the store

        (void) hv_iterinit(this->store);
        ST(0) = hv_iterkeysv(hv_iternext(this->store));
        XSRETURN(1);
    }

    if (this->count > 0) {
        srl_iterator_rewind(aTHX_ this->iter, 0);
        srl_iterator_hash_key(aTHX_ this->iter, &keyname, &keyname_length);
        this->cur_idx = 1; // following call of NEXTKEY will set it to 2
        ST(0) = sv_2mortal(newSVpvn(keyname, keyname_length));
        XSRETURN(1);
    }

    ST(0) = &PL_sv_undef;
    XSRETURN(1);

void
NEXTKEY(this, last)
    sereal_iterator_tied_hash_t *this;
    SV *last;
  PREINIT:
    HE *he;
    U32 stack_idx;
    const char *keyname;
    STRLEN keyname_length;
  PPCODE:
    if (this->cur_idx < 0 && (he = hv_iternext(this->store)) != NULL) {
        ST(0) = hv_iterkeysv(he);
        XSRETURN(1);
    }

    assert(this->depth == srl_iterator_stack_depth(aTHX_ this->iter));
    for (this->cur_idx += 1; this->cur_idx < (I32) this->count; this->cur_idx += 1) {
        stack_idx = srl_iterator_stack_index(aTHX_ this->iter);
        if (this->cur_idx < (I32) stack_idx) {
            srl_iterator_rewind(aTHX_ this->iter, 0);
            stack_idx = 0;
        }

        srl_iterator_next(aTHX_ this->iter, this->cur_idx - stack_idx);
        srl_iterator_hash_key(aTHX_ this->iter, &keyname, &keyname_length);
        ST(0) = sv_2mortal(newSVpvn(keyname, keyname_length));
        this->cur_idx += 1;

        if (this->store == NULL || hv_exists_ent(this->store, ST(0), 0) == 0) {
            XSRETURN(1);
        }
    }

    ST(0) = &PL_sv_undef;
    XSRETURN(1);

void
SCALAR(this)
    sereal_iterator_tied_hash_t *this;
  PPCODE:
    if (this->count > 0) {
        ST(0) = sv_2mortal(newSVuv(1));
    } else if (this->store == NULL) {
        ST(0) = sv_2mortal(newSVuv(0));
    } else {
        ST(0) = hv_scalar(this->store);
    }
    XSRETURN(1);

void
STORE(this, key, value)
    sereal_iterator_tied_hash_t *this;
    SV *key;
    SV *value;
  CODE:
    if (this->store == NULL)
        this->store = newHV();

    hv_store_ent(this->store, key, value, 0);
    SvREFCNT_inc(value);

void
DELETE(this, key)
  CODE:
    croak("Tied to Sereal::Path::Tie::Hash hash is read-only");

void
CLEAR(this)
  CODE:
    croak("Tied to Sereal::Path::Tie::Hash hash is read-only");

void
UNTIE(this)
  CODE:
    croak("UNTIE is not supported");
