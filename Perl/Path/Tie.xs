#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include <assert.h>
#include "srl_common.h"
#include "srl_iterator.h"

/* this SHOULD be newSV_type(SVt_NULL) but newSV(0) is faster :-( */
#if 1
#   define FRESH_SV() newSV(0)
#else
#   define FRESH_SV() newSV_type(SVt_NULL);
#endif

typedef struct sereal_iterator_tied sereal_iterator_tied_t;
typedef struct sereal_iterator_tied_hash sereal_iterator_tied_hash_t;
typedef struct sereal_iterator_tied_array sereal_iterator_tied_array_t;

typedef srl_iterator_t * Sereal__Path__Iterator;
typedef struct sereal_iterator_tied_hash *Sereal__Path__Tie__Hash;
typedef struct sereal_iterator_tied_array *Sereal__Path__Tie__Array;

struct sereal_iterator_tied {
    srl_iterator_t *iter;   // it's assumed that iter_sv owns iter
    SV *iter_sv;
    IV depth;
    U32 count;
};

// same memory layout as in sereal_iterator_tied
struct sereal_iterator_tied_array {
    srl_iterator_t *iter;
    SV *iter_sv;
    IV depth;
    U32 count;
    AV *store;
};

// same memory layout as in sereal_iterator_tied
struct sereal_iterator_tied_hash {
    srl_iterator_t *iter;
    SV *iter_sv;
    IV depth;
    U32 count;
    I32 cur_idx;
    HV *store;
};

SRL_STATIC_INLINE void
srl_tie_goto_depth_and_maybe_copy_iterator(pTHX_ sereal_iterator_tied_t *this)
{
    /* srl_iterator_t *iter = this->iter; */
    /* IV current_depth = srl_iterator_stack_depth(aTHX_ iter); */

    /* if (current_depth > this->depth) { */
        /* if (SvREFCNT(this->iter_sv) > 1) { */
            /* iter = NULL; */
            /* Newx(iter, 1, srl_iterator_t); */
            /* if (iter == NULL) croak("Out of memory"); */

            /* srl_shallow_copy_iterator(aTHX_ this->iter, iter); */
            /* SvREFCNT_dec(this->iter_sv); */

            /* this->iter = iter; */
            /* this->iter_sv = sv_setref_pv(FRESH_SV(), */
                                         /* "Sereal::Path::Iterator", */
                                         /* (void*) iter); */
        /* } */

        /* current_depth = srl_iterator_unite(aTHX_ iter); */
        /* assert(current_depth == this->depth); */
    /* } else if (expect_false(current_depth < this->depth)) { */
        /* croak("Corrupted state! current_depth < this->depth (%"IVdf" < %"IVdf")", */
              /* current_depth, this->depth); */
    /* } */
}

SRL_STATIC_INLINE SV *
srl_tie_new_tied_sv(pTHX_ srl_iterator_t *iter, SV *iter_sv)
{
    IV depth;
    UV count;
    SV *obj, *result;
    const char* tied_class_name;
    sereal_iterator_tied_t *tied;
    UV type = srl_iterator_object_info(aTHX_ iter, &count);

    switch (type) {
        case SRL_ITERATOR_OBJ_IS_SCALAR:
            return srl_iterator_decode(aTHX_ iter);

        case SRL_ITERATOR_OBJ_IS_ARRAY: {
            sereal_iterator_tied_array_t *array = NULL;
            Newx(array, 1, sereal_iterator_tied_array_t);
            if (!array) croak("Out of memory");
            array->store = NULL;

            tied = (sereal_iterator_tied_t*) array;
            tied_class_name = "Sereal::Path::Tie::Array";
            tied->count = count;
            result = sv_2mortal(newRV_noinc((SV*) newAV()));
            break;
        }

        case SRL_ITERATOR_OBJ_IS_HASH: {
            sereal_iterator_tied_hash_t *hash = NULL;
            Newx(hash, 1, sereal_iterator_tied_hash_t);
            if (!hash) croak("Out of memory");

            hash->store = NULL;
            tied = (sereal_iterator_tied_t*) hash;
            tied_class_name = "Sereal::Path::Tie::Hash";
            tied->count = count * 2; // for proper iterating
            result = sv_2mortal(newRV_noinc((SV*) newHV()));
            break;
        }

        default:
            croak("Expect to have ARRAY or HASH in iterator but got type '%"UVuf"'", type);
    }

    {
        // copy iterator logic
        tied->iter = NULL;
        Newx(tied->iter, 1, srl_iterator_t);
        if (tied->iter == NULL) croak("Out of memory");
        tied->iter_sv = sv_setref_pv(FRESH_SV(),
                                     "Sereal::Path::Iterator",
                                     (void*) tied->iter);

        srl_shallow_copy_iterator(aTHX_ iter, tied->iter);
    }

    obj = sv_2mortal(sv_setref_pv(FRESH_SV(), tied_class_name, tied));
    sv_magic(SvRV(result), obj, PERL_MAGIC_tied, NULL, 0);

    depth = srl_iterator_disjoin(aTHX_ tied->iter); // mark current possion as new root
    srl_iterator_step_in(aTHX_ tied->iter, 1);
    tied->depth = depth + 1;

    assert(srl_iterator_stack_depth(aTHX_ tied->iter) == tied->depth);
    return result;
}

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie
PROTOTYPES: DISABLE

SV *
parse(src)
    SV *src;
  PREINIT:
    SV *iter_sv;
    srl_iterator_t *iter;
  PPCODE:
    if (SvTYPE(src) >= SVt_PVAV)
        croak("Argument must be a SCALAR");

    iter = srl_build_iterator_struct(aTHX_ NULL);
    iter_sv = sv_setref_pv(FRESH_SV(), "Sereal::Path::Iterator", (void*) iter);
    iter_sv = sv_2mortal(iter_sv);

    srl_iterator_set(aTHX_ iter, src);
    ST(0) = srl_tie_new_tied_sv(aTHX_ iter, iter_sv);
    XSRETURN(1);

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie::Array
PROTOTYPES: DISABLE

void
DESTROY(this)
    sereal_iterator_tied_array_t *this;
  CODE:
    SvREFCNT_dec(this->iter_sv);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_array_t *this;
    I32 key;
  PREINIT:
    IV idx;
    SV **svptr;
  PPCODE:
    if (this->store != NULL) {
        svptr = av_fetch(this->store, key, 0);
        if (svptr) {
            ST(0) = sv_2mortal(SvREFCNT_inc(*svptr));
            XSRETURN(1);
        }
    }

    srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);

    idx = srl_iterator_array_exists(aTHX_ this->iter, key);
    if (idx == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        if (idx < srl_iterator_stack_index(aTHX_ this->iter)) {
            srl_iterator_rewind(aTHX_ this->iter, 0);
        }
    
        srl_iterator_array_goto(aTHX_ this->iter, key);
        ST(0) = srl_tie_new_tied_sv(aTHX_ this->iter, this->iter_sv);
    }

    XSRETURN(1);

void
FETCHSIZE(this)
    sereal_iterator_tied_array_t *this;
  PPCODE:
    ST(0) = sv_2mortal(newSVuv(this->count));
    XSRETURN(1);

void
EXISTS(this, key)
    sereal_iterator_tied_array_t *this;
    I32 key;
  PREINIT:
    IV result;
  PPCODE:
    srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);
    srl_iterator_rewind(aTHX_ this->iter, 0);
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
    SvREFCNT_dec(this->iter_sv);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PREINIT:
    HE *he;
  PPCODE:
    if (this->store != NULL) {
        he = hv_fetch_ent(this->store, key, 0, 0);
        if (he) {
            ST(0) = sv_2mortal(SvREFCNT_inc(HeVAL(he)));
            XSRETURN(1);
        }
    }

    srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);
    srl_iterator_rewind(aTHX_ this->iter, 0);

    if (srl_iterator_hash_exists_sv(aTHX_ this->iter, key) == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        ST(0) = srl_tie_new_tied_sv(aTHX_ this->iter, this->iter_sv);
    }

    XSRETURN(1);

void
EXISTS(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PPCODE:
    if (this->store != NULL && hv_exists_ent(this->store, key, 0)) {
        ST(0) = &PL_sv_yes;
        XSRETURN(1);
    }

    srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);
    srl_iterator_rewind(aTHX_ this->iter, 0);

    ST(0) = srl_iterator_hash_exists_sv(aTHX_ this->iter, key) == SRL_ITER_NOT_FOUND
          ? &PL_sv_undef
          : &PL_sv_yes;

    XSRETURN(1);

void
FIRSTKEY(this)
    sereal_iterator_tied_hash_t *this;
  PPCODE:
    srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);
    ST(0) = &PL_sv_undef;
    if (this->store != NULL && HvUSEDKEYS(this->store) > 0) {
        (void) hv_iterinit(this->store);
        ST(0) = hv_iterkeysv(hv_iternext(this->store));
        this->cur_idx = -1; //indication that we should try to fetch next key from the store
    }

    if (ST(0) == &PL_sv_undef) {
        this->cur_idx = 0;
        if (this->count == 0) {
            ST(0) = &PL_sv_undef;
        } else {
            srl_iterator_rewind(aTHX_ this->iter, 0);
            ST(0) = srl_iterator_hash_key_sv(aTHX_ this->iter);
        }
    }
    XSRETURN(1);

void
NEXTKEY(this, last)
    sereal_iterator_tied_hash_t *this;
    SV *last;
  PREINIT:
    HE *he;
  PPCODE:
    ST(0) = &PL_sv_undef;
    if (this->cur_idx < 0) {
        if ((he = hv_iternext(this->store)) != NULL) {
            ST(0) = hv_iterkeysv(he);
        }
    }

    while (ST(0) == &PL_sv_undef && (this->cur_idx < 0 || this->cur_idx < this->count)) {
        if (this->cur_idx < 0) {
            this->cur_idx = 0;
            srl_iterator_rewind(aTHX_ this->iter, 0);
            ST(0) = srl_iterator_hash_key_sv(aTHX_ this->iter);
        } else {
            this->cur_idx += 2;
            if (this->cur_idx >= this->count) {
                ST(0) = &PL_sv_undef;
            } else {
                srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);

                if (this->cur_idx < srl_iterator_stack_index(aTHX_ this->iter)) {
                    srl_iterator_rewind(aTHX_ this->iter, 0);
                }

                srl_iterator_until(aTHX_ this->iter, this->depth, this->cur_idx);
                ST(0) = srl_iterator_hash_key_sv(aTHX_ this->iter);
            }
        }
        if (ST(0) != &PL_sv_undef && this->store != NULL && hv_exists_ent(this->store, ST(0), 0)) {
            ST(0) = &PL_sv_undef;
            continue;
        }
    }

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
