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
};

// same memory layout as in sereal_iterator_tied
struct sereal_iterator_tied_hash {
    srl_iterator_t *iter;
    SV *iter_sv;
    IV depth;
    U32 count;
    I32 last_idx;
};

SRL_STATIC_INLINE void
srl_tie_goto_depth_and_maybe_copy_iterator(pTHX_ sereal_iterator_tied_t *this)
{
    srl_iterator_t *iter = this->iter;
    IV current_depth = srl_iterator_stack_depth(aTHX_ iter);

    if (current_depth > this->depth) {
        if (SvREFCNT(this->iter_sv) > 1) {
            iter = NULL;
            Newx(iter, 1, srl_iterator_t);
            if (iter == NULL) croak("Out of memory");

            srl_shallow_copy_iterator(aTHX_ this->iter, iter);
            SvREFCNT_dec(this->iter_sv);

            this->iter = iter;
            this->iter_sv = sv_setref_pv(FRESH_SV(),
                                         "Sereal::Path::Iterator",
                                         (void*) iter);
        }

        current_depth = srl_iterator_unite(aTHX_ iter);
        assert(current_depth == this->depth);
    } else if (expect_false(current_depth < this->depth)) {
        croak("Corrupted state! current_depth < this->depth (%"IVdf" < %"IVdf")",
              current_depth, this->depth);
    }
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

            tied = (sereal_iterator_tied_t*) array;
            tied_class_name = "Sereal::Path::Tie::Array";
            result = sv_2mortal(newRV_noinc((SV*) newAV()));
            break;
        }

        case SRL_ITERATOR_OBJ_IS_HASH: {
            sereal_iterator_tied_hash_t *hash = NULL;
            Newx(hash, 1, sereal_iterator_tied_hash_t);
            if (!hash) croak("Out of memory");

            tied = (sereal_iterator_tied_t*) hash;
            tied_class_name = "Sereal::Path::Tie::Hash";
            result = sv_2mortal(newRV_noinc((SV*) newHV()));
            break;
        }

        default:
            croak("Expect to have ARRAY or HASH in iterator but got type '%"UVuf"'", type);
    }

    tied->iter = iter;
    tied->iter_sv = iter_sv;
    SvREFCNT_inc(iter_sv);

    obj = sv_2mortal(sv_setref_pv(FRESH_SV(), tied_class_name, tied));
    sv_magic(SvRV(result), obj, PERL_MAGIC_tied, NULL, 0);

    depth = srl_iterator_disjoin(aTHX_ iter); // mark current possion as new root
    srl_iterator_step_in(aTHX_ iter, 1);
    tied->depth = depth + 1;
    tied->count = count;

    assert(srl_iterator_stack_depth(aTHX_ iter) == tied->depth);
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
    srl_iterator_stack_ptr stack_ptr;
  PPCODE:
    srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);

    idx = srl_iterator_array_exists(aTHX_ this->iter, key);
    if (idx == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        stack_ptr = srl_iterator_stack(aTHX_ this->iter);
        if (idx > stack_ptr->idx)
            srl_iterator_rewind(aTHX_ this->iter, 0);
    
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
  CODE:
    croak("Tied to Sereal::Path::Tie::Array array is read-only");

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
    SvREFCNT_dec(this->iter_sv);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PREINIT:
  //  char *tmp;
  //  STRLEN len;
  PPCODE:
    // tmp = SvPV(key, len);
    // warn("!!! FETCH %s %d", tmp, SvREFCNT(this->iter_sv));
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
    if (this->count == 0) {
        ST(0) = &PL_sv_undef;
    } else {
        this->last_idx = this->count;
        srl_iterator_rewind(aTHX_ this->iter, 0);
        ST(0) = srl_iterator_hash_key_sv(aTHX_ this->iter);
    }
    XSRETURN(1);

void
NEXTKEY(this, last)
    sereal_iterator_tied_hash_t *this;
    SV *last;
  PREINIT:
    srl_iterator_stack_ptr stack_ptr;
  PPCODE:
    if (this->last_idx <= 2) {
        ST(0) = &PL_sv_undef;
    } else {
        srl_tie_goto_depth_and_maybe_copy_iterator(aTHX_ (sereal_iterator_tied_t*) this);

        this->last_idx -= 2;
        stack_ptr = srl_iterator_stack(aTHX_ this->iter);
        if (this->last_idx > stack_ptr->idx)
            srl_iterator_rewind(aTHX_ this->iter, 0);

        srl_iterator_until(aTHX_ this->iter, this->depth, this->last_idx);
        ST(0) = srl_iterator_hash_key_sv(aTHX_ this->iter);
    }
    XSRETURN(1);

void
SCALAR(this)
    sereal_iterator_tied_hash_t *this;
  PPCODE:
    ST(0) = sv_2mortal(newSVuv(this->count));
    XSRETURN(1);

void
STORE(this, key, value)
  CODE:
    croak("Tied to Sereal::Path::Tie::Hash hash is read-only");

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
