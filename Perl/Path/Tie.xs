#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include <assert.h>
#include "srl_common.h"
#include "srl_iterator.h"

typedef srl_iterator_t * Sereal__Path__Iterator;
typedef struct sereal_iterator_tied_hash *Sereal__Path__Tie__Hash;
typedef struct sereal_iterator_tied_hash sereal_iterator_tied_hash_t;
typedef struct sereal_iterator_tied_array *Sereal__Path__Tie__Array;
typedef struct sereal_iterator_tied_array sereal_iterator_tied_array_t;

struct sereal_iterator_tied_array {
    srl_iterator_t iter;
};

struct sereal_iterator_tied_hash {
    srl_iterator_t iter;
    I32 last_idx;
    IV depth;
};

SRL_STATIC_INLINE SV *
iterator_to_tied_sv(pTHX_ srl_iterator_t *iter)
{
    UV type;
    SV *result;
    SV *hash_obj;
    SV *array_obj;
    sereal_iterator_tied_hash_t *hash = NULL;
    sereal_iterator_tied_array_t *array = NULL;

    type = srl_iterator_object_info(aTHX_ iter, NULL);
    switch (type) {
        case SRL_ITERATOR_OBJ_IS_SCALAR:
            result = srl_iterator_decode(aTHX_ iter);
            break;

        case SRL_ITERATOR_OBJ_IS_ARRAY:
            Newx(array, 1, sereal_iterator_tied_array_t); 
            if (!array) croak("Out of memory");

            srl_shallow_copy_iterator(aTHX_ iter, &array->iter);

            array_obj = sv_2mortal(
                sv_setref_pv(newSV_type(SVt_NULL), "Sereal::Path::Tie::Array", array)
            );

            result = sv_2mortal(newRV_noinc((SV*) newAV()));
            sv_magic(SvRV(result), array_obj, PERL_MAGIC_tied, NULL, 0);

            srl_iterator_disjoin(aTHX_ &array->iter); // mark current possion as new root
            srl_iterator_step_in(aTHX_ &array->iter, 1);
            break;

        case SRL_ITERATOR_OBJ_IS_HASH:
            Newx(hash, 1, sereal_iterator_tied_hash_t); 
            if (!hash) croak("Out of memory");

            srl_shallow_copy_iterator(aTHX_ iter, &hash->iter);

            hash_obj = sv_2mortal(
                sv_setref_pv(newSV_type(SVt_NULL), "Sereal::Path::Tie::Hash", hash)
            );

            result = sv_2mortal(newRV_noinc((SV*) newHV()));
            sv_magic(SvRV(result), hash_obj, PERL_MAGIC_tied, NULL, 0);

            srl_iterator_disjoin(aTHX_ &hash->iter); // mark current possion as new root
            srl_iterator_step_in(aTHX_ &hash->iter, 1);
            hash->depth = srl_iterator_stack_depth(aTHX_ &hash->iter);
            break;

        default:
            croak("Expect to have ARRAY or HASH in iterator but got type '%"UVuf"'", type);
    }

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
    iter_sv = sv_setref_pv(newSV_type(SVt_NULL), "Sereal::Path::Iterator", (void*) iter);
    iter_sv = sv_2mortal(iter_sv);

    srl_iterator_set(aTHX_ iter, src);
    ST(0) = iterator_to_tied_sv(aTHX_ iter);
    XSRETURN(1);

MODULE = Sereal::Path::Tie   PACKAGE = Sereal::Path::Tie::Array
PROTOTYPES: DISABLE

void
DESTROY(this)
    sereal_iterator_tied_array_t *this;
  CODE:
    srl_deinit_iterator(aTHX_ &this->iter);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_array_t *this;
    I32 key;
  PREINIT:
    IV idx;
    srl_iterator_stack_ptr stack_ptr;
  PPCODE:
    idx = srl_iterator_array_exists(aTHX_ &this->iter, key);
    if (idx == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        stack_ptr = srl_iterator_stack(aTHX_ &this->iter);
        if (idx > stack_ptr->idx)
            srl_iterator_step_out(aTHX_ &this->iter, 0);
    
        srl_iterator_array_goto(aTHX_ &this->iter, key);
        ST(0) = iterator_to_tied_sv(aTHX_ &this->iter);
    }

    XSRETURN(1);

void
FETCHSIZE(this)
    sereal_iterator_tied_array_t *this;
  PREINIT:
    UV length_out;
  PPCODE:
    srl_iterator_stack_info(aTHX_ &this->iter, &length_out);
    ST(0) = newSVuv(length_out);
    XSRETURN(1);

void
EXISTS(this, key)
    sereal_iterator_tied_array_t *this;
    I32 key;
  PREINIT:
    IV result;
  PPCODE:
    srl_iterator_step_out(aTHX_ &this->iter, 0); 
    result = srl_iterator_array_exists(aTHX_ &this->iter, key);
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
    srl_deinit_iterator(aTHX_ &this->iter);
    Safefree(this);

void
FETCH(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PPCODE:
    srl_iterator_step_out(aTHX_ &this->iter, 0); 
    if (srl_iterator_hash_exists_sv(aTHX_ &this->iter, key) == SRL_ITER_NOT_FOUND) {
        ST(0) = &PL_sv_undef;
    } else {
        ST(0) = iterator_to_tied_sv(aTHX_ &this->iter);
    }
    XSRETURN(1);

void
EXISTS(this, key)
    sereal_iterator_tied_hash_t *this;
    SV *key;
  PREINIT:
    IV result;
  PPCODE:
    srl_iterator_step_out(aTHX_ &this->iter, 0); 
    result = srl_iterator_hash_exists_sv(aTHX_ &this->iter, key);
    ST(0) = (result == SRL_ITER_NOT_FOUND ? &PL_sv_undef : &PL_sv_yes);
    XSRETURN(1);

void
FIRSTKEY(this)
    sereal_iterator_tied_hash_t *this;
  PREINIT:
    srl_iterator_stack_ptr stack_ptr;
  PPCODE:
    stack_ptr = srl_iterator_stack(aTHX_ &this->iter);
    if (stack_ptr->count == 0) {
        ST(0) = &PL_sv_undef;
    } else {
        this->last_idx = (I32) stack_ptr->count;
        srl_iterator_step_out(aTHX_ &this->iter, 0); 
        ST(0) = srl_iterator_hash_key_sv(aTHX_ &this->iter);
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
        this->last_idx -= 2;
        stack_ptr = srl_iterator_stack(aTHX_ &this->iter);
        if (this->last_idx > stack_ptr->idx)
            srl_iterator_step_out(aTHX_ &this->iter, 0);

        srl_iterator_next_until_depth_and_idx(aTHX_ &this->iter,
                                              this->depth,
                                              this->last_idx);

        ST(0) = srl_iterator_hash_key_sv(aTHX_ &this->iter);
    }
    XSRETURN(1);

void
SCALAR(this)
    sereal_iterator_tied_hash_t *this;
  PREINIT:
    UV length_out;
  PPCODE:
    srl_iterator_stack_info(aTHX_ &this->iter, &length_out);
    ST(0) = newSVuv(length_out);
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
