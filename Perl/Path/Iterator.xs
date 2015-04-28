#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_iterator.h"

typedef srl_iterator_t * Sereal__Path__Iterator;

MODULE = Sereal::Path::Iterator		PACKAGE = Sereal::Path::Iterator
PROTOTYPES: DISABLE

srl_iterator_t *
new(CLASS, src = NULL, opt = NULL)
    char *CLASS;
    SV *src;
    HV *opt;
  CODE:
    RETVAL = srl_build_iterator_struct(aTHX_ opt);
    if (src) srl_iterator_set_document(aTHX_ RETVAL, src);
  OUTPUT: RETVAL

void
DESTROY(iter)
    srl_iterator_t *iter;
  CODE:
    srl_destroy_iterator(aTHX_ iter);

void
set_document(iter, src)
    srl_iterator_t *iter;
    SV *src;
  CODE:
    srl_iterator_set_document(aTHX_ iter, src);

void
reset(iter)
    srl_iterator_t *iter;
  CODE:
    srl_iterator_reset(aTHX_ iter);

UV
eof(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_eof(aTHX_ iter);
  OUTPUT: RETVAL

void
next(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_iterator_next(aTHX_ iter, n ? SvUV(n) : 1);

void
step_in(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_iterator_step_in(aTHX_ iter, n ? SvUV(n) : 1);

void
step_out(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_iterator_step_out(aTHX_ iter, n ? SvUV(n) : 1);

UV
srl_next_until_depth_and_idx(iter, depth, idx)
    srl_iterator_t *iter;
    UV depth;
    U32 idx;
  CODE:
    RETVAL = srl_iterator_next_until_depth_and_idx(aTHX_ iter, depth, idx);
  OUTPUT: RETVAL

UV
offset(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_offset(aTHX_ iter);
  OUTPUT: RETVAL

void
info(iter)
    srl_iterator_t *iter;
  PREINIT:
    UV type;
    UV length;
    SV *str_type;
  PPCODE:
    switch (srl_iterator_object_info(aTHX_ iter, &length)) {
        case SRL_ITERATOR_OBJ_IS_ARRAY:
            str_type = newSVpv("ARRAY", 5);
            break;

        case SRL_ITERATOR_OBJ_IS_HASH:
            str_type = newSVpv("HASH", 4);
            break;

        case SRL_ITERATOR_OBJ_IS_SCALAR:
            str_type = newSVpv("SCALAR", 6);
            break;

        default:
            croak("should not be here");
    }

    EXTEND(SP, 2);
    PUSHs(sv_2mortal(str_type));
    PUSHs(sv_2mortal(newSVuv(length)));

IV
stack_depth(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_stack_depth(aTHX_ iter);
  OUTPUT: RETVAL

UV
stack_index(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_stack_index(aTHX_ iter);
  OUTPUT: RETVAL

void
stack_info(iter)
    srl_iterator_t *iter;
  PREINIT:
    UV type;
    UV length;
    SV *str_type;
  PPCODE:
    switch (srl_iterator_stack_info(aTHX_ iter, &length)) {
        case SRL_ITERATOR_OBJ_IS_ARRAY:
            str_type = newSVpv("ARRAY", 5);
            break;

        case SRL_ITERATOR_OBJ_IS_HASH:
            str_type = newSVpv("HASH", 4);
            break;

        default:
            croak("should not be here");
    }

    EXTEND(SP, 2);
    PUSHs(sv_2mortal(str_type));
    PUSHs(sv_2mortal(newSVuv(length)));

void
array_goto(iter, idx)
    srl_iterator_t *iter;
    IV idx;
  CODE:
    srl_iterator_array_goto(aTHX_ iter, idx);

IV
hash_exists(iter, name)
    srl_iterator_t *iter;
    SV *name;
  CODE:
    RETVAL = srl_iterator_hash_exists_sv(aTHX_ iter, name);
  OUTPUT: RETVAL

SV *
hash_key(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_hash_key_sv(aTHX_ iter);
    SvREFCNT_inc(RETVAL);
  OUTPUT: RETVAL

SV *
decode(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_decode(aTHX_ iter);
    SvREFCNT_inc(RETVAL);
  OUTPUT: RETVAL
