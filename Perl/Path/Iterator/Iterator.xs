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
    if (src) srl_iterator_set(aTHX_ RETVAL, src);
  OUTPUT: RETVAL

void
DESTROY(iter)
    srl_iterator_t *iter;
  CODE:
    srl_destroy_iterator(aTHX_ iter);

void
set(iter, src)
    srl_iterator_t *iter;
    SV *src;
  CODE:
    srl_iterator_set(aTHX_ iter, src);

void
reset(iter)
    srl_iterator_t *iter;
  CODE:
    srl_iterator_reset(aTHX_ iter);

void
unite(iter)
    srl_iterator_t *iter;
  CODE:
    srl_iterator_unite(aTHX_ iter);

void
disjoin(iter)
    srl_iterator_t *iter;
  CODE:
    srl_iterator_disjoin(aTHX_ iter);

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

void
rewind(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_iterator_rewind(aTHX_ iter, n ? SvUV(n) : 0);

void
info(iter)
    srl_iterator_t *iter;
  PREINIT:
    UV type;
    UV length;
    int blessed;
    const char *classname;
    STRLEN classname_length;

  PPCODE:
    type = srl_iterator_info(aTHX_ iter, &length, &classname, &classname_length);
    blessed = (type & SRL_ITERATOR_INFO_BLESSED) == SRL_ITERATOR_INFO_BLESSED;

    EXTEND(SP, blessed ? 3 : 2);
    PUSHs(sv_2mortal(newSVuv(type)));
    PUSHs(sv_2mortal(newSVuv(length)));
    if (blessed) PUSHs(sv_2mortal(newSVpvn(classname, classname_length)));

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

UV
stack_length(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_stack_length(aTHX_ iter);
  OUTPUT: RETVAL

void
array_goto(iter, idx)
    srl_iterator_t *iter;
    IV idx;
  CODE:
    srl_iterator_array_goto(aTHX_ iter, idx);

IV
array_exists(iter, idx)
    srl_iterator_t *iter;
    IV idx;
  CODE:
    RETVAL = srl_iterator_array_exists(aTHX_ iter, idx) == SRL_ITER_NOT_FOUND ? 0 : 1;
  OUTPUT: RETVAL

IV
hash_exists(iter, name)
    srl_iterator_t *iter;
    SV *name;
  PREINIT:
    const char *keyname;
    STRLEN keyname_lenght;
  CODE:
    keyname = (const char*) SvPV(name, keyname_lenght);
    RETVAL = srl_iterator_hash_exists(aTHX_ iter, keyname, keyname_lenght) == SRL_ITER_NOT_FOUND ? 0 : 1;
  OUTPUT: RETVAL

SV *
hash_key(iter)
    srl_iterator_t *iter;
  PREINIT:
    const char *keyname;
    STRLEN keyname_length;
  CODE:
    srl_iterator_hash_key(aTHX_ iter, &keyname, &keyname_length);
    RETVAL = newSVpvn(keyname, keyname_length);
  OUTPUT: RETVAL

SV *
decode(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_decode(aTHX_ iter);
    SvREFCNT_inc(RETVAL);
  OUTPUT: RETVAL

SV *
decode_and_next(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_iterator_decode_and_next(aTHX_ iter);
    SvREFCNT_inc(RETVAL);
  OUTPUT: RETVAL
