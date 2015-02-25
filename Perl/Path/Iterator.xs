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
    if (src) srl_set_document(RETVAL, src);
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
    srl_set_document(iter, src);

void
reset(iter)
    srl_iterator_t *iter;
  CODE:
    srl_reset(iter);

UV
eof(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_eof(iter);
  OUTPUT: RETVAL

void
next(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_next(iter, n ? SvUV(n) : 1);

void
step_in(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_step_in(iter, n ? SvUV(n) : 1);

void
step_out(iter, n = NULL)
    srl_iterator_t *iter;
    SV *n;
  CODE:
    srl_step_out(iter, n ? SvUV(n) : 1);

UV
continue_until_depth(iter, depth)
    srl_iterator_t *iter;
    UV depth;
  CODE:
    RETVAL = srl_continue_until_depth(iter, depth);
  OUTPUT: RETVAL

UV
offset(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_offset(iter);
  OUTPUT: RETVAL

void
info(iter)
    srl_iterator_t *iter;
  PREINIT:
    SV *type;
    UV length;
  PPCODE:
    type = srl_object_info(iter, &length);

    EXTEND(SP, 2);
    PUSHs(type);
    PUSHs(sv_2mortal(newSVuv(length)));

UV
stack_depth(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_stack_depth(iter);
  OUTPUT: RETVAL

UV
stack_index(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_stack_index(iter);
  OUTPUT: RETVAL

void
stack_info(iter)
    srl_iterator_t *iter;
  PREINIT:
    SV *type;
    UV length;
  PPCODE:
    type = srl_stack_info(iter, &length);

    EXTEND(SP, 2);
    PUSHs(type);
    PUSHs(sv_2mortal(newSVuv(length)));

IV
hash_exists(iter, name)
    srl_iterator_t *iter;
    SV *name;
  CODE:
    RETVAL = srl_hash_exists(iter, name);
  OUTPUT: RETVAL

SV *
hash_key(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_hash_key(iter);
    SvREFCNT_inc(RETVAL);
  OUTPUT: RETVAL

SV *
decode(iter)
    srl_iterator_t *iter;
  CODE:
    RETVAL = srl_decode(iter);
    SvREFCNT_inc(RETVAL);
  OUTPUT: RETVAL
