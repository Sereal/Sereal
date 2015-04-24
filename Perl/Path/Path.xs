#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#include "srl_common.h"
#include "srl_protocol.h"
#include "srl_path.h"

typedef srl_path_t     * Sereal__Path;

MODULE = Sereal::Path               PACKAGE = Sereal::Path
PROTOTYPES: DISABLE

srl_path_t *
new(CLASS, src = NULL, opt = NULL)
    char *CLASS;
    SV *src;
    HV *opt;
  CODE:
    RETVAL = srl_build_path_struct(aTHX_ opt);
    if (src) srl_path_reset(RETVAL, src);
  OUTPUT: RETVAL

void
DESTROY(path)
    srl_path_t *path;
  CODE:
    srl_destroy_path(aTHX_ path);

void
reset(path, src)
    srl_path_t *path;
    SV *src;
  CODE:
    srl_path_reset(path, src);

void
_traverse(path, expr, route)
    srl_path_t *path;
    SV *expr;
    SV *route;
  CODE:
    if (SvTYPE(expr) != SVt_RV) croak("query mush be arrayref");
    expr = SvRV(expr);
    if (SvTYPE(expr) != SVt_PVAV) croak("query mush be arrayref");
    srl_traverse(path, (AV*) expr, route);
