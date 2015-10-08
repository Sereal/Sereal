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
    if (src) srl_path_set(aTHX_ RETVAL, src);
  OUTPUT: RETVAL

void
DESTROY(path)
    srl_path_t *path;
  CODE:
    srl_destroy_path(aTHX_ path);

void
set(path, src)
    srl_path_t *path;
    SV *src;
  CODE:
    srl_path_set(aTHX_ path, src);

SV *
results(path)
    srl_path_t *path;
  PPCODE:
    ST(0) = srl_path_results(aTHX_ path);
    XSRETURN(1);

void
_traverse(path, expr, route)
    srl_path_t *path;
    SV *expr;
    SV *route;
  CODE:
    if (SvTYPE(expr) != SVt_RV) croak("query mush be arrayref");
    expr = SvRV(expr);
    if (SvTYPE(expr) != SVt_PVAV) croak("query mush be arrayref");
    srl_path_traverse(aTHX_ path, (AV*) expr, route);

MODULE = Sereal::Path               PACKAGE = Sereal::Path::_tests

SV *
is_range(src)
    SV *src;
  PREINIT:
    AV *av;
    STRLEN len;
    int values[3];
    const char *str;
  PPCODE:
    av = newAV();
    ST(0) = newRV_noinc((SV*) av);
    str = SvPV(src, len);

    if (_is_range(str, len, (int*) &values)) {
        av_push(av, newSViv(values[0]));
        av_push(av, newSViv(values[1]));
        av_push(av, newSViv(values[2]));
    }

    XSRETURN(1);
