#ifndef SRL_PATH_H_
#define SRL_PATH_H_

#include "EXTERN.h"
#include "perl.h"

/* the iterator main struct */
typedef struct {
    struct srl_iterator *iter;
    AV *expr;       /* srl_path_t do *NOT* own expr */
    AV *results;    /* srl_path_t own results */
    int i_own_iterator;
} srl_path_t;

srl_path_t * srl_build_path_struct(pTHX_ HV *opt);
void srl_destroy_path(pTHX_ srl_path_t *path);
void srl_path_set(pTHX_ srl_path_t *path, SV *src);
void srl_path_traverse(pTHX_ srl_path_t *path, AV *expr, SV *route);
SV * srl_path_results(pTHX_ srl_path_t *path); /* return mortalized SV */

/* for testing purposes */
int * _is_range(const char *str, STRLEN len, int *out);

#endif
