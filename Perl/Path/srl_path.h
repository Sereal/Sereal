#ifndef SRL_PATH_H_
#define SRL_PATH_H_

#include "EXTERN.h"
#include "perl.h"

/* the iterator main struct */
typedef struct {
    struct srl_iterator *iter;
    AV *results;
} srl_path_t;

srl_path_t * srl_build_path_struct(pTHX_ HV *opt);
void srl_destroy_path(pTHX_ srl_path_t *path);
void srl_path_reset(pTHX_ srl_path_t *path, SV *src); // TODO
void srl_traverse(pTHX_ srl_path_t *path, AV *expr, SV *route);

#endif
