#ifndef SRL_ITERATOR_H_
#define SRL_ITERATOR_H_

#include "EXTERN.h"
#include "perl.h"

/* the iterator main struct */
typedef struct {
} srl_iterator_t;

srl_iterator_t *srl_build_iterator_struct(pTHX_ HV *opt);         /* constructor from options */
void srl_destroy_iterator(pTHX_ srl_iterator_t *iter);            /* explicit destructor */

#endif
