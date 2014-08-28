
#include "srl_merger.h"

srl_merger_t *
srl_build_merger_struct(pTHX_ HV *opt)
{
    srl_merger_t *merger;

    Newxz(merger, 1, srl_merger_t);

    return merger;
}
