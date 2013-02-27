/* c-basic-offset: 4;  indent-tabs-mode: nil */
#ifndef _UTIL_H_
#define _UTIL_H_

#include "srl_inline.h"

/* Set If and Only If */
/* Be aware that flag and bitmask are evaluated twice */
#define SET_IFF(pred,flag,bitmask) ((flag) = (pred) ? (flag | (bitmask)) : (flag & ~(bitmask)))

#endif
