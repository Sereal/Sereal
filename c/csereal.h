#ifndef CSEREAL_H_
#define CSEREAL_H_

#include <stdlib.h>
#include <string.h>

#include <csrl_defines.h>

typedef struct csrl_decoder csrl_decoder;

#define CSRL_STATUS_OK 0
#define CSRL_STATUS_ERROR 1

/* Allocate/free decoder objects */
csrl_decoder *csrl_new_decoder();
void csrl_free_decoder(csrl_decoder *dec);

/* Return 0/1 indicating whether the given string appears to be Sereal encoded */
int csrl_looks_like_sereal(csrl_decoder *dec, const U8 *input, size_t len);

/* Actual decode run. FIXME figure out actual data structure returned.
 * Returns status code. */
int csrl_decode(csrl_decoder *dec, const U8 *input, size_t len, void **out);

#endif
