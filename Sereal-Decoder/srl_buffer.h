#ifndef SRL_BUFFER_H_
#define SRL_BUFFER_H_

#include "assert.h"
#include "srl_decoder.h"
/* The static's below plus the ifndef sort of make this header only
 * usable in one place per compilation unit. Drop "static" when necessary.
 * For now, potentially smaller code wins. */


/* buffer operations */
#define BUF_POS(enc) ((enc)->pos)
#define BUF_SPACE(enc) ((enc)->buf_end - (enc)->pos)
#define BUF_POS_OFS(enc) ((enc)->pos - (enc)->buf_start)
#define BUF_SIZE(enc) ((enc)->buf_end - (enc)->buf_start)
#define BUF_NOT_DONE(enc) ((enc)->pos < (enc)->buf_end)

#endif
