#ifndef DDL_ENC_H_
#define DDL_ENC_H_

#include "EXTERN.h"
#include "perl.h"

typedef struct PTABLE * ptable_ptr;
typedef struct {
    char *buf_start;     /* ptr to "physical" start of output buffer */
    char *buf_end;       /* ptr to end of output buffer */
    char *pos;           /* ptr to current position within output buffer */

    U32 flags;           /* flag-like options: See F_* defines in srl_encoder.c */
    unsigned int depth;  /* current Perl-ref recursion depth */
    ptable_ptr seenhash; /* ptr table for avoiding circular refs */
} srl_encoder_t;

/* constructor; don't need destructor, this sets up a callback */
srl_encoder_t *build_encoder_struct(pTHX_ HV *opt);

/* Write Sereal packet header to output buffer */
void srl_write_header(pTHX_ srl_encoder_t *enc);
/* Start dumping a top-level SV */
void srl_dump_sv(pTHX_ srl_encoder_t *enc, SV *src);

#endif
