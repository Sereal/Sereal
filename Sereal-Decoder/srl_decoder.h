#ifndef DDL_ENC_H_
#define DDL_ENC_H_

#include "EXTERN.h"
#include "perl.h"



typedef struct PTABLE * ptable_ptr;
typedef struct {
    unsigned char *buf_start;         /* ptr to "physical" start of input buffer */
    unsigned char *buf_end;           /* ptr to end of input buffer */
    unsigned char *pos;               /* ptr to current position within input buffer */

    U32 flags;               /* flag-like options: See F_* defines in srl_decoder.c */
    unsigned int depth;      /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash; /* ptr table for avoiding circular refs */
    ptable_ptr str_seenhash; /* ptr table for issuing COPY commands */

    U32 val_stack_size;
    SV  **val_stack;
} srl_decoder_t;

/* constructor; don't need destructor, this sets up a callback */
srl_decoder_t *build_decoder_struct(pTHX_ HV *opt, SV *src);

/* Read Sereal packet header from buffer */
int srl_read_header(pTHX_ srl_decoder_t *dec);

/* Start deserializing a top-level SV */
SV *srl_read_sv(pTHX_ srl_decoder_t *dec);

#endif
