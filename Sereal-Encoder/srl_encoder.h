#ifndef DDL_ENC_H_
#define DDL_ENC_H_

#include "EXTERN.h"
#include "perl.h"

/* General 'config' constants */
#ifdef MEMDEBUG
#   define INITIALIZATION_SIZE 1
#else
#   define INITIALIZATION_SIZE 64
#endif

typedef struct PTABLE * ptable_ptr;
typedef struct {
    char *buf_start;         /* ptr to "physical" start of output buffer */
    char *buf_end;           /* ptr to end of output buffer */
    char *pos;               /* ptr to current position within output buffer */

    U32 flags;               /* flag-like options: See F_* defines in srl_encoder.c */
    unsigned int depth;      /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash; /* ptr table for avoiding circular refs */
    ptable_ptr weak_seenhash; /* ptr table for avoiding dangling weakrefs */
    ptable_ptr str_seenhash; /* ptr table for issuing COPY commands */
} srl_encoder_t;

/* constructor; don't need destructor, this sets up a callback */
srl_encoder_t *srl_build_encoder_struct(pTHX_ HV *opt);

void srl_clear_encoder(srl_encoder_t *enc);

/* Explicit destructor */
void srl_destroy_encoder(pTHX_ srl_encoder_t *enc);

/* Write Sereal packet header to output buffer */
void srl_write_header(pTHX_ srl_encoder_t *enc);
/* Start dumping a top-level SV */
void srl_dump_data_structure(pTHX_ srl_encoder_t *enc, SV *src);

/* define option bits in srl_encoder_t's flags member */
#define SRL_F_SHARED_HASHKEYS                1UL
#define SRL_F_REUSE_ENCODER                  2UL
#define SRL_HAVE_OPTION(enc, flag_num) ((enc)->flags & flag_num)

#endif
