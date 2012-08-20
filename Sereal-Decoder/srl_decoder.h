#ifndef SRL_DECODER_H_
#define SRL_DECODER_H_

#include "EXTERN.h"
#include "perl.h"
#include "assert.h"

typedef struct PTABLE * ptable_ptr;
typedef struct {
    unsigned char *buf_start;         /* ptr to "physical" start of input buffer */
    unsigned char *buf_end;           /* ptr to end of input buffer */
    unsigned char *pos;               /* ptr to current position within input buffer */
    unsigned char *save_pos;

    U32 flags;               /* flag-like options: See F_* defines in srl_decoder.c */
    unsigned int depth;      /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash; /* ptr table for avoiding circular refs */
    ptable_ptr str_seenhash; /* ptr table for issuing COPY commands */

} srl_decoder_t;

/* constructor; don't need destructor, this sets up a callback */
srl_decoder_t *build_decoder_struct(pTHX_ HV *opt, SV *src);

/* Read Sereal packet header from buffer */
int srl_read_header(pTHX_ srl_decoder_t *dec);

/* Start deserializing a top-level SV */
SV *srl_read_sv(pTHX_ srl_decoder_t *dec);

#define BUF_POS(enc) ((enc)->pos)
#define BUF_SPACE(enc) ((enc)->buf_end - (enc)->pos)
#define BUF_POS_OFS(enc) ((enc)->pos - (enc)->buf_start)
#define BUF_SIZE(enc) ((enc)->buf_end - (enc)->buf_start)
#define BUF_NOT_DONE(enc) ((enc)->pos < (enc)->buf_end)

#define ASSERT_BUF_SPACE(enc,len) STMT_START {      \
    if ((UV)BUF_SPACE((enc)) < (UV)(len)) {                   \
        croak("Unexpected termination of packet");  \
    }                                               \
} STMT_END

#endif
