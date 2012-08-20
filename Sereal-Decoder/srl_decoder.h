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
SV *srl_read_single_value(pTHX_ srl_decoder_t *dec);

#define BUF_POS(dec) ((dec)->pos)
#define BUF_SPACE(dec) ((dec)->buf_end - (dec)->pos)
#define BUF_POS_OFS(dec) ((dec)->pos - (dec)->buf_start)
#define BUF_SIZE(dec) ((dec)->buf_end - (dec)->buf_start)
#define BUF_NOT_DONE(dec) ((dec)->pos < (dec)->buf_end)

#define ERROR(msg) croak("Error: %s", msg)
#define ERROR_UNIMPLEMENTED(dec,tag) STMT_START {      \
    croak("Error: tag %c is uimplemented at ofs: %d", tag, BUF_POS_OFS(dec)); \
    return NULL;                                \
} STMT_END 
#define ERROR_UNTERMINATED(dec, tag) croak("Error: tag %c was not terminated properly", tag)
#define ERROR_BAD_COPY(dec, tag) croak("Error: while processing tag %c encountered a bad COPY tag", tag)
#define ERROR_UNEXPECTED(dec, msg) croak("Error: unexpected tag %c while expecting %s", *(dec)->pos, msg)
#define ERROR_PANIC(dec, msg) croak("Panic: %s", msg);

#define ASSERT_BUF_SPACE(dec,len) STMT_START {      \
    if ((UV)BUF_SPACE((dec)) < (UV)(len)) {                   \
        croak("Unexpected termination of packet");  \
    }                                               \
} STMT_END



#endif
