#ifndef SRL_DECODER_H_
#define SRL_DECODER_H_

#include "EXTERN.h"
#include "perl.h"
#include "assert.h"

typedef struct PTABLE * ptable_ptr;
typedef struct {
    unsigned char *buf_start;           /* ptr to "physical" start of input buffer */
    unsigned char *buf_end;             /* ptr to end of input buffer */
    unsigned char *pos;                 /* ptr to current position within input buffer */
    unsigned char *save_pos;

    U32 flags;                          /* flag-like options: See F_* defines in srl_decoder.c */
    unsigned int depth;                 /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash;            /* ptr table for avoiding circular refs */
    ptable_ptr ref_stashes;             /* ptr table for tracking stashes we will bless into - key: ofs, value: stash */
    ptable_ptr ref_bless_av;            /* ptr table for tracking which objects need to be bless - key: ofs, value: mortal AV (of refs)  */
    AV* weakref_av;
} srl_decoder_t;

/* constructor; don't need destructor, this sets up a callback */
srl_decoder_t *build_decoder_struct(pTHX_ HV *opt, SV *src);

/* Read Sereal packet header from buffer */
int srl_read_header(pTHX_ srl_decoder_t *dec);

/* Start deserializing a top-level SV */
SV *srl_read_single_value(pTHX_ srl_decoder_t *dec, U8* track_pos);

/* Read Sereal packet header from buffer */
int srl_finalize_structure(pTHX_ srl_decoder_t *dec);

#define BUF_POS(dec) ((dec)->pos)
#define BUF_SPACE(dec) ((dec)->buf_end - (dec)->pos)
#define BUF_POS_OFS(dec) ((dec)->pos - (dec)->buf_start)
#define BUF_SIZE(dec) ((dec)->buf_end - (dec)->buf_start)
#define BUF_NOT_DONE(dec) ((dec)->pos < (dec)->buf_end)
#define BUF_DONE(dec) ((dec)->pos >= (dec)->buf_end)


#define MYCROAK(fmt, args...) croak("Sereal: Error in %s line %u: " fmt, __FILE__, __LINE__ , ## args)
#define ERROR(msg) MYCROAK("%s", msg)
#define ERRORf1(fmt,var) MYCROAK(fmt, (var))
#define ERRORf2(fmt,var1,var2) MYCROAK(fmt, (var1),(var2))
#define ERROR_UNIMPLEMENTED(dec,tag,str) STMT_START {      \
    MYCROAK("Tag %u %s is unimplemented at ofs: %d", tag,str, BUF_POS_OFS(dec)); \
    return NULL;                                \
} STMT_END 
#define ERROR_UNTERMINATED(dec, tag,str) MYCROAK("Tag %u %s was not terminated properly at ofs %lu with %lu to go", tag, str,dec->pos - dec->buf_start,dec->buf_end - dec->pos)
#define ERROR_BAD_COPY(dec, tag) MYCROAK("While processing tag %u encountered a bad COPY tag", tag)
#define ERROR_UNEXPECTED(dec, msg) MYCROAK("Unexpected tag %u while expecting %s", *(dec)->pos, msg)
#define ERROR_PANIC(dec, msg) MYCROAK("Panic: %s", msg);

#define ASSERT_BUF_SPACE(dec,len) STMT_START {      \
    if ((UV)BUF_SPACE((dec)) < (UV)(len)) {                   \
        MYCROAK("Unexpected termination of packet, want %lu bytes, only have %lu available", (UV)(len), (UV)BUF_SPACE((dec)));  \
    }                                               \
} STMT_END



#endif
