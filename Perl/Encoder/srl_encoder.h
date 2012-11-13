#ifndef SRL_ENC_H_
#define SRL_ENC_H_

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

    U32 operational_flags;   /* flags that pertain to one encode run (rather than being options): See SRL_OF_* defines */
    U32 flags;               /* flag-like options: See SRL_F_* defines */
    unsigned int depth;      /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash; /* ptr table for avoiding circular refs */
    ptable_ptr weak_seenhash; /* ptr table for avoiding dangling weakrefs */
    ptable_ptr str_seenhash; /* ptr table for issuing COPY commands */

    void *snappy_workmem;    /* lazily allocated if and only if using Snappy */
    IV snappy_threshold;     /* do not compress things smaller than this even if Snappy enabled */
} srl_encoder_t;

/* constructor from options */
srl_encoder_t *srl_build_encoder_struct(pTHX_ HV *opt);
/* clone; "constructor from prototype" */
srl_encoder_t *srl_build_encoder_struct_alike(pTHX_ srl_encoder_t *proto);

void srl_clear_encoder(pTHX_ srl_encoder_t *enc);

/* Explicit destructor */
void srl_destroy_encoder(pTHX_ srl_encoder_t *enc);

/* Write Sereal packet header to output buffer */
void srl_write_header(pTHX_ srl_encoder_t *enc);
/* Start dumping a top-level SV */
void srl_dump_data_structure(pTHX_ srl_encoder_t *enc, SV *src);


/* define option bits in srl_encoder_t's flags member */

/* Will default to "on". If set, hash keys will be shared using COPY.
 * Corresponds to the inverse of constructor option "no_shared_hashkeys" */
#define SRL_F_SHARED_HASHKEYS                1UL
/* If set, then we're using the OO interface and we shouldn't destroy the
 * encoder struct during SAVEDESTRUCTOR_X time */
#define SRL_F_REUSE_ENCODER                  2UL
/* If set in flags, then we rather croak than serialize an object.
 * Corresponds to the 'croak_on_bless' option to the Perl constructor. */
#define SRL_F_CROAK_ON_BLESS                 4UL
/* If set in flags, then we will emit <undef> for all data types
 * that aren't supported.  Corresponds to the 'undef_unknown' option. */
#define SRL_F_UNDEF_UNKNOWN                  8UL
/* If set in flags, then we will stringify (SvPV) all data types
 * that aren't supported.  Corresponds to the 'stringify_unknown' option. */
#define SRL_F_STRINGIFY_UNKNOWN              16UL
/* If set in flags, then we warn() when trying to serialize an unsupported
 * data structure.  Applies only if stringify_unknown or undef_unknown are
 * set since we otherwise croak.  Corresponds to the 'warn_unknown' option. */
#define SRL_F_WARN_UNKNOWN                   32UL

/* WARNING: This is different from the protocol bit SRL_PROTOCOL_ENCODING_SNAPPY in that it's
 *          a flag on the encoder struct indicating that we want to use Snappy. */
#define SRL_F_COMPRESS_SNAPPY                64UL

/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. */
#define SRL_F_NOWARN_UNKNOWN_OVERLOAD        128UL

/* Set while the encoder is in active use / dirty */
#define SRL_OF_ENCODER_DIRTY                 1UL

#define SRL_ENC_HAVE_OPTION(enc, flag_num) ((enc)->flags & flag_num)
#define SRL_ENC_SET_OPTION(enc, flag_num) STMT_START {(enc)->flags |= (flag_num);}STMT_END
#define SRL_ENC_RESET_OPTION(enc, flag_num) STMT_START {(enc)->flags &= ~(flag_num);}STMT_END

#define SRL_ENC_HAVE_OPER_FLAG(enc, flag_num) ((enc)->operational_flags & (flag_num))
#define SRL_ENC_SET_OPER_FLAG(enc, flag_num) STMT_START {(enc)->operational_flags |= (flag_num);}STMT_END
#define SRL_ENC_RESET_OPER_FLAG(enc, flag_num) STMT_START {(enc)->operational_flags &= ~(flag_num);}STMT_END

#endif
