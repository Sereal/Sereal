/* c-basic-offset: 4;  indent-tabs-mode: nil */
#ifndef _SRL_ENCODER_H_
#define _SRL_ENCODER_H_

#include <stdint.h>
#include "srl_inline.h"

#define INITIAL_BUFFER_SIZE 64U

struct PTABLE;
typedef struct {
    char *buf_start;               /* ptr to "physical" start of output buffer */
    char *buf_end;                 /* ptr to end of output buffer  */
    char *pos;                     /* ptr to current position within output buffer */
    uint32_t operational_flags;    /* flags that pertain to one encode run
                                      (rather than being options): 
                                      See SRL_OF_* defines */
    uint32_t flags;                /* flag-like options: See SRL_F_* defines */
    unsigned max_recursion_depth;  /* Configurable limit on the number of
                                      recursive calls we're willing to make */
    unsigned recursion_depth;      /* current recursion depth */

    struct PTABLE *obj_seenhash;

    char *snappy_workmem;          /* scratchpad for snappy */
    unsigned snappy_threshold;     /* never compress dumps smaller than this */

    struct {
        PyObject *module;         /* 're' module is imported if we need */
        long DOTALL;              /* the modifier flags */
        long IGNORECASE;
        long LOCALE;
        long MULTILINE;
        long UNICODE;
        long VERBOSE;
    } re;
} srl_encoder_t;

typedef struct srl_encoder_ctor_args {
    uint32_t flags;
    unsigned long max_recursion_depth; /* Set max_recursion_depth to 0 for 
                                          the default python recursion depth */
    unsigned snappy_threshold;         /* Set snappy_threshold to 0 for no threshold */
} srl_encoder_ctor_args;

extern const srl_encoder_ctor_args default_encoder_ctor_args;

srl_encoder_t *srl_encoder_new(const srl_encoder_ctor_args *);
void srl_encoder_delete(srl_encoder_t *);
int  srl_encoder_ctor(srl_encoder_t *, const srl_encoder_ctor_args *);
void srl_encoder_dtor(srl_encoder_t *);
PyObject *srl_encoder_dump(srl_encoder_t *, PyObject *);

SRL_STATIC_INLINE int
SRL_ENC_HAVE_OPTION(const srl_encoder_t *enc, uint32_t bitmask)
{
    return enc->flags & bitmask;
}

/* Will default to "on". If set, hash keys will be shared using COPY.
 * Corresponds to the inverse of constructor option "no_shared_hashkeys" */
#define SRL_F_SHARED_HASHKEYS                1UL

/* If set, then we're using the OO interface and we shouldn't destroy the
 * encoder struct during SAVEDESTRUCTOR_X time 
#define SRL_F_REUSE_ENCODER                  2UL
*/

/* If set in flags, then we rather croak than serialize an object.
 * Corresponds to the 'croak_on_bless' option to the Perl constructor.
#define SRL_F_CROAK_ON_BLESS                 4UL
*/

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
#define SRL_F_COMPRESS_SNAPPY_INCREMENTAL   128UL


/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. 
#define SRL_F_NOWARN_UNKNOWN_OVERLOAD       256UL
*/

/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. */
#define SRL_F_SORT_KEYS                     512UL

/* Set while the encoder is in active use / dirty */
#define SRL_OF_ENCODER_DIRTY                 1UL

#endif
