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

#include "srl_inline.h"
#include "srl_buffer_types.h"

typedef struct PTABLE * ptable_ptr;
typedef struct {
    srl_buffer_t buf;
    srl_buffer_t tmp_buf;     /* temporary buffer for swapping */

    U32 operational_flags;    /* flags that pertain to one encode run (rather than being options): See SRL_OF_* defines */
    U32 flags;                /* flag-like options: See SRL_F_* defines */
    U32 protocol_version;     /* The version of the Sereal protocol to emit. */
    UV max_recursion_depth;   /* Configurable limit on the number of recursive calls we're willing to make */

    UV recursion_depth;       /* current Perl-ref recursion depth */
    ptable_ptr ref_seenhash;  /* ptr table for avoiding circular refs */
    ptable_ptr weak_seenhash; /* ptr table for avoiding dangling weakrefs */
    ptable_ptr str_seenhash;  /* ptr table for issuing COPY commands based on PTRS (used for classnames and keys)
                               * for now this is also coopted to track which objects we have dumped as objects,
                               * and to ensure we only output a given object once.
                               * Possibly this should be replaced with freezeobj_svhash, but this works fine.
                               */
    ptable_ptr freezeobj_svhash; /* ptr table for tracking objects and their frozen replacments via FREEZE */
    HV *string_deduper_hv;    /* track strings we have seen before, by content */

    void *snappy_workmem;     /* lazily allocated if and only if using Snappy */
    IV compress_threshold;    /* do not compress things smaller than this even if compression enabled */
    IV compress_level;        /* For ZLIB and ZSTD, the compression level */

                              /* only used if SRL_F_ENABLE_FREEZE_SUPPORT is set. */
    SV *sereal_string_sv;     /* SV that says "Sereal" for FREEZE support */
    SV *scratch_sv;           /* SV used by encoder for scratch operations */
} srl_encoder_t;

typedef struct {
    SV *sv;
    U32 hash;
} sv_with_hash;

typedef struct {
    union {
        SV *sv;
    } key;
    union {
        HE *he;
        SV *sv;
    } val;
} HE_SV;

/* constructor from options */
srl_encoder_t *srl_build_encoder_struct(pTHX_ HV *opt, sv_with_hash *options);

/* clone; "constructor from prototype" */
srl_encoder_t *srl_build_encoder_struct_alike(pTHX_ srl_encoder_t *proto);

void srl_clear_encoder(pTHX_ srl_encoder_t *enc);

/* Explicit destructor */
void srl_destroy_encoder(pTHX_ srl_encoder_t *enc);

/* Write Sereal packet header to output buffer */
void srl_write_header(pTHX_ srl_encoder_t *enc, SV *user_header_src, const U32 compress_flags);
/* Start dumping a top-level SV */
SV *srl_dump_data_structure_mortal_sv(pTHX_ srl_encoder_t *enc, SV *src, SV *user_header_src, const U32 flags);


/* define option bits in srl_encoder_t's flags member */

/* Will default to "on". If set, hash keys will be shared using COPY.
 * Corresponds to the inverse of constructor option "no_shared_hashkeys" */
#define SRL_F_SHARED_HASHKEYS                   0x00001UL
/* If set, then we're using the OO interface and we shouldn't destroy the
 * encoder struct during SAVEDESTRUCTOR_X time */
#define SRL_F_REUSE_ENCODER                     0x00002UL
/* If set in flags, then we rather croak than serialize an object.
 * Corresponds to the 'croak_on_bless' option to the Perl constructor. */
#define SRL_F_CROAK_ON_BLESS                    0x00004UL
/* If set in flags, then we will emit <undef> for all data types
 * that aren't supported.  Corresponds to the 'undef_unknown' option. */
#define SRL_F_UNDEF_UNKNOWN                     0x00008UL
/* If set in flags, then we will stringify (SvPV) all data types
 * that aren't supported.  Corresponds to the 'stringify_unknown' option. */
#define SRL_F_STRINGIFY_UNKNOWN                 0x00010UL
/* If set in flags, then we warn() when trying to serialize an unsupported
 * data structure.  Applies only if stringify_unknown or undef_unknown are
 * set since we otherwise croak.  Corresponds to the 'warn_unknown' option. */
#define SRL_F_WARN_UNKNOWN                      0x00020UL

/* WARNING:
 * #define SRL_F_COMPRESS_SNAPPY               0x00040UL
 * #define SRL_F_COMPRESS_SNAPPY_INCREMENTAL   0x00080UL
 * #define SRL_F_COMPRESS_ZLIB                 0x00100UL
 * are moved to srl_compress.h
 * note that there is SRL_F_COMPRESS_ZSTD defined below
 */

/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. */
#define SRL_F_NOWARN_UNKNOWN_OVERLOAD           0x00200UL

/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. */
#define SRL_F_SORT_KEYS                         0x00400UL

/* If set, use a hash to emit COPY() tags for all duplicated strings
 * (slow, but great compression) */
#define SRL_F_DEDUPE_STRINGS                    0x00800UL

/* Like SRL_F_DEDUPE_STRINGS but emits ALIAS() instead of COPY() for
 * non-class-name, non-hash-key strings that are deduped. If set,
 * supersedes SRL_F_DEDUPE_STRINGS. */
#define SRL_F_ALIASED_DEDUPE_STRINGS            0x01000UL

/* If set in flags, then we serialize objects without class information.
 * Corresponds to the 'no_bless_objects' flag found in the Decoder. */
#define SRL_F_NO_BLESS_OBJECTS                  0x02000UL

/* If set in flags, then support calling FREEZE method on objects. */
#define SRL_F_ENABLE_FREEZE_SUPPORT             0x04000UL

/* if set in flags, then do not use ARRAYREF or HASHREF ever */
#define SRL_F_CANONICAL_REFS                    0x08000UL

#define SRL_F_SORT_KEYS_PERL                    0x10000UL
#define SRL_F_SORT_KEYS_PERL_REV                0x20000UL

/* WARNING:
 * SRL_F_COMPRESS_ZSTD is defined in srl_compress.h
 * #define SRL_F_COMPRESS_ZSTD                  0x40000UL
 */

/* ====================================================================
 * oper flags
 */
/* Set while the encoder is in active use / dirty */
#define SRL_OF_ENCODER_DIRTY                 1UL

#define SRL_ENC_HAVE_OPTION(enc, flag_num) ((enc)->flags & (flag_num))
#define SRL_ENC_SET_OPTION(enc, flag_num) STMT_START {(enc)->flags |= (flag_num);}STMT_END
#define SRL_ENC_RESET_OPTION(enc, flag_num) STMT_START {(enc)->flags &= ~(flag_num);}STMT_END

#define SRL_ENC_HAVE_OPER_FLAG(enc, flag_num) ((enc)->operational_flags & (flag_num))
#define SRL_ENC_SET_OPER_FLAG(enc, flag_num) STMT_START {(enc)->operational_flags |= (flag_num);}STMT_END
#define SRL_ENC_RESET_OPER_FLAG(enc, flag_num) STMT_START {(enc)->operational_flags &= ~(flag_num);}STMT_END

#define SRL_ENC_SV_COPY_ALWAYS 0x00000000UL
#define SRL_ENC_SV_REUSE_MAYBE 0x00000001UL

#define SRL_UNSUPPORTED_SvTYPE(svt) (   \
    /* svt == SVt_INVLIST || */         \
    svt == SVt_PVGV ||                  \
    svt == SVt_PVCV ||                  \
    svt == SVt_PVFM ||                  \
    svt == SVt_PVIO ||                  \
    0 )

/* by default we do not allow people to build with support for SRL_HDR_LONG_DOUBLE */
#if defined(SRL_ALLOW_LONG_DOUBLE) && defined(USE_LONG_DOUBLE) && defined(HAS_LONG_DOUBLE)
#define SRL_DO_LONG_DOUBLE      1
#else
#define SRL_DO_LONG_DOUBLE      0
#endif

/* Options Parsing related code */
#define SRL_INIT_OPTION(idx, str) STMT_START {                          \
    MY_CXT.options[idx].sv = newSVpvn((str ""), (sizeof(str) - 1));     \
    PERL_HASH(MY_CXT.options[idx].hash, (str ""), (sizeof(str) - 1));   \
} STMT_END

#define SRL_ENC_OPT_STR_ALIASED_DEDUPE_STRINGS "aliased_dedupe_strings"
#define SRL_ENC_OPT_IDX_ALIASED_DEDUPE_STRINGS 0

#define SRL_ENC_OPT_STR_CANONICAL "canonical"
#define SRL_ENC_OPT_IDX_CANONICAL 1

#define SRL_ENC_OPT_STR_CANONICAL_REFS "canonical_refs"
#define SRL_ENC_OPT_IDX_CANONICAL_REFS 2

#define SRL_ENC_OPT_STR_COMPRESS "compress"
#define SRL_ENC_OPT_IDX_COMPRESS 3

#define SRL_ENC_OPT_STR_COMPRESS_LEVEL "compress_level"
#define SRL_ENC_OPT_IDX_COMPRESS_LEVEL 4

#define SRL_ENC_OPT_STR_COMPRESS_THRESHOLD "compress_threshold"
#define SRL_ENC_OPT_IDX_COMPRESS_THRESHOLD 5

#define SRL_ENC_OPT_STR_CROAK_ON_BLESS "croak_on_bless"
#define SRL_ENC_OPT_IDX_CROAK_ON_BLESS 6

#define SRL_ENC_OPT_STR_DEDUPE_STRINGS "dedupe_strings"
#define SRL_ENC_OPT_IDX_DEDUPE_STRINGS 7

#define SRL_ENC_OPT_STR_FREEZE_CALLBACKS "freeze_callbacks"
#define SRL_ENC_OPT_IDX_FREEZE_CALLBACKS 8

#define SRL_ENC_OPT_STR_MAX_RECURSION_DEPTH "max_recursion_depth"
#define SRL_ENC_OPT_IDX_MAX_RECURSION_DEPTH 9

#define SRL_ENC_OPT_STR_NO_BLESS_OBJECTS "no_bless_objects"
#define SRL_ENC_OPT_IDX_NO_BLESS_OBJECTS 10

#define SRL_ENC_OPT_STR_NO_SHARED_HASHKEYS "no_shared_hashkeys"
#define SRL_ENC_OPT_IDX_NO_SHARED_HASHKEYS 11

#define SRL_ENC_OPT_STR_PROTOCOL_VERSION "protocol_version"
#define SRL_ENC_OPT_IDX_PROTOCOL_VERSION 12

#define SRL_ENC_OPT_STR_SNAPPY "snappy"
#define SRL_ENC_OPT_IDX_SNAPPY 13

#define SRL_ENC_OPT_STR_SNAPPY_INCR "snappy_incr"
#define SRL_ENC_OPT_IDX_SNAPPY_INCR 14

#define SRL_ENC_OPT_STR_SNAPPY_THRESHOLD "snappy_threshold"
#define SRL_ENC_OPT_IDX_SNAPPY_THRESHOLD 15

#define SRL_ENC_OPT_STR_SORT_KEYS "sort_keys"
#define SRL_ENC_OPT_IDX_SORT_KEYS 16

#define SRL_ENC_OPT_STR_STRINGIFY_UNKNOWN "stringify_unknown"
#define SRL_ENC_OPT_IDX_STRINGIFY_UNKNOWN 17

#define SRL_ENC_OPT_STR_UNDEF_UNKNOWN "undef_unknown"
#define SRL_ENC_OPT_IDX_UNDEF_UNKNOWN 18

#define SRL_ENC_OPT_STR_USE_PROTOCOL_V1 "use_protocol_v1"
#define SRL_ENC_OPT_IDX_USE_PROTOCOL_V1 19

#define SRL_ENC_OPT_STR_WARN_UNKNOWN "warn_unknown"
#define SRL_ENC_OPT_IDX_WARN_UNKNOWN 20

#define SRL_ENC_OPT_COUNT 21

#endif
