#ifndef SRL_DECODER_H_
#define SRL_DECODER_H_

#include "EXTERN.h"
#include "perl.h"
#include "assert.h"
#include "srl_reader_types.h"

typedef struct PTABLE * ptable_ptr;
typedef struct {
    srl_reader_buffer_t buf;
    srl_reader_buffer_ptr pbuf;
    const unsigned char *save_pos;      /* used for COPY tags */

    U32 flags;                          /* flag-like options: See SRL_F_DECODER_* defines in srl_decoder.c */
    UV max_recursion_depth;             /* Configurable limit on the number of recursive calls we're willing to make */
    UV max_num_hash_entries;            /* Configured maximum number of acceptable entries in a hash */
    ptable_ptr ref_seenhash;            /* ptr table for avoiding circular refs */
    ptable_ptr ref_thawhash;          /* ptr table for dealing with non ref thawed items */
    ptable_ptr ref_stashes;             /* ptr table for tracking stashes we will bless into - key: ofs, value: stash */
    ptable_ptr ref_bless_av;            /* ptr table for tracking which objects need to be bless - key: ofs, value: mortal AV (of refs)  */
    AV* weakref_av;

    AV* alias_cache; /* used to cache integers of different sizes. */
    IV alias_varint_under;

    UV bytes_consumed;
    UV recursion_depth;                 /* Recursion depth of current decoder */
    U32 flags_readonly;
} srl_decoder_t;

typedef struct {
    SV *sv;
    U32 hash;
} sv_with_hash;

/* utility routine */
IV srl_validate_header_version_pv_len(pTHX_ char *strdata, STRLEN len);

/* constructor; don't need destructor, this sets up a callback */
srl_decoder_t *srl_build_decoder_struct(pTHX_ HV *opt, sv_with_hash *options);

/* main routines */
/* will return a mortal or the new contents of into if that isn't NULL */
SV *srl_decode_into(pTHX_ srl_decoder_t *dec, SV *src, SV *body_into, UV start_offset);
/* will return a mortal or the new contents of header_into if that isn't NULL */
SV *srl_decode_header_into(pTHX_ srl_decoder_t *dec, SV *src, SV *header_into, UV start_offset);
/* decode both header and body - must pass in two SVs to write into */
void srl_decode_all_into(pTHX_ srl_decoder_t *dec, SV *src, SV *header_into, SV *body_into, UV start_offset);
/* main recursive dump routine, for internal usage only!!! */
void srl_decode_single_value(pTHX_ srl_decoder_t *dec, SV* into, SV** container);

/* Explicit destructor */
void srl_destroy_decoder(pTHX_ srl_decoder_t *dec);

/* destructor hook - called automagically */
void srl_decoder_destructor_hook(pTHX_ void *p);

/* Macro to assert that the type of an SV is complex enough to
 * be an RV. Differs on old perls since there used to be an RV type.
 */
#if PERL_VERSION < 12
#   define SVt_RV_FAKE SVt_RV
#else
#   define SVt_RV_FAKE SVt_IV
#endif

/* this is from sv.h in Perl core, which is for some reason guarded
 * by an ifdef PERL_CORE, which I am loathe to enable. */

#define SRL_prepare_SV_for_RV(sv)                                   \
    STMT_START {                                                    \
        if (SvTYPE(sv) < SVt_PV && SvTYPE(sv) != SVt_RV_FAKE)       \
            sv_upgrade(sv, SVt_RV_FAKE);                            \
        else if (SvTYPE(sv) >= SVt_PV) {                            \
            SvPV_free(sv);                                          \
            SvLEN_set(sv, 0);                                       \
            SvCUR_set(sv, 0);                                       \
        }                                                           \
    } STMT_END

#define SRL_sv_set_rv_to(into,referent)             \
    STMT_START {                                    \
        SRL_prepare_SV_for_RV(into);                \
        SvTEMP_off(referent);                       \
        SvRV_set(into, referent);                   \
        SvROK_on(into);                             \
    } STMT_END

/* If set, the decoder struct needs to be cleared instead of freed at
 * the end of a deserialization operation */
#define SRL_F_REUSE_DECODER                     0x00000001UL
/* If set, then the decoder is already in use and srl_decode_into will
 * clone its own new decoder. */
#define SRL_F_DECODER_DIRTY                     0x00000002UL
/* Non-persistent flag! */
#define SRL_F_DECODER_NEEDS_FINALIZE            0x00000004UL
/* Non-persistent flag! */
#define SRL_F_DECODER_DECOMPRESS_SNAPPY         0x00000008UL
/* Non-persistent flag! */
#define SRL_F_DECODER_DECOMPRESS_ZLIB           0x00000010UL
/* Persistent flag: Make the decoder REFUSE Snappy-compressed documents */
#define SRL_F_DECODER_REFUSE_SNAPPY             0x00000020UL
/* Persistent flag: Make the decoder REFUSE zlib-compressed documents */
#define SRL_F_DECODER_REFUSE_ZLIB               0x00000040UL
/* Persistent flag: Make the decoder REFUSE objects */
#define SRL_F_DECODER_REFUSE_OBJECTS            0x00000080UL
/* Persistent flag: Make the decoder validate UTT8 strings */
#define SRL_F_DECODER_VALIDATE_UTF8             0x00000100UL
/* Persistent flag: Make the encoder forget to bless */
#define SRL_F_DECODER_NO_BLESS_OBJECTS          0x00000200UL
/* Persistent flag: Destructive incremental parsing */
#define SRL_F_DECODER_DESTRUCTIVE_INCREMENTAL   0x00000400UL
/* Non-persistent flag: The current packet is using protocol version 1 */
#define SRL_F_DECODER_PROTOCOL_V1               0x00000800UL
/* Persistent flag: alias small integer values in Hashes and Arrays */
#define SRL_F_DECODER_ALIAS_SMALLINT            0x00001000UL
/* Persistent flag: use PL_sv_undef for undef values in Hashes and Arrays */
#define SRL_F_DECODER_ALIAS_VARINT              0x00002000UL
/* Persistent flag: use PL_sv_undef as many places as possible */
#define SRL_F_DECODER_USE_UNDEF                 0x00004000UL
/* Persistent flag: set all SV readonly */
#define SRL_F_DECODER_SET_READONLY              0x00008000UL
/* Persistent flag: set non-ref SV readonly */
#define SRL_F_DECODER_SET_READONLY_SCALARS      0x00010000UL


#define SRL_F_DECODER_ALIAS_CHECK_FLAGS   ( SRL_F_DECODER_ALIAS_SMALLINT | SRL_F_DECODER_ALIAS_VARINT | SRL_F_DECODER_USE_UNDEF )
#define SRL_F_DECODER_READONLY_FLAGS   ( SRL_F_DECODER_SET_READONLY | SRL_F_DECODER_SET_READONLY_SCALARS )

#define SRL_DEC_HAVE_OPTION(dec, flag_num) ((dec)->flags & flag_num)
#define SRL_DEC_SET_OPTION(dec, flag_num) ((dec)->flags |= flag_num)
#define SRL_DEC_UNSET_OPTION(dec, flag_num) ((dec)->flags &= ~flag_num)
#define SRL_DEC_VOLATILE_FLAGS (SRL_F_DECODER_NEEDS_FINALIZE|SRL_F_DECODER_DECOMPRESS_SNAPPY|SRL_F_DECODER_PROTOCOL_V1|SRL_F_DECODER_DIRTY|SRL_F_DECODER_DECOMPRESS_ZLIB)
#define SRL_DEC_RESET_VOLATILE_FLAGS(dec) ((dec)->flags &= ~SRL_DEC_VOLATILE_FLAGS)

#define IS_IV_ALIAS(dec,iv)             \
(                                       \
    ((dec)->alias_varint_under) &&      \
    ((iv) >= -16 )&&                    \
    ((iv) < (dec)->alias_varint_under)  \
)

/* Options Parsing related code */
#define SRL_INIT_OPTION(idx, str) STMT_START {                          \
    MY_CXT.options[idx].sv = newSVpvn((str ""), (sizeof(str) - 1));     \
    PERL_HASH(MY_CXT.options[idx].hash, (str ""), (sizeof(str) - 1));   \
} STMT_END

#define SRL_DEC_OPT_STR_ALIAS_SMALLINT              "alias_smallint"
#define SRL_DEC_OPT_IDX_ALIAS_SMALLINT              0

#define SRL_DEC_OPT_STR_ALIAS_VARINT_UNDER          "alias_varint_under"
#define SRL_DEC_OPT_IDX_ALIAS_VARINT_UNDER          1

#define SRL_DEC_OPT_STR_DESTRUCTIVE_INCREMENTAL     "incremental"
#define SRL_DEC_OPT_IDX_DESTRUCTIVE_INCREMENTAL     2

#define SRL_DEC_OPT_STR_MAX_NUM_HASH_ENTRIES        "max_num_hash_entries"
#define SRL_DEC_OPT_IDX_MAX_NUM_HASH_ENTRIES        3

#define SRL_DEC_OPT_STR_MAX_RECURSION_DEPTH         "max_recursion_depth"
#define SRL_DEC_OPT_IDX_MAX_RECURSION_DEPTH         4

#define SRL_DEC_OPT_STR_NO_BLESS_OBJECTS            "no_bless_objects"
#define SRL_DEC_OPT_IDX_NO_BLESS_OBJECTS            5

#define SRL_DEC_OPT_STR_REFUSE_OBJECTS              "refuse_objects"
#define SRL_DEC_OPT_IDX_REFUSE_OBJECTS              6

#define SRL_DEC_OPT_STR_REFUSE_SNAPPY               "refuse_snappy"
#define SRL_DEC_OPT_IDX_REFUSE_SNAPPY               7

#define SRL_DEC_OPT_STR_REFUSE_ZLIB                 "refuse_zlib"
#define SRL_DEC_OPT_IDX_REFUSE_ZLIB                 8

#define SRL_DEC_OPT_STR_SET_READONLY                "set_readonly"
#define SRL_DEC_OPT_IDX_SET_READONLY                9

#define SRL_DEC_OPT_STR_SET_READONLY_SCALARS        "set_readonly_scalars"
#define SRL_DEC_OPT_IDX_SET_READONLY_SCALARS        10

#define SRL_DEC_OPT_STR_USE_UNDEF                   "use_undef"
#define SRL_DEC_OPT_IDX_USE_UNDEF                   11

#define SRL_DEC_OPT_STR_VALIDATE_UTF8               "validate_utf8"
#define SRL_DEC_OPT_IDX_VALIDATE_UTF8               12

#define SRL_DEC_OPT_COUNT                           13

#endif
