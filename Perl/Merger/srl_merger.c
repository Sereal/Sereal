/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"
#ifdef __cplusplus
}
#endif

#include <stdlib.h>

#ifndef PERL_VERSION
#    include <patchlevel.h>
#    if !(defined(PERL_VERSION) || (PERL_SUBVERSION > 0 && defined(PATCHLEVEL)))
#        include <could_not_find_Perl_patchlevel.h>
#    endif
#    define PERL_REVISION       5
#    define PERL_VERSION        PATCHLEVEL
#    define PERL_SUBVERSION     PERL_SUBVERSION
#endif
#if PERL_VERSION < 8
#   define PERL_MAGIC_qr                  'r' /* precompiled qr// regex */
#   define BFD_Svs_SMG_OR_RMG SVs_RMG
#elif ((PERL_VERSION==8) && (PERL_SUBVERSION >= 1) || (PERL_VERSION>8))
#   define BFD_Svs_SMG_OR_RMG SVs_SMG
#   define MY_PLACEHOLDER PL_sv_placeholder
#else
#   define BFD_Svs_SMG_OR_RMG SVs_RMG
#   define MY_PLACEHOLDER PL_sv_undef
#endif
#if (((PERL_VERSION == 9) && (PERL_SUBVERSION >= 4)) || (PERL_VERSION > 9))
#   define NEW_REGEX_ENGINE 1
#endif
#if (((PERL_VERSION == 8) && (PERL_SUBVERSION >= 1)) || (PERL_VERSION > 8))
#define MY_CAN_FIND_PLACEHOLDERS
#define HAS_SV2OBJ
#endif

#include "srl_protocol.h"
#include "srl_merger.h"
#include "srl_common.h"
#include "srl_buffer.h"

/* allocate an empty merger struct - flags still to be set up */
SRL_STATIC_INLINE srl_merger_t *
srl_empty_merger_struct(pTHX)
{
    srl_merger_t *mrg;
    Newx(mrg, 1, srl_merger_t);
    if (mrg == NULL)
        croak("Out of memory");

    /* Init buffer struct */
    if (expect_false(srl_buf_init_buffer(aTHX_ &(mrg->buf), INITIALIZATION_SIZE) != 0)) {
        Safefree(mrg);
        croak("Out of memory");
    }

    /* Set the tmp buffer struct's char buffer to NULL so we don't free
     * something nasty if it's unused. */
    //mrg->tmp_buf.start = NULL;

    mrg->protocol_version = SRL_PROTOCOL_VERSION;
    //mrg->recursion_depth = 0;
    //mrg->max_recursion_depth = DEFAULT_MAX_RECUR_DEPTH;
    //mrg->operational_flags = 0;
    /*mrg->flags = 0;*/ /* to be set elsewhere */

    //mrg->weak_seenhash = NULL;
    //mrg->str_seenhash = NULL;
    //mrg->ref_seenhash = NULL;
    //mrg->snappy_workmem = NULL;
    //mrg->string_deduper_hv = NULL;

    //mrg->freezeobj_svhash = NULL;
    //mrg->sereal_string_sv = NULL;

    return mrg;
}

srl_merger_t *
srl_build_merger_struct(pTHX_ HV *opt)
{
    srl_merger_t *mrg;
    SV **svp;

    mrg = srl_empty_merger_struct(aTHX);

    /* load options */
    if (opt != NULL) {
        /* Needs to be before the snappy options */
        /* mrg->protocol_version defaults to SRL_PROTOCOL_VERSION. */
        svp = hv_fetchs(opt, "protocol_version", 0);
        if (svp && SvOK(*svp)) {
            mrg->protocol_version = SvUV(*svp);
            if (mrg->protocol_version < 1
                || mrg->protocol_version > SRL_PROTOCOL_VERSION)
            {
                croak("Specified Sereal protocol version ('%lu') is invalid",
                      (unsigned long)mrg->protocol_version);
            }
        }
    }

    DEBUG_ASSERT_BUF_SANE(mrg);
    return mrg;
}

void
srl_destroy_merger(pTHX_ srl_merger_t *mrg) {
    srl_buf_free_buffer(aTHX_ &mrg->buf);
    Safefree(mrg);
}


void srl_merger_finish(pTHX_ srl_merger_t *mrg) {
    // do stuff with mrg
    return;
}

void srl_merger_append(pTHX_ srl_merger_t *mrg, const char * srl_document) {
    printf("blob : %s\n", srl_document);
    // do stuff with mrg and srl_document 
    return;
}
