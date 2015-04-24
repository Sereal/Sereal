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

#ifndef NDEBUG
#   define TRACE_READER 1
#   define TRACE_STACK 1
#endif

#include "srl_common.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "srl_path.h"
#include "srl_reader_error.h"

#include "srl_iterator.h"
#include "srl_iterator.c"

void srl_parse_next(pTHX_ srl_path_t *path, AV *expr, SV *route);
void srl_parse_hash(pTHX_ srl_path_t *path, SV *loc, AV *expr, SV *route);
void srl_parse_array(pTHX_ srl_path_t *path, SV *loc, AV *expr, SV *route);
int is_number(const char *str, size_t len);

srl_path_t *
srl_build_path_struct(pTHX_ HV *opt)
{
    srl_path_t *path = NULL;
    Newx(path, 1, srl_path_t);
    if (path == NULL) croak("Out of memory");

    path->iter = NULL;
    path->results = NULL;
    return path;
}

void
srl_destroy_path(pTHX_ srl_path_t *path)
{
    if (path->results)
        SvREFCNT_dec(path->results);

    if (path->iter)
        srl_destroy_iterator(path->iter);

    Safefree(path);
    return;
}

void
srl_path_reset(pTHX_ srl_path_t *path, SV *src)
{
    if (path->results) SvREFCNT_dec(path->results);
    path->results = newAV();

    if (path->iter) srl_destroy_iterator(path->iter);
    path->iter = srl_build_iterator_struct(NULL);

    if (sv_isa(src, "Sereal::Path::Iterator")) {
        croak("not implemented");
    } else {
        srl_set_document(path->iter, src);
    }
}

void
srl_traverse(pTHX_ srl_path_t *path, AV *expr, SV *route)
{
    if (!path->iter)
        croak("Set Iter first"); // TODO

    // TODO perhaps, copy query and route
    srl_parse_next(path, expr, route);
}

void
srl_parse_next(pTHX_ srl_path_t *path, AV *expr, SV *route)
{
    SV *loc;
    srl_iterator_t *iter = path->iter;

    assert(expr != NULL);
    assert(route != NULL);

    if (srl_eof(iter)) return;
    if (av_top_index(expr) < 0) { // expr is empty
        SV *res = srl_decode(iter);
        av_push(path->results, res); // TODO store route if needed
        return;
    }

    loc = av_shift(expr);
    loc = sv_2mortal(loc);

    switch (srl_object_info(iter, NULL)) {
        case SRL_ITER_OBJ_IS_HASH:
            srl_step_in(iter, 1);
            srl_parse_hash(path, loc, expr, route);
            break;

        case SRL_ITER_OBJ_IS_ARRAY:
            srl_step_in(iter, 1);
            srl_parse_array(path, loc, expr, route);
            break;
    }
}

void
srl_parse_hash(pTHX_ srl_path_t *path, SV *loc, AV *expr, SV *route)
{
    U8 tag;
    U32 count;
    srl_iterator_t *iter = path->iter;
    srl_stack_t *stack = iter->stack;

    assert(loc != NULL);
    assert(expr != NULL);
    assert(!srl_stack_empty(stack));

    tag = stack->ptr->tag;
    count = stack->ptr->count;
    assert(tag == SRL_HDR_HASH || (tag >= SRL_HDR_HASHREF_LOW && tag < SRL_HDR_HASHREF_HIGH));
}

void
srl_parse_array(pTHX_ srl_path_t *path, SV *loc, AV *expr, SV *route)
{
    U8 tag;
    U32 count;
    UV depth;
    STRLEN loc_len;
    const char *loc_str;
    srl_iterator_t *iter = path->iter;
    srl_stack_t *stack = iter->stack;
    STRLEN route_len = SvLEN(route);

    assert(loc != NULL);
    assert(expr != NULL);
    assert(!srl_stack_empty(stack));

    tag = stack->ptr->tag;
    count = stack->ptr->count;
    depth = SRL_STACK_DEPTH(stack);
    assert(tag == SRL_HDR_ARRAY || (tag >= SRL_HDR_ARRAYREF_LOW && tag < SRL_HDR_ARRAYREF_HIGH));

    loc_str = SvPV(loc, loc_len);

    if (strncmp(loc_str, "*", 1) == 0) {
        for (int i = 0; i < count; ++i) {
            assert(SRL_STACK_DEPTH(stack) == depth);

            sv_catpvf(route, ";[%d]", i); // append parsed object to route
            srl_parse_next(path, expr, route);
            SvLEN_set(route, route_len);  // restore original value

            if (SRL_STACK_DEPTH(stack) > depth)
                srl_next_at_depth(iter, depth);
        }
    //} else if (is_number(loc_str, loc_len)) {
    //    croak("not implemented");
    //} else if (is_list(loc_str, loc_len)) {
    //    croak("not implemented");
    }
}

int
is_number(const char *str, size_t len) {
    if (*str == '-') {
        str++;
        len--;
    }

    for (int i = 0; i < len; ++i) {
        if (str[i] < '0' || str[i] > '9')
            return 0;
    }

    return len != 0;
}
