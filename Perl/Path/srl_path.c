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
#include <assert.h>

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
#   if DEBUG > 1
#       define TRACE_READER     1
#       define TRACE_SRL_PATH   1
#   endif
#endif

#include "srl_common.h"
#include "srl_inline.h"
#include "srl_protocol.h"
#include "srl_path.h"
#include "srl_iterator.h"
#include "srl_reader_error.h"

#ifdef TRACE_SRL_PATH
#   define SRL_PATH_TRACE(msg, args...) SRL_RDR_TRACE(msg, ## args)
#else
#   define SRL_PATH_TRACE(msg, args...)
#endif

SRL_STATIC_INLINE void srl_parse_next(pTHX_ srl_path_t *path, int expr_idx, SV *route);
SRL_STATIC_INLINE void srl_parse_next_int(pTHX_ srl_path_t *path, int expr_idx, SV *route, UV n);
SRL_STATIC_INLINE void srl_parse_next_str(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                                          const char *str, STRLEN len);

SRL_STATIC_INLINE void srl_parse_hash(pTHX_ srl_path_t *path, int expr_idx, SV *route);
SRL_STATIC_INLINE void srl_parse_array(pTHX_ srl_path_t *path, int expr_idx, SV *route);

SRL_STATIC_INLINE int is_all(const char *str, STRLEN len);
SRL_STATIC_INLINE int is_list(const char *str, STRLEN len);
SRL_STATIC_INLINE int is_number(const char *str, STRLEN len);
SRL_STATIC_INLINE int next_item_in_list(const char *list, STRLEN list_len,
                                        const char **item_out, STRLEN *item_len_out);
SRL_STATIC_INLINE void print_route(SV *route, const char *str);

srl_path_t *
srl_build_path_struct(pTHX_ HV *opt)
{
    srl_path_t *path = NULL;
    Newx(path, 1, srl_path_t);
    if (path == NULL) croak("Out of memory");

    path->iter = NULL;
    path->expr = NULL;
    path->results = NULL;

    if (opt != NULL) {}
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
    path->expr = NULL;

    if (path->results) SvREFCNT_dec(path->results);
    path->results = newAV();

    if (path->iter) srl_destroy_iterator(path->iter);
    path->iter = srl_build_iterator_struct(NULL);

    if (sv_isa(src, "Sereal::Path::Iterator")) {
        croak("not implemented");
    } else {
        srl_iterator_set_document(path->iter, src);
    }
}

void
srl_path_traverse(pTHX_ srl_path_t *path, AV *expr, SV *route)
{
    SV *route_copy;
    if (!path->iter)
        croak("Set Iter first"); // TODO

    assert(expr != NULL);
    assert(route != NULL);

    path->expr = expr; // TODO perhaps, copy expr
    route_copy = sv_2mortal(newSVsv(route));
    srl_parse_next(path, 0, route_copy);
}

SRL_STATIC_INLINE void
srl_parse_next(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_t *iter = path->iter;

    assert(route != NULL);
    SRL_PATH_TRACE("expr_idx=%d", expr_idx);

    /* for (int i = 0; i <= av_top_index(expr); ++i) {
        SV **sv = av_fetch(expr, i, 0);
        STRLEN len;
        SV *str = SvPV(*sv, len);
        warn("%d => %.*s", i, (int) len, str);
    } */

    if (srl_iterator_eof(iter)) return;
    if (expr_idx > av_top_index(path->expr)) { // scaned entiry expr
        print_route(route, "to decode");
        // SV *res = srl_iterator_decode(iter);
        // SvREFCNT_inc(res);
        // av_push(path->results, res); // TODO store route if needed
        return;
    }

    switch (srl_iterator_object_info(iter, NULL)) {
        case SRL_ITERATOR_OBJ_IS_HASH:
            srl_iterator_step_in(iter, 1);
            srl_parse_hash(path, expr_idx, route);
            break;

        case SRL_ITERATOR_OBJ_IS_ARRAY:
            srl_iterator_step_in(iter, 1);
            srl_parse_array(path, expr_idx, route);
            break;
    }
}

SRL_STATIC_INLINE void
srl_parse_next_str(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                   const char *str, STRLEN len)
{
    STRLEN route_len = SvCUR(route);
    sv_catpvf(route, ";%.*s", (int) len, str); // append parsed object to route
    srl_parse_next(path, expr_idx, route);
    SvCUR_set(route, route_len);  // restore original value
}

SRL_STATIC_INLINE void
srl_parse_next_int(pTHX_ srl_path_t *path, int expr_idx, SV *route, UV n)
{
    STRLEN route_len = SvCUR(route);
    sv_catpvf(route, ";[%"UVuf"]", n); // append parsed object to route
    srl_parse_next(path, expr_idx, route);
    SvCUR_set(route, route_len);  // restore original value
}

SRL_STATIC_INLINE void
srl_parse_hash(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    U8 tag;
    IV depth;
    U32 count;
    srl_iterator_ptr iter = path->iter;
    srl_iterator_stack_ptr stack_ptr = srl_iterator_stack(iter);
    STRLEN loc_len, route_len = SvCUR(route);
    const char *loc_str;
    SV *loc;

    assert(route != NULL);
    assert(expr_idx >= 0);
    assert(expr_idx <= av_top_index(path->expr));
    assert(stack_ptr != NULL);

    tag   = stack_ptr->tag;
    count = stack_ptr->count;
    depth = srl_iterator_stack_depth(iter);
    loc   = *av_fetch(path->expr, expr_idx, 0);
    loc_str = SvPV(loc, loc_len);

    assert(tag == SRL_HDR_HASH || (tag >= SRL_HDR_HASHREF_LOW && tag <= SRL_HDR_HASHREF_HIGH));
    SRL_PATH_TRACE("parse hash tag=%d (0x%x) of size=%d at depth=%d, loc=%s",
                   tag, tag, (int) count, (int) depth, loc_str);

    if (is_all(loc_str, loc_len)) {                                                     // *
        croak("not implemented yet");
    } else if (is_list(loc_str, loc_len)) {                                             // [name1,name2]
        STRLEN item_len;
        const char *item = NULL;
        while (next_item_in_list(loc_str, loc_len, &item, &item_len)) {
            assert(srl_iterator_stack_depth(iter) == depth);
            SRL_PATH_TRACE("scan for item=%.*s in hash at depth=%d", (int) item_len, item,
                           (int) srl_iterator_stack_depth(iter));

            if (item_len == 0) continue;

            if (srl_iterator_hash_exists(iter, item, item_len)) {
                srl_parse_next_str(path, expr_idx + 1, route, item, item_len);
                srl_iterator_step_out(iter, srl_iterator_stack_depth(iter) - depth);
            } else {
                assert(srl_iterator_stack_depth(iter) == depth);
                srl_iterator_step_out(iter, 0);
            }
        }
    } else {                                                                            // name
        if (srl_iterator_hash_exists(iter, loc_str, loc_len)) {
            srl_parse_next_str(path, expr_idx + 1, route, loc_str, loc_len);
        }
    }
}

SRL_STATIC_INLINE void
srl_parse_array(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    U8 tag;
    IV depth;
    U32 count;
    srl_iterator_ptr iter = path->iter;
    srl_iterator_stack_ptr stack_ptr = srl_iterator_stack(iter);
    STRLEN route_len = SvCUR(route);
    const char *loc_str;
    STRLEN loc_len;
    SV *loc;

    assert(route != NULL);
    assert(expr_idx >= 0);
    assert(expr_idx <= av_top_index(path->expr));
    assert(stack_ptr != NULL);

    tag   = stack_ptr->tag;
    count = stack_ptr->count;
    depth = srl_iterator_stack_depth(iter);
    loc = *av_fetch(path->expr, expr_idx, 0);
    loc_str = SvPV(loc, loc_len);

    assert(tag == SRL_HDR_ARRAY || (tag >= SRL_HDR_ARRAYREF_LOW && tag <= SRL_HDR_ARRAYREF_HIGH));
    SRL_PATH_TRACE("parse array tag=%d (0x%x) of size=%d at depth=%d, loc=%s",
                    tag, tag, (int) count, (int) depth, loc_str);

    if (is_all(loc_str, loc_len)) {                                                     // *
        for (U32 i = 0; i < count; ++i) {
            SRL_PATH_TRACE("scan for item=%d in array at depth=%d",
                           (int) i, (int) srl_iterator_stack_depth(iter));

            assert(srl_iterator_stack_depth(iter) == depth);
            srl_parse_next_int(path, expr_idx + 1, route, i);
            if (srl_iterator_stack_depth(iter) > depth)
                srl_iterator_next_at_depth(iter, depth);
        }
    //} else if (is_number(loc_str, loc_len)) {
    //    croak("not implemented");
    //} else if (is_list(loc_str, loc_len)) {
    //    croak("not implemented");
    }
}

SRL_STATIC_INLINE int
is_all(const char *str, STRLEN len)
{
    return len == 1 ? str[0] == '*' : 0;
}

SRL_STATIC_INLINE int
is_list(const char *str, STRLEN len)
{
    for (STRLEN i = 0; i < len; ++i) {
        if (str[i] == ',') return 1;
    }

    return 0;
}

SRL_STATIC_INLINE int
is_number(const char *str, STRLEN len)
{
    if (*str == '-') {
        str++;
        len--;
    }

    for (STRLEN i = 0; i < len; ++i) {
        if (str[i] < '0' || str[i] > '9')
            return 0;
    }

    return len != 0;
}

SRL_STATIC_INLINE int
next_item_in_list(const char *list, STRLEN list_len, const char **item_out, STRLEN *item_len_out)
{
    const char *start_pos = *item_out
                          ? *item_out + *item_len_out + 1
                          : list;

    assert(start_pos >= list);

    if (start_pos - list >= (ptrdiff_t) list_len) return 0;
    list_len -= (start_pos - list);
    list = start_pos;

    while (list_len-- && *list != ',') list++;

    *item_out = start_pos;
    *item_len_out = (list - start_pos);
    return 1;
}

SRL_STATIC_INLINE void
print_route(SV *route, const char *str)
{
#ifdef TRACE_SRL_PATH
    STRLEN len;
    const char *ptr = SvPV(route, len);
    SRL_PATH_TRACE("route (%s): %.*s", str, (int) len, ptr);
#endif
}
