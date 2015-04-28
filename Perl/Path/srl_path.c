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

#if PERL_VERSION < 12
#   define SVt_RV_FAKE SVt_RV
#else
#   define SVt_RV_FAKE SVt_IV
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
SRL_STATIC_INLINE void srl_parse_next_int(pTHX_ srl_path_t *path, int expr_idx, SV *route, IV n);
SRL_STATIC_INLINE void srl_parse_next_str(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                                          const char *str, STRLEN len);

SRL_STATIC_INLINE void srl_parse_hash(pTHX_ srl_path_t *path, int expr_idx, SV *route);
SRL_STATIC_INLINE void srl_parse_hash_all(pTHX_ srl_path_t *path, int expr_idx, SV *route);
SRL_STATIC_INLINE void srl_parse_hash_list(pTHX_ srl_path_t *path, int expr_idx, SV *route, const char *list, STRLEN list_len);
SRL_STATIC_INLINE void srl_parse_hash_item(pTHX_ srl_path_t *path, int expr_idx, SV *route, const char *str, STRLEN str_len);

SRL_STATIC_INLINE void srl_parse_array(pTHX_ srl_path_t *path, int expr_idx, SV *route);
SRL_STATIC_INLINE void srl_parse_array_all(pTHX_ srl_path_t *path, int expr_idx, SV *route);
SRL_STATIC_INLINE void srl_parse_array_list(pTHX_ srl_path_t *path, int expr_idx, SV *route, const char *list, STRLEN list_len);
SRL_STATIC_INLINE void srl_parse_array_item(pTHX_ srl_path_t *path, int expr_idx, SV *route, I32 idx);

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
        srl_destroy_iterator(aTHX_ path->iter);

    Safefree(path);
    return;
}

void
srl_path_reset(pTHX_ srl_path_t *path, SV *src)
{
    path->expr = NULL;

    if (path->results) {
        SvREFCNT_dec(path->results);
        path->results = NULL;
    }

    if (path->iter) srl_destroy_iterator(aTHX_ path->iter);
    path->iter = srl_build_iterator_struct(aTHX_ NULL);

    if (sv_isa(src, "Sereal::Path::Iterator")) {
        croak("not implemented");
    } else {
        srl_iterator_set_document(aTHX_ path->iter, src);
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

    path->results = newAV();
    path->expr = expr; // TODO perhaps, copy expr
    route_copy = sv_2mortal(newSVsv(route));

    srl_iterator_reset(aTHX_ path->iter);
    srl_parse_next(aTHX_ path, 0, route_copy);
}

SV *
srl_path_results(pTHX_ srl_path_t *path)
{
    SV *into, *referent;
    AV *results = path->results;
    if (!results)
        return sv_2mortal(newSV_type(SVt_NULL));

    path->results = NULL;
    referent = (SV*) results;
    into = sv_2mortal(newSV_type(SVt_NULL));

    // copy-pasted from decoder
    sv_upgrade(into, SVt_RV_FAKE);
    SvTEMP_off(referent);
    SvRV_set(into, referent);
    SvROK_on(into);
    return into;
}

SRL_STATIC_INLINE void
srl_parse_next(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_t *iter = path->iter;

    assert(route != NULL);
    SRL_PATH_TRACE("expr_idx=%d", expr_idx);

    if (srl_iterator_eof(aTHX_ iter)) return;
    if (expr_idx > av_top_index(path->expr)) { // scaned entiry expr
        print_route(route, "to decode");
        SV *res = srl_iterator_decode(aTHX_ iter);
        SvREFCNT_inc(res); // TODO ????
        av_push(path->results, res); // TODO store route if needed
        return;
    }

    switch (srl_iterator_object_info(aTHX_ iter, NULL)) {
        case SRL_ITERATOR_OBJ_IS_HASH:
            srl_iterator_step_in(aTHX_ iter, 1);
            srl_parse_hash(aTHX_ path, expr_idx, route);
            break;

        case SRL_ITERATOR_OBJ_IS_ARRAY:
            srl_iterator_step_in(aTHX_ iter, 1);
            srl_parse_array(aTHX_ path, expr_idx, route);
            break;
    }
}

SRL_STATIC_INLINE void
srl_parse_next_str(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                   const char *str, STRLEN len)
{
    STRLEN route_len = SvCUR(route);
    sv_catpvf(route, ";%.*s", (int) len, str); // append parsed object to route
    srl_parse_next(aTHX_ path, expr_idx, route);
    SvCUR_set(route, route_len);  // restore original value
}

SRL_STATIC_INLINE void
srl_parse_next_int(pTHX_ srl_path_t *path, int expr_idx, SV *route, IV n)
{
    STRLEN route_len = SvCUR(route);
    sv_catpvf(route, ";[%"UVuf"]", n); // append parsed object to route
    srl_parse_next(aTHX_ path, expr_idx, route);
    SvCUR_set(route, route_len);  // restore original value
}

SRL_STATIC_INLINE void
srl_parse_hash(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_ptr iter = path->iter;
    const char *loc_str;
    STRLEN loc_len;
    SV *loc;

    assert(route != NULL);
    assert(expr_idx >= 0);
    assert(expr_idx <= av_top_index(path->expr));
    assert(srl_iterator_stack(aTHX_ iter) != NULL);

    loc   = *av_fetch(path->expr, expr_idx, 0);
    loc_str = SvPV(loc, loc_len);

    if (is_all(loc_str, loc_len)) {                                                     // *
        srl_parse_hash_all(aTHX_ path, expr_idx, route);
    } else if (is_list(loc_str, loc_len)) {                                             // [name1,name2]
        srl_parse_hash_list(aTHX_ path, expr_idx, route, loc_str, loc_len);
    } else {                                                                            // name
        srl_parse_hash_item(aTHX_ path, expr_idx, route, loc_str, loc_len);
    }
}

SRL_STATIC_INLINE void
srl_parse_hash_all(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_ptr iter = path->iter;
    srl_iterator_stack_ptr stack_ptr = srl_iterator_stack(aTHX_ iter);
    IV expected_depth = srl_iterator_stack_depth(aTHX_ iter);
    U32 expected_idx = stack_ptr->idx;
    U32 count = stack_ptr->count;
    const char *item = NULL;
    STRLEN item_len;
    U32 idx;

    assert(expected_idx == count); // we're at the begining
    SRL_PATH_TRACE("parse all items in hash of size=%d at depth=%"IVdf,
                   count, expected_depth);

    for (idx = 0; idx < count; idx += 2, expected_idx -= 2) {
        srl_iterator_next_until_depth_and_idx(aTHX_ iter, expected_depth, expected_idx);
        assert(srl_iterator_stack(aTHX_ iter)->idx == expected_idx);
        assert(srl_iterator_stack_depth(aTHX_ iter) == expected_depth);

        item = srl_iterator_hash_key(aTHX_ iter, &item_len);
        SRL_PATH_TRACE("walk over item=%.*s in hash at depth=%"IVdf,
                       (int) item_len, item, srl_iterator_stack_depth(aTHX_ iter));

        srl_iterator_next(aTHX_ iter, 1);
        srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
    }
}

SRL_STATIC_INLINE void
srl_parse_hash_list(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                   const char *list, STRLEN list_len)
{
    STRLEN item_len;
    const char *item = NULL;
    srl_iterator_ptr iter = path->iter;
    IV depth = srl_iterator_stack_depth(aTHX_ iter);

    SRL_PATH_TRACE("parse items '%.*s' in hash of size=%d at depth=%"IVdf,
                   (int) list_len, list, srl_iterator_stack(aTHX_ iter)->count, depth);

    while (next_item_in_list(list, list_len, &item, &item_len)) {
        assert(srl_iterator_stack_depth(aTHX_ iter) == depth);
        if (item_len == 0) continue;

        SRL_PATH_TRACE("scan for item=%.*s in hash at depth=%"IVdf,
                       (int) item_len, item, srl_iterator_stack_depth(aTHX_ iter));

        if (srl_iterator_hash_exists(aTHX_ iter, item, item_len)) {
            srl_parse_next_str(aTHX_ path, expr_idx + 1, route, item, item_len);
            srl_iterator_step_out(aTHX_ iter, srl_iterator_stack_depth(aTHX_ iter) - depth);
        } else {
            srl_iterator_step_out(aTHX_ iter, 0);
        }
    }
}

SRL_STATIC_INLINE void
srl_parse_hash_item(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                    const char *str, STRLEN str_len)
{
    srl_iterator_ptr iter = path->iter;
    SRL_PATH_TRACE("parse item '%.*s' in hash of size=%d at depth=%"IVdf,
                   (int) str_len, str,
                   srl_iterator_stack(aTHX_ iter)->count,
                   srl_iterator_stack_depth(aTHX_ iter));

    if (srl_iterator_hash_exists(aTHX_ iter, str, str_len)) {
        srl_parse_next_str(aTHX_ path, expr_idx + 1, route, str, str_len);
    }
}

SRL_STATIC_INLINE void
srl_parse_array(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_ptr iter = path->iter;
    srl_iterator_stack_ptr stack_ptr = srl_iterator_stack(aTHX_ iter);
    const char *loc_str;
    STRLEN loc_len;
    SV *loc;

    assert(route != NULL);
    assert(expr_idx >= 0);
    assert(expr_idx <= av_top_index(path->expr));
    assert(srl_iterator_stack(aTHX_ iter) != NULL);

    loc = *av_fetch(path->expr, expr_idx, 0);
    loc_str = SvPV(loc, loc_len);

    if (is_all(loc_str, loc_len)) {                                                     // *
        srl_parse_array_all(aTHX_ path, expr_idx, route);
    } else if (is_number(loc_str, loc_len)) {                                           // [10]
        srl_parse_array_item(aTHX_ path, expr_idx, route, atoi(loc_str));
    } else if (is_list(loc_str, loc_len)) {                                             // [0,1,2]
        srl_parse_array_list(aTHX_ path, expr_idx, route, loc_str, loc_len);
    }
}

SRL_STATIC_INLINE void
srl_parse_array_all(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_ptr iter = path->iter;
    srl_iterator_stack_ptr stack_ptr = srl_iterator_stack(aTHX_ iter);
    IV expected_depth  = srl_iterator_stack_depth(aTHX_ iter);
    U32 expected_idx = stack_ptr->idx;
    U32 count = stack_ptr->count;
    U32 idx;

    assert(expected_idx == count); // we're at the begining
    SRL_PATH_TRACE("parse all items in array of size=%d at depth=%"IVdf,
                   count, expected_depth);

    for (idx = 0; idx < count; ++idx, --expected_idx) {
        srl_iterator_next_until_depth_and_idx(aTHX_ iter, expected_depth, expected_idx);
        assert(srl_iterator_stack_depth(aTHX_ iter) == expected_depth);

        SRL_PATH_TRACE("walk over item=%d in array at depth=%d",
                       idx, (int) srl_iterator_stack_depth(aTHX_ iter));

        srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
    }
}

SRL_STATIC_INLINE void
srl_parse_array_list(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                     const char *list, STRLEN list_len)
{
    I32 idx;
    STRLEN item_len;
    const char *item = NULL;
    srl_iterator_ptr iter = path->iter;
    IV depth = srl_iterator_stack_depth(aTHX_ iter);

    SRL_PATH_TRACE("parse items '%.*s' in array of size=%d at depth=%"IVdf,
                   (int) list_len, list, srl_iterator_stack(iter)->count, depth);

    while (next_item_in_list(list, list_len, &item, &item_len)) {
        assert(srl_iterator_stack_depth(aTHX_ iter) == depth);
        if (item_len == 0) continue;

        idx = atoi(item);
        SRL_PATH_TRACE("scan for item=%d in array at depth=%"IVdf,
                       idx, srl_iterator_stack_depth(aTHX_ iter));

        srl_iterator_array_goto(aTHX_ iter, idx);
        srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
        srl_iterator_step_out(aTHX_ iter, srl_iterator_stack_depth(aTHX_ iter) - depth);
    }
}

SRL_STATIC_INLINE void
srl_parse_array_item(pTHX_ srl_path_t *path, int expr_idx, SV *route, I32 idx)
{
    srl_iterator_ptr iter = path->iter;
    SRL_PATH_TRACE("parse item %d in array of size=%d at depth=%"IVdf,
                   idx, srl_iterator_stack(aTHX_ iter)->count, srl_iterator_stack_depth(aTHX_ iter));

    srl_iterator_array_goto(aTHX_ iter, idx);
    srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
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
