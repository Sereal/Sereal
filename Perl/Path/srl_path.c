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
#include "Iterator/srl_iterator.h"
#include "srl_reader_error.h"

#ifdef TRACE_SRL_PATH
#   define SRL_PATH_TRACE(msg, args...) \
        fprintf(stderr, "%s:%d:%s(): "msg"\n", __FILE__, __LINE__, __func__, ## args)
#else
#   define SRL_PATH_TRACE(msg, args...)
#endif

#define CLEAR_RESULTS(p) STMT_START {   \
    if ((p)->results) {                 \
        SvREFCNT_dec((p)->results);     \
        (p)->results = NULL;            \
    }                                   \
} STMT_END

#define CLEAR_ITERATOR(p) STMT_START {       \
    if ((p)->iter && (p)->i_own_iterator) {  \
        srl_destroy_iterator(aTHX_ p->iter); \
    }                                        \
} STMT_END

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
SRL_STATIC_INLINE void srl_parse_array_range(pTHX_ srl_path_t *path, int expr_idx, SV *route, int *range);
SRL_STATIC_INLINE void srl_parse_array_item(pTHX_ srl_path_t *path, int expr_idx, SV *route, I32 idx);
SRL_STATIC_INLINE void run_until(pTHX_ srl_path_t *path, UV expected_depth, U32 expected_idx);

SRL_STATIC_INLINE int is_all(const char *str, STRLEN len);
SRL_STATIC_INLINE int is_list(const char *str, STRLEN len);
SRL_STATIC_INLINE int is_number(const char *str, STRLEN len);
SRL_STATIC_INLINE int * is_range(const char *str, STRLEN len, int *out);
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
    path->i_own_iterator = 0;

    if (opt != NULL) {}
    return path;
}

void
srl_destroy_path(pTHX_ srl_path_t *path)
{
    CLEAR_RESULTS(path);
    CLEAR_ITERATOR(path);
    Safefree(path);
}

void
srl_path_set(pTHX_ srl_path_t *path, SV *src)
{
    path->expr = NULL;
    CLEAR_RESULTS(path);
    CLEAR_ITERATOR(path);

    if (sv_isobject(src) && sv_isa(src, "Sereal::Path::Iterator")) {
        path->iter = INT2PTR(srl_iterator_ptr, SvIV((SV*) SvRV(src)));
        path->i_own_iterator = 0;
    } else if (SvPOK(src)) {
        path->iter = srl_build_iterator_struct(aTHX_ NULL);
        path->i_own_iterator = 1;
        srl_iterator_set(aTHX_ path->iter, src);
    } else {
        croak("Sereal::Path: input should be either Sereal::Path::Iterator object or encoded Sereal document");
    }
}

void
srl_path_traverse(pTHX_ srl_path_t *path, AV *expr, SV *route)
{
    SV *route_copy;
    if (!path->iter) croak("No document to traverse");

    assert(expr != NULL);
    assert(route != NULL);

    CLEAR_RESULTS(path);

    path->results = newAV();
    path->expr = expr;
    route_copy = sv_2mortal(newSVsv(route));

    srl_iterator_reset(aTHX_ path->iter);
    srl_parse_next(aTHX_ path, 0, route_copy);
}

SV *
srl_path_results(pTHX_ srl_path_t *path)
{
    AV *results = path->results ? path->results : newAV();
    path->results = NULL;
    return sv_2mortal(newRV_noinc((SV*) results));
}

SRL_STATIC_INLINE void
srl_parse_next(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    U32 type;
    srl_iterator_t *iter = path->iter;

    assert(route != NULL);
    SRL_PATH_TRACE("expr_idx=%d", expr_idx);

    if (srl_iterator_eof(aTHX_ iter)) return;
    if (expr_idx > av_len(path->expr)) { /* scaned entiry expr */
        SV *res;
        print_route(route, "to decode");
        res = srl_iterator_decode(aTHX_ iter);
        SvREFCNT_inc(res);
        av_push(path->results, res); /* TODO store route if needed */
        return;
    }

    type = srl_iterator_info(aTHX_ iter, NULL, NULL, NULL);
    if ((type & SRL_ITERATOR_INFO_HASH) == SRL_ITERATOR_INFO_HASH) {
        srl_iterator_step_in(aTHX_ iter, 1);
        srl_parse_hash(aTHX_ path, expr_idx, route);
    } else if ((type & SRL_ITERATOR_INFO_ARRAY) == SRL_ITERATOR_INFO_ARRAY) {
        srl_iterator_step_in(aTHX_ iter, 1);
        srl_parse_array(aTHX_ path, expr_idx, route);
    }
}

SRL_STATIC_INLINE void
srl_parse_next_str(pTHX_ srl_path_t *path, int expr_idx, SV *route,
                   const char *str, STRLEN len)
{
    STRLEN route_len = SvCUR(route);
    sv_catpvf(route, ";%.*s", (int) len, str); /* append parsed object to route */
    srl_parse_next(aTHX_ path, expr_idx, route);
    SvCUR_set(route, route_len);  /* restore original value */
}

SRL_STATIC_INLINE void
srl_parse_next_int(pTHX_ srl_path_t *path, int expr_idx, SV *route, IV n)
{
    STRLEN route_len = SvCUR(route);
    sv_catpvf(route, ";[%"UVuf"]", n); /* append parsed object to route */
    srl_parse_next(aTHX_ path, expr_idx, route);
    SvCUR_set(route, route_len);  /* restore original value */
}

SRL_STATIC_INLINE void
srl_parse_hash(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    const char *loc_str;
    STRLEN loc_len;
    SV *loc;

    assert(route != NULL);
    assert(expr_idx >= 0);
    assert(expr_idx <= av_len(path->expr));

    loc   = *av_fetch(path->expr, expr_idx, 0);
    loc_str = SvPV(loc, loc_len);

    if (is_all(loc_str, loc_len)) {                                                     /* * */
        srl_parse_hash_all(aTHX_ path, expr_idx, route);
    } else if (is_list(loc_str, loc_len)) {                                             /* [name1,name2] */
        srl_parse_hash_list(aTHX_ path, expr_idx, route, loc_str, loc_len);
    } else {                                                                            /* name */
        srl_parse_hash_item(aTHX_ path, expr_idx, route, loc_str, loc_len);
    }
}

SRL_STATIC_INLINE void
srl_parse_hash_all(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    srl_iterator_ptr iter = path->iter;
    IV depth = srl_iterator_stack_depth(aTHX_ iter);
    U32 length = srl_iterator_stack_length(aTHX_ iter);
    const char *item = NULL;
    STRLEN item_len;
    U32 idx;

    SRL_PATH_TRACE("parse all items in hash of size=%d at depth=%"IVdf, length, depth);

    for (idx = 0; idx < length; idx += 2) {
        run_until(aTHX_ path, depth, idx);
        srl_iterator_hash_key(aTHX_ iter, &item, &item_len);
        SRL_PATH_TRACE("walk over item=%.*s in hash at depth=%"IVdf,
                       (int) item_len, item, srl_iterator_stack_depth(aTHX_ iter));
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
                   (int) list_len, list, srl_iterator_stack_length(aTHX_ iter), depth);

    while (next_item_in_list(list, list_len, &item, &item_len)) {
        assert(srl_iterator_stack_depth(aTHX_ iter) == depth);
        if (item_len == 0) continue;

        SRL_PATH_TRACE("scan for item=%.*s in hash at depth=%"IVdf,
                       (int) item_len, item, srl_iterator_stack_depth(aTHX_ iter));

        if (srl_iterator_hash_exists(aTHX_ iter, item, item_len) != SRL_ITER_NOT_FOUND) {
            srl_parse_next_str(aTHX_ path, expr_idx + 1, route, item, item_len);
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
                   srl_iterator_stack_length(aTHX_ iter),
                   srl_iterator_stack_depth(aTHX_ iter));

    if (srl_iterator_hash_exists(aTHX_ iter, str, str_len) != SRL_ITER_NOT_FOUND) {
        srl_parse_next_str(aTHX_ path, expr_idx + 1, route, str, str_len);
    }
}

SRL_STATIC_INLINE void
srl_parse_array(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    int range[3];
    const char *loc_str;
    STRLEN loc_len;
    SV *loc;

    assert(route != NULL);
    assert(expr_idx >= 0);
    assert(expr_idx <= av_len(path->expr));

    loc = *av_fetch(path->expr, expr_idx, 0);
    loc_str = SvPV(loc, loc_len);

    if (is_all(loc_str, loc_len)) {                                                     /* * */
        srl_parse_array_all(aTHX_ path, expr_idx, route);
    } else if (is_number(loc_str, loc_len)) {                                           /* [10] */
        srl_parse_array_item(aTHX_ path, expr_idx, route, atoi(loc_str));
    } else if (is_list(loc_str, loc_len)) {                                             /* [0,1,2] */
        srl_parse_array_list(aTHX_ path, expr_idx, route, loc_str, loc_len);
    } else if (is_range(loc_str, loc_len, (int*) &range)) {                             /* [start:stop:step] */
        srl_parse_array_range(aTHX_ path, expr_idx, route, (int*) &range);
    }
}

SRL_STATIC_INLINE void
srl_parse_array_all(pTHX_ srl_path_t *path, int expr_idx, SV *route)
{
    U32 idx;
    srl_iterator_ptr iter = path->iter;
    IV depth  = srl_iterator_stack_depth(aTHX_ iter);
    U32 length = srl_iterator_stack_length(aTHX_ iter);

    SRL_PATH_TRACE("parse all items in array of size=%d at depth=%"IVdf, length, depth);

    for (idx = 0; idx < length; ++idx) {
        run_until(aTHX_ path, depth, idx);

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
                   (int) list_len, list, srl_iterator_stack_length(aTHX_ iter), depth);

    while (next_item_in_list(list, list_len, &item, &item_len)) {
        if (item_len == 0) continue;

        idx = atoi(item);
        SRL_PATH_TRACE("scan for item=%d in array at depth=%"IVdf,
                       idx, srl_iterator_stack_depth(aTHX_ iter));

        if (srl_iterator_array_exists(aTHX_ iter, idx) != SRL_ITER_NOT_FOUND) {
            srl_iterator_array_goto(aTHX_ iter, idx);
            srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
        }
    }
}

SRL_STATIC_INLINE void
srl_parse_array_range(pTHX_ srl_path_t *path, int expr_idx, SV *route, int *range)
{
    I32 idx, start, stop, step;
    srl_iterator_ptr iter = path->iter;
    U32 length = srl_iterator_stack_length(aTHX_ iter);
    IV depth = srl_iterator_stack_depth(aTHX_ iter);

    start = range[0];
    stop = range[1];
    step = range[2];

#   define SRL_MIN(a,b) (((a)<(b))?(a):(b))
#   define SRL_MAX(a,b) (((a)>(b))?(a):(b))
    start = start < 0 ? SRL_MAX(0, start + (I32) length) : SRL_MIN((I32) length, start);
    stop  = stop  < 0 ? SRL_MAX(0, stop  + (I32) length) : SRL_MIN((I32) length, stop);
    step  = step ? step : 1;

    if (step < 0) croak("negative step in not supported");

    SRL_PATH_TRACE("parse items '%d:%d:%d' in array of size=%d at depth=%"IVdf,
                   start, stop, step, length, depth);

    for (idx = start; idx < stop; idx += step) {
        run_until(aTHX_ path, depth, idx);

        SRL_PATH_TRACE("walk over item=%d in array at depth=%d",
                       idx, (int) srl_iterator_stack_depth(aTHX_ iter));

        srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
    }
}

SRL_STATIC_INLINE void
srl_parse_array_item(pTHX_ srl_path_t *path, int expr_idx, SV *route, I32 idx)
{
    srl_iterator_ptr iter = path->iter;
    SRL_PATH_TRACE("parse item %d in array of size=%d at depth=%"IVdf,
                   idx, srl_iterator_stack_length(aTHX_ iter), srl_iterator_stack_depth(aTHX_ iter));

    if (srl_iterator_array_exists(aTHX_ iter, idx) != SRL_ITER_NOT_FOUND) {
        srl_iterator_array_goto(aTHX_ iter, idx);
        srl_parse_next_int(aTHX_ path, expr_idx + 1, route, idx);
    }
}

SRL_STATIC_INLINE void
run_until(pTHX_ srl_path_t *path, UV expected_depth, U32 expected_idx)
{
    U32 idx;
    srl_iterator_ptr iter = path->iter;
    IV depth = srl_iterator_stack_depth(aTHX_ iter);

    SRL_PATH_TRACE("expected_depth=%"UVuf" expected_idx=%u at depth=%"IVdf,
                   expected_depth, expected_idx, depth);

    if ((IV) expected_depth > depth) {
        croak("run_until: expected_depth > depth (%"UVuf" > %"IVdf")", expected_depth, depth);
    }

    srl_iterator_step_out(aTHX_ iter, depth - expected_depth);
    assert((IV) expected_depth == srl_iterator_stack_depth(aTHX_ iter));

    idx = srl_iterator_stack_index(aTHX_ iter);
    if (expected_idx < idx) {
        croak("run_until: expected_idx < idx (%u < %d))", expected_idx, idx);
    }

    srl_iterator_next(aTHX_ iter, expected_idx - idx);
    assert(expected_idx == srl_iterator_stack_index(aTHX_ iter));
}

SRL_STATIC_INLINE int
is_all(const char *str, STRLEN len)
{
    return len == 1 ? str[0] == '*' : 0;
}

SRL_STATIC_INLINE int
is_list(const char *str, STRLEN len)
{
    STRLEN i;
    for (i = 0; i < len; ++i) {
        if (str[i] == ',') return 1;
    }

    return 0;
}

int * _is_range(const char *str, STRLEN len, int *out) { return is_range(str, len, out); }

SRL_STATIC_INLINE int *
is_range(const char *str, STRLEN len, int *out)
{
    char *ptr;
    STRLEN pos[2];
    STRLEN i, ndel;
    STRLEN start_len, stop_len, step_len;
    int valid = 0;

    for (i = 0, ndel = 0; i < len; ++i) {
        if (str[i] == ':') {
            if (ndel >= 2) return NULL;
            pos[ndel++] = i;
        }
    }

    switch (ndel) {
        case 2: /* [start:stop:step] */
            start_len = pos[0];
            stop_len = pos[1] - pos[0] - 1;
            step_len = len - pos[1] - 1;
            assert(start_len + stop_len + step_len + 2 == len);

            valid =    (start_len != 0 || stop_len != 0 || step_len != 0)
                    && (start_len == 0 || is_number(str, start_len))
                    && (stop_len == 0 || is_number(str + pos[0] + 1, stop_len))
                    && (step_len == 0 || is_number(str + pos[1] + 1, step_len));

            if (!valid) return NULL;

            ptr = strndup(str, len);
            ptr[pos[0]] = '\0';
            ptr[pos[1]] = '\0';

            out[0] = start_len == 0 ? 0 : atoi(ptr);                        /* start */
            out[1] = stop_len == 0 ? 0x7FFFFFFF : atoi(ptr+ pos[0] + 1);    /* stop */
            out[2] = step_len == 0 ? 1 : atoi(ptr + pos[1] + 1);            /* step */
            free((void*) ptr);

            return out;

        case 1: /* [start:stop] */
            start_len = pos[0];
            stop_len = len - pos[0] - 1;
            assert(start_len + stop_len + 1 == len);

            valid =    (start_len != 0 || stop_len != 0)
                    && (start_len == 0 || is_number(str, pos[0]))
                    && (stop_len == 0 || is_number(str + pos[0] + 1, stop_len));

            if (!valid) return NULL;

            ptr = strndup(str, len);
            ptr[pos[0]] = '\0';

            out[0] = start_len == 0 ? 0 : atoi(ptr);                        /* start */
            out[1] = stop_len == 0 ? 0x7FFFFFFF : atoi(ptr+ pos[0] + 1);    /* stop */
            out[2] = 1;
            free((void*) ptr);

            return out;
    }

    return NULL;
}

SRL_STATIC_INLINE int
is_number(const char *str, STRLEN len)
{
    STRLEN i;
    if (*str == '-') {
        str++;
        len--;
    }

    for (i = 0; i < len; ++i) {
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
