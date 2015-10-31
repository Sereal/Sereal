/* Must be defined before including Perl header files or we slow down by 2x! */
#define PERL_NO_GET_CONTEXT

#ifdef __cplusplus
extern "C" {
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"
#endif
#ifdef __cplusplus
}
#endif

//#include <stdlib.h>
//#include <assert.h>
#include "srl_index.h"

#ifdef TRACE_INDEX
#   define SRL_INDEX_TRACE(msg, args...)                                      \
        fprintf(stderr, "%s:%d:%s(): "msg"\n", __FILE__, __LINE__, __func__, args)
#else
#   define SRL_INDEX_TRACE(msg, args...)
#endif

#define SRL_INDEX_SIZE(index)  (((index)->end - (index)->beg))
#define SRL_INDEX_USED(index)  (((index)->ptr - (index)->beg))
#define SRL_INDEX_LEFT(index)  (((index)->end - (index)->ptr))

#define SRL_INDEX_TYPE_MASK         (0xFF000000)
#define SRL_INDEX_SIZE_MASK         (0x00FFFFFF)

#define SRL_INDEX_TYPE_SCALAR_SRL      (0x00000000) // offset points to tag
#define SRL_INDEX_TYPE_POINTER_IDX     (0x01000000) // offset points to child
#define SRL_INDEX_TYPE_ARRAY_IDX       (0x02000000) // offset points to child
#define SRL_INDEX_TYPE_ARRAY_SRL       (0x03000000) // offset points to tag
#define SRL_INDEX_TYPE_HASH_IDX        (0x04000000) // offset points to tag
#define SRL_INDEX_TYPE_HASH_KEY_SMALL  (0x05000000) // key is small, stored directly in idx
#define SRL_INDEX_TYPE_HASH_KEY_LARGE  (0x06000000) // key is large, offset points to tag
#define SRL_INDEX_TYPE_HASH_SRL        (0x07000000) // offset points to tag
#define SRL_INDEX_TYPE_LAST            (0x08000000)

/* Allocate new array (but not the index struct) */
SRL_STATIC_INLINE int
srl_index_init(pTHX_ srl_index_t * index, int size, srl_iterator_t* iter)
{
    assert(size > 0);
    assert(index != NULL);

    fprintf(stderr, "Will reset iterator\n");
    srl_iterator_reset(aTHX_ iter);
    index->iter = iter;

    fprintf(stderr, "Will allocate [%d] bytes\n", size);
    index->beg = NULL;
    Newx(index->beg, size, char);
    if (expect_false(index->beg == NULL))
        return 1;

    index->end = index->beg + size;
    index->ptr = index->beg;

    fprintf(stderr, "Will assert correct size is [%d] bytes\n", size);
    assert(SRL_INDEX_SIZE(index) == (int) size);
    return 0;
}

/* Free index array (but not the index struct) */
SRL_STATIC_INLINE void
srl_index_deinit(pTHX_ srl_index_t *index)
{
    if (index == NULL) return;
    Safefree(index->beg);
}

srl_index_t *
srl_index_build(pTHX_
                srl_iterator_t* iter)
{
    srl_index_t *index = NULL;
    Newx(index, 1, srl_index_t);
    fprintf(stderr, "Allocated index\n");
    if (index == NULL) croak("Out of memory");
    srl_index_init(aTHX_ index, 1000000, iter);
    return index;
}

void
srl_index_destroy(pTHX_
                  srl_index_t *index)
{
    srl_index_deinit(aTHX_ index);
    Safefree(index);
}

#define srl_index_ptr(index)   ((index)->ptr)
#define srl_index_clear(index) STMT_START {                           \
    (index)->ptr = (index)->beg;                                      \
    DEBUG_ASSERT_INDEX_SANE(index);                                   \
} STMT_END
#define srl_index_offset_for_ptr(index, elem)   (((char*) (elem)) - (index)->beg)
#define srl_index_ptr_for_offset(index, offset) ((char*) ((index)->beg + offset))

SRL_STATIC_INLINE char *
srl_index_allocate(pTHX_ srl_index_t *index, size_t size)
{
    char* ptr = 0;
    SRL_INDEX_TRACE("new allocation request of size %zu", size);
    if (expect_false(SRL_INDEX_LEFT(index) < size)) {
        SRL_INDEX_TRACE("not enough space");
        return NULL;
    }

    ptr = index->ptr;
    index->ptr += size;
    DEBUG_ASSERT_INDEX_SANE(index);
    return ptr;
}

SRL_STATIC_INLINE srl_indexed_element_t*
srl_allocate_element(pTHX_ srl_index_t *index, uint32_t type, uint32_t offset)
{
    size_t size;
    srl_indexed_element_t *ptr;
    SRL_INDEX_TRACE("index scalar");

    size = sizeof(srl_indexed_element_t);
    ptr = (srl_indexed_element_t*) srl_index_allocate(aTHX_ index, size);
    if (expect_false(ptr == NULL)) return NULL;

    ptr->offset = offset;
    ptr->flags = type;
    return (srl_indexed_element_t*) ptr;
}

SRL_STATIC_INLINE srl_indexed_array_t*
srl_allocate_array(pTHX_ srl_index_t *index, size_t length, uint32_t type, uint32_t offset)
{
    size_t size;
    srl_indexed_array_t *ptr;
    fprintf(stderr, "Will allocate array with [%zu] elements\n", length);
    SRL_INDEX_TRACE("index array of length %zu", length);
    if (expect_false(length > SRL_INDEX_SIZE_MASK)) return NULL;

    size = sizeof(srl_indexed_array_t) + length * sizeof(srl_indexed_array_element_t);
    fprintf(stderr, "That means %ld bytes\n", (long) size);
    ptr = (srl_indexed_array_t*) srl_index_allocate(aTHX_ index, size);
    if (expect_false(ptr == NULL)) return NULL;

    ptr->offset = offset;
    ptr->flags = type | length;
    fprintf(stderr, "Type %x, Length %lx (%lu), Flags %x\n", type, length, length, ptr->flags);
    return ptr;
}

SRL_STATIC_INLINE srl_indexed_hash_t*
srl_allocate_hash(pTHX_ srl_index_t *index, size_t length, uint32_t type, uint32_t offset)
{
    size_t size;
    srl_indexed_hash_t *ptr;
    SRL_INDEX_TRACE("index hash of length %zu", length);
    if (expect_false(length > SRL_INDEX_SIZE_MASK)) return NULL;

    size = sizeof(srl_indexed_hash_t) + length * sizeof(srl_indexed_hash_element_t);
    ptr = (srl_indexed_hash_t*) srl_index_allocate(aTHX_ index, size);
    if (expect_false(ptr == NULL)) return NULL;

    ptr->offset = offset;
    ptr->flags = type | length;
    return ptr;
}

#if 1

static const char* GetObjType(int type)
{
    static const char* name[] = {
        "SCALAR_SRL",
        "POINTER_IDX",
        "ARRAY_IDX",
        "ARRAY_SRL",
        "HASH_IDX",
        "HASH_KEY_SMALL",
        "HASH_KEY_LARGE",
        "HASH_SRL",
    };

    if (type < 0 || type >= SRL_INDEX_TYPE_LAST) {
        return "UNKNOWN";
    }
    return name[type];
}

static void dump_index_data(srl_index_t* index, srl_indexed_element_t* elem, int depth)
{
    uint32_t type = 0;
    uint32_t size = 0;

    type = (elem->flags & SRL_INDEX_TYPE_MASK) >> 24;
    size = (elem->flags & SRL_INDEX_SIZE_MASK);

    fprintf(stderr, "[%d] Elem %p Flags 0x%08X Type 0x%02X - %s\n",
            depth, elem, elem->flags, type, GetObjType(type));
    switch (type) {
    case (SRL_INDEX_TYPE_SCALAR_SRL >> 24):
        fprintf(stderr, "[%d] Scalar in SRL offset %u\n", depth, elem->offset);
        break;

    case (SRL_INDEX_TYPE_POINTER_IDX >> 24): {
        srl_indexed_element_t* ref = srl_index_ptr_for_offset(index, elem->offset);
        fprintf(stderr, "[%d] Pointer %p in IDX\n", depth, ref);
        dump_index_data(index, ref, depth+1);
        break;
    }

    case (SRL_INDEX_TYPE_ARRAY_IDX >> 24): {
        int j;
        srl_indexed_array_t* array = (srl_indexed_array_t*) elem;

        fprintf(stderr, "[%d] Array %p in IDX with %u elements\n", depth, array, size);
        for (j = 0; j < size; ++j) {
            srl_indexed_element_t* elem = (srl_indexed_element_t*) &array->dataset[j];
            srl_indexed_element_t* ref = 0;
            // ref = srl_index_ptr_for_offset(index, elem->offset);
            fprintf(stderr, "[%d] Member #%d %p", depth, j, elem);
            if (ref) {
                fprintf(stderr, " points to %p", ref);
            }
            fprintf(stderr, "\n");
            dump_index_data(index, ref ? ref : elem, depth+1);
        }
    }

    case (SRL_INDEX_TYPE_ARRAY_SRL >> 24):
        fprintf(stderr, "[%d] Array in SRL offset %u\n", depth, elem->offset);
        break;

    case (SRL_INDEX_TYPE_HASH_IDX >> 24):
    case (SRL_INDEX_TYPE_HASH_KEY_SMALL >> 24):
    case (SRL_INDEX_TYPE_HASH_KEY_LARGE >> 24):
    case (SRL_INDEX_TYPE_HASH_SRL >> 24):
    default:
        fprintf(stderr, "[%d] UNSUPPORTED\n", depth);
        break;
    }
}

static void dump_index(srl_index_t* index)
{
    uint32_t used = 0;

    fprintf(stderr, "NICE index at %p, ", index);
    used = SRL_INDEX_USED(index);
    if (used == 0) {
        fprintf(stderr, "EMPTY\n");
    } else {
        fprintf(stderr, "using %u bytes\n", used);
        dump_index_data(index, (srl_indexed_element_t*) index->beg, 0);
    }
}

static const char* GetSVType(int type)
{
    if (type == SRL_ITERATOR_OBJ_IS_SCALAR) {
        return "SCALAR";
    }
    if (type == SRL_ITERATOR_OBJ_IS_ARRAY) {
        return "ARRAY";
    }
    if (type == SRL_ITERATOR_OBJ_IS_HASH) {
        return "HASH";
    }
    if (type == SRL_ITERATOR_OBJ_IS_ROOT) {
        return "ROOT";
    }
    return "UNKNOWN";
}

static void dump_sv(pTHX_ SV* sv)
{
    if (SvROK(sv)) {
        fprintf(stderr, "%s", "REF\n");
    } else if (SvTYPE(sv) == SVt_PVAV) {
        fprintf(stderr, "%s", "ARRAY\n");
    } else if (SvTYPE(sv) == SVt_PVHV) {
        fprintf(stderr, "%s", "HASH\n");
    } else if (SvTYPE(sv) >= SVt_PVAV) {
        fprintf(stderr, "%s", "OTHER\n");
    } else if (SvIOK(sv)) {
        IV v = SvIV(sv);
        fprintf(stderr, "INT[%ld]\n", (long) v);
    } else if (SvNOK(sv)) {
        double v = SvNV(sv);
        fprintf(stderr, "REAL[%lf]\n", v);
    } else if (SvPOK(sv)) {
        STRLEN l;
        char* v = SvPV(sv, l);
        fprintf(stderr, "STRING[%*.*s]\n", (int) l, (int) l, v);
    }
}

static srl_indexed_element_t* walk_iterator(pTHX_
                                            srl_index_t* index,
                                            int depth);
static srl_indexed_element_t* walk_iterator_scalar(pTHX_
                                                   srl_index_t* index,
                                                   int depth,
                                                   UV offset,
                                                   UV length);
static srl_indexed_element_t* walk_iterator_array(pTHX_
                                                  srl_index_t* index,
                                                  int depth,
                                                  UV offset,
                                                  UV length);
#if 0
static srl_indexed_element_t* walk_iterator_hash(pTHX_
                                                 srl_index_t* iter,
                                                 srl_indexed_element_t* elem,
                                                 int depth,
                                                 UV offset,
                                                 UV length);
#endif

srl_index_t* srl_create_index(pTHX_ srl_iterator_t* iter)
{
    srl_index_t* index = srl_index_build(aTHX_ iter);
    dump_index(index);
    walk_iterator(aTHX_ index, 0);
    dump_index(index);
    return index;
}

static srl_indexed_element_t* walk_iterator(pTHX_
                                            srl_index_t* index,
                                            int depth)
{
    UV type   = 0;
    UV offset = 0;
    UV length = 0;

    srl_iterator_t* iter = index->iter;
    if (srl_iterator_eof(aTHX_ iter)) {
        return 0;
    }

    type = srl_iterator_object_info(aTHX_ iter, &length);
    offset = srl_iterator_offset(aTHX_ iter);
    fprintf(stderr, "GONZO: we got a %s, length %zu, offset %zu\n",
            GetSVType(type), length, offset);
    ++depth;
    switch (type) {
    case SRL_ITERATOR_OBJ_IS_SCALAR:
        return walk_iterator_scalar(aTHX_ index, depth, offset, length);

    case SRL_ITERATOR_OBJ_IS_ARRAY:
        return walk_iterator_array(aTHX_ index, depth, offset, length);

#if 0
    case SRL_ITERATOR_OBJ_IS_HASH:
        return walk_iterator_hash(aTHX_ index, depth, offset, length);
#endif

    case SRL_ITERATOR_OBJ_IS_ROOT:
    default:
        fprintf(stderr, "GONZO: can't handle sereal type\n");
        return 0;
    }
}

static srl_indexed_element_t* walk_iterator_scalar(pTHX_
                                                   srl_index_t* index,
                                                   int depth,
                                                   UV offset,
                                                   UV length)
{
    SV* val = 0;

    srl_iterator_t* iter = index->iter;
    if (srl_iterator_eof(aTHX_ iter)) {
        return 0;
    }

    fprintf(stderr, "GONZO: walking scalar, length %zu, offset %zu\n", length, offset);
    val = srl_iterator_decode(aTHX_ iter);
    dump_sv(aTHX_ val);

    //    if (! elem) {
    return srl_allocate_element(aTHX_ index, SRL_INDEX_TYPE_SCALAR_SRL, offset);
        //    } else {
        // elem->offset = offset;
        //elem->flags = SRL_INDEX_TYPE_SCALAR_SRL;
        //fprintf(stderr, "GONZO: prexisting scalar offset %u\n", offset);
        //}
}

static srl_indexed_element_t* walk_iterator_array(pTHX_
                                                  srl_index_t* index,
                                                  int depth,
                                                  UV offset,
                                                  UV length)
{
    int pos = 0;
    srl_indexed_array_t* array = 0;

    srl_iterator_t* iter = index->iter;
    if (srl_iterator_eof(aTHX_ iter)) {
        return 0;
    }

    fprintf(stderr, "GONZO: walking array, length %zu, offset %zu\n", length, offset);
    array = srl_allocate_array(aTHX_
                               index,
                               length,
                               SRL_INDEX_TYPE_ARRAY_IDX,
                               offset);
    fprintf(stderr, "GONZO: allocated array %p\n", array);
    srl_iterator_step_in(aTHX_ iter, 1);
    while (1) {
        srl_indexed_element_t* cur;
        srl_indexed_element_t* elem;
        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }
        if (pos >= length) {
            break;
        }

        fprintf(stderr, "GONZO: processing element %d\n", pos);
        elem = walk_iterator(aTHX_ index, depth);
        cur = &array->dataset[pos];
        cur->offset = srl_index_offset_for_ptr(index, elem);
        cur->flags = SRL_INDEX_TYPE_POINTER_IDX;

        ++pos;
        srl_iterator_next(aTHX_ iter, 1);
    }
    srl_iterator_step_out(aTHX_ iter, 1);

    return array;
}

#if 0
static srl_indexed_element_t* walk_iterator_hash(pTHX_
                                                 srl_index_t* index,
                                                 srl_indexed_element_t* elem,
                                                 int depth,
                                                 UV offset,
                                                 UV length)
{
    int pos = 0;
    srl_indexed_hash_t* hash = 0;

    srl_iterator_t* iter = index->iter;
    if (srl_iterator_eof(aTHX_ iter)) {
        return 0;
    }

    fprintf(stderr, "GONZO: walking hash, length %zu, offset %zu\n", length, offset);
    hash = srl_allocate_hash(aTHX_
                             index,
                             length,
                             SRL_INDEX_TYPE_HASH_IDX,
                             offset);

    srl_iterator_step_in(aTHX_ iter, 1);
    while (1) {
        SV* key;
        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }
        if (pos >= length) {
            break;
        }

        key = srl_iterator_hash_key_sv(aTHX_ iter);
        fprintf(stderr, "GONZO: processing key %d\n", pos);
        dump_sv(aTHX_ key);

        srl_iterator_next(aTHX_ iter, 1);

        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }
        walk_iterator(aTHX_ index, &elem->dataset[pos], depth);

        ++pos;
        srl_iterator_next(aTHX_ iter, 1);
    }
    srl_iterator_step_out(aTHX_ iter, 1);

    return elem;
}
#endif

#endif
