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

#define SRL_INDEX_TYPE_MASK           (0xFF000000)
#define SRL_INDEX_SIZE_MASK           (0x00FFFFFF)
#define SRL_INDEX_FLAG_MASK           (0xc0000000)

#define SRL_INDEX_FLAG_POINTER_INDEX  (1<<31)      // offset point to index
#define SRL_INDEX_FLAG_KEY_SMALL      (1<<30)      // key is small, stored in index

#define SRL_INDEX_FLAG_GET(var, flag) (var &   (flag))
#define SRL_INDEX_FLAG_SET(var, flag) (var |=  (flag))
#define SRL_INDEX_FLAG_CLR(var, flag) (var &= ~(flag))

#define SRL_INDEX_TYPE_EMPTY          (0x00000000)
#define SRL_INDEX_TYPE_SCALAR         (0x01000000)
#define SRL_INDEX_TYPE_ARRAY          (0x02000000)
#define SRL_INDEX_TYPE_HASH           (0x03000000)
#define SRL_INDEX_TYPE_LAST           (0x04000000)

#define SRL_INDEX_TYPE_GET(var)       ((var & SRL_INDEX_TYPE_MASK) & ~SRL_INDEX_FLAG_MASK)

/* Allocate new array (but not the index struct) */
SRL_STATIC_INLINE int
srl_index_init(pTHX_
               srl_index_t* index,
               srl_iterator_t* iter,
               srl_index_options_t* options)
{
    assert(options->memory_size > 0);
    assert(index != NULL);

    srl_iterator_reset(aTHX_ iter);
    index->iter = iter;
    index->options = *options;

    index->beg = NULL;
    Newx(index->beg, index->options.memory_size, char);
    if (expect_false(index->beg == NULL))
        return 1;

    index->end = index->beg + index->options.memory_size;
    index->ptr = index->beg;

    assert(SRL_INDEX_SIZE(index) == index->options.memory_size);
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
                srl_iterator_t* iter,
                srl_index_options_t* options)
{
    srl_index_t *index = NULL;
    Newx(index, 1, srl_index_t);
    if (index == NULL) croak("Out of memory");
    srl_index_init(aTHX_ index, iter, options);
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
    memset(ptr, 0, size);
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
    SRL_INDEX_TRACE("index array of length %zu", length);
    if (expect_false(length > SRL_INDEX_SIZE_MASK)) return NULL;

    size = sizeof(srl_indexed_array_t) + length * sizeof(srl_indexed_array_element_t);
    ptr = (srl_indexed_array_t*) srl_index_allocate(aTHX_ index, size);
    if (expect_false(ptr == NULL)) return NULL;

    ptr->offset = offset;
    ptr->flags = type | length;
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

static const char* GetObjType(uint32_t type)
{
    static const char* type_name[SRL_INDEX_TYPE_LAST] = {
        "EMPTY",
        "SCALAR",
        "ARRAY",
        "HASH",
    };
    static char ret[100];

    uint32_t flag_index = SRL_INDEX_FLAG_GET(type, SRL_INDEX_FLAG_POINTER_INDEX);
    type = SRL_INDEX_TYPE_GET(type);
    switch (type) {
        case SRL_INDEX_TYPE_EMPTY:
            sprintf(ret, "%s",
                    type_name[type >> 24]);
            break;

        case SRL_INDEX_TYPE_SCALAR:
        case SRL_INDEX_TYPE_ARRAY:
        case SRL_INDEX_TYPE_HASH:
            sprintf(ret, "%s:%s",
                    type_name[type >> 24],
                    flag_index ? "PtrIndex" : "PtrSereal");
            break;

        default:
            sprintf(ret, "%s",
                    "UNKNOWN");
            break;
    }

    return ret;
}

static void dump_index_data(srl_index_t* index, srl_indexed_element_t* elem, int depth)
{
    uint32_t type = elem->flags & SRL_INDEX_TYPE_MASK;
    uint32_t size = elem->flags & SRL_INDEX_SIZE_MASK;
    uint32_t flag_index = SRL_INDEX_FLAG_GET(type, SRL_INDEX_FLAG_POINTER_INDEX);
    uint32_t flag_small = SRL_INDEX_FLAG_GET(type, SRL_INDEX_FLAG_KEY_SMALL);

    fprintf(stderr, "=[%d] Elem %p Flags 0x%08X Type %s\n",
            depth, elem, elem->flags, GetObjType(type));
    type = SRL_INDEX_TYPE_GET(type);

    switch (type) {
        case SRL_INDEX_TYPE_EMPTY:
            // empty slot, probably for an oversized hash table, just ignore
            break;

        case SRL_INDEX_TYPE_SCALAR:
            if (flag_index) {
                // We don't show this scalar pointing to the index, since it is
                // just a placeholder to the real indexed element
                elem = (srl_indexed_element_t*) srl_index_ptr_for_offset(index, elem->offset);
                dump_index_data(index, elem, depth);  // "tail optimization"
            } else {
                fprintf(stderr, "=[%d] Scalar offset %u pointing to %s\n",
                        depth, elem->offset, "Sereal");
            }
            break;

        case SRL_INDEX_TYPE_ARRAY: {
            int j;
            srl_indexed_array_t* array = (srl_indexed_array_t*) elem;

            fprintf(stderr, "=[%d] Array offset %u pointing to %s, %u elements\n",
                    depth, elem->offset,
                    flag_index ? "Index" : "Sereal",
                    size);

            for (j = 0; j < size; ++j) {
                srl_indexed_array_element_t* elem = (srl_indexed_array_element_t*) &array->dataset[j];
                fprintf(stderr, "=[%d] Member #%d %p\n", depth, j, elem);
                dump_index_data(index, (srl_indexed_element_t*) elem, depth+1);
            }
            break;
        }

        case SRL_INDEX_TYPE_HASH: {
            int j;
            srl_indexed_hash_t* hash = (srl_indexed_hash_t*) elem;

            fprintf(stderr, "=[%d] Hash offset %u pointing to %s, %u elements\n",
                    depth, elem->offset,
                    flag_index ? "Index" : "Sereal",
                    size);

            for (j = 0; j < size; ++j) {
                srl_indexed_hash_element_t* elem = (srl_indexed_hash_element_t*) &hash->dataset[j];
                uint32_t ktype = elem->flags & SRL_INDEX_TYPE_MASK;
                uint32_t ksize = elem->flags & SRL_INDEX_SIZE_MASK;
                uint32_t kflag_small = SRL_INDEX_FLAG_GET(ktype, SRL_INDEX_FLAG_KEY_SMALL);
                fprintf(stderr, "=[%d] Member #%d %p\n", depth, j, elem);

                if (kflag_small) {
                    fprintf(stderr, "=[%d] Hash key SMALL: [%*.*s]\n",
                            depth, (int) ksize, (int) ksize, elem->key.str);
                } else {
                    // srl_indexed_element_t* kref = (srl_indexed_element_t*) srl_index_ptr_for_offset(index, elem->key.h.str);
                    fprintf(stderr, "=[%d] Hash key LARGE, %d bytes, hash %u, offset %u\n",
                            depth, ksize, elem->key.h.hash, elem->key.h.str);
                }

                dump_index_data(index, (srl_indexed_element_t*) elem, depth+1);
            }
            break;
        }

        default:
            fprintf(stderr, "=[%d] UNSUPPORTED\n", depth);
            break;
    }
}

static void dump_index(srl_index_t* index)
{
    uint32_t used = 0;

    fprintf(stderr, "START dumping index at %p, mem %d bytes, depth %d, hash %f",
            index,
            index->options.memory_size,
            index->options.index_depth,
            index->options.hash_factor);

    used = SRL_INDEX_USED(index);
    if (used == 0) {
        fprintf(stderr, " EMPTY\n");
    } else {
        fprintf(stderr, ", using %u bytes\n", used);
        dump_index_data(index, (srl_indexed_element_t*) index->beg, 0);
        fprintf(stderr, "DONE dumping index\n");
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
        fprintf(stderr, "%s", "R\n");
    } else if (SvTYPE(sv) == SVt_PVAV) {
        fprintf(stderr, "%s", "A\n");
    } else if (SvTYPE(sv) == SVt_PVHV) {
        fprintf(stderr, "%s", "H\n");
    } else if (SvTYPE(sv) >= SVt_PVAV) {
        fprintf(stderr, "%s", "O\n");
    } else if (SvIOK(sv)) {
        IV v = SvIV(sv);
        fprintf(stderr, "I[%ld]\n", (long) v);
    } else if (SvNOK(sv)) {
        double v = SvNV(sv);
        fprintf(stderr, "R[%lf]\n", v);
    } else if (SvPOK(sv)) {
        STRLEN l;
        char* v = SvPV(sv, l);
        fprintf(stderr, "S[%*.*s]\n", (int) l, (int) l, v);
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
static srl_indexed_element_t* walk_iterator_hash(pTHX_
                                                 srl_index_t* iter,
                                                 int depth,
                                                 UV offset,
                                                 UV length);

srl_index_t* srl_create_index(pTHX_ srl_iterator_t* iter,
                              srl_index_options_t* options)
{
    srl_index_t* index = srl_index_build(aTHX_ iter, options);
    if (!index) {
        return 0;
    }

    fprintf(stderr, "====================\n");
    walk_iterator(aTHX_ index, 0);
    dump_index(index);
    return index;
}

#if 1

typedef struct StackInfo {
    UV ptyp;
    UV ppos;
    UV plen;
    srl_indexed_element_t* pdat;
    UV hsiz;
    UV hpos;
} StackInfo;

typedef struct Stack {
    struct StackInfo info[256]; // TODO
    int pos;
} Stack;

static void stack_init(Stack* stack)
{
    stack->pos = 0;
    memset(&stack->info[stack->pos], 0, sizeof(StackInfo));
}

static void stack_push(Stack* stack, UV plen, UV ptyp, srl_indexed_element_t* pdat, UV hsiz)
{
    ++stack->pos;
    stack->info[stack->pos].ppos = 0;
    stack->info[stack->pos].plen = plen;
    stack->info[stack->pos].ptyp = ptyp;
    stack->info[stack->pos].pdat = pdat;
    stack->info[stack->pos].hsiz = hsiz;
    stack->info[stack->pos].hpos = 0;
}

static void stack_pop(Stack* stack)
{
    --stack->pos;
    ++stack->info[stack->pos].ppos;
}

static uint32_t hash_string(const char* str)
{
    // I've had nice results with djb2 by Dan Bernstein.

    uint32_t h = 5381;
    int c;

    while ((c = *str++))
      h = ((h << 5) + h) + c; /* h * 33 + c */

    return h;
}

static void save_hash_key(const char* kstr,
                          UV klen,
                          UV offset,
                          Stack* stack,
                          srl_indexed_hash_element_t* data)
{
    // Storing key
    uint32_t h = hash_string(kstr);
    uint32_t r = stack->info[stack->pos].hpos = h % stack->info[stack->pos].hsiz;
    fprintf(stderr, "+[%d] GONZO: processing KEY [%lu:%*.*s], hash %u\n",
            stack->pos, klen, (int) klen, (int) klen, kstr, h);
    while (1) {
        uint32_t ktyp = data[stack->info[stack->pos].hpos].flags & SRL_INDEX_TYPE_MASK;
        if (ktyp != SRL_INDEX_TYPE_EMPTY) {
            fprintf(stderr, "+[%d] GONZO: current slot %lu taken, trying next slot\n",
                    stack->pos, stack->info[stack->pos].hpos);
            stack->info[stack->pos].hpos = (stack->info[stack->pos].hpos + 1) % stack->info[stack->pos].hsiz;
            if (stack->info[stack->pos].hpos == r) {
                fprintf(stderr, "+[%d] GONZO: found no available slot, WTF!\n", stack->pos);
                return;
            }
            continue;
        }

        if (klen <= SRL_INDEX_HASH_KEY_SMALL_LENGTH) {
            SRL_INDEX_FLAG_SET(ktyp, SRL_INDEX_FLAG_KEY_SMALL);
            fprintf(stderr, "+[%d] GONZO: using slot %lu, small key of length %lu\n",
                    stack->pos, stack->info[stack->pos].hpos, klen);
            memcpy(data[stack->info[stack->pos].hpos].key.str, kstr, klen);
        } else {
            SRL_INDEX_FLAG_CLR(ktyp, SRL_INDEX_FLAG_KEY_SMALL);
            fprintf(stderr, "+[%d] GONZO: using slot %lu, large key of length %lu\n",
                    stack->pos, stack->info[stack->pos].hpos, klen);
            data[stack->info[stack->pos].hpos].key.h.hash = h;
            data[stack->info[stack->pos].hpos].key.h.str = offset;
        }
        data[stack->info[stack->pos].hpos].flags = ktyp | klen;
        break;
    }
}

static void save_data_in_array(pTHX_
                               Stack* stack,
                               UV type,
                               UV offset)
{
    srl_indexed_array_element_t* data = (srl_indexed_array_element_t*) stack->info[stack->pos].pdat;
    fprintf(stderr, "+[%d] GONZO: indexing data in array => %p\n", stack->pos, data);
    if (!data) {
        fprintf(stderr, "+[%d] GONZO: ERROR null parent array data\n", stack->pos);
        return;
    }

    fprintf(stderr, "+[%d] GONZO: storing in parent array slot %zu\n", stack->pos, stack->info[stack->pos].ppos);
    data[stack->info[stack->pos].ppos].offset = offset;
    data[stack->info[stack->pos].ppos].flags = type;
}

static void save_data_in_hash(pTHX_
                              srl_iterator_t* iter,
                              Stack* stack,
                              UV type,
                              UV offset)
{
    srl_indexed_hash_element_t* data = (srl_indexed_hash_element_t*) stack->info[stack->pos].pdat;
    int hkey = (stack->info[stack->pos].ppos % 2) == 0;
    fprintf(stderr, "+[%d] GONZO: indexing %s in hash => %p\n", stack->pos, hkey ? "KEY" : "VAL", data);
    if (!data) {
        fprintf(stderr, "+[%d] GONZO: ERROR null parent hash data\n", stack->pos);
        return;
    }

    if (hkey) {
        // Storing key
        const char* kstr = 0;
        STRLEN klen = 0;
        kstr = srl_iterator_hash_key(aTHX_ iter, &klen);
        save_hash_key(kstr, klen, offset, stack, data);
    } else {
        // Storing value
        uint32_t ktyp = data[stack->info[stack->pos].hpos].flags & SRL_INDEX_TYPE_MASK;
        uint32_t klen = data[stack->info[stack->pos].hpos].flags & SRL_INDEX_SIZE_MASK;

        fprintf(stderr, "+[%d] GONZO: storing VAL in parent hash slot %zu\n", stack->pos, stack->info[stack->pos].hpos);
        ktyp |= type;
        data[stack->info[stack->pos].hpos].offset = offset;
        data[stack->info[stack->pos].hpos].flags = ktyp | klen;
    }
}

static void process_scalar(pTHX_
                           srl_index_t* index,
                           srl_iterator_t* iter,
                           UV length,
                           UV offset,
                           Stack* stack)
{
    UV type = SRL_INDEX_TYPE_SCALAR;
    switch (stack->info[stack->pos].ptyp) {
        case 0:
            // Scalar is at the top level
            fprintf(stderr, "+[%d] GONZO: indexing top-level scalar\n", stack->pos);
            srl_allocate_element(aTHX_ index, type, offset);
            break;

        case SRL_ITERATOR_OBJ_IS_ARRAY: {
            // Scalar's parent is an array, store it directly there
            save_data_in_array(aTHX_ stack, type, offset);
            break;
        }

        case SRL_ITERATOR_OBJ_IS_HASH: {
            // Scalar's parent is a hash, store it directly there (key or value)
            save_data_in_hash(aTHX_ iter, stack, type, offset);
            break;
        }
    }

    srl_iterator_next(aTHX_ iter, 1);
    ++stack->info[stack->pos].ppos;
}

static void process_array(pTHX_
                          srl_index_t* index,
                          srl_iterator_t* iter,
                          UV length,
                          UV offset,
                          Stack* stack)
{
    UV type = SRL_INDEX_TYPE_ARRAY;
    srl_indexed_array_t* array = 0;

    if (index->options.index_depth != 0 &&
        index->options.index_depth <= stack->pos) {
        // TODO
        return;
    }

    SRL_INDEX_FLAG_SET(type, SRL_INDEX_FLAG_POINTER_INDEX);
    array = srl_allocate_array(aTHX_
                               index,
                               length,
                               type,
                               offset);

    type = SRL_INDEX_TYPE_SCALAR;
    SRL_INDEX_FLAG_SET(type, SRL_INDEX_FLAG_POINTER_INDEX);
    offset = srl_index_offset_for_ptr(index, array);
    switch (stack->info[stack->pos].ptyp) {
        case 0:
            // Array is at the top level
            fprintf(stderr, "+[%d] GONZO: indexing top-level array\n", stack->pos);
            break;

        case SRL_ITERATOR_OBJ_IS_ARRAY: {
            // Array's parent is an array, store it directly there
            save_data_in_array(aTHX_ stack, type, offset);
            break;
        }

        case SRL_ITERATOR_OBJ_IS_HASH: {
            // Array's parent is a hash, store it directly there
            save_data_in_hash(aTHX_ iter, stack, type, offset);
            break;
        }
    }

    fprintf(stderr, "+[%d] GONZO: STEP IN => %p\n", stack->pos, array->dataset);

    stack_push(stack, length, SRL_ITERATOR_OBJ_IS_ARRAY, (srl_indexed_element_t*) array->dataset, 0);
    srl_iterator_step_in(aTHX_ iter, 1);
}

static void process_hash(pTHX_
                         srl_index_t* index,
                         srl_iterator_t* iter,
                         UV length,
                         UV offset,
                         Stack* stack)
{
    UV type = SRL_INDEX_TYPE_HASH;
    srl_indexed_hash_t* hash = 0;

    if (index->options.index_depth != 0 &&
        index->options.index_depth <= stack->pos) {
        // TODO
    }

    SRL_INDEX_FLAG_SET(type, SRL_INDEX_FLAG_POINTER_INDEX);
    hash = srl_allocate_hash(aTHX_
                             index,
                             length,
                             type,
                             offset);

    type = SRL_INDEX_TYPE_SCALAR;
    SRL_INDEX_FLAG_SET(type, SRL_INDEX_FLAG_POINTER_INDEX);
    offset = srl_index_offset_for_ptr(index, hash);
    switch (stack->info[stack->pos].ptyp) {
        case 0:
            // Hash is at the top level
            fprintf(stderr, "+[%d] GONZO: indexing top-level hash\n", stack->pos);
            break;

        case SRL_ITERATOR_OBJ_IS_ARRAY: {
            // Hash's parent is an array, store it directly there
            save_data_in_array(aTHX_ stack, type, offset);
            break;
        }

        case SRL_ITERATOR_OBJ_IS_HASH: {
            // Hash's parent is a hash, store it directly there
            save_data_in_hash(aTHX_ iter, stack, type, offset);
            break;
        }
    }

    // 2*length because we must iterate over keys and values
    stack_push(stack, 2*length, SRL_ITERATOR_OBJ_IS_HASH, (srl_indexed_element_t*) hash->dataset, length);
    srl_iterator_step_in(aTHX_ iter, 1);

    fprintf(stderr, "+[%d] GONZO: STEP IN => %p\n", stack->pos, hash->dataset);
}

static /* UNUSED */ srl_indexed_element_t*
walk_iterator(pTHX_
              srl_index_t* index,
              int /* UNUSED */ depth)
{
    Stack stack;
    srl_iterator_t* iter = index->iter;

    stack_init(&stack);
    while (iter) {
        UV type   = 0;
        UV length = 0;
        UV offset = 0;

        if (stack.info[stack.pos].plen > 0 && stack.info[stack.pos].ppos >= stack.info[stack.pos].plen) {
            fprintf(stderr, "+[%d] GONZO: STEP OUT\n", stack.pos);
            if (stack.pos <= 0) {
                break;
            }
            stack_pop(&stack);
            srl_iterator_step_out(aTHX_ iter, 1);
            continue;
        }

        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }

        type   = srl_iterator_object_info(aTHX_ iter, &length);
        offset = srl_iterator_offset(aTHX_ iter);
        fprintf(stderr, "+[%d] GONZO: pos %3zu, type %6s, len %4zu, off %4zu",
                stack.pos, stack.info[stack.pos].ppos, GetSVType(type), length, offset);
        switch (type) {
            SV* val = 0;

            case SRL_ITERATOR_OBJ_IS_SCALAR:
                fprintf(stderr, " (in %6s) => ", GetSVType(stack.info[stack.pos].ptyp));
                val = srl_iterator_decode(aTHX_ iter);
                dump_sv(aTHX_ val);

                process_scalar(aTHX_ index, iter, length, offset, &stack);
                break;

            case SRL_ITERATOR_OBJ_IS_ARRAY:
                fprintf(stderr, "\n");

                process_array(aTHX_ index, iter, length, offset, &stack);
                break;

            case SRL_ITERATOR_OBJ_IS_HASH:
                fprintf(stderr, "\n");

                process_hash(aTHX_ index, iter, length, offset, &stack);
                break;

            case SRL_ITERATOR_OBJ_IS_ROOT:
            default:
                fprintf(stderr, "\n");

                fprintf(stderr, "+[%d] GONZO: can't handle sereal type\n", stack.pos);
                break;
        }

    }
    return 0;
}

#else

static srl_indexed_element_t* walk_iterator(pTHX_
                                            srl_index_t* index,
                                            int depth)
{
    UV type   = 0;
    UV offset = 0;
    UV length = 0;
    srl_indexed_element_t* elem = 0;

    srl_iterator_t* iter = index->iter;
    if (srl_iterator_eof(aTHX_ iter)) {
        return 0;
    }

    type = srl_iterator_object_info(aTHX_ iter, &length);
    offset = srl_iterator_offset(aTHX_ iter);
    fprintf(stderr, "[%d] GONZO: we got a %s, length %zu, offset %zu\n",
            depth, GetSVType(type), length, offset);

    switch (type) {
    case SRL_ITERATOR_OBJ_IS_SCALAR:
        elem = walk_iterator_scalar(aTHX_ index, depth+1, offset, length);
        break;

    case SRL_ITERATOR_OBJ_IS_ARRAY:
        elem = walk_iterator_array(aTHX_ index, depth+1, offset, length);
        break;

    case SRL_ITERATOR_OBJ_IS_HASH:
        elem = walk_iterator_hash(aTHX_ index, depth+1, offset, length);
        break;

    case SRL_ITERATOR_OBJ_IS_ROOT:
    default:
        fprintf(stderr, "[%d] GONZO: can't handle sereal type\n", depth);
        break;
    }

    fprintf(stderr, "[%d] GONZO: walked and got %s pointer\n",
            depth, elem ? "INDIRECT" : "DIRECT");
    // srl_iterator_next(aTHX_ iter, 1);
    return elem;
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

    fprintf(stderr, "[%d] GONZO: pointer to scalar in SRL, offset %zu\n", depth, offset);
    val = srl_iterator_decode(aTHX_ iter);
    dump_sv(aTHX_ val);

    return srl_allocate_element(aTHX_ index, SRL_INDEX_TYPE_OFFSET_SRL, offset);
}

static void show_iterator(pTHX_ const char* msg, srl_iterator_t* iter)
{
    IV sd = srl_iterator_stack_depth(aTHX_ iter);
    UV si = srl_iterator_stack_index(aTHX_ iter);
    fprintf(stderr, "GONZO: %s => stack is depth %ld / index %lu\n", msg, sd, si);
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

    if (index->options.index_depth > 0 && depth >= index->options.index_depth) {
        fprintf(stderr, "[%d] GONZO: pointer to array in SRL, offset %zu\n", depth, offset);
        return srl_allocate_element(aTHX_ index, SRL_INDEX_TYPE_OFFSET_SRL, offset);
    }

    fprintf(stderr, "[%d] GONZO: walking array, length %zu, offset %zu\n", depth, length, offset);
    show_iterator(aTHX_ "entering walk_array", iter);
    array = srl_allocate_array(aTHX_
                               index,
                               length,
                               SRL_INDEX_TYPE_ARRAY_IDX,
                               offset);
    fprintf(stderr, "[%d] GONZO: allocated array %p\n", depth, array);
    srl_iterator_step_in(aTHX_ iter, 1);
    fprintf(stderr, "[%d] GONZO: vvv IN\n", depth);
    show_iterator(aTHX_ "after step_in", iter);
    while (1) {
        srl_indexed_array_element_t* cur = 0;
        srl_indexed_element_t* elem = 0;
        if (pos >= length) {
            break;
        }
        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }

        fprintf(stderr, "[%d] GONZO: processing element %d\n", depth, pos);
        cur = &array->dataset[pos];
        elem = walk_iterator(aTHX_ index, depth);
        if (!elem) {
            cur->offset = offset;
            cur->flags = SRL_INDEX_TYPE_OFFSET_SRL;
        } else {
            cur->offset = srl_index_offset_for_ptr(index, elem);
            cur->flags = SRL_INDEX_TYPE_AELEM_IDX;
        }

        ++pos;
        srl_iterator_next(aTHX_ iter, 1);
        fprintf(stderr, "[%d] GONZO: >>> NEXT (***%d***)\n",
                depth, srl_iterator_eof(aTHX_ iter) ? 1 : 0);
        show_iterator(aTHX_ "after next", iter);
    }
    srl_iterator_step_out(aTHX_ iter, 1);
    fprintf(stderr, "[%d] GONZO: ^^^ OUT\n", depth);
    show_iterator(aTHX_ "after step_out", iter);
    fprintf(stderr, "[%d] GONZO: finished walking array\n", depth);

    return (srl_indexed_element_t*) array;
}

static srl_indexed_element_t* walk_iterator_hash(pTHX_
                                                 srl_index_t* index,
                                                 int depth,
                                                 UV offset,
                                                 UV length)
{
    int pos = 0;
    srl_indexed_hash_t* hash = 0;
    UV hsize = length;
    int collisions = 0;

    srl_iterator_t* iter = index->iter;
    if (srl_iterator_eof(aTHX_ iter)) {
        return 0;
    }

    if (index->options.index_depth > 0 && depth >= index->options.index_depth) {
        fprintf(stderr, "[%d] GONZO: pointer to hash in SRL, offset %zu\n", depth, offset);
        return srl_allocate_element(aTHX_ index, SRL_INDEX_TYPE_OFFSET_SRL, offset);
    }

    if (index->options.hash_factor > 1.0) {
        hsize *= index->options.hash_factor;
        fprintf(stderr, "[%d] GONZO: index hash will use %zu slots to store %zu elements\n",
                depth, hsize, length);
    }
    fprintf(stderr, "[%d] GONZO: walking hash, length %zu, offset %zu\n", depth, length, offset);
    hash = srl_allocate_hash(aTHX_
                             index,
                             hsize,
                             SRL_INDEX_TYPE_HASH_IDX,
                             offset);
    fprintf(stderr, "[%d] GONZO: allocated hash %p\n", depth, hash);
    srl_iterator_step_in(aTHX_ iter, 1);
    fprintf(stderr, "[%d] GONZO: vvv IN\n", depth);
    while (1) {
        const char* key = 0;
        STRLEN len = 0;
        srl_indexed_element_t* elem = 0;
        uint32_t h = 0;
        uint32_t j = 0;
        uint32_t r = 0;

        if (pos >= length) {
            break;
        }
        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }

        key = srl_iterator_hash_key(aTHX_ iter, &len);
        offset = srl_iterator_offset(aTHX_ iter);
        fprintf(stderr, "[%d] GONZO: processing key %d => [%lu:%*.*s]\n",
                depth, pos, len, (int) len, (int) len, key);
        // dump_sv(aTHX_ key);

        srl_iterator_next(aTHX_ iter, 1);
        fprintf(stderr, "[%d] GONZO: >>> NEXT\n", depth);
        if (srl_iterator_eof(aTHX_ iter)) {
            break;
        }
        elem = walk_iterator(aTHX_ index, depth);
        h = hash_string(key);
        r = j = h % hsize;
        fprintf(stderr, "[%d] GONZO: hash %u, slot %d\n", depth, h, j);
        while (1) {
            srl_indexed_hash_element_t* cur = &hash->dataset[j];
            uint32_t ktype = cur->flags & SRL_INDEX_TYPE_MASK;
            fprintf(stderr, "[%d] GONZO: current slot %d - %p is type 0x%02X - %s\n",
                    depth, j, cur, ktype >> 24, GetObjType(ktype));

            if (ktype == SRL_INDEX_TYPE_EMPTY) {
                if (len <= SRL_INDEX_HASH_KEY_SMALL_LENGTH) {
                    fprintf(stderr, "[%d] GONZO: found slot, small key of length %lu\n", depth, len);
                    memcpy(cur->key.str, key, len);
                    ktype = elem ? SRL_INDEX_TYPE_HELEM_KS_IDX : SRL_INDEX_TYPE_HELEM_KS_SRL;
                } else {
                    fprintf(stderr, "[%d] GONZO: found slot, large key of length %lu\n", depth, len);
                    cur->key.h.hash = h;
                    cur->key.h.str = offset;
                    ktype = elem ? SRL_INDEX_TYPE_HELEM_KL_IDX : SRL_INDEX_TYPE_HELEM_KL_SRL;
                }
                cur->flags = ktype | len;
                if (!elem) {
                    cur->offset = offset;
                } else {
                    cur->offset = srl_index_offset_for_ptr(index, elem);
                }
                fprintf(stderr, "[%d] GONZO: slot %d - %p ended up as type 0x%02X - %s\n",
                        depth, j, cur, ktype >> 24, GetObjType(ktype));
                break;
            }

            ++collisions;
            j = (j + 1) % hsize;
            fprintf(stderr, "[%d] GONZO: trying next slot %d\n", depth, j);
            if (j == r) {
                // No free slots, impossible!!!
                abort();
            }
        }

        ++pos;
        srl_iterator_next(aTHX_ iter, 1);
        fprintf(stderr, "[%d] GONZO: >>> NEXT\n", depth);
    }
    srl_iterator_step_out(aTHX_ iter, 1);
    fprintf(stderr, "[%d] GONZO: ^^^ OUT\n", depth);
    fprintf(stderr, "[%d] GONZO: finished walking hash\n", depth);
    fprintf(stderr, "[%d] GONZO: there were %d collisions\n", depth, collisions);

    return (srl_indexed_element_t*) hash;
}

#endif

#endif
