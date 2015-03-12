#ifndef SRL_READER_H_
#define SRL_READER_H_

#ifndef srl_reader_t
#error define srl_reader_t before including srl_reader.h
#endif

#include <assert.h>

#include "srl_common.h"
#include "srl_reader_types.h"

/* reader buffer operations */
#define SRL_RB_SIZE(r)           ((r)->rb_end -  (r)->rb_start)
#define SRL_RB_SPACE_LEFT(r)     ((r)->rb_end -  (r)->rb_pos)
#define SRL_RB_DONE(r)           ((r)->rb_pos >= (r)->rb_end)
#define SRL_RB_NOT_DONE(r)       ((r)->rb_pos <  (r)->rb_end)
#define SRL_RB_POS_OFS(r)        ((r)->rb_pos -  (r)->rb_start)
#define SRL_RB_BODY_POS_OFS(r)   ((r)->rb_pos -  (r)->rb_body_pos)

#define SRL_RB_CLEAR(r) STMT_START {                                            \
    (r)->rb_start = NULL;                                                       \
    (r)->rb_end = NULL;                                                         \
    (r)->rb_pos = NULL;                                                         \
    (r)->rb_body_pos = NULL;                                                    \
    (r)->rb_protocol_version = 0;                                               \
} STMT_END

/* Sereal v1 and newer version use start body offset from different positions */
#define SRL_RB_SET_BODY_POS(r, pos) ((r)->rb_body_pos = pos)
#define SRL_RB_UPDATE_BODY_POS(r) STMT_START {                                  \
    if (expect_false(((r)->rb_protocol_version) == 1)) {                        \
        SRL_RB_SET_BODY_POS((r), (r)->rb_start);                                \
    } else {                                                                    \
        SRL_RB_SET_BODY_POS((r), (r)->rb_pos - 1);                              \
    }                                                                           \
} STMT_END

#define SRL_RB_ASSERT_SPACE(r, len, msg) STMT_START {                           \
    if (expect_false( (UV)SRL_RB_SPACE_LEFT((r)) < (UV)(len) )) {               \
        SRL_RDR_ERRORf3((r), "Unexpected termination of packet%s, "             \
                        "want %"UVuf" bytes, only have %"UVuf" available",      \
                        (msg), (UV)(len), (UV)SRL_RB_SPACE_LEFT((r)));          \
    }                                                                           \
} STMT_END

#ifndef NDEBUG
#   define DEBUG_ASSERT_RB_SANE(rdr) STMT_START {                               \
        if (expect_false(                                                       \
               (rdr)->rb_pos < (rdr)->rb_start                                  \
            || (rdr)->rb_pos > (rdr)->rb_end                                    \
        )) {                                                                    \
            warn("failed sanity assertion check - pos: %"UVuf" [%p %p %p] %"UVuf, \
                 (UV)SRL_RB_POS_OFS(rdr), (rdr)->rb_start, (rdr)->rb_pos,       \
                 (rdr)->rb_end, (UV)SRL_RB_SPACE_LEFT(rdr));                    \
        }                                                                       \
    } STMT_END 
#else
#   define DEBUG_ASSERT_RB_SANE(rdr)
#endif

#endif
