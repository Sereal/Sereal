#ifndef SRL_READER_H_
#define SRL_READER_H_

#include <assert.h>

#include "srl_common.h"
#include "srl_reader_types.h"

/* reader buffer operations */
#define SRL_RDR_SIZE(buf)           ((buf)->end -  (buf)->start)
#define SRL_RDR_SPACE_LEFT(buf)     ((buf)->end -  (buf)->pos)
#define SRL_RDR_DONE(buf)           ((buf)->pos >= (buf)->end)
#define SRL_RDR_NOT_DONE(buf)       ((buf)->pos <  (buf)->end)
#define SRL_RDR_POS_OFS(buf)        ((buf)->pos -  (buf)->start)
#define SRL_RDR_BODY_POS_OFS(buf)   ((buf)->pos -  (buf)->body_pos)

#define SRL_RDR_CLEAR(buf) STMT_START {                                        \
    (buf)->start = NULL;                                                       \
    (buf)->end = NULL;                                                         \
    (buf)->pos = NULL;                                                         \
    (buf)->body_pos = NULL;                                                    \
} STMT_END

/* Sereal v1 and newer version use start body offset from different positions */
#define SRL_RDR_SET_BODY_POS(buf, pos) ((buf)->body_pos = (pos))
#define SRL_RDR_UPDATE_BODY_POS(buf, protocol_version) STMT_START {            \
    if (expect_false((protocol_version) == 1)) {                               \
        SRL_RDR_SET_BODY_POS((buf), (buf)->start);                             \
    } else {                                                                   \
        SRL_RDR_SET_BODY_POS((buf), (buf)->pos - 1);                           \
    }                                                                          \
} STMT_END

#define SRL_RDR_ASSERT_SPACE(buf, len, msg) STMT_START {                       \
    if (expect_false((UV)SRL_RDR_SPACE_LEFT((buf)) < (UV)(len))) {             \
        SRL_RDR_ERRORf3((buf), "Unexpected termination of packet%s, "          \
                        "want %"UVuf" bytes, only have %"UVuf" available",     \
                        (msg), (UV)(len), (UV)SRL_RDR_SPACE_LEFT((buf)));      \
    }                                                                          \
} STMT_END

#ifndef NDEBUG
#   define DEBUG_ASSERT_RDR_SANE(buf) STMT_START {                             \
    if (expect_false(                                                          \
           (buf)->pos < (buf)->start                                           \
        || (buf)->pos > (buf)->end                                             \
    )) {                                                                       \
        warn("failed sanity assertion check - pos: %"UVuf" [%p %p %p] %"UVuf,  \
             (UV)SRL_RDR_POS_OFS(buf), (buf)->start, (buf)->pos,               \
             (buf)->end, (UV)SRL_RDR_SPACE_LEFT(buf));                         \
    }                                                                          \
    assert(((buf)->start <= (buf)->pos) && ((buf)->pos <= (buf)->end));        \
} STMT_END 
#else
#   define DEBUG_ASSERT_RDR_SANE(buf)
#endif

#endif
