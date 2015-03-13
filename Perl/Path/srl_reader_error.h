#ifndef SRL_RDRER_ERROR_H_
#define SRL_RDRER_ERROR_H_

#include "srl_taginfo.h"

#define SRL_RDR_BASE_ERROR_FORMAT  "Sereal: Error in %s:%u and char %"UVuf" of input: "
#define SRL_RDR_BASE_ERROR_ARGS(r) __FILE__, __LINE__, (UV) (1 + (r)->rb_pos - (r)->rb_start)

#define SRL_RDR_ERROR(r, msg)                              croak(SRL_RDR_BASE_ERROR_FORMAT "%s", SRL_RDR_BASE_ERROR_ARGS((r)), (msg))
#define SRL_RDR_ERRORf1(r, fmt, var)                       croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var))
#define SRL_RDR_ERRORf2(r, fmt, var1, var2)                croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var1), (var2))
#define SRL_RDR_ERRORf3(r, fmt, var1, var2, var3)          croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var1), (var2), (var3))
#define SRL_RDR_ERRORf4(r, fmt, var1, var2, var3, var4)    croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var1), (var2), (var3), (var4))

#define SRL_RDR_ERROR_UNIMPLEMENTED(r, tag, str)           SRL_RDR_ERRORf3((r), "Tag %u (0x%x) '%s' is unimplemented", (tag), (tag), (str))
#define SRL_RDR_ERROR_UNEXPECTED(r, tag, msg)              SRL_RDR_ERRORf2((r), "Unexpected tag SRL_HDR_%s while expecting %s", SRL_TAG_NAME((tag)), (msg))
#define SRL_RDR_ERROR_BAD_COPY(r, tag)                     SRL_RDR_ERRORf1((r), "While processing tag SRL_HDR_%s encountered a bad COPY tag", SRL_TAG_NAME((tag)))
#define SRL_RDR_ERROR_EOF(r, msg)                          SRL_RDR_ERRORf1((r), "Premature end of document while expecting %s", (msg));

/* trace functions */
#ifdef TRACE_READER
#   define SRL_RB_TRACE(msg, args...) \
        fprintf(stderr, "%s:%d:%s(): "msg"\n", __FILE__, __LINE__, __func__, ## args)
#else
#   define SRL_RB_TRACE(msg, args...)
#endif

#define SRL_RDR_REPORT_TAG(rdr, tag) STMT_START {                                   \
    SRL_RB_TRACE(                                                                   \
        "Reader: tag SRL_HDR_%s (int: %d hex: 0x%x) at ofs %"UVuf" body_ofs %"UVuf, \
        SRL_TAG_NAME((tag)),                                                        \
        (tag), (tag),                                                               \
        (UV) SRL_RB_POS_OFS((rdr)),                                                 \
        (UV) SRL_RB_BODY_POS_OFS((rdr))                                             \
    );                                                                              \
} STMT_END

#endif
