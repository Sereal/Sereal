#ifndef SRL_RDRER_ERROR_H_
#define SRL_RDRER_ERROR_H_

#include "srl_taginfo.h"

#define SRL_RDR_BASE_ERROR_FORMAT  "Sereal: Error in %s:%u and char %"UVuf" of input: "
#define SRL_RDR_BASE_ERROR_ARGS(buf) __FILE__, __LINE__, (UV) (1 + (buf)->pos - (buf)->start)

#define SRL_RDR_ERROR(buf, msg)                              croak(SRL_RDR_BASE_ERROR_FORMAT "%s", SRL_RDR_BASE_ERROR_ARGS((buf)), (msg))
#define SRL_RDR_ERRORf1(buf, fmt, var)                       croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((buf)), (var))
#define SRL_RDR_ERRORf2(buf, fmt, var1, var2)                croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((buf)), (var1), (var2))
#define SRL_RDR_ERRORf3(buf, fmt, var1, var2, var3)          croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((buf)), (var1), (var2), (var3))
#define SRL_RDR_ERRORf4(buf, fmt, var1, var2, var3, var4)    croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((buf)), (var1), (var2), (var3), (var4))

#define SRL_RDR_ERROR_UNIMPLEMENTED(buf, tag, str)           SRL_RDR_ERRORf3((buf), "Tag %u (0x%x) '%s' is unimplemented", (tag), (tag), (str))
#define SRL_RDR_ERROR_UNEXPECTED(buf, tag, msg)              SRL_RDR_ERRORf2((buf), "Unexpected tag SRL_HDR_%s while expecting %s", SRL_TAG_NAME((tag)), (msg))
#define SRL_RDR_ERROR_BAD_COPY(buf, tag)                     SRL_RDR_ERRORf1((buf), "While processing tag SRL_HDR_%s encountered a bad COPY tag", SRL_TAG_NAME((tag)))
#define SRL_RDR_ERROR_EOF(buf, msg)                          SRL_RDR_ERRORf1((buf), "Premature end of document while expecting %s", (msg));

/* trace functions */
#ifdef TRACE_READER
#   define SRL_RDR_TRACE(msg, args...) \
        fprintf(stderr, "%s:%d:%s(): "msg"\n", __FILE__, __LINE__, __func__, ## args)
#else
#   define SRL_RDR_TRACE(msg, args...)
#endif

#endif
