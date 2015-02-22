#ifndef SRL_RDRER_ERROR_H_
#define SRL_RDRER_ERROR_H_

#include "srl_taginfo.h"

#define SRL_RDR_BASE_ERROR_FORMAT  "Sereal: Error in %s line %u and char %"UVuf" of input: "
#define SRL_RDR_BASE_ERROR_ARGS(r) __FILE__, __LINE__, (UV) (1 + (r)->rb_pos - (r)->rb_start)

#define SRL_RDR_ERROR(r, msg)                              croak(SRL_RDR_BASE_ERROR_FORMAT "%s", SRL_RDR_BASE_ERROR_ARGS((r)), (msg))
#define SRL_RDR_ERRORf1(r, fmt, var)                       croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var))
#define SRL_RDR_ERRORf2(r, fmt, var1, var2)                croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var1), (var2))
#define SRL_RDR_ERRORf3(r, fmt, var1, var2, var3)          croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var1), (var2), (var3))
#define SRL_RDR_ERRORf4(r, fmt, var1, var2, var3, var4)    croak(SRL_RDR_BASE_ERROR_FORMAT fmt,  SRL_RDR_BASE_ERROR_ARGS((r)), (var1), (var2), (var3), (var4))

#define SRL_RDR_ERROR_UNIMPLEMENTED(r, tag, str)           SRL_RDR_ERRORf2((r), "Tag %u SRL_HDR_%s is unimplemented", (tag), (str))
#define SRL_RDR_ERROR_UNEXPECTED(r, tag, msg)              SRL_RDR_ERRORf2((r), "Unexpected tag SRL_HDR_%s while expecting %s", SRL_TAG_NAME((tag)), (msg))
#define SRL_RDR_ERROR_BAD_COPY(r, tag)                     SRL_RDR_ERRORf1((r), "While processing tag SRL_HDR_%s encountered a bad COPY tag", SRL_TAG_NAME((tag)))

#endif
