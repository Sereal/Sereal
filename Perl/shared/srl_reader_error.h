#ifndef SRL_READER_ERROR_H_
#define SRL_READER_ERROR_H_

#include "srl_taginfo.h"

#define SRL_RDR_BASE_ERROR_FORMAT_START  "Sereal: Error: "
#define SRL_RDR_BASE_ERROR_FORMAT_END    " at offset %"UVuf" of input at %s line %u"
#define SRL_RDR_BASE_ERROR_FORMAT(whatever) SRL_RDR_BASE_ERROR_FORMAT_START whatever SRL_RDR_BASE_ERROR_FORMAT_END
#define SRL_RDR_BASE_ERROR_ARGS(buf)  (UV)(1 + (buf)->pos - (buf)->start), __FILE__, __LINE__

#define SRL_RDR_ERROR(buf, msg)                              croak(SRL_RDR_BASE_ERROR_FORMAT("%s"),  (msg), SRL_RDR_BASE_ERROR_ARGS((buf)))
#define SRL_RDR_ERRORf1(buf, fmt, var)                       croak(SRL_RDR_BASE_ERROR_FORMAT(fmt),  (var), SRL_RDR_BASE_ERROR_ARGS((buf)))
#define SRL_RDR_ERRORf2(buf, fmt, var1, var2)                croak(SRL_RDR_BASE_ERROR_FORMAT(fmt),  (var1), (var2), SRL_RDR_BASE_ERROR_ARGS((buf)))
#define SRL_RDR_ERRORf3(buf, fmt, var1, var2, var3)          croak(SRL_RDR_BASE_ERROR_FORMAT(fmt),  (var1), (var2), (var3), SRL_RDR_BASE_ERROR_ARGS((buf)))
#define SRL_RDR_ERRORf4(buf, fmt, var1, var2, var3, var4)    croak(SRL_RDR_BASE_ERROR_FORMAT(fmt),  (var1), (var2), (var3), (var4), SRL_RDR_BASE_ERROR_ARGS((buf)))

#define SRL_RDR_ERROR_UNIMPLEMENTED(buf, tag, str)           SRL_RDR_ERRORf3((buf), "Tag %u (0x%x) '%s' is unimplemented", (tag), (tag), (str))
#define SRL_RDR_ERROR_UNEXPECTED(buf, tag, msg)              SRL_RDR_ERRORf2((buf), "Unexpected tag SRL_HDR_%s while expecting %s", SRL_TAG_NAME((tag)), (msg))
#define SRL_RDR_ERROR_BAD_COPY(buf, tag)                     SRL_RDR_ERRORf1((buf), "While processing tag SRL_HDR_%s encountered a bad COPY tag", SRL_TAG_NAME((tag)))
#define SRL_RDR_ERROR_EOF(buf, msg)                          SRL_RDR_ERRORf1((buf), "Premature end of document while expecting %s", (msg));
#define SRL_RDR_ERROR_REFUSE_OBJECT(buf)                     SRL_RDR_ERROR((buf),   "Encountered object in input, but the 'refuse_objects' option is in effect");
#define SRL_RDR_ERROR_PANIC(buf, msg)                        SRL_RDR_ERRORf1((buf), "Panic: %s", msg);

#endif
