#ifndef SRL_ERROR_H_
#define SRL_ERROR_H_
#include "srl_taginfo.h"

#define SRL_BASE_ERROR_FORMAT "Sereal: Error in %s:%u and char %i of input: "
#define SRL_BASE_ERROR_ARGS __FILE__, __LINE__, (int) (1 + dec->pos - dec->buf_start)

#define SRL_ERROR(msg)                          croak(SRL_BASE_ERROR_FORMAT "%s", SRL_BASE_ERROR_ARGS, (msg))
#define SRL_ERRORf1(fmt,var)                    croak(SRL_BASE_ERROR_FORMAT fmt,  SRL_BASE_ERROR_ARGS, (var))
#define SRL_ERRORf2(fmt,var1,var2)              croak(SRL_BASE_ERROR_FORMAT fmt,  SRL_BASE_ERROR_ARGS, (var1), (var2))
#define SRL_ERRORf3(fmt,var1,var2,var3)         croak(SRL_BASE_ERROR_FORMAT fmt,  SRL_BASE_ERROR_ARGS, (var1), (var2), (var3))
#define SRL_ERRORf4(fmt,var1,var2,var3,var4)    croak(SRL_BASE_ERROR_FORMAT fmt,  SRL_BASE_ERROR_ARGS, (var1), (var2), (var3), (var4))

#define SRL_ERROR_UNIMPLEMENTED(dec,tag,str) \
    SRL_ERRORf3("Tag %u %s is unimplemented at ofs: %lu", (tag), (str), (unsigned long) BUF_POS_OFS(dec))

#define SRL_ERROR_UNTERMINATED(dec,tag,str)                                                                 \
    SRL_ERRORf4("Tag SRL_HDR_%s %s was not terminated properly at ofs %lu with %lu to go",                  \
                tag_name[(tag) & 127], (str), (dec)->pos - (dec)->buf_start, (dec)->buf_end - (dec)->pos)

#define SRL_ERROR_BAD_COPY(dec, tag) \
    SRL_ERRORf1("While processing tag SRL_HDR_%s encountered a bad COPY tag", tag_name[(tag) & 127])

#define SRL_ERROR_UNEXPECTED(dec, tag, msg) SRL_ERRORf2("Unexpected tag %s while expecting %s", tag_name[(tag) & 127], msg)
#define SRL_ERROR_REFUSE_OBJECT()           SRL_ERROR("Encountered object in input, but the 'refuse_objects' option is in effect");
#define SRL_ERROR_PANIC(dec, msg)           SRL_ERRORf1("Panic: %s", msg);

#endif
