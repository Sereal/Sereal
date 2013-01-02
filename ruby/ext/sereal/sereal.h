#ifndef _MAIN_H
#define _MAIN_H
#include <ruby.h>
#include <ruby/encoding.h>
#include <string.h>     /* memcpy */
#include "proto.h"

typedef unsigned long long      u64;
typedef unsigned int            u32;
typedef unsigned short          u16;
typedef unsigned char           u8;
typedef struct _sereal          sereal_t;
typedef struct _track_entry     track_t;
#define TRUE 1
#define FALSE 0
#define MAX_RECURSION_DEPTH 100
#define COPY(src,dst,len) memcpy((dst),(src),len)
#define is_ascii_string(str) (rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT)
#define THRESH(x,min,max) ((x) >= (min) && (x) <= (max))

#define IS_SHORT_BINARY(t) THRESH(t,SRL_HDR_SHORT_BINARY_LOW,SRL_HDR_SHORT_BINARY_HIGH)
#define IS_ARRAYREF(t) THRESH(t,SRL_HDR_ARRAYREF_LOW,SRL_HDR_ARRAYREF_HIGH)
#define IS_HASHREF(t) THRESH(t,SRL_HDR_HASHREF_LOW,SRL_HDR_HASHREF_HIGH)
#define IS_STRING(t) ((t) == SRL_HDR_STR_UTF8 || (t) == SRL_HDR_BINARY || IS_SHORT_BINARY((t)))

#define SRL_HDR_SYM SRL_HDR_RESERVED_LOW
#define SRL_HDR_RB_OBJ (SRL_HDR_RESERVED_LOW+1)
#define _D(fmt,arg...) fprintf(stderr,"%s(): " fmt "\n",__func__,##arg)

#ifdef ONIGURUMA_H
#define IGNORECASE ONIG_OPTION_IGNORECASE
#define MULTILINE ONIG_OPTION_MULTILINE
#define EXTENDED ONIG_OPTION_EXTEND
#else
#define IGNORECASE RE_OPTION_IGNORECASE
#define MULTILINE RE_OPTION_MULTILINE
#define EXTENDED RE_OPTION_EXTENDED
#endif
#define S_READERS_COUNT 255
struct _sereal {
        u8 *data;
        u32 size;
        u32 pos;
        u32 rsize;
        u32 level;
        VALUE (*reader[S_READERS_COUNT])(sereal_t *, u8);
};

VALUE method_sereal_encode(VALUE self, VALUE args);
VALUE method_sereal_decode(VALUE self, VALUE payload);

void *alloc_or_raise(u32 s);
sereal_t * s_create(void);
void s_register(sereal_t *s, u8 pos, VALUE (*c)(sereal_t *,u8));
void s_destroy(sereal_t *s);
void s_alloc(sereal_t *s, u32 len);
void s_append(sereal_t *s, void *suffix, u32 s_len);
void s_append_u8(sereal_t *s,u8 b);
void s_append_u32(sereal_t *s,u32 b);

void *s_get_p_at_pos(sereal_t *s, u32 pos, u32 req);
void *s_get_p(sereal_t *s);
u8 s_get_u8(sereal_t *s);
u8 s_get_u8_bang(sereal_t *s);

void s_dump(sereal_t *s);

static VALUE s_default_reader(sereal_t *s, u8 tag) {
        // s_dump(s);
        rb_raise(rb_eTypeError,"unsupported tag %d",tag);
        return Qnil;
}

#define S_RECURSE_INC(s)                                          \
do {                                                              \
        if((s)->level++ > MAX_RECURSION_DEPTH)                    \
                rb_raise(rb_eArgError,                            \
                         "max recursion depth reached: %d",       \
                         MAX_RECURSION_DEPTH);                    \
} while(0);

#define S_RECURSE_DEC(s) ((s)->level--)

#endif
