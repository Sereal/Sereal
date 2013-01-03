#include "sereal.h"
static inline void *realloc_or_raise(void *p, u32 s) {
        u8 *buf = realloc(p,s);
        if (!buf)
                rb_raise(rb_eNoMemError,"memory allocation failure for %d bytes",s);
        return buf;
}
static inline void *alloc_or_raise(u32 s) {
        return realloc_or_raise(NULL,s);
}

static inline sereal_t * s_create(void) {
        sereal_t *s = alloc_or_raise(sizeof(*s));
        s->data = NULL;
        s->size = 0;
        s->rsize = 0;
        s->level = 0;
        s->pos = 0;
        return s;
}

static inline void s_destroy(sereal_t *s) {
        if (s->data)
                free(s->data);
        free(s);
}

static inline void s_alloc(sereal_t *s, u32 len) {
        if (s->rsize > s->size + len)
                return;

        u32 size = s->size + len + 512; // every time allocate 512b more, so we wont alloc for a while
        u8 *buf = realloc_or_raise(s->data,size); 
        s->data = buf;
        s->rsize = size;
}

static inline void s_append(sereal_t *s, void *suffix, u32 s_len) {
        s_alloc(s,s_len);
        COPY(suffix,(s->data + s->size), s_len);
        s->size += s_len;
        s->pos += s_len;
}

static inline void s_append_u8(sereal_t *s,u8 b) {
        s_append(s,&b,sizeof(b));
}

static inline void s_append_u32(sereal_t *s,u32 b) {
        s_append(s,&b,sizeof(b));
}

static inline void *s_get_p_at_pos(sereal_t *s, u32 pos,u32 req) {
        if (pos + req > s->size) {
                rb_raise(rb_eRangeError,"position is out of bounds (%d + %d >  %d)",pos,req,s->size);
                return NULL;
        }
        return &s->data[pos];
}

static inline void *s_get_p(sereal_t *s) {
        return s_get_p_at_pos(s,s->pos,0);
}
static inline u8 s_get_u8(sereal_t *s) {
        return *((u8 *) s_get_p(s));
}

static inline u8 s_get_u8_bang(sereal_t *s) {
        u8 r = s_get_u8(s);
        s->pos++;
        return r;
}

static inline u32 s_get_u32_bang(sereal_t *s) {
        u32 *r = (u32 *) s_get_p_at_pos(s,s->pos,sizeof(u32) - 1); /* current position + 3 bytes */
        s->pos += sizeof(u32);
        return *r;
}

static inline u32 s_shift_position_bang(sereal_t *s, u32 len) {
        s->pos += len;
        return len;
}
static inline void s_dump(sereal_t *s) {
        int i;
        for (i = 0; i < s->size; i++) {
                if (i == s->pos) 
                        fprintf(stderr," [%c %d] ",s->data[i],s->data[i]);
                else
                        fprintf(stderr," (%c %d) ",s->data[i],s->data[i]);
        }
        fprintf(stderr,"\n");
}

