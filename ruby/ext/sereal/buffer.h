#include "sereal.h"
static inline void *realloc_or_raise(void *p, u32 n) {
        u8 *buf = realloc(p,n);
        if (!buf)
                rb_raise(rb_eNoMemError,"memory allocation failure for %d bytes",n);
        return buf;
}
static inline void *alloc_or_raise(u32 s) {
        return realloc_or_raise(NULL,s);
}

static inline sereal_t * s_create(void) {
        sereal_t *s = alloc_or_raise(sizeof(*s));
	ZERO(s,sizeof(*s));
        return s;
}

static inline void s_destroy(sereal_t *s) {
        if (s->tracked != Qnil)
                rb_gc_unregister_address(&s->tracked);
        if (s->data && (s->flags & FLAG_NOT_MINE) == 0)
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
        if (pos + req > s->size) 
                rb_raise(rb_eRangeError,"position is out of bounds (%d + %d >  %d)",pos,req,s->size);
        return &s->data[pos];
}

static inline void *s_get_p_at_pos_bang(sereal_t *s, u32 pos,u32 req) {
        void *p = s_get_p_at_pos(s,pos,req);
        s->pos += req;
        return p;
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

static void b_dump(u8 *p, u32 len, u32 pos) {
        int i;

        fprintf(stderr,"\n-----------\n");
        for (i = 0; i < len; i++) {
                if (i == pos) 
                        fprintf(stderr," [%c %d] ",p[i],p[i]);
                else
                        fprintf(stderr," (%c %d) ",p[i],p[i]);
        }
        fprintf(stderr,"\n-----------\n");
}

static void s_dump(sereal_t *s) {
        b_dump(s->data,s->size,s->pos);
}

