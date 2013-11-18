#include "sereal.h"
#include "buffer.h"
#include "decode.h"
#include "snappy/csnappy_decompress.c"

static VALUE s_default_reader(sereal_t *s, u8 tag) {
        // s_dump(s);
        rb_raise(rb_eTypeError,"unsupported tag %d",tag);
        return Qnil;
}

/* VARINT */
static u64 s_get_varint_bang(sereal_t *s) {
        u64 uv = 0;
        unsigned int lshift = 0;
        while (s_get_u8(s) & 0x80) {
                uv |= ((u64)(s_get_u8_bang(s) & 0x7F) << lshift);
                lshift += 7;
                if (lshift > (sizeof(uv) * 8)) 
                        rb_raise(rb_eTypeError, "varint too big");
        }
        uv |= ((u64)(s_get_u8_bang(s)) << lshift);
        return uv;
}

/* ZIGZAG */
static VALUE s_read_zigzag(sereal_t *s, u8 tag) {
        signed long long z = 0;
        u64 v = s_get_varint_bang(s);
        if (v & 1) {
                z = ((long) v >> 1);
        } else {
                z = (long) v >> 1;
        }
        return LL2NUM(z);
}

/* VARINT */
static VALUE s_read_varint(sereal_t *s, u8 tag) {
        return ULL2NUM(s_get_varint_bang(s));
}

/* FLOAT */
static VALUE s_read_float(sereal_t *s, u8 tag) {
        float *f = (float *) s_get_p_at_pos(s,s->pos,sizeof(*f));
        return DBL2NUM((double) *f);
}

/* DOUBLE */
static VALUE s_read_double(sereal_t *s, u8 tag) {
        double *d = (double *) s_get_p_at_pos(s,s->pos,sizeof(*d));
        return DBL2NUM(*d);
}

/* LONG DOUBLE */
static VALUE s_read_long_double(sereal_t *s, u8 tag) {
        long double *d = (long double *) s_get_p_at_pos(s,s->pos,sizeof(*d));
        return DBL2NUM((double) *d);
}

/* POS */
static VALUE s_read_small_positive_int(sereal_t *s, u8 tag) {
        return INT2FIX(tag);
}

/* NEG */
static VALUE s_read_small_negative_int(sereal_t *s, u8 tag) {
        return INT2FIX(tag - 32);
}

/* ARRAY */ 
static inline VALUE s_read_array_with_len(sereal_t *s, u32 len) {
        VALUE arr[len];
        register u32 i;
        for (i = 0; i < len; i++)
                arr[i] = sereal_to_rb_object(s);
        return rb_ary_new4(len,arr);
}

/* ARRAY */ 
static VALUE s_read_array(sereal_t *s,u8 tag) {
        return s_read_array_with_len(s,s_get_varint_bang(s));
}

/* ARRAY */ 
static VALUE s_read_arrayref(sereal_t *s, u8 tag) {
        return s_read_array_with_len(s,tag & SRL_MASK_ARRAYREF_COUNT);       
}

/* HASH */ 
static inline VALUE s_read_hash_with_len(sereal_t *s, u32 len) {
        VALUE hash = rb_hash_new();
        register u32 i;
        for (i = 0; i < len; i++) {
                VALUE key = sereal_to_rb_object(s);
                VALUE value = sereal_to_rb_object(s);
                rb_hash_aset(hash,key,value);
        }
        return hash;
}
/* HASH */ 
static VALUE s_read_hash(sereal_t *s, u8 tag) {
        return s_read_hash_with_len(s,s_get_varint_bang(s));
}
/* HASH */ 
static VALUE s_read_hashref(sereal_t *s, u8 tag) {
        return s_read_hash_with_len(s,tag  & SRL_MASK_HASHREF_COUNT);
}
static VALUE s_read_rb_string_bang(sereal_t *s,u8 t) {
        u32 len = 0;
        VALUE string;
        #define RETURN_STRING(fx_l,fx_gen)              \
        do {                                            \
                len = fx_l;                             \
                string = fx_gen;                        \
                s_shift_position_bang(s,len);           \
                return string;                          \
        } while(0);
        if (t == SRL_HDR_STR_UTF8) {
                RETURN_STRING(s_get_varint_bang(s),
                              rb_enc_str_new(s_get_p(s),len,
                              rb_utf8_encoding()));
        } else if (t == SRL_HDR_BINARY || t == SRL_HDR_SYM) {
                RETURN_STRING(s_get_varint_bang(s),
                              rb_str_new(s_get_p(s),len));
        } else if (IS_SHORT_BINARY(t)) {
                RETURN_STRING((t & SRL_MASK_SHORT_BINARY_LEN),
                              rb_str_new(s_get_p(s), len));
        }
        #undef RETURN_STRING
        rb_raise(rb_eTypeError, "undefined string type %d",t);
}

static VALUE s_read_next_rb_string_bang(sereal_t *s) {
        return s_read_rb_string_bang(s,s_get_u8_bang(s));
}

static VALUE s_read_regexp(sereal_t *s, u8 tag) {
        VALUE pattern = s_read_next_rb_string_bang(s);
        VALUE modifiers = s_read_next_rb_string_bang(s);

        u32 flags = 0;
        if (strchr(RSTRING_PTR(modifiers),'i')) 
                flags |= IGNORECASE;       
        
        if (strchr(RSTRING_PTR(modifiers),'m')) 
                flags |= MULTILINE;       

        if (strchr(RSTRING_PTR(modifiers),'x')) 
                flags |= EXTENDED;
	#ifdef RUBINIUS
		return rb_reg_new(RSTRING_PTR(pattern),RSTRING_LEN(pattern),flags);
	#else
		return rb_reg_new_str(pattern,flags);
	#endif
}

static VALUE s_read_nil(sereal_t *s, u8 tag) {
        return Qnil;
}

static VALUE s_read_true(sereal_t *s, u8 tag) {
        return Qtrue;
}

static VALUE s_read_false(sereal_t *s, u8 tag) {
        return Qfalse;
}

static VALUE s_read_pad(sereal_t *s, u8 tag) {
        /* just skip this byte and go forward */
        return sereal_to_rb_object(s);
}
static VALUE s_read_extend(sereal_t *s, u8 tag) {
        rb_raise(rb_eArgError,"extend tags are not supported");
}

VALUE sereal_to_rb_object(sereal_t *s) {
        u8 t;
        S_RECURSE_INC(s);
        while (s->pos < s->size) {
                t = s_get_u8_bang(s);
                if (t & SRL_HDR_TRACK_FLAG)
                        rb_raise(rb_eArgError, "trackable objects are not supported");
                S_RECURSE_DEC(s);
                return (*READERS[t])(s,t);
        }
        return Qnil;
}

VALUE method_sereal_decode(VALUE self, VALUE args) {
        u32 argc = RARRAY_LEN(args);
        if (argc < 1)
                rb_raise(rb_eArgError,"need at least 1 argument (object)");
        VALUE payload = rb_ary_shift(args);
        if (TYPE(payload) != T_STRING) 
                rb_raise(rb_eTypeError,"can not decode objects of type %s",rb_obj_classname(payload));

        sereal_t *s = s_create();
        u64 offset = 0;
again: 
        if (offset >= RSTRING_LEN(payload)) {
            free(s);
            return Qnil;
        }
        s->data = RSTRING_PTR(payload) + offset;
        s->size = RSTRING_LEN(payload) - offset;
        s->pos = 0;
        if (s->size < 6 || s_get_u32_bang(s) != SRL_MAGIC_STRING_LILIPUTIAN) 
                rb_raise(rb_eTypeError,"invalid header"); 

        u8 version = s_get_u8_bang(s);
        u8 suffix = s_get_varint_bang(s);
        u8 is_compressed;

        if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY)
                is_compressed = __SNAPPY;
        else if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY_INCR)
                is_compressed = __SNAPPY_INCR;
        else if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_LZ4_INCR)
                is_compressed = __LZ4_INCR;
        else
                is_compressed = __RAW;
        if (is_compressed) {
                u32 uncompressed_len;
                u32 compressed_len;

                if (is_compressed == __LZ4_INCR) {
                        uncompressed_len = s_get_varint_bang(s);
                        if (!uncompressed_len)
                                rb_raise(rb_eTypeError,"LZ4 compression requires <varint uncompressed size><varint compressed size><blob>, unable to get uncompressed_len");

                        compressed_len = s_get_varint_bang(s);
                } else {
                        if (is_compressed == __SNAPPY_INCR) {
                                compressed_len = s_get_varint_bang(s);
                        } else {
                                compressed_len = s->size - s->pos;
                        }

                        int snappy_header_len = csnappy_get_uncompressed_length(s_get_p(s),
                                                                                compressed_len,
                                                                                &uncompressed_len);
                        if (snappy_header_len == CSNAPPY_E_HEADER_BAD) 
                                rb_raise(rb_eTypeError,"invalid snappy header");
                }
                u8 *uncompressed = alloc_or_raise(uncompressed_len);
                int done;
                if (is_compressed == __LZ4_INCR) {
                        done = LZ4_decompress_safe(s_get_p(s),
                                                   uncompressed,
                                                   compressed_len,
                                                   uncompressed_len) == uncompressed_len ? 1 : 0;
                } else {
                        done = csnappy_decompress(s_get_p(s),
                                              compressed_len,
                                              uncompressed,
                                              uncompressed_len) == CSNAPPY_E_OK ? 1 : 0;
                }
                if (!done)
                        rb_raise(rb_eTypeError, "decompression failed error: %d type: %d, unompressed size: %d compressed size: %d",done,is_compressed,uncompressed_len,compressed_len);

                offset += s->pos + compressed_len;
                s->data = uncompressed;
                s->size = uncompressed_len;
                s->pos = 0;
        }

        VALUE result = sereal_to_rb_object(s);
        if (is_compressed && rb_block_given_p()) {
                free(s->data);
                s->data = NULL;
                rb_yield(result);
                goto again;
        }

        if (is_compressed)
                s_destroy(s);
        else
                free(s); // we do not destroy because it will free s->data which is RSTRING_PTR(payload) 

        return result;
}

