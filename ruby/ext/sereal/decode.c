#include "main.h"
#include "snappy/csnappy_decompress.c"
static VALUE sereal_to_rb_object(sereal_t *s);

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
        float *d = (float *) s_get_p_at_pos(s,s->pos,sizeof(*d));
        return FLT2NUM(*d);
}

/* DOUBLE */
static VALUE s_read_double(sereal_t *s, u8 tag) {
        double *d = (double *) s_get_p_at_pos(s,s->pos,sizeof(*d));
        return DBL2NUM(*d);
}

/* LONG DOUBLE */
static VALUE s_read_long_double(sereal_t *s, u8 tag) {
        long double *d = (long double *) s_get_p_at_pos(s,s->pos,sizeof(*d));
        return DBL2NUM(*d);
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
        register int i;
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
        register int i;
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
                              rb_enc_str_new(s_get_p(s),len,rb_utf8_encoding()));
        } else if (t == SRL_HDR_BINARY || t == SRL_HDR_SYM) {
                RETURN_STRING(s_get_varint_bang(s),
                              rb_str_new(s_get_p(s),len));
        } else if (IS_SHORT_BINARY(t)) {
                RETURN_STRING((t & SRL_MASK_SHORT_BINARY_LEN),
                              rb_str_new(s_get_p(s), len));
        }
                rb_raise(rb_eTypeError, "undefined string type %d",t);
        #undef CREATE_STRING
        return Qnil;
}

static VALUE s_read_next_rb_string_bang(sereal_t *s) {
        u8 t = s_get_u8_bang(s);
        return s_read_rb_string_bang(s,t);
}

static VALUE s_read_object(sereal_t *s,u8 tag) {
        u32 len,i;
        VALUE klass_name = s_read_next_rb_string_bang(s);
        VALUE klass = rb_const_get(rb_cObject,rb_intern_str(klass_name));
        VALUE argv[0];
        VALUE object = rb_class_new_instance(0, argv, klass);
        len = s_get_varint_bang(s);
        for (i = 0; i < len; i++) {
                VALUE iv_key = sereal_to_rb_object(s);
                VALUE iv_value = sereal_to_rb_object(s);
                rb_ivar_set(object,rb_intern_str(iv_key),iv_value);
        }
        return object;
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

        return rb_reg_new_str(pattern,flags);
}

static VALUE s_read_sym(sereal_t *s,u8 tag) {
        return rb_str_intern(s_read_rb_string_bang(s,tag));
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

static VALUE sereal_to_rb_object(sereal_t *s) {
        u8 t;
        u32 varint,i,len;
        S_RECURSE_INC(s);
        while (s->pos < s->size) {
                t = s_get_u8_bang(s);
                if (t & SRL_HDR_TRACK_FLAG)
                        rb_raise(rb_eArgError, "trackable objects are not supported");
                S_RECURSE_DEC(s);
                return (*s->reader[t])(s,t);
        }
        return Qnil;
}

VALUE method_sereal_decode(VALUE self, VALUE payload) {
        if (TYPE(payload) != T_STRING) 
                rb_raise(rb_eTypeError,"can not decode objects of type %s",rb_obj_classname(payload));

        sereal_t *s = s_create();
        s->data = RSTRING_PTR(payload);
        s->size = RSTRING_LEN(payload);

        if (s->size < 6 || s_get_u32_bang(s) != 0x6c72733d) 
                rb_raise(rb_eTypeError,"invalid header"); 

        u8 version = s_get_u8_bang(s);
        u8 suffix = s_get_varint_bang(s);
        int is_compressed = (version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY ? TRUE : FALSE;

        if (is_compressed) {
                u32 uncompressed_len;
                int snappy_header_len = csnappy_get_uncompressed_length(s_get_p(s),
                                                                        (s->size - s->pos),
                                                                        &uncompressed_len);
                if (snappy_header_len == CSNAPPY_E_HEADER_BAD) 
                        rb_raise(rb_eTypeError,"invalid snappy header");

                u8 *uncompressed = alloc_or_raise(uncompressed_len);
                int done = csnappy_decompress(s_get_p(s),
                                              (s->size - s->pos),
                                              uncompressed,
                                              uncompressed_len);
                if (done)
                        rb_raise(rb_eTypeError, "snappy decompression failed error: %d",done);
                s->data = uncompressed;
                s->size = uncompressed_len;
                s->pos = 0;
        }

        #define REGISTER(s,min,max,fx)          \
        do {                                    \
                int i;                          \
                for (i = min; i <= max; i++)    \
                        s_register(s,i,fx);     \
        } while(0);


        REGISTER(s,SRL_HDR_POS_LOW,SRL_HDR_POS_HIGH,s_read_small_positive_int);
        REGISTER(s,SRL_HDR_NEG_LOW,SRL_HDR_NEG_HIGH,s_read_small_negative_int);
        s_register(s,SRL_HDR_VARINT,s_read_varint);
        s_register(s,SRL_HDR_DOUBLE,s_read_double);
        s_register(s,SRL_HDR_LONG_DOUBLE,s_read_long_double);
        s_register(s,SRL_HDR_FLOAT,s_read_float);
        s_register(s,SRL_HDR_ZIGZAG,s_read_zigzag);

        s_register(s,SRL_HDR_REGEXP,s_read_regexp);
        s_register(s,SRL_HDR_UNDEF,s_read_nil);
        s_register(s,SRL_HDR_TRUE,s_read_true);
        s_register(s,SRL_HDR_FALSE,s_read_false);
        s_register(s,SRL_HDR_RB_OBJ,s_read_object); 

        s_register(s,SRL_HDR_SYM,s_read_sym);
        s_register(s,SRL_HDR_BINARY,s_read_rb_string_bang);
        s_register(s,SRL_HDR_STR_UTF8,s_read_rb_string_bang);
        REGISTER(s,SRL_HDR_SHORT_BINARY_LOW,SRL_HDR_SHORT_BINARY_HIGH,s_read_rb_string_bang);

        s_register(s,SRL_HDR_HASH,s_read_hash);
        REGISTER(s,SRL_HDR_HASHREF_LOW,SRL_HDR_HASHREF_HIGH,s_read_hashref);
        
        s_register(s,SRL_HDR_ARRAY,s_read_array);
        REGISTER(s,SRL_HDR_ARRAYREF_LOW,SRL_HDR_ARRAYREF_HIGH,s_read_arrayref);

        #undef REGISTER
        VALUE result = sereal_to_rb_object(s); 
        if (is_compressed)
                s_destroy(s);
        else
                free(s); // we do not destroy because it will free s->data which is RSTRING_PTR(payload) 
        return result;
}

