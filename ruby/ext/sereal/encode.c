#include "sereal.h"
#include "buffer.h"
#include "encode.h"
#include "snappy/csnappy_compress.c"

/* function pointer array */
void (*WRITER[sizeof(T_FIXNUM)])(sereal_t *,VALUE);
void s_init_writers(void) {
        u32 i;
        for (i = 0; i < sizeof(T_FIXNUM); i++) 
                WRITER[i] = s_default_writer;
        WRITER[T_FIXNUM] = s_append_integer;
        WRITER[T_BIGNUM] = s_append_integer;
        WRITER[T_FLOAT]  = s_append_double;
        WRITER[T_OBJECT] = s_append_object;
        WRITER[T_REGEXP] = s_append_regexp;
        WRITER[T_STRING] = s_append_rb_string;
        WRITER[T_ARRAY]  = s_append_array;
        WRITER[T_HASH]   = s_append_hash;
        WRITER[T_SYMBOL] = s_append_symbol;
        WRITER[T_TRUE]   = s_append_true;
        WRITER[T_FALSE]  = s_append_false;
        WRITER[T_NIL]    = s_append_nil;
}

static void s_default_writer(sereal_t *s, VALUE object) {
        rb_raise(rb_eTypeError, "invalid type for input %s",rb_obj_classname(object));
}


static inline void s_append_varint(sereal_t *s,u64 n) {
        while (n >= 0x80) {
                s_append_u8(s,((n & 0x7f) | 0x80));
                n >>= 7; 
        }
        s_append_u8(s,n);
}

static inline void s_append_hdr_with_varint(sereal_t *s,u8 hdr, u64 n) {
        s_append_u8(s,hdr);
        s_append_varint(s,n);
}

static inline void s_append_zigzag(sereal_t *s,u64 n) {
        s_append_hdr_with_varint(s,SRL_HDR_ZIGZAG,
                                 (n << 1) ^ (n >> (sizeof(long) * 8 - 1)));
}

static inline void s_append_string(sereal_t *s,u8 *string, u32 len,u8 is_utf8) {
        if (is_utf8) {
                s_append_hdr_with_varint(s,SRL_HDR_STR_UTF8,len);
        } else {
                if (len < SRL_MASK_SHORT_BINARY_LEN) {
                        s_append_u8(s,SRL_HDR_SHORT_BINARY_LOW | (u8)len);
                } else {
                        s_append_hdr_with_varint(s,SRL_HDR_BINARY,len); 
                }
        }
        s_append(s,string,len);
}

static void s_append_rb_string(sereal_t *s, VALUE object) {
        s_append_string(s,RSTRING_PTR(object),
                        RSTRING_LEN(object),
                        (is_ascii_string(object) ? FALSE : TRUE));
}

#define REF_THRESH(thresh,low,high)                     \
do {                                                    \
        if (len < (thresh))                             \
                s_append_u8(s, low | (u8) len);         \
        else                                            \
                s_append_hdr_with_varint(s,high,len);   \
} while(0);
static void s_append_array(sereal_t *s, VALUE object) {
        u32 i,len = RARRAY_LEN(object);
        REF_THRESH(SRL_MASK_ARRAYREF_COUNT,SRL_HDR_ARRAYREF,SRL_HDR_ARRAY);
        
        for (i = 0; i < len; i++)
                rb_object_to_sereal(s,rb_ary_entry(object,i));
}


int s_hash_foreach(VALUE key, VALUE value, VALUE sereal_t_object) {
        if (key == Qundef)
                return ST_CONTINUE;
        rb_object_to_sereal((sereal_t *) sereal_t_object,key);
        rb_object_to_sereal((sereal_t *) sereal_t_object,value);
        return ST_CONTINUE;
}
static void s_append_hash(sereal_t *s, VALUE object) {
        u32 len = RHASH_SIZE(object);
        REF_THRESH(SRL_MASK_HASHREF_COUNT,SRL_HDR_HASHREF,SRL_HDR_HASH);
        rb_hash_foreach(object, s_hash_foreach, (VALUE) s);
}
#undef REF_THRESH

/* 
  not standartized, we are using RESERVED_LOW as SRL_HDR_SYM
  protocol is the same as LATIN1 strings but with different tag:
        SRL_HDR_SYM TAG
        len <VARINT>
        value - <len> bytes
*/
static void s_append_symbol(sereal_t *s, VALUE object) {
        u32 len;
        VALUE string  = rb_sym_to_s(object);
        len = RSTRING_LEN(string);

        s_append_hdr_with_varint(s,SRL_HDR_SYM,len);
        s_append(s,RSTRING_PTR(string),len);
}

/*
  this is not standartized, we are using (RESERVED_LOW + 1) tag as SRL_HDR_RB_OBJ
  and the protocol is as follows:
        SRL_HDR_RB_OBJ TAG
        class name <STR>
        instance variables count <VARINT>
        0..count
                instance variable name <STR>
                instance variable value <ITEM> 

*/
static void s_append_object(sereal_t *s, VALUE object) {
        u32 i,len;
        VALUE ivars = rb_obj_instance_variables(object);

        s_append_u8(s,SRL_HDR_RB_OBJ);
        s_append_rb_string(s,rb_obj_as_string(rb_obj_class(object)));

        len = RARRAY_LEN(ivars);
        s_append_varint(s,len);

        for (i = 0; i < len; i++) {
                VALUE var_name_sym = rb_ary_entry(ivars,i);
                VALUE iv = rb_ivar_get(object,SYM2ID(var_name_sym));
                s_append_rb_string(s,rb_sym_to_s(var_name_sym));
                rb_object_to_sereal(s,iv);
        }
}


// <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>
static void s_append_regexp(sereal_t *s, VALUE object) {
        s_append_u8(s,SRL_HDR_REGEXP);
        rb_encoding *enc = rb_enc_get(object);

        VALUE pattern = rb_enc_str_new(RREGEXP_SRC_PTR(object),RREGEXP_SRC_LEN(object), enc);
        s_append_rb_string(s,pattern);

        int flags = rb_reg_options(object);
        VALUE f = rb_str_new("",0);
        if (flags & IGNORECASE) 
                rb_str_cat(f,"i",1);
        if (flags & EXTENDED)
                rb_str_cat(f,"x",1);
        if (flags & MULTILINE)
                rb_str_cat(f,"m",1);
        s_append_rb_string(s,f);
}


static void s_append_integer(sereal_t *s, VALUE object) {
        long long v = FIXNUM_P(object) ? FIX2LONG(object) : rb_num2ll(v);
        if (v >= 0) {
                if (v < 16) 
                        s_append_u8(s,SRL_HDR_POS_LOW | (u8) v);
                else {
                        unsigned long long ullv = 0;
                        if (!FIXNUM_P(object))
                                s_append_hdr_with_varint(s,SRL_HDR_VARINT,NUM2ULL(object));
                        else
                                s_append_hdr_with_varint(s,SRL_HDR_VARINT,v);
                }
        } else {
                if (v > -17)
                        s_append_u8(s,SRL_HDR_NEG_LOW | ((u8) v + 32));
                else
                        s_append_zigzag(s,v);                        
        }
}
static void s_append_double(sereal_t *s, VALUE object) {
        double d = NUM2DBL(object);
        s_append_u8(s,SRL_HDR_DOUBLE);
        s_append(s,&d,sizeof(d));
}
static void s_append_true(sereal_t *s, VALUE object) {
        s_append_u8(s,SRL_HDR_TRUE);
}
static void s_append_false(sereal_t *s, VALUE object) {
        s_append_u8(s,SRL_HDR_FALSE);
}
static void s_append_nil(sereal_t *s, VALUE object) {
        s_append_u8(s,SRL_HDR_UNDEF);
}

/* writer function pointers */

static void rb_object_to_sereal(sereal_t *s, VALUE object) {
        S_RECURSE_INC(s);
        (*WRITER[TYPE(object)])(s,object);
        S_RECURSE_DEC(s);
}

VALUE method_sereal_encode(VALUE self, VALUE args) {
        sereal_t *s = s_create();
        u32 argc = RARRAY_LEN(args);
        if (argc < 1)
                rb_raise(rb_eArgError,"need at least 1 argument (object)");
        VALUE payload = rb_ary_shift(args);
        VALUE compress = Qfalse;
        if (argc == 2) 
                compress = rb_ary_shift(args);
        int do_compress = (compress == Qtrue ? TRUE : FALSE);

        // setup header
        s_append_u32(s,SRL_MAGIC_STRING_LILIPUTIAN);
        s_append_u8(s,SRL_PROTOCOL_VERSION | (do_compress ? SRL_PROTOCOL_ENCODING_SNAPPY : SRL_PROTOCOL_ENCODING_RAW));
        s_append_u8(s,0x0);
        u32 s_header_len = s->pos;
        
        // serialize
        rb_object_to_sereal(s,payload);

        // compress
        if (do_compress) {
                u32 s_body_len = s->size - s_header_len;
                u32 compressed_len = csnappy_max_compressed_length(s_body_len);

                u8 *working_buf = alloc_or_raise(CSNAPPY_WORKMEM_BYTES);
                u8 *compressed = alloc_or_raise(compressed_len + s_header_len);

                COPY(s_get_p_at_pos(s,0,0),compressed,s_header_len);
                
                csnappy_compress(s_get_p_at_pos(s,s_header_len,1), 
                                 s_body_len,
                                 (compressed + s_header_len),
                                 &compressed_len,
                                 working_buf, 
                                 CSNAPPY_WORKMEM_BYTES_POWER_OF_TWO);
                free(s->data);
                s->data = compressed;
                s->size = compressed_len + s_header_len;
        }

        VALUE result = rb_str_new(s->data,s->size);
        s_destroy(s);
        return result;
}
