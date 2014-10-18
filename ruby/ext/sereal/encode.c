#include "sereal.h"
#include "buffer.h"
#include "encode.h"
#include "snappy/csnappy_compress.c"
#define W_SIZE 32
#if T_FIXNUM > W_SIZE
	#define W_SIZE T_FIXNUM
#elif T_BIGNUM > W_SIZE
	#define W_SIZE T_BIGNUM
#elif T_FLOAT > W_SIZE
	#define W_SIZE T_FLOAT
#elif T_OBJECT > W_SIZE
	#define W_SIZE T_OBJECT
#elif T_REGEXP > W_SIZE
	#define W_SIZE T_REGEXP
#elif T_STRING > W_SIZE
	#define W_SIZE T_STRING
#elif T_ARRAY > W_SIZE
	#define W_SIZE T_ARRAY
#elif T_HASH > W_SIZE
	#define W_SIZE T_HASH
#elif T_SYMBOL > W_SIZE
	#define W_SIZE T_SYMBOL
#elif T_TRUE > W_SIZE
	#define W_SIZE T_TRUE
#elif T_FALSE > W_SIZE
	#define W_SIZE T_FALSE
#elif T_NIL > W_SIZE
	#define W_SIZE T_NIL
#elif T_DATA > W_SIZE
    #define W_SIZE T_DATA
#endif
#define COMPLEX(object)                         \
    (TYPE(object) == T_ARRAY  ||                \
     TYPE(object) == T_HASH   ||                \
     TYPE(object) == T_SYMBOL ||                \
     TYPE(object) == T_OBJECT ||                \
     TYPE(object) == T_STRING ||                \
     TYPE(object) == T_DATA)

/* function pointer array */
void (*WRITER[W_SIZE])(sereal_t *,VALUE);

static void rb_object_to_sereal(sereal_t *s, VALUE object);
static void s_append_varint(sereal_t *s,u64 n);
static void s_append_hdr_with_varint(sereal_t *s,u8 hdr, u64 n);
static void s_append_zigzag(sereal_t *s,long long n);
static void s_append_string(sereal_t *s, VALUE object);
static void s_append_array(sereal_t *s, VALUE object);
static void s_append_hash(sereal_t *s, VALUE object);
static void s_append_symbol(sereal_t *s, VALUE object);
static void s_append_object(sereal_t *s, VALUE object);
static void s_append_regexp(sereal_t *s, VALUE object);
static void s_append_bignum(sereal_t *s, VALUE object);
static void s_append_fixnum(sereal_t *s, VALUE object);
static void s_append_double(sereal_t *s, VALUE object);
static void s_append_true(sereal_t *s, VALUE object);
static void s_append_false(sereal_t *s, VALUE object);
static void s_append_nil(sereal_t *s, VALUE object);
static void s_default_writer(sereal_t *s, VALUE object);

void s_init_writers(void) {
    u32 i;
    for (i = 0; i < sizeof(WRITER)/sizeof(WRITER[0]); i++)
        WRITER[i] = s_default_writer;

    WRITER[T_FIXNUM] = s_append_fixnum;
    WRITER[T_BIGNUM] = s_append_bignum;
    WRITER[T_FLOAT]  = s_append_double;
    WRITER[T_OBJECT] = s_append_object;
    WRITER[T_REGEXP] = s_append_regexp;
    WRITER[T_STRING] = s_append_string;
    WRITER[T_ARRAY]  = s_append_array;
    WRITER[T_HASH]   = s_append_hash;
    WRITER[T_SYMBOL] = s_append_symbol;
    WRITER[T_TRUE]   = s_append_true;
    WRITER[T_FALSE]  = s_append_false;
    WRITER[T_NIL]    = s_append_nil;
    WRITER[T_DATA]   = s_append_object;
}

static void s_default_writer(sereal_t *s, VALUE object) {
    s_raise(s,rb_eTypeError, "invalid type for input %s",rb_obj_classname(object));
}

static inline void s_append_varint(sereal_t *s,u64 n) {
    s_prepare(s,SRL_MAX_VARINT_LENGTH+1);
    while (likely(n >= 0x80)) {
        s->data[s->size++] = (n & 0x7f) | 0x80;
        n = n >> 7;
    }
    s->data[s->size++] = n;
    s->pos = s->size;
}

static inline void s_append_hdr_with_varint(sereal_t *s,u8 hdr, u64 n) {
    s_append_u8(s,hdr);
    s_append_varint(s,n);
}

static inline void s_append_zigzag(sereal_t *s,long long n) {
    s_append_hdr_with_varint(s,SRL_HDR_ZIGZAG,(n << 1) ^ (n >> 63));
}

static void s_append_string(sereal_t *s, VALUE object) {
    u32 len = (u32) RSTRING_LEN(object);
    if (likely(is_ascii_string(object))) {
        if (likely(len < SRL_MASK_SHORT_BINARY_LEN)) {
            s_append_u8(s,SRL_HDR_SHORT_BINARY_LOW | (u8)len);
        } else {
            s_append_hdr_with_varint(s,SRL_HDR_BINARY,len); 
        }
    } else {
        s_append_hdr_with_varint(s,SRL_HDR_STR_UTF8,len);
    }
    s_append(s,(u8 *)RSTRING_PTR(object),len);
}

#define REF_THRESH(thresh,low,high)                     \
    do {                                                \
        if (unlikely(len < (thresh)))                   \
            s_append_u8(s, low | (u8) len);             \
        else                                            \
            s_append_hdr_with_varint(s,high,len);       \
    } while(0);

static void s_append_array(sereal_t *s, VALUE object) {
    u32 i,len = RARRAY_LEN(object);
    REF_THRESH(SRL_MASK_ARRAYREF_COUNT,SRL_HDR_ARRAYREF,SRL_HDR_ARRAY);

    for (i = 0; i < len; i++)
        rb_object_to_sereal(s,rb_ary_entry(object,i));
}


int s_hash_foreach(VALUE key, VALUE value, VALUE sereal_t_object) {
    if (unlikely(key == Qundef))
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
	convert symbols to strings
*/
static void s_append_symbol(sereal_t *s, VALUE object) {
    s_append_string(s,rb_sym_to_s(object));
}

static void s_append_copy(sereal_t *s, u8 tag,VALUE object) {
    u32 pos = FIX2LONG(object);
    s_append_hdr_with_varint(s,tag,pos - s->hdr_end + 1);
}

static VALUE s_copy_or_keep_in_mind(sereal_t *s, VALUE object, u8 offset) {
    if (likely(s->copy == Qnil))
        return Qnil;

    VALUE stored_position = rb_hash_lookup(s->copy,object);
    if (unlikely(stored_position == Qnil))
        rb_hash_aset(s->copy,object,INT2FIX(s->pos + offset));
    return stored_position;
}


/*
        try to FREEZE the object so it can be THAW-ed at decode time
        if not possible (no Sereal::THAW argument or object does not
        repsond to FREEZE), just call object.to_srl and serialize the
        result
*/
static void s_append_object(sereal_t *s, VALUE object) {
    if (s->flags & __THAW && rb_obj_respond_to(object,FREEZE,0)) {
        VALUE klass = rb_class_path(CLASS_OF(object));
        // keep in mind with offset + 1
        // because of the SRL_HDR_OBJECT_FREEZE header taking 1 byte
        // and we want to point to the next string
        VALUE copy = s_copy_or_keep_in_mind(s,klass,1);
        if (copy != Qnil) {
            s_append_copy(s,SRL_HDR_OBJECTV_FREEZE,copy);
        } else {
            s_append_u8(s,SRL_HDR_OBJECT_FREEZE);
            s_append_string(s,klass);
        }
        VALUE frozen = rb_funcall(object,FREEZE,1,ID2SYM(SEREAL));
        if (TYPE(frozen) != T_ARRAY)
            s_raise(s,rb_eTypeError,"Sereal spec requires FREEZE to return array instead %s",rb_obj_classname(frozen));

        // REFN + ARRAY
        s_append_u8(s,SRL_HDR_REFN);
        s_append_hdr_with_varint(s,SRL_HDR_ARRAY,RARRAY_LEN(frozen));
        int i;
        for (i = 0; i < RARRAY_LEN(frozen); i++)
            rb_object_to_sereal(s,rb_ary_entry(frozen,i));

    } else {
        rb_object_to_sereal(s,rb_funcall(object,TO_SRL,0));
    }
}


// <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>
static void s_append_regexp(sereal_t *s, VALUE object) {
    s_append_u8(s,SRL_HDR_REGEXP);
    rb_encoding *enc = rb_enc_get(object);
    VALUE pattern;
#ifndef RREGEXP_SRC_PTR
    VALUE string = RREGEXP_SRC(object);
    pattern = rb_enc_str_new(RSTRING_PTR(string),RSTRING_LEN(string),enc);
#else
    pattern = rb_enc_str_new(RREGEXP_SRC_PTR(object),RREGEXP_SRC_LEN(object), enc);
#endif
    s_append_string(s,pattern);

    int flags = rb_reg_options(object);
    VALUE f = rb_str_new("",0);
    if (flags & IGNORECASE)
        rb_str_cat(f,"i",1);
    if (flags & EXTENDED)
        rb_str_cat(f,"x",1);
    if (flags & MULTILINE)
        rb_str_cat(f,"m",1);
    s_append_string(s,f);
}

#define I_APPEND(_v,_unsigned)                                          \
do {                                                                    \
    if (likely(_v >= 0)) {                                              \
            if (unlikely(_v < 16))                                      \
                s_append_u8(s,SRL_HDR_POS_LOW | (u8) _v);               \
            else                                                        \
                s_append_hdr_with_varint(s,SRL_HDR_VARINT,(u64)_v);     \
    } else {                                                            \
        if (unlikely(!_unsigned && _v > -17))                           \
            s_append_u8(s,SRL_HDR_NEG_LOW | ((u8) _v + 32));            \
        else                                                            \
            s_append_zigzag(s,_v);                                      \
    }                                                                   \
} while(0)

static void s_append_fixnum(sereal_t *s, VALUE object) {
    long long v = likely(FIXNUM_P(object)) ? FIX2LONG(object) : NUM2LL(object);
    I_APPEND(v,0);
}

static void s_append_bignum(sereal_t *s, VALUE object) {
    if (likely(RBIGNUM_POSITIVE_P(object))) {
        unsigned long long uv = rb_big2ull(object);
        I_APPEND(uv,1);
    } else {
        long long v = rb_big2ll(object);
        I_APPEND(v,0);
    }
}
#undef I_APPEND

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

static void s_append_refp(sereal_t *s, VALUE object) {
    u32 pos = FIX2LONG(object);
    s_append_hdr_with_varint(s,SRL_HDR_REFP,pos - s->hdr_end + 1);
    s_set_flag_at_pos(s,pos,SRL_HDR_TRACK_FLAG);
}

/* writer function pointers */
static void rb_object_to_sereal(sereal_t *s, VALUE object) {
    S_RECURSE_INC(s);
    u32 pos = s->pos;
    if (unlikely(s->tracked != Qnil || s->copy != Qnil) && likely(COMPLEX(object))) {
        VALUE stored;
        if (s->tracked != Qnil) {
            VALUE id = rb_obj_id(object);
            stored = rb_hash_lookup(s->tracked,id);
            if (stored != Qnil) {
                s_append_refp(s,stored);
                goto out;
            }
            rb_hash_aset(s->tracked,id,INT2FIX(pos));
        }
        stored = s_copy_or_keep_in_mind(s,object,0);
        if (stored != Qnil) {
            s_append_copy(s,SRL_HDR_COPY,stored);
            goto out;
        }
    }

    (*WRITER[TYPE(object)])(s,object);
out:
    S_RECURSE_DEC(s);
}

// https://github.com/Sereal/Sereal/blob/master/Perl/Encoder/srl_encoder.c#L623
void fixup_varint_from_to(u8 *varint_start, u8 *varint_end, u32 number) {
    while (number >= 0x80) {
        *varint_start++ = (number & 0x7f) | 0x80;
        number = number >> 7;
    }
    if ( varint_start == varint_end ) {
        *varint_start = number;
    } else {
        *varint_start++ = (number & 0x7f) | 0x80;
        while ( varint_start < varint_end )
            *varint_start++ = 0x80;
        *varint_start= 0;
    }
}

/*
 * Encodes object into Sereal
 */
VALUE
method_sereal_encode(VALUE self, VALUE args) {
    u32 argc = RARRAY_LEN(args);
    if (argc < 1)
        rb_raise(rb_eArgError,"need at least 1 argument (object)");

    sereal_t *s = s_create();
    VALUE payload = rb_ary_entry(args,0);
    VALUE compress = Qfalse;
    if (argc == 2)
        compress = rb_ary_entry(args,1);

    u8 do_compress;
    u8 version = SRL_PROTOCOL_VERSION;

    if (TYPE(compress) == T_FIXNUM) {
        do_compress = (u8) FIX2LONG(compress);
    } else {
        do_compress = (compress == Qtrue ? 1 : 0);
    }

    s->flags = do_compress & __ARGUMENT_FLAGS;
    do_compress &=~ __ARGUMENT_FLAGS;
    if (s->flags & __REF)
        s_init_tracker(s);

    if (s->flags & __COPY)
        s_init_copy(s);

    switch(do_compress) {
        case __SNAPPY:
            version |= SRL_PROTOCOL_ENCODING_SNAPPY;
            break;
        case __SNAPPY_INCR:
            version |= SRL_PROTOCOL_ENCODING_SNAPPY_INCR;
            break;
        case __RAW:
        default:
            version |= SRL_PROTOCOL_ENCODING_RAW;
    }

    // setup header
    s_append_u32(s,SRL_MAGIC_STRING_UINT_LE);
    s_append_u8(s,version);
    s_append_u8(s,0x0);
    u32 s_header_len = s->pos;

    // serialize
    s->hdr_end = s->pos;
    rb_object_to_sereal(s,payload);

    // compress
    if (do_compress) {
        u8 *start_compressed_varint = NULL, *start_un_compressed_varint= NULL, *compressed, *end;
        u32 compressed_len = 0;
        u32 compressed_len_varint = 0;
        u32 un_compressed_len_varint = 0;
        u32 s_body_len = s->size - s_header_len;
        // snappy <compressed blob>
        // snappy incr <varint blob len><compressed blob>
        if (do_compress == __SNAPPY || do_compress == __SNAPPY_INCR) {
            compressed_len = csnappy_max_compressed_length(s_body_len);
        }

        if (do_compress == __SNAPPY_INCR) {
            start_compressed_varint = s_end_p(s);
            s_append_varint(s,compressed_len);
            end = s_end_p(s);
            compressed_len_varint = end - start_compressed_varint;
        }

        compressed = s_alloc_or_raise(s,compressed_len + s_header_len + compressed_len_varint + un_compressed_len_varint);

        COPY(s_get_p_at_pos(s,0,0),compressed,s_header_len);

        if (start_un_compressed_varint)
            COPY(start_un_compressed_varint,
                compressed + s_header_len,
                un_compressed_len_varint);
        if (start_compressed_varint)
            COPY(start_compressed_varint,
                 compressed + s_header_len + un_compressed_len_varint,
                 compressed_len_varint);

        u8 *start = s_get_p_at_pos(s,s_header_len,0);
        u8 *working_buf = s_alloc_or_raise(s,CSNAPPY_WORKMEM_BYTES);
        csnappy_compress((char *)start,
                         s_body_len,
                         (char *)(compressed + s_header_len + compressed_len_varint + un_compressed_len_varint),
                         &compressed_len,
                         working_buf,
                         CSNAPPY_WORKMEM_BYTES_POWER_OF_TWO);

        free(working_buf);
        if (compressed_len == 0)
            s_raise(s,rb_eTypeError,"failed to compress");
        if (start_compressed_varint)
            fixup_varint_from_to(compressed + s_header_len + un_compressed_len_varint, 
                                 compressed + s_header_len + un_compressed_len_varint + compressed_len_varint - 1,
                                 compressed_len);
        free(s->data);
        s->data = compressed;
        s->size = compressed_len + s_header_len + compressed_len_varint + un_compressed_len_varint;
        s->pos = s->size;
    }
    VALUE ret = rb_str_new((char *) s->data,s->size);
    s_destroy(s);
    return ret;
}
