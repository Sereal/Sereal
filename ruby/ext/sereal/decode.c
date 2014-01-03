#include "sereal.h"
#include "buffer.h"
#include "decode.h"
#include "snappy/csnappy_decompress.c"

static VALUE s_default_reader(sereal_t *s, u8 tag) {
    s_raise(s,rb_eTypeError,"unsupported tag %d [ 0x%x ]",tag,tag);
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
            s_raise(s,rb_eTypeError, "varint too big");
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
    return DBL2NUM(s_get_float_bang(s));
}

/* DOUBLE */
static VALUE s_read_double(sereal_t *s, u8 tag) {
    return DBL2NUM(s_get_double_bang(s));
}

/* LONG DOUBLE */
static VALUE s_read_long_double(sereal_t *s, u8 tag) {
    return DBL2NUM((double) s_get_long_double(s));
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
// len - 1: we also use the current byte, similar to u32,float.. casts
#define RETURN_STRING(fx_l,fx_gen)                             \
    do {                                                       \
        len = fx_l;                                            \
        u8 *ptr = len == 0 ? 0 : s_get_p_req_inclusive(s,len); \
        string = fx_gen;                                       \
        s_shift_position_bang(s,len);                          \
        return string;                                         \
    } while(0);

    if (t == SRL_HDR_STR_UTF8) {
        RETURN_STRING(s_get_varint_bang(s),
                  rb_enc_str_new(ptr,len,
                  rb_utf8_encoding()));
    } else if (t == SRL_HDR_BINARY || t == SRL_HDR_SYM) {
        RETURN_STRING(s_get_varint_bang(s),
                  rb_str_new(ptr,len));
    } else if (IS_SHORT_BINARY(t)) {
        RETURN_STRING((t & SRL_MASK_SHORT_BINARY_LEN),
                  rb_str_new(ptr, len));
    }
#undef RETURN_STRING
    s_raise(s,rb_eTypeError, "undefined string type %d",t);
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
    s_raise(s,rb_eArgError,"extend tags are not supported");
}

static VALUE s_read_ref(sereal_t *s, u8 tag) {
    u64 off = s_get_varint_bang(s);
    if (s->tracked == Qnil)
        s_raise(s,rb_eArgError,"there are no references stored");
    return rb_hash_aref(s->tracked,INT2FIX(off + s->hdr_end));
}

static VALUE s_read_copy(sereal_t *s, u8 tag) {
    VALUE ref = s_red_ref(s,tag);
    return rb_obj_dup(ref);
}

VALUE sereal_to_rb_object(sereal_t *s) {
    u8 t, tracked;
    S_RECURSE_INC(s);
    u32 pos;
    while (s->pos < s->size || (s->flags & FLAG_STREAM)) {
        t = s_get_u8_bang(s);
        tracked = (t & SRL_HDR_TRACK_FLAG ? 1 : 0);
        t &= ~SRL_HDR_TRACK_FLAG;
        pos = s->pos;

        S_RECURSE_DEC(s);

        VALUE decoded = (*READERS[t])(s,t);
        if (tracked) {
            s_init_tracker(s);
            rb_hash_aset(s->tracked,INT2FIX(pos),decoded);
        }
        return decoded;
    }
    s_raise(s,rb_eArgError,"bad packet, or broken decoder");
    return Qnil;
}

VALUE method_sereal_decode(VALUE self, VALUE args) {
    u32 argc = RARRAY_LEN(args);
    if (argc < 1)
        rb_raise(rb_eArgError,"need at least 1 argument (object)");
    VALUE payload = rb_ary_shift(args);
    u8 have_block = rb_block_given_p();
    sereal_t *s = s_create();
    u64 offset = 0;

    if (TYPE(payload) == T_FILE) {
        if (!have_block)
            s_raise(s,rb_eTypeError,"block is required when reading from a stream")

        rb_io_t *fptr;
        GetOpenFile(payload, fptr);
        s->flags |= FLAG_STREAM;
        s->fd = fptr->fd;
    } else if (TYPE(payload) != T_STRING) {
        rb_raise(rb_eTypeError,"can not decode objects of type %s",rb_obj_classname(payload));
    }

again: 
    s->pos = 0;
    s_reset_tracker(s);

    if (s->flags & FLAG_STREAM) {
        s->size = 0;
        s->rsize = 0;
        if (s_read_stream(s,__MIN_SIZE) < 0) {
            s_destroy(s);
            return Qnil;
        }

    } else {
        u32 size = RSTRING_LEN(payload) - offset;
        if (offset > RSTRING_LEN(payload) || (offset > 0 && size < __MIN_SIZE)) {
            s_destroy(s);
            return Qnil;
        }
        if (size < __MIN_SIZE)
            s_raise(s,rb_eTypeError,"size(%d) is less then min packet size %d, offset: %d",size,__MIN_SIZE,offset);

        s->flags |= FLAG_NOT_MINE;
        s->data = RSTRING_PTR(payload) + offset;
        s->size = size;
    }

    u32 magic = s_get_u32_bang(s);
    if (magic != SRL_MAGIC_STRING_LILIPUTIAN)
        s_raise(s,rb_eTypeError,"invalid header: %d (%x)",magic,magic);

    u8 version = s_get_u8_bang(s);
    u8 suffix = s_get_varint_bang(s);
    u8 is_compressed;

    if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY)
        is_compressed = __SNAPPY;
    else if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY_INCR)
        is_compressed = __SNAPPY_INCR;
    else
        is_compressed = __RAW;

    if (is_compressed) {
        u32 uncompressed_len;
        u32 compressed_len;

        if (is_compressed == __SNAPPY_INCR) {
            compressed_len = s_get_varint_bang(s);
        } else {
            if (s->flags & FLAG_STREAM)
                s_raise(s,rb_eTypeError,"parsing non incremental compressed objects, from stream of data, is not supported");

            compressed_len = s->size - s->pos;
        }

        if (s->flags & FLAG_STREAM)
            s_get_p_req_inclusive(s,compressed_len);

        int snappy_header_len = csnappy_get_uncompressed_length(s_get_p_req_inclusive(s,compressed_len),
                                                                compressed_len,
                                                                &uncompressed_len);
        if (snappy_header_len == CSNAPPY_E_HEADER_BAD)
            s_raise(s,rb_eTypeError,"invalid snappy header");

        u8 *uncompressed = s_alloc_or_raise(s,uncompressed_len);
        int done = csnappy_decompress(s_get_p_req_inclusive(s,compressed_len),
                                      compressed_len,
                                      uncompressed,
                                      uncompressed_len) == CSNAPPY_E_OK ? 1 : 0;
        if (!done)
            s_raise(s,rb_eTypeError, "decompression failed error: %d type: %d, unompressed size: %d compressed size: %d",
                                      done,is_compressed,uncompressed_len,compressed_len);

        s_free_data_if_not_mine(s);
        s->data = uncompressed;
        s->size = uncompressed_len;
        offset += s->pos + compressed_len;
        s->pos = 0;
        s->flags &= ~FLAG_NOT_MINE;
    }
    s->hdr_end = s->pos;
    VALUE result = sereal_to_rb_object(s);
    if (!is_compressed)
        offset += s->pos;
    if (have_block) {
        rb_yield(result);
        s_free_data_if_not_mine(s);
        goto again;
    } else {
        if (s->pos < s->size) {
            s_raise(s,rb_eTypeError,"there is still some data left in the buffer, use block read, otherwise we are losing it");
        }
        s_destroy(s);
        return result;
    }
}

