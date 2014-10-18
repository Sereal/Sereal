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
    z = (v >> 1) ^ -(v & 0x01);
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
    return DBL2NUM((double) s_get_long_double_bang(s));
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
#define RETURN_STRING(fx_l,fx_gen)              \
    do {                                        \
        len       = (u32) fx_l;                 \
        char *ptr = (char *) (len == 0 ? 0 : s_get_p_req_inclusive(s,len)); \
        string    = fx_gen;                     \
        s_shift_position_bang(s,len);           \
        return string;                          \
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
    return Qnil;
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
    return Qnil;
}

static VALUE s_read_ref(sereal_t *s, u8 tag) {
    if (s->tracked == Qnil)
        s_raise(s,rb_eArgError,"there are no references stored");
    u64 off = s_get_varint_bang(s);
    VALUE object = rb_hash_lookup(s->tracked,INT2FIX(off + s->hdr_end));
    SD(s,"reading reference from offset: %" PRIu64 ", id: %d",off + s->hdr_end,FIX2INT(rb_obj_id(object)));
    return object;
}

#define TRAVEL(s,__stored)                                              \
    do {                                                                \
        u32 offset = ((u32) s_get_varint_bang(s)) - 1;                  \
        __stored = s->pos;                                              \
        s->pos = offset + s->hdr_end;                                   \
        SD(s,"going back offset: %d, stored position: %d (tag: %d) new pos: %d",offset,stored_pos,tag,s->pos); \
    } while(0)
#define BACK(s,__stored)                                \
    do {                                                \
        if (__stored > 0) {                             \
            SD(s,"going forward to pos: %d",__stored);  \
            s->pos = __stored;                          \
        }                                               \
    } while(0);


static VALUE s_read_copy(sereal_t *s, u8 tag) {
    u32 stored_pos = 0;
    TRAVEL(s,stored_pos);
    VALUE object = sereal_to_rb_object(s);
    BACK(s,stored_pos);
    return object;
}


#define MUST_BE_SOMETHING(__klass,__type)                               \
    if (TYPE(__klass) != __type)                                        \
        s_raise(s,rb_eTypeError,"unexpected object type: %s (expecting: %s(%d) got: %s(%d))",rb_obj_classname(__klass),(__type == T_STRING ? "String" : (__type == T_ARRAY ? "Array" : "_unknown_")),__type, rb_obj_classname(__klass),TYPE(__klass));

static VALUE s_read_perl_object(sereal_t *s, u8 tag) {
    u32 stored_pos = 0;
    if (tag == SRL_HDR_OBJECTV)
        TRAVEL(s,stored_pos);

    VALUE s_klass = sereal_to_rb_object(s);
    BACK(s,stored_pos);
    MUST_BE_SOMETHING(s_klass,T_STRING);

    SD(s,"fetched perl class named: %s",RSTRING_PTR(s_klass));

    VALUE object = sereal_to_rb_object(s);

    VALUE pobj =  rb_class_new_instance(0,NULL,SerealPerlObject);
    rb_ivar_set(pobj,ID_CLASS,s_klass);
    rb_ivar_set(pobj,ID_VALUE,object);
    return pobj;
}

static VALUE s_read_object_freeze(sereal_t *s, u8 tag) {
    if (!(s->flags & __THAW))
        s_raise(s,rb_eTypeError,"object_freeze received, but decoder is initialized without Sereal::THAW option");

    u32 stored_pos = 0;
    if (tag == SRL_HDR_OBJECTV_FREEZE) {
        TRAVEL(s,stored_pos);
    }
    VALUE s_klass = sereal_to_rb_object(s);
    BACK(s,stored_pos);
    MUST_BE_SOMETHING(s_klass,T_STRING);

    // hash it?
    VALUE klass = rb_path_to_class(s_klass);
    if (!rb_obj_respond_to(klass,THAW,0))
        s_raise(s,rb_eTypeError,"class: %s does not respond to THAW",
                rb_obj_classname(s_klass));

    VALUE object = sereal_to_rb_object(s);
    MUST_BE_SOMETHING(object,T_ARRAY);
    rb_ary_unshift(object,ID2SYM(SEREAL));

    return rb_funcall2(klass,THAW,RARRAY_LEN(object),RARRAY_PTR(object));
}
#undef TRAVEL
#undef BACK

VALUE sereal_to_rb_object(sereal_t *s) {
    u8 t, tracked;
    S_RECURSE_INC(s);
    u32 pos;
    while (s->pos < s->size || (s->flags & __STREAM)) {
        t = s_get_u8_bang(s);
        tracked = (t & SRL_HDR_TRACK_FLAG ? 1 : 0);
        t &= ~SRL_HDR_TRACK_FLAG;

        pos = s->pos;

        VALUE decoded = (*READERS[t])(s,t);

        if (unlikely(tracked)) {
            s_init_tracker(s);
            SD(s,"tracking object of class: %s(id: %d) at position: %d",rb_obj_classname(decoded),FIX2INT(rb_obj_id(decoded)),pos);
            VALUE v_pos = INT2FIX(pos);
            if (rb_hash_lookup(s->tracked,v_pos) == Qnil)
                rb_hash_aset(s->tracked,INT2FIX(pos),decoded);
        }

        SD(s,"object: %s: %s",rb_obj_classname(decoded),RSTRING_PTR(rb_funcall(decoded,rb_intern("to_s"),0)));
        S_RECURSE_DEC(s);
        return decoded;
    }
    s_raise(s,rb_eArgError,"bad packet, or broken decoder");
    return Qnil;
}

VALUE method_sereal_decode(VALUE self, VALUE args) {
    u32 argc = RARRAY_LEN(args);
    if (argc < 1)
        rb_raise(rb_eArgError,"need at least 1 argument (object)");
    VALUE payload = rb_ary_entry(args,0);

    u8 have_block = rb_block_given_p();
    sereal_t *s = s_create();
    if (argc == 2) {
        VALUE flags = rb_ary_entry(args,1);
        if (flags != Qnil && flags != Qfalse) {
            if (TYPE(flags) == T_FIXNUM)
                s->flags = FIX2LONG(flags) & __ARGUMENT_FLAGS;
            else
                s_raise(s,rb_eArgError,"second argument must be an integer (used only for flags) %s given",rb_obj_classname(flags));
        }
    }
    u64 offset = 0;

    if (TYPE(payload) == T_FILE) {
        if (!have_block)
            s_raise(s,rb_eTypeError,"block is required when reading from a stream")

        rb_io_t *fptr;
        GetOpenFile(payload, fptr);
        s->flags |= __STREAM;
        s->fd = fptr->fd;
        SD(s,"reading strea with fd: %d",s->fd);
    } else if (TYPE(payload) != T_STRING) {
        rb_raise(rb_eTypeError,"can not decode objects of type %s",rb_obj_classname(payload));
    }

again: 
    s->pos = 0;
    s_reset_tracker(s);

    if (s->flags & __STREAM) {
        s->size = 0;
        s->rsize = 0;
        if (s_read_stream(s,__MIN_SIZE) < 0) {
            s_destroy(s);
            return Qnil;
        }
    } else {
        u32 size = RSTRING_LEN(payload) - offset;
        if ((RSTRING_LEN(payload) > 0 && offset > (u32) RSTRING_LEN(payload)) || (offset > 0 && size < __MIN_SIZE)) {
            s_destroy(s);
            return Qnil;
        }
        if (size < __MIN_SIZE)
            s_raise(s,rb_eTypeError,"size(%d) is less then min packet size %d, offset: %" PRIu64,size,__MIN_SIZE,offset);

        s->flags |= __NOT_MINE;
        s->data = (u8 *) RSTRING_PTR(payload) + offset;
        s->size = size;
    }

    u32 magic = s_get_u32_bang(s);
    if (magic != SRL_MAGIC_STRING_UINT_LE && magic != SRL_MAGIC_STRING_UINT_LE_HB)
        s_raise(s,rb_eTypeError,"invalid header: %d (%x) vs %x",magic,magic,SRL_MAGIC_STRING_UINT_LE);

    u8 version = s_get_u8_bang(s);
    u8 suffix = s_get_varint_bang(s);
    if (suffix > 0)
        s_raise(s,rb_eTypeError,"HEADER-SUFFIX-SIZE is not supported yet");

    u8 is_compressed;

    if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY) {
        is_compressed = __SNAPPY;
    } else if ((version & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_SNAPPY_INCR) {
        is_compressed = __SNAPPY_INCR;
    } else {
        is_compressed = __RAW;
    }

    SD(s,"initialized (s) with compression type: %d",is_compressed);

    if (is_compressed) {
        u32 uncompressed_len;
        u32 compressed_len;

        if (is_compressed == __SNAPPY_INCR) {
            compressed_len = s_get_varint_bang(s);
        } else {
            if (s->flags & __STREAM)
                s_raise(s,rb_eTypeError,"parsing non incremental compressed objects, from stream of data, is not supported");

            compressed_len = s->size - s->pos;
        }
        SD(s,"compressed len: %d",compressed_len);
        if (s->flags & __STREAM)
            s_get_p_req_inclusive(s,compressed_len);

        int snappy_header_len = csnappy_get_uncompressed_length(s_get_p_req_inclusive(s,compressed_len),
                                                                compressed_len,
                                                                &uncompressed_len);
        if (snappy_header_len == CSNAPPY_E_HEADER_BAD)
            s_raise(s,rb_eTypeError,"invalid snappy header");

        u8 *uncompressed = s_alloc_or_raise(s,uncompressed_len);
        int done = csnappy_decompress(s_get_p_req_inclusive(s,compressed_len),
                                      compressed_len,
                                      (char *) uncompressed,
                                      uncompressed_len) == CSNAPPY_E_OK ? 1 : 0;
        if (!done)
            s_raise(s,rb_eTypeError, "decompression failed error: %d type: %d, unompressed size: %d compressed size: %d",
                                      done,is_compressed,uncompressed_len,compressed_len);

        s_free_data_if_not_mine(s);
        s->data = uncompressed;
        s->size = uncompressed_len;
        offset += s->pos + compressed_len;
        s->pos = 0;
        s->flags &= ~__NOT_MINE;
    }

    s->hdr_end = s->pos;
    SD(s,"header end at %d",s->hdr_end);
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

