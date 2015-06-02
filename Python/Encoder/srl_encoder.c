/* c-basic-offset: 4;  indent-tabs-mode: nil */
#include <Python.h>
#include <stdint.h>
#include "srl_inline.h"
#include "srl_encoder.h"
#include "srl_protocol.h"
#include "srl_buffer.h"
#include "ptable.h"
#include "snappy/csnappy_compress.c"

SRL_STATIC_INLINE unsigned VARINT_LEN(unsigned long x) { return 1 + (x / (1<<7)); }

SRL_STATIC_INLINE int SRL_ENTER_RECURSIVE_CALL(srl_encoder_t *enc, char *msg);
SRL_STATIC_INLINE void SRL_LEAVE_RECURSIVE_CALL(srl_encoder_t *enc);

SRL_STATIC_INLINE int srl_write_header(srl_encoder_t *enc);

SRL_STATIC_INLINE int srl_dump_long(srl_encoder_t *enc, long n);
SRL_STATIC_INLINE int srl_dump_binary(srl_encoder_t *enc, const char *, Py_ssize_t);

SRL_STATIC_INLINE int srl_dump_pyobj(srl_encoder_t *enc, PyObject *obj);
SRL_STATIC_INLINE int srl_dump_pyint(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);
SRL_STATIC_INLINE int srl_dump_pyfloat(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);
SRL_STATIC_INLINE int srl_dump_pystring(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);
SRL_STATIC_INLINE int srl_dump_pyunicode(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);
SRL_STATIC_INLINE int srl_dump_pylist(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);
SRL_STATIC_INLINE int srl_dump_pydict(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);
SRL_STATIC_INLINE int srl_dump_re(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs);

SRL_STATIC_INLINE int srl_track_obj(srl_encoder_t *enc, PyObject *obj);
SRL_STATIC_INLINE ptrdiff_t srl_find_obj(srl_encoder_t *enc, PyObject *obj);

SRL_STATIC_INLINE int srl_import_re(srl_encoder_t *enc);

const srl_encoder_ctor_args default_encoder_ctor_args = 
{
    SRL_F_SHARED_HASHKEYS, /* Default flags */
    0,                     /* Default max recursion depth */
    1024,                  /* Default snappy threshold */
};

srl_encoder_t *srl_encoder_new(const srl_encoder_ctor_args *args)
{
    srl_encoder_t *enc;

    if (!args)
        args = &default_encoder_ctor_args;

    enc = PyMem_Malloc(sizeof(*enc));
    if (!enc)
        return NULL;

    if (-1 == srl_encoder_ctor(enc, args)) {
        PyMem_Free(enc);
        return (void*)PyErr_NoMemory();
    }

    return enc;
}

void srl_encoder_delete(srl_encoder_t *enc)
{
    assert(enc);

    srl_encoder_dtor(enc);
    PyMem_Free(enc);
}

int srl_encoder_ctor(srl_encoder_t *enc, const srl_encoder_ctor_args *args)
{
    assert(enc);

    enc->buf_start = PyMem_Malloc(INITIAL_BUFFER_SIZE);
    if (!enc->buf_start)
        return -1;

    enc->buf_end = enc->buf_start + INITIAL_BUFFER_SIZE - 1;
    enc->pos = enc->buf_start;

    enc->operational_flags = 0;

    enc->flags = args->flags;
    /* SNAPPY_INCREMENTAL trumps SNAPPY  */
    if ((enc->flags & SRL_F_COMPRESS_SNAPPY) &&
        (enc->flags & SRL_F_COMPRESS_SNAPPY_INCREMENTAL))
        enc->flags &= ~SRL_F_COMPRESS_SNAPPY;

    enc->max_recursion_depth = args->max_recursion_depth;

    enc->recursion_depth = 0;

    enc->obj_seenhash = NULL;
    enc->snappy_workmem = NULL;
    enc->snappy_threshold = args->snappy_threshold;

    memset(&enc->re, 0, sizeof(enc->re));

    return 0;
}

void srl_encoder_dtor(srl_encoder_t *enc)
{
    assert(enc);

    PyMem_Free(enc->buf_start);
    if (enc->obj_seenhash)
        PTABLE_free(enc->obj_seenhash);
    if (enc->snappy_workmem)
        PyMem_Free(enc->snappy_workmem);
    Py_XDECREF(enc->re.module);
    enc->buf_start = enc->buf_end = enc->pos = NULL;
}

PyObject *srl_encoder_dump (srl_encoder_t *enc, PyObject *obj)
{
    assert(enc);

    if (!SRL_ENC_HAVE_OPTION(enc, 
                             SRL_F_COMPRESS_SNAPPY |
                             SRL_F_COMPRESS_SNAPPY_INCREMENTAL))
    {
        if(-1 == srl_write_header(enc))
            return NULL;
        if (-1 == srl_dump_pyobj(enc, obj))
            return NULL;
    } else {
        unsigned long sereal_header_len;
        unsigned long uncompressed_body_len;

        srl_write_header(enc);
        sereal_header_len = enc->pos - enc->buf_start;
        if (-1 == srl_dump_pyobj(enc, obj))
            return NULL;
        assert(BUF_POS_OFS(enc) > sereal_header_len);
        uncompressed_body_len = BUF_POS_OFS(enc) - sereal_header_len;

        if (enc->snappy_threshold > 0
            && uncompressed_body_len < enc->snappy_threshold)
        {
            char *flags_and_version_byte = enc->buf_start + sizeof(SRL_MAGIC_STRING) - 1;
            /* disable snappy flag in header */
            *flags_and_version_byte = SRL_PROTOCOL_ENCODING_RAW |
                                      (*flags_and_version_byte & SRL_PROTOCOL_VERSION_MASK);
        } else {
            /* Snappy or Snappy Incremental

               Snappy: <SRL_HDR><COMPRESSED DATA>
               Snappy Incremental: <SRL_HDR><VARINT><COMPRESSED DATA>
             */
            uint32_t dest_len;
            char *uncompressed;
            char *varint_start;
            char *varint_end;

            varint_start = varint_end = NULL;

            dest_len =
                csnappy_max_compressed_length(uncompressed_body_len)
                + sereal_header_len + 1;

            /*
              We do not know the final compressed size before hand.
              So we allocate SRL_MAX_VARINT_LENGTH and fill in the correct
              value after compressing the dump
            */
            if (SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY_INCREMENTAL))
                dest_len += SRL_MAX_VARINT_LENGTH;

            uncompressed = enc->buf_start;
            enc->buf_start = PyMem_Malloc(dest_len); /*freed by dtor*/
            enc->buf_end = enc->buf_start + dest_len;
            enc->pos = enc->buf_start;

            if (!enc->snappy_workmem)
                enc->snappy_workmem = PyMem_Malloc(CSNAPPY_WORKMEM_BYTES); /*freed by dtor*/

            if (!enc->buf_start || !enc->snappy_workmem) {
                if (uncompressed)
                    PyMem_Free(uncompressed);
                return PyErr_NoMemory();
            }

            memcpy(enc->pos, uncompressed, sereal_header_len);
            enc->pos += sereal_header_len;

            if (SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY_INCREMENTAL)) {
                varint_start = enc->pos;
                srl_buf_cat_varint_nocheck(enc, 0, dest_len);
                varint_end = enc->pos - 1;
            }

            csnappy_compress(
                uncompressed+sereal_header_len, uncompressed_body_len,
                enc->pos, &dest_len,
                enc->snappy_workmem, CSNAPPY_WORKMEM_BYTES_POWER_OF_TWO);

            enc->pos += dest_len;
            PyMem_Free(uncompressed);

            /*
              Fill in the correct length value
             */
            if (varint_start) {
                /* overwrite the max size varint with the real size of the compressed data */
                uint32_t n = dest_len;
                while (n >= 0x80) {                      /* while we are larger than 7 bits long */
                    *varint_start++ = (n & 0x7f) | 0x80; /* write out the least significant 7 bits, set the high bit */
                    n = n >> 7;                          /* shift off the 7 least significant bits */
                }
                /* if it is the same size we can use a canonical varint */
                if ( varint_start == varint_end ) {
                    *varint_start = n;                     /* encode the last 7 bits without the high bit being set */
                } else {
                    /* if not we produce a non-canonical varint, basically we stuff
                     * 0 bits (via 0x80) into the "tail" of the varint, until we can
                     * stick in a null to terminate the sequence. This means that the
                     * varint is effectively "self-padding", and we only need special
                     * logic in the encoder - a decoder will happily process a non-canonical
                     * varint with no problem */
                    *varint_start++ = (n & 0x7f) | 0x80;
                    while ( varint_start < varint_end )
                        *varint_start++ = 0x80;
                    *varint_start= 0;
                }
            }
        }
    }

    return PyString_FromStringAndSize(enc->buf_start, BUF_POS_OFS(enc));
}

int srl_write_header(srl_encoder_t *enc)
{
    /*
      <MAGIC> <VERSION-TYPE> <HEADER-SUFFIX-SIZE> <OPT-SUFFIX>
    */
    /* 4th to 8th bit are flags. Using 4th for snappy flag. FIXME needs to go in spec. */
    const uint8_t version_and_flags = 
        SRL_PROTOCOL_VERSION | (
            SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY)
            ? SRL_PROTOCOL_ENCODING_SNAPPY
            : SRL_ENC_HAVE_OPTION(enc, SRL_F_COMPRESS_SNAPPY_INCREMENTAL)
            ? SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL
            : SRL_PROTOCOL_ENCODING_RAW
            );

    /* 4 byte magic string + proto version
     * + potentially uncompressed size varint
     * +  1 byte varint that indicates zero-length header */
    if (-1 == BUF_SIZE_ASSERT(enc, sizeof(SRL_MAGIC_STRING) + 1 + 1))
        return -1;
    srl_buf_cat_str_s_nocheck(enc, SRL_MAGIC_STRING);
    srl_buf_cat_char_nocheck(enc, version_and_flags);
    srl_buf_cat_char_nocheck(enc, '\0'); /* variable header length (0 right now) */
    return 0;
}

/*
  srl_dump_pyobj delegates to serialize:
    PyString
    PyUnicode
    PyInt
    PyBool
    PyFloat
    PyList
    PyNone
    PyDict
    Regex Patterns from 're' module

  At the moment we do not support structures containing:
      PyLong
      PyComplex
      PyByteArray
      PyTuple       

  Also, we do not use:
    <ALIAS>,<OBJECT>,<OBJECTV>,
    <WEAKEN>,
    <EXTEND>,<PAD>
    
*/
int srl_dump_pyobj(srl_encoder_t *enc, PyObject *obj)
{
    int ret;
    ptrdiff_t offs;

    enum {
        NO_TYPE = 0,
        UNDEF,
        BOOL,INT,FLOAT,
        STRING,UNICODE,
        LIST,
        DICT,
        REGEX
    } type;

    assert(enc);
    assert(obj);

    if (-1 == SRL_ENTER_RECURSIVE_CALL(enc, " while Sereal dumping object")) {
        return -1;
    }

    ret = -1;
    offs = 0;
    type = NO_TYPE;

    /*
      First we do fast exact checks for the base types.
      If those fail then we do slow subclass checks.
     */
    if (PyInt_CheckExact(obj))          type = INT;
    else if (PyFloat_CheckExact(obj))   type = FLOAT;
    else if (PyString_CheckExact(obj))  type = STRING;
    else if (PyUnicode_CheckExact(obj)) type = UNICODE;
    else if (PyList_CheckExact(obj))    type = LIST;
    else if (PyDict_CheckExact(obj))    type = DICT;
    else if (Py_None == obj)            type = UNDEF;
    else {
        if (PyBool_Check(obj))         type = BOOL; /*Bool a subclass of Int*/
        else if (PyInt_Check(obj))     type = INT;  /*so we check it first  */
        else if (PyFloat_Check(obj))   type = FLOAT;
        else if (PyString_Check(obj))  type = STRING;
        else if (PyUnicode_Check(obj)) type = UNICODE;
        else if (PyList_Check(obj))    type = LIST;
        else if (PyDict_Check(obj))    type = DICT;
    }

    if (!type) {
        /* Dirty Hack:
           I don't have any handle to a regex pattern TypeObject
           before importing the 're' module. So to avoid importing
           the module unless absolutely necessary I abuse the fact
           that the pattern type-name is '_sre.SRE_Pattern'.
         */
        char sre_name[] = "_sre.SRE_Pattern";
        if (!strncmp(Py_TYPE(obj)->tp_name, sre_name, sizeof(sre_name)))
            type = REGEX;
    }

    /*
      I COPY track immutable data:
        String,Unicode,Int,Bool,Float(,Tuple - not impl)

      I REFP track mutable data:
        List(,Dictionary,ByteArray - not impl)
     */

    if (type && type != BOOL && type != UNDEF) {
        offs = srl_find_obj(enc, obj);
        if (!offs) {
            if (-1 == srl_track_obj(enc, obj))
                goto finally;
        }
    }

    switch (type) {
        case INT:
            if (-1 == srl_dump_pyint(enc, obj, offs))
                goto finally;
            break;
        case BOOL:
            if (-1 == srl_buf_cat_char(enc, 
                                       obj == Py_True
                                       ? SRL_HDR_TRUE 
                                       : SRL_HDR_FALSE))
                goto finally;
            break;
        case UNDEF:
            if (-1 == srl_buf_cat_char(enc, SRL_HDR_UNDEF))
                goto finally;
            break;
        case STRING:
            if (-1 == srl_dump_pystring(enc, obj, offs))
                goto finally;
            break;
        case UNICODE:
            if (-1 == srl_dump_pyunicode(enc, obj, offs))
                goto finally;
            break;
        case FLOAT:
            if (-1 == srl_dump_pyfloat(enc, obj, offs))
                goto finally;
            break;
        case LIST:
            if (-1 == srl_dump_pylist(enc, obj, offs))
                goto finally;
            break;
        case DICT:
            if (-1 == srl_dump_pydict(enc, obj, offs))
                goto finally;
            break;
        case REGEX:
            if (-1 == srl_dump_re(enc, obj, offs))
                goto finally;
            break;
        default:
            PyErr_Format(PyExc_NotImplementedError,
                         "srl_dump_pyobjc: %s dumping not implemented yet",
                         Py_TYPE(obj)->tp_name);
            goto finally;
    }

    ret = 0;
finally:
    SRL_LEAVE_RECURSIVE_CALL(enc);
    return ret;
}

SRL_STATIC_INLINE
int srl_dump_pyint(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    long l;

    assert(enc);
    assert(obj);
    assert(PyInt_Check(obj));

    l = PyInt_AS_LONG(obj);
    if (offs) {
        /*  Only <COPY> if it saves space,
            so we store the shorter <VARINT>
         */
        unsigned long zz;

        if (l > 0) {  /* varint */
            /* each byte of VARINT encodes 7-bits 
               this test has false-negatives,
               we can miss a <COPY> that saves 1-byte
             */
            if (VARINT_LEN(l) > VARINT_LEN(offs))
                return srl_buf_cat_varint(enc, SRL_HDR_COPY, offs);
        } else { /* zigzag */
            zz = (l << 1) ^ (l >> (sizeof(long) * 8 - 1));
            if (VARINT_LEN(zz) > VARINT_LEN(offs))
                return srl_buf_cat_varint(enc, SRL_HDR_COPY, offs);
        }
    }
    return srl_dump_long(enc, l);
}

SRL_STATIC_INLINE
int srl_dump_long(srl_encoder_t *enc, long n)
{
    assert(enc);

    if (n >= 0)
        return 
            n < 16 
            ? srl_buf_cat_char(enc, SRL_HDR_POS_LOW | (char)n)
            : srl_buf_cat_varint(enc, SRL_HDR_VARINT, n);
    else 
        return 
            n > -17
            ? srl_buf_cat_char(enc, SRL_HDR_NEG_LOW | (char)(n + 32))
            : srl_buf_cat_zigzag(enc, SRL_HDR_ZIGZAG, n);
}

SRL_STATIC_INLINE
int srl_dump_pyfloat(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    assert(enc);
    assert(obj);
    assert(PyFloat_Check(obj));

    if (offs && VARINT_LEN(offs) < sizeof(double))
        return srl_buf_cat_varint(enc, SRL_HDR_COPY, offs);

    return srl_buf_cat_double(enc, SRL_HDR_DOUBLE, PyFloat_AS_DOUBLE(obj));
}

SRL_STATIC_INLINE
int srl_dump_pystring(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    char *p;
    Py_ssize_t n;

    assert(enc);
    assert(obj);
    assert(PyString_Check(objc));

    if (-1 == PyString_AsStringAndSize(obj, &p, &n))
        return -1;

    if (offs && VARINT_LEN(offs) < (unsigned)n)
        return srl_buf_cat_varint(enc, SRL_HDR_COPY, offs);

    return srl_dump_binary(enc, p, n);
}

SRL_STATIC_INLINE
int srl_dump_pyunicode(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    PyObject *utf8;
    char *p;
    Py_ssize_t n;
    int ret;

    assert(enc);
    assert(obj);
    assert(PyUnicode_Check(obj));

    ret = -1;
    utf8 = PyUnicode_AsUTF8String(obj);
    if (!utf8)
        goto finally;

    if (-1 == PyString_AsStringAndSize(utf8, &p, &n))
        goto finally;

    assert(p);
    assert(n >= 0);

    if (offs && VARINT_LEN(offs) < (unsigned)n) {
        ret = srl_buf_cat_varint(enc, SRL_HDR_COPY, offs);
        goto finally;
    }

    /* overallocate a bit sometimes */
    if (-1 == BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH + n))
        goto finally;

    srl_buf_cat_varint_nocheck(enc, SRL_HDR_STR_UTF8, n);
    srl_buf_cat_str_nocheck(enc, p, n);

    ret = 0;
finally:
    Py_XDECREF(utf8);
    return ret;
}


SRL_STATIC_INLINE 
int srl_dump_binary(srl_encoder_t *enc, const char *p, Py_ssize_t n)
{
    assert(enc);
    assert(p);
    assert(n >= 0);

    /* overallocate a bit sometimes */
    if (-1 == BUF_SIZE_ASSERT(enc, 1 + SRL_MAX_VARINT_LENGTH + n))
        return -1;

    if (n <= SRL_MASK_SHORT_BINARY_LEN)
        srl_buf_cat_char_nocheck(enc, SRL_HDR_SHORT_BINARY_LOW | (char) n);
    else
        srl_buf_cat_varint_nocheck(enc, SRL_HDR_BINARY, n);

    srl_buf_cat_str_nocheck(enc, p, n);
    return 0;
}

SRL_STATIC_INLINE
int srl_dump_pylist(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    Py_ssize_t len;
    int ret;

    assert(enc);
    assert(obj);
    assert(PyList_Check(obj));

    if (-1 == SRL_ENTER_RECURSIVE_CALL(enc, " serializing list"))
        return -1;

    ret = -1;

    len = PyList_GET_SIZE(obj);
    assert(len >= 0);
    if (offs) {
        /* <REFP> to <ARRAYREF_N> 
           or
           <REFP> one past <REFN><ARRAY>
        */
        if (len > SRL_MASK_ARRAYREF_COUNT) 
            offs++;
        SRL_SET_TRACK_FLAG(*(enc->buf_start + offs));
        ret = srl_buf_cat_varint(enc, SRL_HDR_REFP, offs);
        goto finally;
    }

    if (len <= SRL_MASK_ARRAYREF_COUNT) {
        /* <ARRAYREF_N> */
        if (-1 == srl_buf_cat_char(enc, SRL_HDR_ARRAYREF_LOW+(char)len))
            goto finally;
    } else {
        /* <REFN><ARRAY><COUNT-VARINT> */
        if (-1 == BUF_SIZE_ASSERT(enc, 1+1+SRL_MAX_VARINT_LENGTH))
            goto finally;
        srl_buf_cat_char_nocheck(enc, SRL_HDR_REFN);
        srl_buf_cat_varint_nocheck(enc, SRL_HDR_ARRAY, len);
    }
    /* [<ITEM-TAG> ... ] */
    {
        int i;
        for (i = 0; i < len; i++)
            if (-1 == srl_dump_pyobj(enc, PyList_GET_ITEM(obj, i)))
                goto finally;
    }

    ret = 0;
finally:
    SRL_LEAVE_RECURSIVE_CALL(enc);
    return ret;
}

SRL_STATIC_INLINE
int srl_dump_pydict(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    Py_ssize_t n;
    int ret;

    assert(enc);
    assert(obj);
    assert(PyDict_Check(obj));

    if (-1 == SRL_ENTER_RECURSIVE_CALL(enc, " serializing dict"))
        return -1;

    ret = -1;

    n = PyDict_Size(obj);
    assert(n > 0);

    if (offs) {
        if (n > SRL_MASK_HASHREF_COUNT)
            offs++;
        SRL_SET_TRACK_FLAG(*(enc->buf_start + offs));
        ret = srl_buf_cat_varint(enc, SRL_HDR_REFP, offs);
        goto finally;
    }

    /*
      Heuristic: <REFN><HASH><COUNT_VARINT>[<KEY><VAL> ..]
    */
    if (-1 == BUF_SIZE_ASSERT(enc, 2+SRL_MAX_VARINT_LENGTH+2*n))
        goto finally;

    if (n <= SRL_MASK_HASHREF_COUNT) {
        /* <HASHREF_N> */
        srl_buf_cat_char_nocheck(enc, SRL_HDR_HASHREF_LOW | (char)n);
    } else {
        /* <REFN><HASH><COUNT_VARINT> */
        srl_buf_cat_char_nocheck(enc, SRL_HDR_REFN);
        srl_buf_cat_varint_nocheck(enc, SRL_HDR_HASH, n);
    }
    /* [<KEY><VAL> ... ] */
    {
        PyObject *key, *val;
        Py_ssize_t pos;

        pos = 0;
        while (PyDict_Next(obj, &pos, &key, &val)) {
            ptrdiff_t key_offs;

            key_offs = 0;

            if (SRL_ENC_HAVE_OPTION(enc, SRL_F_SHARED_HASHKEYS)) {
                key_offs = srl_find_obj(enc, key);
                if (!pos) {
                    if (-1 == srl_track_obj(enc, key))
                        goto finally;
                }
            }

            if (PyString_Check(key)) {
                if (-1 == srl_dump_pystring(enc, key, key_offs))
                    goto finally;
            } else if (PyUnicode_Check(key)) {
                if (-1 == srl_dump_pyunicode(enc, key, key_offs))
                    goto finally;
            } else {
                PyErr_SetString(PyExc_RuntimeError,
                                "encoder error: using non-string as hash key");
                goto finally;
            }

            if (-1 == srl_dump_pyobj(enc, val))
                goto finally;
        }
    }

    ret = 0;
finally:
    SRL_LEAVE_RECURSIVE_CALL(enc);
    return ret;
}

SRL_STATIC_INLINE
int srl_dump_re(srl_encoder_t *enc, PyObject *obj, ptrdiff_t offs)
{
    /* <SRL_HDR_REGEXP> <PATTERN-STR-TAG> <MODIFIERS-STR-TAG> */
    PyObject *pypattern;
    PyObject *pyflags;
    char *p, modifiers[7];
    long flags;
    int ret;

    assert(enc);
    assert(obj);

    pypattern = pyflags = NULL;
    ret = -1;

    if (offs) {
        ret = srl_buf_cat_varint(enc, SRL_HDR_COPY, offs);
        goto finally;
    }

    if (-1 == srl_import_re(enc))
        goto finally;

    pypattern = PyObject_GetAttrString(obj, "pattern");
    pyflags = PyObject_GetAttrString(obj, "flags");

    if (!pypattern && !pyflags)
        goto finally;

    flags = PyInt_AS_LONG(pyflags);

    p = &modifiers[0];
    if (flags & enc->re.IGNORECASE) *p++ = 'i';
    if (flags & enc->re.LOCALE)     *p++ = 'l';
    if (flags & enc->re.MULTILINE)  *p++ = 'm';
    if (flags & enc->re.DOTALL)     *p++ = 's';
    if (flags & enc->re.UNICODE)    *p++ = 'u';
    if (flags & enc->re.VERBOSE)    *p++ = 'x';
    *p = '\0';

    srl_buf_cat_char(enc, SRL_HDR_REGEXP);
    srl_dump_pystring(enc, pypattern, 0);
    srl_dump_binary(enc, modifiers, p - modifiers);

    ret = 0;
finally:
    Py_XDECREF(pypattern);
    Py_XDECREF(pyflags);
    return ret;
}

SRL_STATIC_INLINE
int srl_track_obj(srl_encoder_t *enc, PyObject *obj)
{
    if (!enc->obj_seenhash)
        /* magic const from Perl module */
        if (!(enc->obj_seenhash = PTABLE_new_size(4)))
            return -1;
    return PTABLE_store(enc->obj_seenhash, obj, (void *)(enc->pos-enc->buf_start));
}

SRL_STATIC_INLINE
ptrdiff_t srl_find_obj(srl_encoder_t *enc, PyObject *obj)
{
    if (!enc->obj_seenhash)
        return 0;
    return (ptrdiff_t)PTABLE_fetch(enc->obj_seenhash, obj);
}

SRL_STATIC_INLINE
int SRL_ENTER_RECURSIVE_CALL(srl_encoder_t *enc, char *msg)
{
    assert(enc);
    assert(msg);

    if (-1 == Py_EnterRecursiveCall(msg))
        return -1;

    enc->recursion_depth++;
    if (enc->max_recursion_depth && 
        enc->recursion_depth >= enc->max_recursion_depth) {
        PyErr_Format(PyExc_RuntimeError, 
                     "Hit maximum recursion depth (%lu),"
                     "aborting serialization",
                     (unsigned long)enc->max_recursion_depth);
        return -1;
    }

    return 0;
}

SRL_STATIC_INLINE
void SRL_LEAVE_RECURSIVE_CALL(srl_encoder_t *enc)
{
    assert(enc);

    Py_LeaveRecursiveCall();
    enc->recursion_depth--;
}

SRL_STATIC_INLINE int srl_import_re(srl_encoder_t *enc)
{
    if (!enc->re.module) {
        PyObject *m, *d;

        m = PyImport_ImportModule("re");
        if (!m)
            return -1;
        d = PyModule_GetDict(m);

        enc->re.module = m;
#define GETFLAG(name) PyInt_AS_LONG(PyDict_GetItemString(d, name))
        enc->re.DOTALL     = GETFLAG("DOTALL");
        enc->re.IGNORECASE = GETFLAG("IGNORECASE");
        enc->re.LOCALE     = GETFLAG("LOCALE");
        enc->re.MULTILINE  = GETFLAG("MULTILINE");
        enc->re.UNICODE    = GETFLAG("UNICODE");
        enc->re.VERBOSE    = GETFLAG("VERBOSE");
#undef GETFLAG
    }

    return 0;
}
