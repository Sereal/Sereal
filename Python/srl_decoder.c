#include "srl_decoder.h"
#include "srl_inline.h"
#include "srl_common.h"
#include "srl_protocol.h"
#include <stdlib.h>
#include <Python.h>
#include <stdbool.h>
#include <stddef.h>

#include "snappy/csnappy_decompress.c"

#if PY_MAJOR_VERSION >= 3
#define PyInt_FromLong PyLong_FromLong
#define PyInt_Check PyLong_Check
#define PyInt_AsLong PyLong_AsLong
#define PyIntObject PyLongObject
#endif

/* Error handling */
#define MUST(x) do { if(!(x)) { return 0; } } while(0)
#define MUSTg(x) do { if(!(x)) { goto out_err; } } while(0)
#define ASSERT_BUF_SPACE(dec,len,msg) do {  \
    if (expect_false( BUF_SPACE((dec)) < (len) )) { \
        ERRORf3("Unexpected termination of packet%s, want %d bytes, only have %d available", (msg), (len), BUF_SPACE((dec)));  \
    } \
} while(0)
#define IS_SRL_HDR_ARRAYREF(tag) (((tag) & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF)
#define IS_SRL_HDR_HASHREF(tag) (((tag) & SRL_HDR_HASHREF) == SRL_HDR_HASHREF)
#define IS_SRL_HDR_SHORT_BINARY(tag) (((tag) & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW)
#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)

/* All our internal definitions */
static SRL_INLINE bool srl_read_header(srl_decoder_t *dec);                    /* Validate header */
static SRL_INLINE bool srl_decompress(srl_decoder_t *dec);                     /* Decompress snappy-compressed document */
static SRL_INLINE PyObject *srl_read_single_value(srl_decoder_t *dec);

static SRL_INLINE bool srl_read_varint_length(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr);
static SRL_INLINE bool srl_read_varint_internal(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr);
static SRL_INLINE bool srl_read_varint_safe(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr);
static SRL_INLINE bool srl_read_varint_nocheck(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr);

static SRL_INLINE PyObject *srl_read_array(srl_decoder_t *dec, char tag);
static SRL_INLINE PyObject *srl_read_hash(srl_decoder_t *dec, char tag);
static SRL_INLINE PyObject *srl_read_varint(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_zigzag(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_float(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_double(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_long_double(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_string(srl_decoder_t *dec, bool decode);
static SRL_INLINE PyObject *srl_read_weaken(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_refn(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_refp(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_object(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_objectv(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_copy(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_extend(srl_decoder_t *dec);
static SRL_INLINE PyObject *srl_read_regexp(srl_decoder_t *dec);

/* The semi-public API */
srl_decoder_t *srl_decoder_new() {
    srl_decoder_t *dec = (srl_decoder_t *)calloc(1,sizeof(srl_decoder_t));
    return dec;
}

void srl_decoder_free(srl_decoder_t *dec) {
    if(dec->uncompressed_data)
        free(dec->uncompressed_data);
    free(dec);
}

PyObject *srl_decode(srl_decoder_t *dec) {
    dec->buf_end = dec->buf_start + dec->buf_len;
    dec->pos = dec->buf_start;
    dec->bytes_consumed = 0;

    MUST(srl_read_header(dec));
    if (SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_DECOMPRESS_SNAPPY)) {
        MUST(srl_decompress(dec));
    }

    return srl_read_single_value(dec);
}

static SRL_INLINE bool srl_read_header(srl_decoder_t *dec) {
    Py_ssize_t header_len;
    unsigned char proto_version_and_flags;

    /* 4 byte magic string + version/flags + hdr len at least */
    ASSERT_BUF_SPACE(dec, 4 + 1 + 1," while reading header");
    if (strncmp((char*)dec->pos, SRL_MAGIC_STRING, 4)) {
        ERROR("bad header");
    }
    dec->pos += 4;
    proto_version_and_flags = *dec->pos++;

    if (expect_false( (proto_version_and_flags & SRL_PROTOCOL_VERSION_MASK) != 1 ))
        ERRORf1("Unsupported Sereal protocol version %u", proto_version_and_flags & SRL_PROTOCOL_VERSION_MASK);

    if ((proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK) == SRL_PROTOCOL_ENCODING_RAW) {
        /* no op */
    }
    else
    if (( proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) == SRL_PROTOCOL_ENCODING_SNAPPY) {
        dec->flags |= SRL_F_DECODER_DECOMPRESS_SNAPPY;
        if (expect_false( dec->flags & SRL_F_DECODER_REFUSE_SNAPPY) ) {
            ERROR("Sereal document is compressed with Snappy, "
                  "but this decoder is configured to refuse Snappy-compressed input.");
        }
    }
    else
    {
        ERRORf1("Serial document encoded in an unknown format '%d'", ( proto_version_and_flags & SRL_PROTOCOL_ENCODING_MASK ) >> 4 );
    }
    MUST(srl_read_varint_length(dec, &header_len, " while reading header"));
    dec->pos += header_len;
    return 1;
}

static SRL_INLINE bool srl_read_varint_length(srl_decoder_t *dec, Py_ssize_t *header_len, const char * const errstr) {
    MUST(srl_read_varint_internal(dec, header_len, errstr));
    ASSERT_BUF_SPACE(dec, *header_len, errstr);
    return 1;
}

static SRL_INLINE bool srl_decompress(srl_decoder_t *dec) {
    uint32_t dest_len;
    unsigned char *old_pos;

    const ptrdiff_t compressed_packet_len = dec->buf_end - dec->pos;
    const ptrdiff_t sereal_header_len = dec->pos - dec->buf_start;
    int decompress_ok;

    int header_len = csnappy_get_uncompressed_length(
                        (char *)dec->pos,
                        dec->buf_end - dec->pos,
                        &dest_len
                     );
    if (header_len == CSNAPPY_E_HEADER_BAD)
        ERROR("Invalid Snappy header in Snappy-compressed Sereal packet");

    dec->uncompressed_data = calloc(1, sereal_header_len + dest_len + 1);

    /* FIXME probably unnecessary to copy the Sereal header! */
    memcpy(dec->uncompressed_data, dec->buf_start, sereal_header_len);

    old_pos = dec->pos;
    dec->buf_start = dec->uncompressed_data;;
    dec->pos = dec->buf_start + sereal_header_len;
    dec->buf_end = dec->pos + dest_len;
    dec->buf_len = dest_len + sereal_header_len;

    decompress_ok = csnappy_decompress_noheader((char *)(old_pos + header_len),
                                                compressed_packet_len - header_len,
                                                (char *)dec->pos,
                                                &dest_len);
    if (expect_false( decompress_ok != 0 )) {
        ERRORf1("Snappy decompression of Sereal packet payload failed with error %i!", decompress_ok);
    }
    return true;
}

/* Main dispatch sub */
static SRL_INLINE PyObject *srl_read_single_value(srl_decoder_t *dec) {
    Py_ssize_t len;
    PyObject *into;
    unsigned char tag;

read_again:
    if (expect_false( BUF_DONE(dec) ))
        ERROR("unexpected end of input stream while expecting a single value");

    tag = *dec->pos;
    if (expect_false(tag & SRL_HDR_TRACK_FLAG)) {
        tag = tag & ~SRL_HDR_TRACK_FLAG;
        ERROR("Tracking not yet implemented"); /* XXX */
    }
    dec->pos++;

    if ( tag <= SRL_HDR_POS_HIGH ) {
        into = PyInt_FromLong((long)tag);
    }
    else
    if ( tag <= SRL_HDR_NEG_HIGH) {
        into = PyInt_FromLong((long)tag - 32);
    }
    else
    if ( IS_SRL_HDR_SHORT_BINARY(tag) ) {
        len = (Py_ssize_t)SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag);
        ASSERT_BUF_SPACE(dec, len, " while reading ascii string");
        into = PyString_FromStringAndSize((char*)dec->pos, len);
        dec->pos += len;
    }
    else
    if ( IS_SRL_HDR_HASHREF(tag) ) {
        into = srl_read_hash(dec, tag);
    }
    else
    if ( IS_SRL_HDR_ARRAYREF(tag) ) {
        into = srl_read_array(dec, tag);
    }
    else {
        switch (tag) {
            case SRL_HDR_VARINT:        into = srl_read_varint(dec);           break;
            case SRL_HDR_ZIGZAG:        into = srl_read_zigzag(dec);           break;

            case SRL_HDR_FLOAT:         into = srl_read_float(dec);            break;
            case SRL_HDR_DOUBLE:        into = srl_read_double(dec);           break;
            case SRL_HDR_LONG_DOUBLE:   into = srl_read_long_double(dec);      break;

            case SRL_HDR_TRUE:          Py_INCREF(Py_True); into = Py_True;    break;
            case SRL_HDR_FALSE:         Py_INCREF(Py_False); into = Py_False;  break;
            case SRL_HDR_UNDEF:         Py_INCREF(Py_None); into = Py_None;    break;
            case SRL_HDR_BINARY:        into = srl_read_string(dec, 0);        break;
            case SRL_HDR_STR_UTF8:      into = srl_read_string(dec, 1);        break;

            case SRL_HDR_WEAKEN:        into = srl_read_weaken(dec);           break;
            case SRL_HDR_REFN:          into = srl_read_refn(dec);             break;
            case SRL_HDR_REFP:          into = srl_read_refp(dec);             break;
            case SRL_HDR_OBJECT:        into = srl_read_object(dec);           break;
            case SRL_HDR_OBJECTV:       into = srl_read_objectv(dec);          break;
            case SRL_HDR_COPY:          into = srl_read_copy(dec);             break;
            case SRL_HDR_EXTEND:        into = srl_read_extend(dec);           break;
            case SRL_HDR_HASH:          into = srl_read_hash(dec, 0);          break;
            case SRL_HDR_ARRAY:         into = srl_read_array(dec, 0);         break;
            case SRL_HDR_REGEXP:        into = srl_read_regexp(dec);           break;

            case SRL_HDR_PAD:           /* no op */
                while (BUF_NOT_DONE(dec) && *dec->pos == SRL_HDR_PAD)
                    dec->pos++;
                goto read_again;
            break;
            default:
                ERROR_UNEXPECTED(dec,tag, " single value");
            break;
        }
    }
    return into;
}

/* And the ectual decoders */
static SRL_INLINE bool srl_read_varint_internal(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr) {
    if (expect_true( dec->buf_end - dec->pos > 10 ))
        return srl_read_varint_nocheck(dec, ret, errstr);
    else
        return srl_read_varint_safe(dec, ret, errstr);
}

static SRL_INLINE bool srl_read_varint_safe(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr) {
    Py_ssize_t val = 0;
    unsigned int lshift= 0;

    while (BUF_NOT_DONE(dec) && *dec->pos & 0x80) {
        val |= ((*dec->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (lshift > (sizeof(Py_ssize_t) * 8))
            ERRORf1("varint too big%s", errstr);
    }
    if (expect_true( BUF_NOT_DONE(dec) )) {
        val |= (*dec->pos++ << lshift);
    } else {
        ERRORf1("varint terminated prematurely%s", errstr);
    }
    *ret = val;
    return 1;
}

static SRL_INLINE bool srl_read_varint_nocheck(srl_decoder_t *dec, Py_ssize_t *ret, const char * const errstr) { 
    Py_ssize_t val = 0;
    unsigned int lshift= 0;

    while (*dec->pos & 0x80) {
        val |= ((*dec->pos++ & 0x7F) << lshift);
        lshift += 7;
        if (expect_false( lshift > (sizeof(Py_ssize_t) * 8) ))
            ERROR("varint too big");
    }
    val |= ((*dec->pos++) << lshift);
    *ret = val;
    return 1;
}

static SRL_INLINE PyObject *srl_read_array(srl_decoder_t *dec, char tag){
    Py_ssize_t len;
    PyObject *item;
    PyObject *ret;
    int i;
    if(tag)
        len = tag & 0x15;
    else {
        MUST(srl_read_varint_internal(dec, &len, " while reading array"));
    }
    ret = PyList_New(len);
    for(i=0; i < len; i++) {
        MUSTg(item = srl_read_single_value(dec));
        PyList_SET_ITEM(ret, i, item);
    }
    return ret;
out_err:
    Py_XDECREF(ret);
    return NULL;
}

static SRL_INLINE PyObject *srl_read_hash(srl_decoder_t *dec, char tag){
    Py_ssize_t len;
    PyObject *key, *val, *ret;
    int i;

    if(tag)
        len = tag & 0x15;
    else {
        MUST(srl_read_varint_internal(dec, &len, " while reading hash"));
    }
    ret = PyDict_New();
    for(i=0; i < len; i++) {
        val = NULL; /* To avoid decref'ing too much if the first MUSTg fails */
        MUSTg(key = srl_read_single_value(dec));
        MUSTg(val = srl_read_single_value(dec));
        PyDict_SetItem(ret, key, val);
    }
    return ret;
out_err:
    Py_XDECREF(ret);
    Py_XDECREF(val);
    return NULL;
}

static SRL_INLINE PyObject *srl_read_varint(srl_decoder_t *dec){
    Py_ssize_t result;
    MUST(srl_read_varint_internal(dec, &result, " while reading single value"));
    return PyLong_FromLong(result);
}

static SRL_INLINE PyObject *srl_read_zigzag(srl_decoder_t *dec){
    Py_ssize_t result;
    MUST(srl_read_varint_internal(dec, &result, " while reading single value"));
    if(result & 1)
        result =  - (1 + (result >> 1));
    else
        result = result >> 1;
    return PyLong_FromLong(result);
}

static SRL_INLINE PyObject *srl_read_float(srl_decoder_t *dec){
    PyObject *ret;
    ASSERT_BUF_SPACE(dec, sizeof(float), " while reading FLOAT");
    ret = PyFloat_FromDouble((double)(*((float*)dec->pos)));
    dec->pos += sizeof(float);
    return ret;
}

static SRL_INLINE PyObject *srl_read_double(srl_decoder_t *dec){
    PyObject *ret;
    ASSERT_BUF_SPACE(dec, sizeof(double), " while reading FLOAT");
    ret = PyFloat_FromDouble(*((double*)dec->pos));
    dec->pos += sizeof(double);
    return ret;
}

static SRL_INLINE PyObject *srl_read_long_double(srl_decoder_t *dec){
    PyObject *ret;
    ASSERT_BUF_SPACE(dec, sizeof(long double), " while reading FLOAT");
    ret = PyFloat_FromDouble(*((long double*)dec->pos));
    dec->pos += sizeof(long double);
    return ret;
}

static SRL_INLINE PyObject *srl_read_string(srl_decoder_t *dec, bool unicode){
    Py_ssize_t len;
    MUST(srl_read_varint_length(dec, &len, " while reading header"));

    if(unicode) {
        if(SRL_DEC_HAVE_OPTION(dec, SRL_F_DECODER_VALIDATE_UTF8)) {
            return PyUnicode_Decode((char*)dec->pos, len, "utf-8", "strict");
        }
        else {
            return PyUnicode_Decode((char*)dec->pos, len, "utf-8", "replace");
        }
    }
    return PyString_FromStringAndSize((char*)dec->pos, len);
}

static SRL_INLINE PyObject *srl_read_weaken(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading weaken not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_refn(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading refn not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_refp(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading refp not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_object(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading object not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_objectv(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading objectv not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_copy(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading copy not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_extend(srl_decoder_t *dec){
    PyErr_SetString(PyExc_NotImplementedError, "Reading extend not yet implemented");
    return NULL;
}

static SRL_INLINE PyObject *srl_read_regexp(srl_decoder_t *dec){
    PyObject *ret = NULL, *pattern = NULL, *flags_ = NULL;
    int flags = 0;
    MUSTg(pattern = srl_read_single_value(dec));
    MUSTg(PyString_Check(pattern) || PyUnicode_Check(pattern));
    ASSERT_BUF_SPACE(dec, 1, " while reading regexp modifer tag");
    /* For now we will serialize the flags as ascii strings. Maybe we should use
     * something else but this is easy to debug and understand - since the modifiers
     * are tagged it doesn't matter much, we can add other tags later */
    if ( expect_true( IS_SRL_HDR_SHORT_BINARY(*dec->pos) ) ) {
        unsigned char mod_len = SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(*dec->pos++);
        ASSERT_BUF_SPACE(dec, mod_len, " while reading regexp modifiers");
        while (mod_len > 0) {
            mod_len--;
            switch (*dec->pos++) {
                case 'm': flags = flags | RE_MULTILINE;  break;
                case 's': flags = flags | RE_DOTALL;     break;
                case 'i': flags = flags | RE_IGNORECASE; break;
                case 'x': flags = flags | RE_VERBOSE;    break;
                case 'u': flags = flags | RE_UNICODE;    break;
                case 'l': flags = flags | RE_LOCALE;     break;
                default:
                    ERRORf1("bad modifier: %c", *(dec->pos - 1));
                    break;
            }
        }
    }
    flags_ = PyInt_FromLong(flags);
    ret = PyObject_CallFunctionObjArgs(srl_re_compile, pattern, flags_, NULL);

out_err:
    Py_XDECREF(pattern);
    Py_XDECREF(flags_);
    return ret;
}

