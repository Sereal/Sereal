#ifndef SRL_DECODER_H_
#define SRL_DECODER_H_

#include <Python.h>

typedef struct {
    unsigned char *buf_start;           /* ptr to "physical" start of input buffer */
    unsigned char *buf_end;             /* ptr to end of input buffer */
    unsigned char *pos;                 /* ptr to current position within input buffer */
    unsigned char *save_pos;            /* used for COPY tags */
    Py_ssize_t buf_len;

    uint32_t flags;                     /* flag-like options: See F_* defines in srl_decoder.c */
    PyDictObject *seen;                 /* ptr table for avoiding circular refs */
    void *uncompressed_data;            /* For snappy-compressed packets */

    Py_ssize_t bytes_consumed;
} srl_decoder_t;

#define BUF_POS(dec) ((dec)->pos)
#define BUF_SPACE(dec) ((dec)->buf_end - (dec)->pos)
#define BUF_POS_OFS(dec) ((dec)->pos - (dec)->buf_start)
#define BUF_SIZE(dec) ((dec)->buf_end - (dec)->buf_start)
#define BUF_NOT_DONE(dec) ((dec)->pos < (dec)->buf_end)
#define BUF_DONE(dec) ((dec)->pos >= (dec)->buf_end)

#define SRL_BASE_ERROR_FORMAT "Sereal: Error in %s line %u: "
#define SRL_BASE_ERROR_ARGS __FILE__, __LINE__

#define croak(msg, ...) do {PyErr_Format(PyExc_ValueError, msg, __VA_ARGS__); return NULL; } while(0)

#define ERROR(msg)                          croak(SRL_BASE_ERROR_FORMAT "%s", SRL_BASE_ERROR_ARGS, (msg))
#define ERRORf1(fmt,var)                    croak(SRL_BASE_ERROR_FORMAT fmt, SRL_BASE_ERROR_ARGS, (var))
#define ERRORf2(fmt,var1,var2)              croak(SRL_BASE_ERROR_FORMAT fmt, SRL_BASE_ERROR_ARGS, (var1),(var2))
#define ERRORf3(fmt,var1,var2,var3)         croak(SRL_BASE_ERROR_FORMAT fmt, SRL_BASE_ERROR_ARGS, (var1),(var2),(var3))
#define ERRORf4(fmt,var1,var2,var3,var4)    croak(SRL_BASE_ERROR_FORMAT fmt, SRL_BASE_ERROR_ARGS, (var1),(var2),(var3),(var4))
#define ERROR_UNIMPLEMENTED(dec,tag,str) \
    ERRORf3("Tag %u %s is unimplemented at ofs: %d", (tag), (str), BUF_POS_OFS(dec))
#define ERROR_UNTERMINATED(dec,tag,str) \
    ERRORf4("Tag SRL_HDR_%s %s was not terminated properly at ofs %lu with %lu to go", \
            tag_name[(tag) & 127], (str), (dec)->pos - (dec)->buf_start, (dec)->buf_end - (dec)->pos)
#define ERROR_BAD_COPY(dec, tag) \
    ERRORf1("While processing tag SRL_HDR_%s encountered a bad COPY tag", tag_name[(tag) & 127])
#define ERROR_UNEXPECTED(dec, tag, msg) \
    ERRORf2("Unexpected tag %s while expecting %s", tag_name[(tag || *(dec)->pos) & 127], msg)
#define ERROR_REFUSE_OBJECT() \
    ERROR("Encountered object in input, but the 'refuse_objects' option is in effect");
#define ERROR_PANIC(dec, msg) ERRORf1("Panic: %s", msg);

/* If set, the decoder struct needs to be cleared instead of freed at
 * the end of a deserialization operation */
#define SRL_F_REUSE_DECODER 1
/* If set, then the decoder destructor was already pushed to the
 * callback stack */
#define SRL_F_DECODER_DESTRUCTOR_OK 2
/* Non-persistent flag! */
#define SRL_F_DECODER_NEEDS_FINALIZE 4
/* Non-persistent flag! */
#define SRL_F_DECODER_DECOMPRESS_SNAPPY 8
/* Persistent flag: Make the decoder REFUSE compressed documents */
#define SRL_F_DECODER_REFUSE_SNAPPY 16
/* Persistent flag: Make the decoder REFUSE opjects */
#define SRL_F_DECODER_REFUSE_OBJECTS 32
/* Persistent flag: Make the decoder validate UTT8 strings */
#define SRL_F_DECODER_VALIDATE_UTF8 64

#define SRL_DEC_HAVE_OPTION(dec, flag_num) ((dec)->flags & flag_num)
#define SRL_DEC_SET_OPTION(dec, flag_num) ((dec)->flags |= flag_num)
#define SRL_DEC_UNSET_OPTION(dec, flag_num) ((dec)->flags &= ~flag_num)
#define SRL_DEC_VOLATILE_FLAGS (SRL_F_DECODER_NEEDS_FINALIZE|SRL_F_DECODER_DECOMPRESS_SNAPPY)
#define SRL_DEC_RESET_VOLATILE_FLAGS(dec) ((dec)->flags &= ~SRL_DEC_VOLATILE_FLAGS)

srl_decoder_t *srl_decoder_new(void);
void srl_decoder_free(srl_decoder_t *dec);
PyObject *srl_decode(srl_decoder_t *dec);
PyObject *srl_re_compile;
int RE_IGNORECASE, RE_DOTALL, RE_VERBOSE, RE_MULTILINE;
/* Not necessarily compatible with other languages! */
int RE_UNICODE, RE_LOCALE;

/* 
=for autoupdater start

* NOTE this section is autoupdated by author_tools/update_from_header.pl
*/
static const char * const tag_name[] = {
	"POS_0",             /*        0 0x00 0b00000000 */
	"POS_1",             /*        1 0x01 0b00000001 */
	"POS_2",             /*        2 0x02 0b00000010 */
	"POS_3",             /*        3 0x03 0b00000011 */
	"POS_4",             /*        4 0x04 0b00000100 */
	"POS_5",             /*        5 0x05 0b00000101 */
	"POS_6",             /*        6 0x06 0b00000110 */
	"POS_7",             /* "\a"   7 0x07 0b00000111 */
	"POS_8",             /* "\b"   8 0x08 0b00001000 */
	"POS_9",             /* "\t"   9 0x09 0b00001001 */
	"POS_10",            /* "\n"  10 0x0a 0b00001010 */
	"POS_11",            /*       11 0x0b 0b00001011 */
	"POS_12",            /* "\f"  12 0x0c 0b00001100 */
	"POS_13",            /* "\r"  13 0x0d 0b00001101 */
	"POS_14",            /*       14 0x0e 0b00001110 */
	"POS_15",            /*       15 0x0f 0b00001111 */
	"NEG_16",            /*       16 0x10 0b00010000 */
	"NEG_15",            /*       17 0x11 0b00010001 */
	"NEG_14",            /*       18 0x12 0b00010010 */
	"NEG_13",            /*       19 0x13 0b00010011 */
	"NEG_12",            /*       20 0x14 0b00010100 */
	"NEG_11",            /*       21 0x15 0b00010101 */
	"NEG_10",            /*       22 0x16 0b00010110 */
	"NEG_9",             /*       23 0x17 0b00010111 */
	"NEG_8",             /*       24 0x18 0b00011000 */
	"NEG_7",             /*       25 0x19 0b00011001 */
	"NEG_6",             /*       26 0x1a 0b00011010 */
	"NEG_5",             /* "\e"  27 0x1b 0b00011011 */
	"NEG_4",             /*       28 0x1c 0b00011100 */
	"NEG_3",             /*       29 0x1d 0b00011101 */
	"NEG_2",             /*       30 0x1e 0b00011110 */
	"NEG_1",             /*       31 0x1f 0b00011111 */
	"VARINT",            /* " "   32 0x20 0b00100000 */
	"ZIGZAG",            /* "!"   33 0x21 0b00100001 */
	"FLOAT",             /* "\""  34 0x22 0b00100010 */
	"DOUBLE",            /* "#"   35 0x23 0b00100011 */
	"LONG_DOUBLE",       /* "\$"  36 0x24 0b00100100 */
	"UNDEF",             /* "%"   37 0x25 0b00100101 */
	"BINARY",            /* "&"   38 0x26 0b00100110 */
	"STR_UTF8",          /* "'"   39 0x27 0b00100111 */
	"REFN",              /* "("   40 0x28 0b00101000 */
	"REFP",              /* ")"   41 0x29 0b00101001 */
	"HASH",              /* "*"   42 0x2a 0b00101010 */
	"ARRAY",             /* "+"   43 0x2b 0b00101011 */
	"OBJECT",            /* ","   44 0x2c 0b00101100 */
	"OBJECTV",           /* "-"   45 0x2d 0b00101101 */
	"ALIAS",             /* "."   46 0x2e 0b00101110 */
	"COPY",              /* "/"   47 0x2f 0b00101111 */
	"WEAKEN",            /* "0"   48 0x30 0b00110000 */
	"REGEXP",            /* "1"   49 0x31 0b00110001 */
	"RESERVED_0",        /* "2"   50 0x32 0b00110010 */
	"RESERVED_1",        /* "3"   51 0x33 0b00110011 */
	"RESERVED_2",        /* "4"   52 0x34 0b00110100 */
	"RESERVED_3",        /* "5"   53 0x35 0b00110101 */
	"RESERVED_4",        /* "6"   54 0x36 0b00110110 */
	"RESERVED_5",        /* "7"   55 0x37 0b00110111 */
	"RESERVED_6",        /* "8"   56 0x38 0b00111000 */
	"RESERVED_7",        /* "9"   57 0x39 0b00111001 */
	"FALSE",             /* ":"   58 0x3a 0b00111010 */
	"TRUE",              /* ";"   59 0x3b 0b00111011 */
	"MANY",              /* "<"   60 0x3c 0b00111100 */
	"PACKET_START",      /* "="   61 0x3d 0b00111101 */
	"EXTEND",            /* ">"   62 0x3e 0b00111110 */
	"PAD",               /* "?"   63 0x3f 0b00111111 */
	"ARRAYREF_0",        /* "\@"  64 0x40 0b01000000 */
	"ARRAYREF_1",        /* "A"   65 0x41 0b01000001 */
	"ARRAYREF_2",        /* "B"   66 0x42 0b01000010 */
	"ARRAYREF_3",        /* "C"   67 0x43 0b01000011 */
	"ARRAYREF_4",        /* "D"   68 0x44 0b01000100 */
	"ARRAYREF_5",        /* "E"   69 0x45 0b01000101 */
	"ARRAYREF_6",        /* "F"   70 0x46 0b01000110 */
	"ARRAYREF_7",        /* "G"   71 0x47 0b01000111 */
	"ARRAYREF_8",        /* "H"   72 0x48 0b01001000 */
	"ARRAYREF_9",        /* "I"   73 0x49 0b01001001 */
	"ARRAYREF_10",       /* "J"   74 0x4a 0b01001010 */
	"ARRAYREF_11",       /* "K"   75 0x4b 0b01001011 */
	"ARRAYREF_12",       /* "L"   76 0x4c 0b01001100 */
	"ARRAYREF_13",       /* "M"   77 0x4d 0b01001101 */
	"ARRAYREF_14",       /* "N"   78 0x4e 0b01001110 */
	"ARRAYREF_15",       /* "O"   79 0x4f 0b01001111 */
	"HASHREF_0",         /* "P"   80 0x50 0b01010000 */
	"HASHREF_1",         /* "Q"   81 0x51 0b01010001 */
	"HASHREF_2",         /* "R"   82 0x52 0b01010010 */
	"HASHREF_3",         /* "S"   83 0x53 0b01010011 */
	"HASHREF_4",         /* "T"   84 0x54 0b01010100 */
	"HASHREF_5",         /* "U"   85 0x55 0b01010101 */
	"HASHREF_6",         /* "V"   86 0x56 0b01010110 */
	"HASHREF_7",         /* "W"   87 0x57 0b01010111 */
	"HASHREF_8",         /* "X"   88 0x58 0b01011000 */
	"HASHREF_9",         /* "Y"   89 0x59 0b01011001 */
	"HASHREF_10",        /* "Z"   90 0x5a 0b01011010 */
	"HASHREF_11",        /* "["   91 0x5b 0b01011011 */
	"HASHREF_12",        /* "\\"  92 0x5c 0b01011100 */
	"HASHREF_13",        /* "]"   93 0x5d 0b01011101 */
	"HASHREF_14",        /* "^"   94 0x5e 0b01011110 */
	"HASHREF_15",        /* "_"   95 0x5f 0b01011111 */
	"SHORT_BINARY_0",    /* "`"   96 0x60 0b01100000 */
	"SHORT_BINARY_1",    /* "a"   97 0x61 0b01100001 */
	"SHORT_BINARY_2",    /* "b"   98 0x62 0b01100010 */
	"SHORT_BINARY_3",    /* "c"   99 0x63 0b01100011 */
	"SHORT_BINARY_4",    /* "d"  100 0x64 0b01100100 */
	"SHORT_BINARY_5",    /* "e"  101 0x65 0b01100101 */
	"SHORT_BINARY_6",    /* "f"  102 0x66 0b01100110 */
	"SHORT_BINARY_7",    /* "g"  103 0x67 0b01100111 */
	"SHORT_BINARY_8",    /* "h"  104 0x68 0b01101000 */
	"SHORT_BINARY_9",    /* "i"  105 0x69 0b01101001 */
	"SHORT_BINARY_10",   /* "j"  106 0x6a 0b01101010 */
	"SHORT_BINARY_11",   /* "k"  107 0x6b 0b01101011 */
	"SHORT_BINARY_12",   /* "l"  108 0x6c 0b01101100 */
	"SHORT_BINARY_13",   /* "m"  109 0x6d 0b01101101 */
	"SHORT_BINARY_14",   /* "n"  110 0x6e 0b01101110 */
	"SHORT_BINARY_15",   /* "o"  111 0x6f 0b01101111 */
	"SHORT_BINARY_16",   /* "p"  112 0x70 0b01110000 */
	"SHORT_BINARY_17",   /* "q"  113 0x71 0b01110001 */
	"SHORT_BINARY_18",   /* "r"  114 0x72 0b01110010 */
	"SHORT_BINARY_19",   /* "s"  115 0x73 0b01110011 */
	"SHORT_BINARY_20",   /* "t"  116 0x74 0b01110100 */
	"SHORT_BINARY_21",   /* "u"  117 0x75 0b01110101 */
	"SHORT_BINARY_22",   /* "v"  118 0x76 0b01110110 */
	"SHORT_BINARY_23",   /* "w"  119 0x77 0b01110111 */
	"SHORT_BINARY_24",   /* "x"  120 0x78 0b01111000 */
	"SHORT_BINARY_25",   /* "y"  121 0x79 0b01111001 */
	"SHORT_BINARY_26",   /* "z"  122 0x7a 0b01111010 */
	"SHORT_BINARY_27",   /* "{"  123 0x7b 0b01111011 */
	"SHORT_BINARY_28",   /* "|"  124 0x7c 0b01111100 */
	"SHORT_BINARY_29",   /* "}"  125 0x7d 0b01111101 */
	"SHORT_BINARY_30",   /* "~"  126 0x7e 0b01111110 */
	"SHORT_BINARY_31"    /*      127 0x7f 0b01111111 */
};
/*
* NOTE the above section is auto-updated by author_tools/update_from_header.pl

=for autoupdater stop
*/
#endif
