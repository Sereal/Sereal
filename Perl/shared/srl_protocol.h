#ifndef SEEN_SRL_PROTOCOL_H_
#define SEEN_SRL_PROTOCOL_H_

/*
=for autoupdater start


                  Tag | Char | Dec |  Hex |     Binary | Follow
    ------------------+------+-----+------+----------- |-----------------------------------------
    POS_0             |      |   0 | 0x00 | 0b00000000 | small positive integer - value in low 4 bits (identity)
    POS_1             |      |   1 | 0x01 | 0b00000001 |
    POS_2             |      |   2 | 0x02 | 0b00000010 |
    POS_3             |      |   3 | 0x03 | 0b00000011 |
    POS_4             |      |   4 | 0x04 | 0b00000100 |
    POS_5             |      |   5 | 0x05 | 0b00000101 |
    POS_6             |      |   6 | 0x06 | 0b00000110 |
    POS_7             | "\a" |   7 | 0x07 | 0b00000111 |
    POS_8             | "\b" |   8 | 0x08 | 0b00001000 |
    POS_9             | "\t" |   9 | 0x09 | 0b00001001 |
    POS_10            | "\n" |  10 | 0x0a | 0b00001010 |
    POS_11            |      |  11 | 0x0b | 0b00001011 |
    POS_12            | "\f" |  12 | 0x0c | 0b00001100 |
    POS_13            | "\r" |  13 | 0x0d | 0b00001101 |
    POS_14            |      |  14 | 0x0e | 0b00001110 |
    POS_15            |      |  15 | 0x0f | 0b00001111 | small positive integer - value in low 4 bits (identity)
    NEG_16            |      |  16 | 0x10 | 0b00010000 | small negative integer - value in low 4 bits (k+32)
    NEG_15            |      |  17 | 0x11 | 0b00010001 |
    NEG_14            |      |  18 | 0x12 | 0b00010010 |
    NEG_13            |      |  19 | 0x13 | 0b00010011 |
    NEG_12            |      |  20 | 0x14 | 0b00010100 |
    NEG_11            |      |  21 | 0x15 | 0b00010101 |
    NEG_10            |      |  22 | 0x16 | 0b00010110 |
    NEG_9             |      |  23 | 0x17 | 0b00010111 |
    NEG_8             |      |  24 | 0x18 | 0b00011000 |
    NEG_7             |      |  25 | 0x19 | 0b00011001 |
    NEG_6             |      |  26 | 0x1a | 0b00011010 |
    NEG_5             | "\e" |  27 | 0x1b | 0b00011011 |
    NEG_4             |      |  28 | 0x1c | 0b00011100 |
    NEG_3             |      |  29 | 0x1d | 0b00011101 |
    NEG_2             |      |  30 | 0x1e | 0b00011110 |
    NEG_1             |      |  31 | 0x1f | 0b00011111 | small negative integer - value in low 4 bits (k+32)
    VARINT            | " "  |  32 | 0x20 | 0b00100000 | <VARINT> - Varint variable length integer
    ZIGZAG            | "!"  |  33 | 0x21 | 0b00100001 | <ZIGZAG-VARINT> - Zigzag variable length integer
    FLOAT             | "\"" |  34 | 0x22 | 0b00100010 | <IEEE-FLOAT>
    DOUBLE            | "#"  |  35 | 0x23 | 0b00100011 | <IEEE-DOUBLE>
    LONG_DOUBLE       | "\$" |  36 | 0x24 | 0b00100100 | <IEEE-LONG-DOUBLE>
    UNDEF             | "%"  |  37 | 0x25 | 0b00100101 | None - Perl undef var; eg my $var= undef;
    BINARY            | "&"  |  38 | 0x26 | 0b00100110 | <LEN-VARINT> <BYTES> - binary/(latin1) string
    STR_UTF8          | "'"  |  39 | 0x27 | 0b00100111 | <LEN-VARINT> <UTF8> - utf8 string
    REFN              | "("  |  40 | 0x28 | 0b00101000 | <ITEM-TAG>    - ref to next item
    REFP              | ")"  |  41 | 0x29 | 0b00101001 | <OFFSET-VARINT> - ref to previous item stored at offset
    HASH              | "*"  |  42 | 0x2a | 0b00101010 | <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs
    ARRAY             | "+"  |  43 | 0x2b | 0b00101011 | <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items
    OBJECT            | ","  |  44 | 0x2c | 0b00101100 | <STR-TAG> <ITEM-TAG> - class, object-item
    OBJECTV           | "-"  |  45 | 0x2d | 0b00101101 | <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item
    ALIAS             | "."  |  46 | 0x2e | 0b00101110 | <OFFSET-VARINT> - alias to item defined at offset
    COPY              | "/"  |  47 | 0x2f | 0b00101111 | <OFFSET-VARINT> - copy of item defined at offset
    WEAKEN            | "0"  |  48 | 0x30 | 0b00110000 | <REF-TAG> - Weaken the following reference
    REGEXP            | "1"  |  49 | 0x31 | 0b00110001 | <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>
    OBJECT_FREEZE     | "2"  |  50 | 0x32 | 0b00110010 | <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding
    OBJECTV_FREEZE    | "3"  |  51 | 0x33 | 0b00110011 | <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT)
    RESERVED_0        | "4"  |  52 | 0x34 | 0b00110100 | reserved
    RESERVED_1        | "5"  |  53 | 0x35 | 0b00110101 |
    RESERVED_2        | "6"  |  54 | 0x36 | 0b00110110 |
    RESERVED_3        | "7"  |  55 | 0x37 | 0b00110111 |
    RESERVED_4        | "8"  |  56 | 0x38 | 0b00111000 | reserved
    CANONICAL_UNDEF   | "9"  |  57 | 0x39 | 0b00111001 | undef (PL_sv_undef) - "the" Perl undef (see notes)
    FALSE             | ":"  |  58 | 0x3a | 0b00111010 | false (PL_sv_no)
    TRUE              | ";"  |  59 | 0x3b | 0b00111011 | true  (PL_sv_yes)
    MANY              | "<"  |  60 | 0x3c | 0b00111100 | <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3)
    PACKET_START      | "="  |  61 | 0x3d | 0b00111101 | (first byte of magic string in header)
    EXTEND            | ">"  |  62 | 0x3e | 0b00111110 | <BYTE> - for additional tags
    PAD               | "?"  |  63 | 0x3f | 0b00111111 | (ignored tag, skip to next byte)
    ARRAYREF_0        | "\@" |  64 | 0x40 | 0b01000000 | [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)
    ARRAYREF_1        | "A"  |  65 | 0x41 | 0b01000001 |
    ARRAYREF_2        | "B"  |  66 | 0x42 | 0b01000010 |
    ARRAYREF_3        | "C"  |  67 | 0x43 | 0b01000011 |
    ARRAYREF_4        | "D"  |  68 | 0x44 | 0b01000100 |
    ARRAYREF_5        | "E"  |  69 | 0x45 | 0b01000101 |
    ARRAYREF_6        | "F"  |  70 | 0x46 | 0b01000110 |
    ARRAYREF_7        | "G"  |  71 | 0x47 | 0b01000111 |
    ARRAYREF_8        | "H"  |  72 | 0x48 | 0b01001000 |
    ARRAYREF_9        | "I"  |  73 | 0x49 | 0b01001001 |
    ARRAYREF_10       | "J"  |  74 | 0x4a | 0b01001010 |
    ARRAYREF_11       | "K"  |  75 | 0x4b | 0b01001011 |
    ARRAYREF_12       | "L"  |  76 | 0x4c | 0b01001100 |
    ARRAYREF_13       | "M"  |  77 | 0x4d | 0b01001101 |
    ARRAYREF_14       | "N"  |  78 | 0x4e | 0b01001110 |
    ARRAYREF_15       | "O"  |  79 | 0x4f | 0b01001111 | [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)
    HASHREF_0         | "P"  |  80 | 0x50 | 0b01010000 | [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)
    HASHREF_1         | "Q"  |  81 | 0x51 | 0b01010001 |
    HASHREF_2         | "R"  |  82 | 0x52 | 0b01010010 |
    HASHREF_3         | "S"  |  83 | 0x53 | 0b01010011 |
    HASHREF_4         | "T"  |  84 | 0x54 | 0b01010100 |
    HASHREF_5         | "U"  |  85 | 0x55 | 0b01010101 |
    HASHREF_6         | "V"  |  86 | 0x56 | 0b01010110 |
    HASHREF_7         | "W"  |  87 | 0x57 | 0b01010111 |
    HASHREF_8         | "X"  |  88 | 0x58 | 0b01011000 |
    HASHREF_9         | "Y"  |  89 | 0x59 | 0b01011001 |
    HASHREF_10        | "Z"  |  90 | 0x5a | 0b01011010 |
    HASHREF_11        | "["  |  91 | 0x5b | 0b01011011 |
    HASHREF_12        | "\\" |  92 | 0x5c | 0b01011100 |
    HASHREF_13        | "]"  |  93 | 0x5d | 0b01011101 |
    HASHREF_14        | "^"  |  94 | 0x5e | 0b01011110 |
    HASHREF_15        | "_"  |  95 | 0x5f | 0b01011111 | [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)
    SHORT_BINARY_0    | "`"  |  96 | 0x60 | 0b01100000 | <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag
    SHORT_BINARY_1    | "a"  |  97 | 0x61 | 0b01100001 |
    SHORT_BINARY_2    | "b"  |  98 | 0x62 | 0b01100010 |
    SHORT_BINARY_3    | "c"  |  99 | 0x63 | 0b01100011 |
    SHORT_BINARY_4    | "d"  | 100 | 0x64 | 0b01100100 |
    SHORT_BINARY_5    | "e"  | 101 | 0x65 | 0b01100101 |
    SHORT_BINARY_6    | "f"  | 102 | 0x66 | 0b01100110 |
    SHORT_BINARY_7    | "g"  | 103 | 0x67 | 0b01100111 |
    SHORT_BINARY_8    | "h"  | 104 | 0x68 | 0b01101000 |
    SHORT_BINARY_9    | "i"  | 105 | 0x69 | 0b01101001 |
    SHORT_BINARY_10   | "j"  | 106 | 0x6a | 0b01101010 |
    SHORT_BINARY_11   | "k"  | 107 | 0x6b | 0b01101011 |
    SHORT_BINARY_12   | "l"  | 108 | 0x6c | 0b01101100 |
    SHORT_BINARY_13   | "m"  | 109 | 0x6d | 0b01101101 |
    SHORT_BINARY_14   | "n"  | 110 | 0x6e | 0b01101110 |
    SHORT_BINARY_15   | "o"  | 111 | 0x6f | 0b01101111 |
    SHORT_BINARY_16   | "p"  | 112 | 0x70 | 0b01110000 |
    SHORT_BINARY_17   | "q"  | 113 | 0x71 | 0b01110001 |
    SHORT_BINARY_18   | "r"  | 114 | 0x72 | 0b01110010 |
    SHORT_BINARY_19   | "s"  | 115 | 0x73 | 0b01110011 |
    SHORT_BINARY_20   | "t"  | 116 | 0x74 | 0b01110100 |
    SHORT_BINARY_21   | "u"  | 117 | 0x75 | 0b01110101 |
    SHORT_BINARY_22   | "v"  | 118 | 0x76 | 0b01110110 |
    SHORT_BINARY_23   | "w"  | 119 | 0x77 | 0b01110111 |
    SHORT_BINARY_24   | "x"  | 120 | 0x78 | 0b01111000 |
    SHORT_BINARY_25   | "y"  | 121 | 0x79 | 0b01111001 |
    SHORT_BINARY_26   | "z"  | 122 | 0x7a | 0b01111010 |
    SHORT_BINARY_27   | "{"  | 123 | 0x7b | 0b01111011 |
    SHORT_BINARY_28   | "|"  | 124 | 0x7c | 0b01111100 |
    SHORT_BINARY_29   | "}"  | 125 | 0x7d | 0b01111101 |
    SHORT_BINARY_30   | "~"  | 126 | 0x7e | 0b01111110 |
    SHORT_BINARY_31   |      | 127 | 0x7f | 0b01111111 | <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag

=for autoupdater stop
*/

/* magic string, protocol version and encoding information */
#define SRL_MAGIC_STRLEN                4               /* Length of SRL_MAGIC_STRING */

#define SRL_MAGIC_STRING                "=srl"          /* Magic string for header. Every packet starts with this or "=\xF3rl",
                                                         * which is the high-bit-set-on-the-"s" equivalent. */
#define SRL_MAGIC_STRING_UINT_LE                0x6C72733D  /* SRL_MAGIC_STRING as a little endian integer */
#define SRL_MAGIC_STRING_UINT_BE                0x3D73726C  /* SRL_MAGIC_STRING as a big endian integer */

#define SRL_MAGIC_STRING_HIGHBIT                "=\xF3rl"   /* Magic string for header, with high bit set for UTF8 sanity check. */
#define SRL_MAGIC_STRING_HIGHBIT_UINT_LE        0x6C72F33D  /* SRL_MAGIC_STRING_HIGHBIT as a little endian integer */
#define SRL_MAGIC_STRING_HIGHBIT_UINT_BE        0x3DF3726C  /* SRL_MAGIC_STRING_HIGHBIT as a big endian integer */

#define SRL_MAGIC_STRING_HIGHBIT_UTF8           "=\xC3\xB3rl"   /* Magic string for header, corrupted by accidental UTF8 encoding */
#define SRL_MAGIC_STRING_HIGHBIT_UTF8_UINT_LE   0x72B3C33D      /* first four bytes of SRL_MAGIC_STRING encoded as UTF8, little endian */
#define SRL_MAGIC_STRING_HIGHBIT_UTF8_UINT_BE   0x3DC3B372      /* first four bytes of SRL_MAGIC_STRING encoded as UTF8, big endian */

#define SRL_PROTOCOL_VERSION            ( 3 )
#define SRL_PROTOCOL_VERSION_BITS       ( 4 )           /* how many bits we use for the version, the rest go to the encoding */
#define SRL_PROTOCOL_VERSION_MASK       ( ( 1 << SRL_PROTOCOL_VERSION_BITS ) - 1 )

#define SRL_PROTOCOL_ENCODING_MASK      ( SRL_PROTOCOL_VERSION_MASK << SRL_PROTOCOL_VERSION_BITS )
#define SRL_PROTOCOL_ENCODING_RAW       ( 0 << SRL_PROTOCOL_VERSION_BITS )
#define SRL_PROTOCOL_ENCODING_SNAPPY    ( 1 << SRL_PROTOCOL_VERSION_BITS )
#define SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL    ( 2 << SRL_PROTOCOL_VERSION_BITS )
#define SRL_PROTOCOL_ENCODING_ZLIB      ( 3 << SRL_PROTOCOL_VERSION_BITS )

/* Bits in the header bitfield */
#define SRL_PROTOCOL_HDR_USER_DATA      ( 1 )
#define SRL_PROTOCOL_HDR_CONTINUE       ( 8 ) /* TODO Describe in spec - not urgent since not meaningful yet */

/* Useful constants */
/* See also range constants below for the header byte */
#define SRL_POS_MAX_SIZE           15
#define SRL_NEG_MIN_SIZE           16

/* All constants have the F bit (SRL_HDR_TRACK_FLAG) unset! */
/* _LOW and _HIGH versions refering to INCLUSIVE range boundaries */


#define SRL_HDR_POS             ((U8)0)       /* small positive integer - value in low 4 bits (identity) */
#define SRL_HDR_POS_LOW         ((U8)0)       /* small positive integer - value in low 4 bits (identity) */
#define SRL_HDR_POS_HIGH        ((U8)15)      /* small positive integer - value in low 4 bits (identity) */

#define SRL_HDR_NEG             ((U8)16)      /* small negative integer - value in low 4 bits (k+32) */
#define SRL_HDR_NEG_LOW         ((U8)16)      /* small negative integer - value in low 4 bits (k+32) */
#define SRL_HDR_NEG_HIGH        ((U8)31)      /* small negative integer - value in low 4 bits (k+32) */

#define SRL_HDR_VARINT          ((U8)32)      /* <VARINT> - Varint variable length integer */
#define SRL_HDR_ZIGZAG          ((U8)33)      /* <ZIGZAG-VARINT> - Zigzag variable length integer */
#define SRL_HDR_FLOAT           ((U8)34)      /* <IEEE-FLOAT> */
#define SRL_HDR_DOUBLE          ((U8)35)      /* <IEEE-DOUBLE> */
#define SRL_HDR_LONG_DOUBLE     ((U8)36)      /* <IEEE-LONG-DOUBLE> */
#define SRL_HDR_UNDEF           ((U8)37)      /* None - Perl undef var; eg my $var= undef; */
#define SRL_HDR_BINARY          ((U8)38)      /* <LEN-VARINT> <BYTES> - binary/(latin1) string */
#define SRL_HDR_STR_UTF8        ((U8)39)      /* <LEN-VARINT> <UTF8> - utf8 string */

#define SRL_HDR_REFN            ((U8)40)      /* <ITEM-TAG>    - ref to next item */
#define SRL_HDR_REFP            ((U8)41)      /* <OFFSET-VARINT> - ref to previous item stored at offset */
#define SRL_HDR_HASH            ((U8)42)      /* <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs */
#define SRL_HDR_ARRAY           ((U8)43)      /* <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items */
#define SRL_HDR_OBJECT          ((U8)44)      /* <STR-TAG> <ITEM-TAG> - class, object-item */
#define SRL_HDR_OBJECTV         ((U8)45)      /* <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item */
#define SRL_HDR_ALIAS           ((U8)46)      /* <OFFSET-VARINT> - alias to item defined at offset */
#define SRL_HDR_COPY            ((U8)47)      /* <OFFSET-VARINT> - copy of item defined at offset */

#define SRL_HDR_WEAKEN          ((U8)48)      /* <REF-TAG> - Weaken the following reference */
#define SRL_HDR_REGEXP          ((U8)49)      /* <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>*/

#define SRL_HDR_OBJECT_FREEZE   ((U8)50)      /* <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding */
#define SRL_HDR_OBJECTV_FREEZE  ((U8)51)      /* <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT) */

/* Note: Can do reserved check with a range now, but as we start using
 *       them, might have to explicit == check later. */
#define SRL_HDR_RESERVED        ((U8)52)      /* reserved */
#define SRL_HDR_RESERVED_LOW    ((U8)52)
#define SRL_HDR_RESERVED_HIGH   ((U8)56)

#define SRL_HDR_CANONICAL_UNDEF ((U8)57)      /* undef (PL_sv_undef) - "the" Perl undef (see notes) */
#define SRL_HDR_FALSE           ((U8)58)      /* false (PL_sv_no)  */
#define SRL_HDR_TRUE            ((U8)59)      /* true  (PL_sv_yes) */

#define SRL_HDR_MANY            ((U8)60)      /* <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3) */
#define SRL_HDR_PACKET_START    ((U8)61)      /* (first byte of magic string in header) */


#define SRL_HDR_EXTEND          ((U8)62)      /* <BYTE> - for additional tags */
#define SRL_HDR_PAD             ((U8)63)      /* (ignored tag, skip to next byte) */
#define SRL_HDR_ARRAYREF        ((U8)64)      /* [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)*/
#define SRL_MASK_ARRAYREF_COUNT ((U8)15)      /* mask to get low bits from tag */
#define SRL_HDR_ARRAYREF_LOW    ((U8)64)
#define SRL_HDR_ARRAYREF_HIGH   ((U8)79)


#define SRL_HDR_HASHREF         ((U8)80)      /* [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)*/
#define SRL_MASK_HASHREF_COUNT  ((U8)15)      /* mask to get low bits from tag */
#define SRL_HDR_HASHREF_LOW     ((U8)80)
#define SRL_HDR_HASHREF_HIGH    ((U8)95)

#define SRL_HDR_SHORT_BINARY    ((U8)96)      /* <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
#define SRL_HDR_SHORT_BINARY_LOW       ((U8)96)
#define SRL_HDR_SHORT_BINARY_HIGH      ((U8)127)
#define SRL_MASK_SHORT_BINARY_LEN      ((U8)31)      /* mask to get length of SRL_HDR_SHORT_BINARY type tags */

#define SRL_HDR_TRACK_FLAG      ((U8)128)         /* if this bit is set track the item */

/* TODO */

#define SRL_SET_TRACK_FLAG(where) ((where) |= SRL_HDR_TRACK_FLAG)

#define SRL_HDR_SHORT_BINARY_LEN_FROM_TAG(tag) ((tag) & SRL_MASK_SHORT_BINARY_LEN)
#define SRL_HDR_ARRAYREF_LEN_FROM_TAG(tag)     ((tag) & SRL_MASK_ARRAYREF_COUNT)
#define SRL_HDR_HASHREF_LEN_FROM_TAG(tag)      ((tag) & SRL_MASK_HASHREF_COUNT)

#endif
