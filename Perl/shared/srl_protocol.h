#ifndef SRL_PROTOCOL_H_
#define SRL_PROTOCOL_H_

/*
 * Sereal Protocol version 1, see constants below docs.
 *
 * Generally speaking, structures are serialized depth-first and each item
 * is preceded/defined by a 1-byte control character, see table below.
 * All packets of Sereal data must be preceded by a header. The header structure
 * is as follows:
 * Bytes 1-4: Magic string "=srl".
 * Byte 5: low bits: The protocol version. high bits: flags
 *         Flags defined are:
 *          - lowest flag bit (5): Payload uses Snappy compression
 * Next: A varint describing the length of the rest of the header.
 *       Since in protocol version 1, there is currently nothing else in the header,
 *       this varint is always 0 (but that will change). Decoders must be able to
 *       skip the parts of the header that they know nothing about using the total
 *       header length.
 *
 * +--------+-----------------+-----------------+--------------------------------------------------------------------------
 *          |Bit              | follow          | Description
 *          | 7 6 5 4 3 2 1 0 | bytes           |
 * ---------+-----------------+-----------------+--------------------------------------------------------------------------
 *          | F 0 0 s x x x x | -               | tiny ints
 * POS      |       0 x x x x | -               | Positive nibble   0 .. 15
 * NEG      |       1 x x x x | -               | Negative nibble -16 .. -1
 * 
 *          | F 0 1 x x x x x |
 *----------|-----------------+-----------------+---------------------------------------------------------------------------
 *          |       0 0 x x x |                 |
 * VARINT   |           0 0 0 | varint          | varint
 * ZIGZAG   |           0 0 1 | varint          | zigzag encoded varint
 * FLOAT    |           0 1 0 |                 | float
 * DOUBLE   |           0 1 1 |                 | double
 * LDOUBLE  |           1 0 0 |                 | long double
 * UNDEF    |           1 0 1 | -               | undef
 * STR      |           1 1 0 | varint          | string, whatever, varint=length
 * STR_UTF8 |           1 1 1 | varint          | string, utf8, varint=length
 *          |                 |                 |
 *          |       0 1 X X X |                 | Ref/Object(ish)
 * REF      |           0 0 0 | varint          | scalar ref - if varint is 0 then it is to the next item, if otherwise it is to a
 *          |                 |                 | previously emitted scalar (which will be tagged).
 * REUSE    |           0 0 1 | varint          | second/third/... occurrence of a multiply-occurring
 *          |                 |                 | substructure (always points at a form of reference)
 * HASH     |           0 1 0 | nkeys V/K*      | hash, nkeys=varint, contents are in VALUE, KEY tuples
 * ARRAY    |           0 1 1 | varint V*       | array, varint=length
 * OBJECT   |           1 0 0 | TAG(STR) TAG    | item that is instance of class indicated by TAG
 * OBJECTV  |           1 0 1 | varint   TAG    | item that is instance of class indicated by varint *provisional*
 * ALIAS    |           1 1 0 | varint          | alias to previous item indicated by varint
 * COPY     |           1 1 1 | varint          | copy item at offset
 * 
 *          |       1 0 y y y |                 | Miscellaneous
 * EXTEND   |           0 0 0 | tbyte           | tbyte indicates action.
 * LIST     |           0 0 1 | tbyte vint pad  | numeric array (s=0 unsigned, s=1 signed), varint=length, pad if needed for alignment
 * WEAKEN   |           0 1 0 |                 | Following item is a reference and it is weakened
 * REGEXP   |           0 1 1 | TAG             | next item is a regexp
 * PAD      |           1 0 0 | -               | ignored byte, used by encoder to pad if necessary
 *          |                 |                 |
 * RESERVED |           1 0 1 | varint          |
 * RESERVED |           1 1 1 | varint          |
 * RESERVED |       1 1 x x x | varint          | *reserved*
 * ---------+-----------------+-----------------+---------------------------------------------------------------------------------------
 * ASCII    | F 1 x x x x x x | str             | Short ascii string, x=length
 * 
 * 
 * 
 * F = Flag bit to indicate if the item needs to be tracked during deserialization.
 *     The offset of the tag byte should be remembered, so that it can be referenced
 *     later.
 * 
 * * Dealing with self referential and cyclic structures:
 * While dumping any item with a refcount>1 (including weakrefs) the offset of the tag
 * needs to be tracked. The items F flag is NOT set. Should the item later be encountered
 * during dumping an alias or ref item will be generated with the offset in a varint, and
 * the F flag will be set. 
 * 
 * * Handling objects
 * During dumping the dumper is expected to maintain a mapping of class name to id. Whenever
 * it encounters a new class name it emits a "declare class tag" and then emits the appropriate
 * ref tag with the "is class" bit set.
 *    
 * * Varints
 * Varints are variable length integers where the high bit of each segment (normally a byte
 * but in some cases less) indicates if there is another byte to follow, with the bytes in 
 * least significant order first.
 *
 *
 * TODO: FALSE?
 * TODO: What's with floats?
 */

/* protocol version */
#define SRL_PROTOCOL_VERSION 1
#define SRL_PROTOCOL_VERSION_MASK (~(16 + 32 + 64 + 128))

/* Flag bits in the protocol-version & flags byte of the header */
#define SRL_F_SNAPPY (1<<4) /* 5th bit */

/* Useful constants */
/* See also range constants below for the header byte */
#define SRL_POS_MAX_SIZE           15
#define SRL_NEG_MIN_SIZE           16

/* All constants have the F bit (SRL_HDR_TRACK_FLAG) unset! */
/* _LOW and _HIGH versions refering to INCLUSIVE range boundaries */


#define SRL_HDR_POS             ((char)0)       /* small positive integer - value in low 4 bits (identity) */
#define SRL_HDR_POS_LOW         ((char)0)       /* small positive integer - value in low 4 bits (identity) */
#define SRL_HDR_POS_HIGH        ((char)15)      /* small positive integer - value in low 4 bits (identity) */

#define SRL_HDR_NEG             ((char)16)      /* small negative integer - value in low 4 bits (k+32) */
#define SRL_HDR_NEG_LOW         ((char)16)      /* small negative integer - value in low 4 bits (k+32) */
#define SRL_HDR_NEG_HIGH        ((char)31)      /* small negative integer - value in low 4 bits (k+32) */

#define SRL_HDR_VARINT          ((char)32)      /* <VARINT> - Varint variable length integer */
#define SRL_HDR_ZIGZAG          ((char)33)      /* <ZIGZAG-VARINT> - Zigzag variable length integer */
#define SRL_HDR_FLOAT           ((char)34)      /* <IEEE-FLOAT> */
#define SRL_HDR_DOUBLE          ((char)35)      /* <IEEE-DOUBLE> */
#define SRL_HDR_LONG_DOUBLE     ((char)36)      /* <IEEE-LONG-DOUBLE> */
#define SRL_HDR_UNDEF           ((char)37)      /* None - Perl undef */
#define SRL_HDR_STRING          ((char)38)      /* <LEN-VARINT> <BYTES> - binary/(latin1) string */
#define SRL_HDR_STRING_UTF8     ((char)39)      /* <LEN-VARINT> <UTF8> - utf8 string */

#define SRL_HDR_REFN            ((char)40)      /* <ITEM-TAG>    - ref to next item */
#define SRL_HDR_REFP            ((char)41)      /* <OFFSET-VARINT> - ref to previous item stored at offset */
#define SRL_HDR_HASH            ((char)42)      /* <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs */
#define SRL_HDR_ARRAY           ((char)43)      /* <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items */
#define SRL_HDR_OBJECT          ((char)44)      /* <STR-TAG> <ITEM-TAG> - class, object-item */
#define SRL_HDR_OBJECTV         ((char)45)      /* <OFFSET-VARINT> <ITEM-TAG> - class name at offset - object-item */
#define SRL_HDR_ALIAS           ((char)46)      /* <OFFSET-VARINT> - alias to item defined at offset */
#define SRL_HDR_COPY            ((char)47)      /* <OFFSET-VARINT> - copy of item defined at offset */

#define SRL_HDR_WEAKEN          ((char)48)      /* <REF-TAG> - Weaken the following reference */
#define SRL_HDR_REGEXP          ((char)49)      /* <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>*/

/* Note: Can do reserved check with a range now, but as we start using
 *       them, might have to explicit == check later. */
#define SRL_HDR_INT1            ((char)50)      /* <BYTE>  - one byte integer   #proposed# */
#define SRL_HDR_INT2            ((char)51)      /* <BYTES> - two byte integer   #proposed# */
#define SRL_HDR_INT3            ((char)52)      /* <BYTES> - three byte integer #proposed# */
#define SRL_HDR_INT4            ((char)53)      /* <BYTES> - four byte integer  #proposed# */

#define SRL_HDR_UINT1           ((char)54)      /* <BYTE>  - one byte unsigned integer   #proposed# */
#define SRL_HDR_UINT2           ((char)55)      /* <BYTES> - two byte unsigned integer   #proposed# */
#define SRL_HDR_UINT3           ((char)56)      /* <BYTES> - three byte unsigned integer #proposed# */
#define SRL_HDR_UINT4           ((char)57)      /* <BYTES> - four byte unsigned integer  #proposed# */

#define SRL_HDR_FALSE           ((char)58)      /* false (PL_sv_no)  */
#define SRL_HDR_TRUE            ((char)59)      /* true  (PL_sv_yes) */

#define SRL_HDR_REPEATED        ((char)60)      /* <LEN-VARINT> <TAG-BYTE> <TAG-DATA> - repeated tag (unimplemented) */
#define SRL_HDR_PACKET_START    ((char)61)      /* (first byte of magic string in header) */

#define SRL_MAGIC_STRING         "=srl"         /* Magic string for header. Every packet starts with this */
#define SRL_MAGIC_STRING_LILIPUTIAN 0x6c72733d  /* SRL_MAGIC_STRING as a little endian integer */

#define SRL_HDR_EXTEND          ((char)62)      /* <BYTE> - for additional tags */
#define SRL_HDR_PAD             ((char)63)      /* (ignored tag, skip to next byte) */

/* Note: Can do reserved check with a range now, but as we start using
 *       them, might have to explicit == check later. */
#define SRL_HDR_ARRAYREF        ((char)64)      /* [<ITEM-TAG> ...] - count of itmes in low 4 bits (ARRAY must be refcnt=1)*/
#define SRL_MASK_ARRAYREF_COUNT ((char)15)      /* mask to get low bits from tag */
#define SRL_HDR_ARRAYREF_LOW    ((char)64)
#define SRL_HDR_ARRAYREF_HIGH   ((char)79)


#define SRL_HDR_HASHREF         ((char)80)      /* [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt= 1)*/
#define SRL_MASK_HASHREF_COUNT  ((char)15)      /* mask to get low bits from tag */
#define SRL_HDR_HASHREF_LOW     ((char)80)
#define SRL_HDR_HASHREF_HIGH    ((char)95)

#define SRL_HDR_ASCII           ((char)96)      /* <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
#define SRL_HDR_ASCII_LOW       ((char)96)
#define SRL_HDR_ASCII_HIGH      ((char)127)
#define SRL_MASK_ASCII_LEN      ((char)31)      /* mask to get length of SRL_HDR_ASCII type tags */

#define SRL_HDR_TRACK_FLAG      ((char)128)         /* if this bit is set track the item */

/* TODO */

#define SRL_SET_FBIT(where) ((where) |= SRL_HDR_TRACK_FLAG)

#endif
