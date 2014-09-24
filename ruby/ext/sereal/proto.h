#define SRL_MAGIC_STRING                "=srl"          /* Magic string for header. Every packet starts with this */
#define SRL_MAGIC_STRING_UINT_LE     0x6c72733d      /* SRL_MAGIC_STRING as a little endian integer */
#define SRL_MAGIC_STRING_UINT_LE_HB  0x6c72f33d      /* "=\xF3rl" */

#define SRL_PROTOCOL_VERSION            ( 2 )           /* this is the first. for some reason we did not use 0 */
#define SRL_PROTOCOL_VERSION_BITS       ( 4 )           /* how many bits we use for the version, the rest go to the encoding */
#define SRL_PROTOCOL_VERSION_MASK       ( ( 1 << SRL_PROTOCOL_VERSION_BITS ) - 1 )

#define SRL_PROTOCOL_ENCODING_MASK      ( ~SRL_PROTOCOL_VERSION_MASK )
#define SRL_PROTOCOL_ENCODING_RAW         ( 0 << SRL_PROTOCOL_VERSION_BITS )
#define SRL_PROTOCOL_ENCODING_SNAPPY      ( 1 << SRL_PROTOCOL_VERSION_BITS )
#define SRL_PROTOCOL_ENCODING_SNAPPY_INCR ( 2 << SRL_PROTOCOL_VERSION_BITS )

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
#define SRL_HDR_BINARY          ((char)38)      /* <LEN-VARINT> <BYTES> - binary/(latin1) string */
#define SRL_HDR_STR_UTF8        ((char)39)      /* <LEN-VARINT> <UTF8> - utf8 string */

#define SRL_HDR_REFN            ((char)40)      /* <ITEM-TAG>    - ref to next item */
#define SRL_HDR_REFP            ((char)41)      /* <OFFSET-VARINT> - ref to previous item stored at offset */
#define SRL_HDR_HASH            ((char)42)      /* <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs */
#define SRL_HDR_ARRAY           ((char)43)      /* <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items */
#define SRL_HDR_OBJECT          ((char)44)      /* <STR-TAG> <ITEM-TAG> - class, object-item */
#define SRL_HDR_OBJECTV         ((char)45)      /* <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item */

#define SRL_HDR_ALIAS           ((char)46)      /* <OFFSET-VARINT> - alias to item defined at offset */
#define SRL_HDR_COPY            ((char)47)      /* <OFFSET-VARINT> - copy of item defined at offset */

#define SRL_HDR_WEAKEN          ((char)48)      /* <REF-TAG> - Weaken the following reference */
#define SRL_HDR_REGEXP          ((char)49)      /* <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>*/
#define SRL_HDR_OBJECT_FREEZE   ((char)50)      /* <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding */
#define SRL_HDR_OBJECTV_FREEZE  ((char)51)      /* <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT) */

/* Note: Can do reserved check with a range now, but as we start using
 *       them, might have to explicit == check later. */
#define SRL_HDR_RESERVED        ((char)52)      /* reserved */
#define SRL_HDR_RESERVED_LOW    ((char)52)
#define SRL_HDR_RESERVED_HIGH   ((char)57)

#define SRL_HDR_FALSE           ((char)58)      /* false (PL_sv_no)  */
#define SRL_HDR_TRUE            ((char)59)      /* true  (PL_sv_yes) */

#define SRL_HDR_MANY            ((char)60)      /* <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 2) */
#define SRL_HDR_PACKET_START    ((char)61)      /* (first byte of magic string in header) */


#define SRL_HDR_EXTEND          ((char)62)      /* <BYTE> - for additional tags */
#define SRL_HDR_PAD             ((char)63)      /* (ignored tag, skip to next byte) */
#define SRL_HDR_ARRAYREF        ((char)64)      /* [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)*/
#define SRL_MASK_ARRAYREF_COUNT ((char)15)      /* mask to get low bits from tag */
#define SRL_HDR_ARRAYREF_LOW    ((char)64)
#define SRL_HDR_ARRAYREF_HIGH   ((char)79)


#define SRL_HDR_HASHREF         ((char)80)      /* [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)*/
#define SRL_MASK_HASHREF_COUNT  ((char)15)      /* mask to get low bits from tag */
#define SRL_HDR_HASHREF_LOW     ((char)80)
#define SRL_HDR_HASHREF_HIGH    ((char)95)

#define SRL_HDR_SHORT_BINARY    ((char)96)      /* <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
#define SRL_HDR_SHORT_BINARY_LOW       ((char)96)
#define SRL_HDR_SHORT_BINARY_HIGH      ((char)127)
#define SRL_MASK_SHORT_BINARY_LEN      ((char)31)      /* mask to get length of SRL_HDR_SHORT_BINARY type tags */

#define SRL_HDR_TRACK_FLAG      ((char)128)         /* if this bit is set track the item */

