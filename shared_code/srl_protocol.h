#ifndef SRL_PROTOCOL_H_
#define SRL_PROTOCOL_H_

/*
 * Protocol version 1, see constants below docs.
 *
 * +--------+-----------------+-------------+--------------------------------------------------------------------------
 *          |Bit              | follow      | Description
 *          |7 6 5 4 3 2 1 0  | bytes       | 
 * ----------------+----------+--------------------------------------------------------------------------
 * ASCII    | F 0 x x x x x x | str         | Short ascii string, xxxxxx=length 
 * POS      | F 1 0 x x x x x | x...        | Positive varint 0 .. 2^5-1
 * NEG      | F 1 1 0 x x x x | x...        | Negative varint 2^4 .. -1
 * 
 *          | F 1 1 1 0 y y y |             | Ref/Object(ish)
 * REF      |         0 0 0 x | varint?     | ref, x=0 means to next item, x=1 means to the item indicated by varint
 * HASH     |         0 0 1 0 | varint      | hash, varint=length
 * ARRAY    |         0 0 1 1 | varint      | array, varint=length 
 * BLESS    |         0 1 0 0 | TAG(STR) TAG| bless item into class indicated by TAG
 *          |         0 1 0 1 |             | *reserved*
 *          |         0 1 1 x |             | *reserved*
 * 
 *          | F 1 1 1 1 y y y |             | Miscellaneous        
 * STRING   |         1 0 0 x | varint      | string, x= utf8 flag, varint=length
 * ALIAS    |         1 0 1 0 | varint      | alias to previous item indicated by varint
 * COPY     |         1 0 1 1 | varint      | copy item at offset
 * UNDEF    |         1 1 0 0 | -           | undef
 * REGEXP   |         1 1 0 1 | TAG         | next item is a regexp 
 * FLOAT    |         1 1 1 0 | (FLOAT)     | float
 * CONTINUE |         1 1 1 1 |             | INDICATES NEXT BYTE AS STATE BYTE
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
 * TODO: weakrefs?
 * TODO: Later versions: BLESSV
 * TODO: Later versions: list of packed ints/etc
 * TODO: FALSE?
 * TODO: Simply choose to require 64bit for floats (doubles)?
 */

/* Note: both indicating protocol version, keep in sync */
#define SRL_PROTOCOL_VERSION 1
#define SRL_MAGIC_STRING "srl\x01"

/* Useful constants */
/* See also range constants below for the header byte */
#define SRL_ASCII_SHORT_STRING_MAX_LEN (2 << 5) /* six bits */
#define SRL_POS_INT_MAX_SIZE           ((2 << 4) - 1) /* five bits */
#define SRL_NEG_INT_MAX_SIZE           ((2 << 3) - 1) /* four bits */

/* FIXME usefulness of the below are a bit unclear*/

/* All with F bit unset! */
/* _LOW and _HIGH versions refering to INCLUSIVE range boundaries */
#define SRL_HDR_ASCII_LOW   (0b00000000)
#define SRL_HDR_ASCII_HIGH  (0b01111111)
#define SRL_HDR_POS_LOW     (0b10000000)
#define SRL_HDR_POS_HIGH    (0b10111111)
#define SRL_HDR_NEG_LOW     (0b11000000)
#define SRL_HDR_NEG_HIGH    (0b11011111)

#define SRL_HDR_REF_FWD     (0b01110000) /* ref to next item */
#define SRL_HDR_REF         (0b01110000) /* ref with varint offset */
#define SRL_HDR_HASH        (0b01110010)
#define SRL_HDR_ARRAY       (0b01110011)
#define SRL_HDR_BLESS       (0b01110100)

#define SRL_HDR_STRING      (0b011111000)
#define SRL_HDR_STRING_UTF8 (0b011111001)
#define SRL_HDR_ALIAS       (0b011111010)
#define SRL_HDR_COPY        (0b011111011)
#define SRL_HDR_UNDEF       (0b011111100)
#define SRL_HDR_REGEXP      (0b011111101)
#define SRL_HDR_FLOAT       (0b011111110)

#define SRL_HDR_CONTINUE    (0b011111111)
 
/* TODO */

#endi
