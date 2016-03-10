package com.booking.sereal;

public interface SerealHeader {

	// 0x6c72733d but little endian for some reason
	static final int MAGIC = (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24);

	// 0x6c72f33d but little endian for some reason
	static final int MAGIC_V3 = (0x6c) + (0x72 << 8) + (0xf3 << 16) + (0x3d << 24);

	static final byte SRL_MASK_SHORT_BINARY_LEN = (byte) 31; // lower 5 bits
	
	
	/*
	Note: Despite this interface already being named SerealHeader we still use SRL_HDR_
			as a prefix so grepping will show both these and the C ones.
	
=for autoupdater start

* NOTE this section is autoupdated by author_tools/update_from_header.pl */
	static final byte SRL_HDR_POS               = (byte)   0; /*   0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
	static final byte SRL_HDR_POS_LOW           = (byte)   0; /*   0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
	static final byte SRL_HDR_POS_HIGH          = (byte)  15; /*  15 0x0f 0b00001111 small positive integer - value in low 4 bits (identity) */
	static final byte SRL_HDR_NEG               = (byte)  16; /*  16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
	static final byte SRL_HDR_NEG_LOW           = (byte)  16; /*  16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
	static final byte SRL_HDR_NEG_HIGH          = (byte)  31; /*  31 0x1f 0b00011111 small negative integer - value in low 4 bits (k+32) */
	static final byte SRL_HDR_VARINT            = (byte)  32; /*  32 0x20 0b00100000 <VARINT> - Varint variable length integer */
	static final byte SRL_HDR_ZIGZAG            = (byte)  33; /*  33 0x21 0b00100001 <ZIGZAG-VARINT> - Zigzag variable length integer */
	static final byte SRL_HDR_FLOAT             = (byte)  34; /*  34 0x22 0b00100010 <IEEE-FLOAT> */
	static final byte SRL_HDR_DOUBLE            = (byte)  35; /*  35 0x23 0b00100011 <IEEE-DOUBLE> */
	static final byte SRL_HDR_LONG_DOUBLE       = (byte)  36; /*  36 0x24 0b00100100 <IEEE-LONG-DOUBLE> */
	static final byte SRL_HDR_UNDEF             = (byte)  37; /*  37 0x25 0b00100101 None - Perl undef var; eg my $var= undef; */
	static final byte SRL_HDR_BINARY            = (byte)  38; /*  38 0x26 0b00100110 <LEN-VARINT> <BYTES> - binary/(latin1) string */
	static final byte SRL_HDR_STR_UTF8          = (byte)  39; /*  39 0x27 0b00100111 <LEN-VARINT> <UTF8> - utf8 string */
	static final byte SRL_HDR_REFN              = (byte)  40; /*  40 0x28 0b00101000 <ITEM-TAG>    - ref to next item */
	static final byte SRL_HDR_REFP              = (byte)  41; /*  41 0x29 0b00101001 <OFFSET-VARINT> - ref to previous item stored at offset */
	static final byte SRL_HDR_HASH              = (byte)  42; /*  42 0x2a 0b00101010 <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs */
	static final byte SRL_HDR_ARRAY             = (byte)  43; /*  43 0x2b 0b00101011 <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items */
	static final byte SRL_HDR_OBJECT            = (byte)  44; /*  44 0x2c 0b00101100 <STR-TAG> <ITEM-TAG> - class, object-item */
	static final byte SRL_HDR_OBJECTV           = (byte)  45; /*  45 0x2d 0b00101101 <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item */
	static final byte SRL_HDR_ALIAS             = (byte)  46; /*  46 0x2e 0b00101110 <OFFSET-VARINT> - alias to item defined at offset */
	static final byte SRL_HDR_COPY              = (byte)  47; /*  47 0x2f 0b00101111 <OFFSET-VARINT> - copy of item defined at offset */
	static final byte SRL_HDR_WEAKEN            = (byte)  48; /*  48 0x30 0b00110000 <REF-TAG> - Weaken the following reference */
	static final byte SRL_HDR_REGEXP            = (byte)  49; /*  49 0x31 0b00110001 <PATTERN-STR-TAG> <MODIFIERS-STR-TAG> */
	static final byte SRL_HDR_OBJECT_FREEZE     = (byte)  50; /*  50 0x32 0b00110010 <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding */
	static final byte SRL_HDR_OBJECTV_FREEZE    = (byte)  51; /*  51 0x33 0b00110011 <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT) */
	static final byte SRL_HDR_RESERVED          = (byte)  52; /*  52 0x34 0b00110100 reserved */
	static final byte SRL_HDR_RESERVED_LOW      = (byte)  52; /*  52 0x34 0b00110100 reserved */
	static final byte SRL_HDR_RESERVED_HIGH     = (byte)  56; /*  56 0x38 0b00111000 reserved */
	static final byte SRL_HDR_CANONICAL_UNDEF   = (byte)  57; /*  57 0x39 0b00111001 undef (PL_sv_undef) - "the" Perl undef (see notes) */
	static final byte SRL_HDR_FALSE             = (byte)  58; /*  58 0x3a 0b00111010 false (PL_sv_no) */
	static final byte SRL_HDR_TRUE              = (byte)  59; /*  59 0x3b 0b00111011 true  (PL_sv_yes) */
	static final byte SRL_HDR_MANY              = (byte)  60; /*  60 0x3c 0b00111100 <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3) */
	static final byte SRL_HDR_PACKET_START      = (byte)  61; /*  61 0x3d 0b00111101 (first byte of magic string in header) */
	static final byte SRL_HDR_EXTEND            = (byte)  62; /*  62 0x3e 0b00111110 <BYTE> - for additional tags */
	static final byte SRL_HDR_PAD               = (byte)  63; /*  63 0x3f 0b00111111 (ignored tag, skip to next byte) */
	static final byte SRL_HDR_ARRAYREF          = (byte)  64; /*  64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
	static final byte SRL_HDR_ARRAYREF_LOW      = (byte)  64; /*  64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
	static final byte SRL_HDR_ARRAYREF_HIGH     = (byte)  79; /*  79 0x4f 0b01001111 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
	static final byte SRL_HDR_HASHREF           = (byte)  80; /*  80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
	static final byte SRL_HDR_HASHREF_LOW       = (byte)  80; /*  80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
	static final byte SRL_HDR_HASHREF_HIGH      = (byte)  95; /*  95 0x5f 0b01011111 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
	static final byte SRL_HDR_SHORT_BINARY      = (byte)  96; /*  96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
	static final byte SRL_HDR_SHORT_BINARY_LOW  = (byte)  96; /*  96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
	static final byte SRL_HDR_SHORT_BINARY_HIGH = (byte) 127; /* 127 0x7f 0b01111111 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
	static final byte SRL_HDR_TRACK_FLAG        = (byte) 128; /* 128 0x80 0b10000000 if this bit is set track the item */
/*
* NOTE the above section is auto-updated by author_tools/update_from_header.pl

=for autoupdater stop
	*/
}


