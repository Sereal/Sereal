package com.booking.sereal;

public interface SerealHeader {

	// 0x6c72733d but little endian for some reason
	static final int MAGIC = (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24);

	static final byte SRL_HDR_POS_HIGH = (byte) 15;
	static final byte SRL_HDR_NEG_HIGH = (byte) 31;
	static final byte SRL_HDR_VARINT = (byte) 32;
	static final byte SRL_HDR_DOUBLE = (byte) 35;
	static final byte SRL_HDR_UNDEF = (byte) 37;
	static final byte SRL_HDR_BINARY = (byte) 38;
	static final byte SRL_HDR_REFN = (byte) 40;
	static final byte SRL_HDR_REFP = (byte) 41;
	static final byte SRL_HDR_HASH = (byte) 42;
	static final byte SRL_HDR_ARRAY = (byte) 43;
	static final byte SRL_HDR_OBJECT = (byte) 44;
	static final byte SRL_HDR_ALIAS = (byte) 46;
	static final byte SRL_HDR_REGEXP = (byte) 49;
	static final byte SRL_HDR_ARRAYREF = (byte) 64;
	static final byte SRL_HDR_HASHREF = (byte) 80;
	static final byte SRL_HDR_SHORT_BINARY_LOW = (byte) 96;

	static final byte SRL_HDR_TRACK_FLAG = (byte) 128;

	static final byte SRL_MASK_SHORT_BINARY_LEN = (byte) 31; // lower 5 bits
}