package com.booking.sereal;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * WIP
 * Functions for encoding various things. 
 *
 */
public class Encoder {

	private byte[] varint_buf = new byte[12];

	/**
	 * Encode an integer as a varint
	 * @param n positive integer
	 * @return
	 */
	byte[] write_varint(int n) {

		int length = 0;

		while( n > 127 ) {
			varint_buf[length++] = (byte) ((n & 127) | 128);
			n >>= 7;
		}
		varint_buf[length++] = (byte) n;

		return Arrays.copyOf( varint_buf, length );
	}

	/**
	 * Encode a short ascii string 
	 * @param s String to encode as US-ASCII bytes
	 * @return
	 * @throws SerealException if the string is not short enough
	 */
	byte[] write_short_binary(String s) throws SerealException {

		int length = s.length();

		if( length > 31 ) {
			throw new SerealException( "Cannot create short binary for " + s + ": too long" );
		}

		// 0 reserves space for the length byte
		byte[] out = Charset.forName( "US-ASCII" ).encode( 0 + s ).array();
		// length of string
		out[0] = (byte) (length | SerealHeader.SRL_HDR_SHORT_BINARY);

		return out;
	}

	/**
	 * Encode a regex
	 * @param p regex pattern. Only support flags "smix": DOTALL | MULTILINE | CASE_INSENSITIVE | COMMENTS
	 * @return
	 * @throws SerealException if the pattern is longer that a short binary string
	 */
	byte[] write_regex(Pattern p) throws SerealException {

		String flags = "";
		flags += (p.flags() & Pattern.MULTILINE) != 0 ? "m" : "";
		flags += (p.flags() & Pattern.DOTALL) != 0 ? "s" : "";
		flags += (p.flags() & Pattern.CASE_INSENSITIVE) != 0 ? "i" : "";
		flags += (p.flags() & Pattern.COMMENTS) != 0 ? "x" : "";

		String pattern = p.pattern();

		int length = pattern.length();
		if( length < 32 ) {

			// make array with bytes for (pattern + pattern length tag) + space for flags length tag + flags
			byte[] bytes = Arrays.copyOf( write_short_binary( pattern ), (length + 1) + 1 + flags.length() );
			length++; // advance over the pattern length tag
			bytes[length++] = (byte) (flags.length() | SerealHeader.SRL_HDR_SHORT_BINARY);
			// copy in the flags
			for(byte b : flags.getBytes( Charset.forName( "US-ASCII" ) )) {
				bytes[length++] = b;
			}

			return bytes;

		}

		throw new SerealException( "Don't know how to write a pattern of length " + length );
	}

	/**
	 * Encodes a byte array
	 * @param in
	 * @return
	 */
	byte[] write_binary(byte[] in) {
		
		byte[] size = write_varint( in.length );		
		byte[] out = Arrays.copyOf( size, size.length + in.length );
		
		for(int pos = size.length; pos<out.length; pos++) {
			out[pos] = in[ pos - size.length];
		}
		
		return out;
	}
	
}
