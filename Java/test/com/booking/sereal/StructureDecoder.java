package com.booking.sereal;

import static com.booking.sereal.SerealHeader.SRL_HDR_ALIAS;
import static com.booking.sereal.SerealHeader.SRL_HDR_ARRAY;
import static com.booking.sereal.SerealHeader.SRL_HDR_ARRAYREF;
import static com.booking.sereal.SerealHeader.SRL_HDR_BINARY;
import static com.booking.sereal.SerealHeader.SRL_HDR_COPY;
import static com.booking.sereal.SerealHeader.SRL_HDR_DOUBLE;
import static com.booking.sereal.SerealHeader.SRL_HDR_FALSE;
import static com.booking.sereal.SerealHeader.SRL_HDR_HASH;
import static com.booking.sereal.SerealHeader.SRL_HDR_HASHREF;
import static com.booking.sereal.SerealHeader.SRL_HDR_NEG_HIGH;
import static com.booking.sereal.SerealHeader.SRL_HDR_OBJECT;
import static com.booking.sereal.SerealHeader.SRL_HDR_OBJECTV;
import static com.booking.sereal.SerealHeader.SRL_HDR_PAD;
import static com.booking.sereal.SerealHeader.SRL_HDR_POS_HIGH;
import static com.booking.sereal.SerealHeader.SRL_HDR_REFN;
import static com.booking.sereal.SerealHeader.SRL_HDR_REFP;
import static com.booking.sereal.SerealHeader.SRL_HDR_REGEXP;
import static com.booking.sereal.SerealHeader.SRL_HDR_SHORT_BINARY_LOW;
import static com.booking.sereal.SerealHeader.SRL_HDR_STR_UTF8;
import static com.booking.sereal.SerealHeader.SRL_HDR_TRACK_FLAG;
import static com.booking.sereal.SerealHeader.SRL_HDR_TRUE;
import static com.booking.sereal.SerealHeader.SRL_HDR_UNDEF;
import static com.booking.sereal.SerealHeader.SRL_HDR_VARINT;
import static com.booking.sereal.SerealHeader.SRL_HDR_WEAKEN;
import static com.booking.sereal.SerealHeader.SRL_HDR_ZIGZAG;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Map;

public class StructureDecoder {
	private ByteBuffer buf;
	private StringBuilder sb;
	private String[] tag2name;
	private boolean perlRefs;
	private boolean preservePadding;

	public Object decodeFile(File f) throws SerealException, IOException {

		if( !f.exists() ) {
			throw new FileNotFoundException( "No such file: " + f.getCanonicalPath() );
		}

		// read everything
		int size = (int) f.length(); // yeah yeah truncate
		buf = ByteBuffer.allocate( size );
		new FileInputStream( f ).getChannel().read( buf );

		String structure = decode();

		return structure;
	}

	private String decode() {

		sb = new StringBuilder();

		sb.append("Bytes: " + Utils.hexStringFromByteArray( buf.array(), 4 ));
		sb.append("\n");


		buf.position( 6 );

		sb.append( "HEADER" );

		read();

		return sb.toString();
	}

	private void read() {

		byte tag = buf.get();

		String hex = Utils.hexStringFromByteArray( new byte[] { tag } );
		sb.append( ", " + hex + "=" );

		int track = 0;
		if( (tag & SRL_HDR_TRACK_FLAG) != 0 ) {
			tag = (byte) (tag & ~SRL_HDR_TRACK_FLAG);
			sb.append( "track|" );
		}

		if( tag <= SRL_HDR_POS_HIGH ) {
			sb.append( "pos: " + tag );
		} else if( tag <= SRL_HDR_NEG_HIGH ) {
			sb.append( "neg: " + (tag - 32) );
		} else if( (tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW ) {
			sb.append("short_binary");
			read_short_binary( tag );
		} else if( (tag & SRL_HDR_HASHREF) == SRL_HDR_HASHREF ) {
			sb.append("hashref");
			read_hash( tag );
		} else if( (tag & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF ) {
			sb.append( "arrayref" );
			read_array( tag );
		} else {
			switch (tag) {
			case SRL_HDR_VARINT:
				sb.append("varint");
				read_varint();
				break;
			case SRL_HDR_ZIGZAG:
				sb.append("zigzag");
				read_zigzag();
				break;
			case SRL_HDR_DOUBLE:
				sb.append("double");
				buf.getDouble();
				break;
			case SRL_HDR_TRUE:
				sb.append("true");
				break;
			case SRL_HDR_FALSE:
				sb.append("false");
				break;
			case SRL_HDR_UNDEF:
				sb.append("undef");
				break;
			case SRL_HDR_BINARY:
				sb.append("bytes");
				byte[] bytes = read_binary();
				break;
			case SRL_HDR_STR_UTF8:
				sb.append("UTF8");
				read_UTF8();
				break;
			case SRL_HDR_REFN:
				sb.append( "refn" );
				read();
				break;
			case SRL_HDR_REFP:
				sb.append("refp to " + read_varint());
				break;
			case SRL_HDR_OBJECT:
				sb.append("object");
				read_object();
				break;
			case SRL_HDR_OBJECTV:
				sb.append("objectv at " + read_varint()); // offset to classname!
				read(); // actual item
				break;
			case SRL_HDR_COPY:
				sb.append("copy offset=" + read_varint());
				break;
			case SRL_HDR_ALIAS:
				sb.append("alias offset=" + read_varint());
				break;
			case SRL_HDR_WEAKEN:
				sb.append("weaken");
				read();
				break;
			case SRL_HDR_HASH:
				sb.append("hash");
				read_hash( (byte) 0 );
				break;
			case SRL_HDR_ARRAY:
				sb.append( "array" );
				read_array( (byte) 0 );
				break;
			case SRL_HDR_REGEXP:
				sb.append("regex");
				read_regex();
				break;
			case SRL_HDR_PAD:
				sb.append("pad");
				break;
			default:
				sb.append("UNKNOWN!");
			}
		}

		if( track != 0 ) {
			sb.append( "|track" );
		}

	}

	private void read_regex() {
		read(); // string pattern
		sb.append("=pattern");
		read_short_binary( buf.get() ); // modifiers
		sb.append("=modifiers");
	}

	private Object read_copy() {
		// TODO Auto-generated method stub
		return null;
	}

	private void read_object() {
		read(); //string name
		read(); // data;
	}

	private void track_stuff(int track, PerlReference refn) {
		// TODO Auto-generated method stub

	}

	private String read_UTF8() {
		// TODO Auto-generated method stub
		return null;
	}

	private byte[] read_binary() {
		// TODO Auto-generated method stub
		return null;
	}

	private long read_zigzag() {
		// TODO Auto-generated method stub
		return 0;
	}

	long read_varint() {

		long uv = 0;
		int lshift = 0;

		byte b = buf.get();
		while( buf.hasRemaining() && (b < 0) ) {
			uv |= (b & 127) << lshift; // add 7 bits
			lshift += 7;
			b = buf.get();
		}
		uv |= b << lshift; // add final (or first if there is only 1)

		return uv;

	}

	private void read_array(byte tag) {
		int length = 0;
		if( tag == 0 ) {
			length = (int) read_varint();
		} else {
			length = tag & 15;
		}
		sb.append( " count=" + length );
		for(int i=0; i<length; i++) {
			sb.append(" item: " + i);
			read();
		}
	}

	private Map<String, Object> read_hash(byte tag) {
		// TODO Auto-generated method stub
		return null;
	}

	private void read_short_binary(byte tag) {
		int length = tag & SerealHeader.SRL_MASK_SHORT_BINARY_LEN;
		sb.append(" length="+length );
		buf.position( buf.position() + length );
	}
}
