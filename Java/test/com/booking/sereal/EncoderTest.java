package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.nio.ByteBuffer;
import java.util.regex.Pattern;

import org.junit.Before;
import org.junit.Test;

public class EncoderTest {

	private Encoder encoder;

	@Before
	public void setup() {
		encoder = new Encoder( null );
	}

	@Test
	public void header() {

		ByteBuffer data = encoder.getData();

		assertEquals( "Minimal header size incorrect", 6, data.limit() );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, data.getInt() );
		assertEquals( "Protocol version fail", 1, data.get() );
		assertEquals( "Header suffix not 0", 0, data.get() ); // is a varint, but should be 0

	}

	@Test
	public void short_binary() {

		try {
			encoder.write_short_binary( "foo" );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

		ByteBuffer data = encoder.getData();
		data.position( 6 ); // advance over the header

		byte[] short_binary = new byte[4]; // 1 for the tag, then 3 bytes
		data.get( short_binary );
		assertArrayEquals( "Short binary encoding fail", new byte[] { 0x63, 0x66, 0x6f, 0x6f }, short_binary );

	}

	@Test
	public void allTypes() {

		try {

			encoder.write_bytearray( new byte[] { 0x66, 0x6f, 0x6f } );
			encoder.write_regex( Pattern.compile( "(?:foo)[0-9]{3}\\z", Pattern.CASE_INSENSITIVE ) );
			encoder.write_short_binary( "Hello, Sereal!" );
			encoder.write_varint( 2395846 );
			encoder.write_zigzag( -345 );

			encoder.getData();

		} catch (SerealException e) {
			fail( e.getMessage() );
		}

	}

	@Test
	public void copy() {

		// write 3 copies of a string (that should be copied)
		try {
			encoder.write_short_binary( "This is quite a long string" );
			encoder.write_short_binary( "This is quite a long string" );
			encoder.write_short_binary( "This is quite a long string" );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

		ByteBuffer data = encoder.getData();
		
		// should end with 0x2f06 (x2) (0x26 is copy tag, 0x06 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c01007b546869732069732071756974652061206c6f6e6720737472696e672f062f06",
				Utils.hexStringFromByteArray( data.array() ) );
		
	}
}
