package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
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
	public void header() throws SerealException {

		ByteBuffer data = encoder.write(0);

		data.rewind();
		assertEquals( "Minimal header size incorrect", 7, data.limit() );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, data.getInt() );
		assertEquals( "Protocol version fail", 1, data.get() );
		assertEquals( "Header suffix not 0", 0, data.get() ); // is a varint, but should be 0

	}

	@Test
	public void short_binary() {

		try {
			encoder.write( new Latin1String("foo").getBytes() );
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

			encoder.write( new byte[] { 0x66, 0x6f, 0x6f } );
			encoder.write( Pattern.compile( "(?:foo)[0-9]{3}\\z", Pattern.CASE_INSENSITIVE ) );
			encoder.write( new Latin1String("Hello, Sereal!").getBytes() );
			encoder.write( 2395846 );
			encoder.write( -345 );

			encoder.getData();

		} catch (SerealException e) {
			fail( e.getMessage() );
		}

	}

	@Test
	public void bytearrayCopy() {
		// write 3 copies of a string (that should be copied)
		try {
			encoder.write( new Object[] {
				new Latin1String("This is quite a long string").getBytes(),
				new Latin1String("This is quite a long string").getBytes(),
				new Latin1String("This is quite a long string").getBytes(),
			});
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

		ByteBuffer data = encoder.getData();

		// should end with 0x2f09 (x2) (0x2f is copy tag, 0x09 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0100282b037b546869732069732071756974652061206c6f6e6720737472696e672f092f09",
				Utils.hexStringFromByteArray( data.array() ) );
	}

	@Test
	public void stringCopy() {
		// write 3 copies of a string (that should be copied)
		try {
			encoder.write( new Object[] {
				new String("This is quite a long string"),
				new String("This is quite a long string"),
				new String("This is quite a long string"),
			});
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

		ByteBuffer data = encoder.getData();

		// should end with 0x2f09 (x2) (0x2f is copy tag, 0x09 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0100282b03271b546869732069732071756974652061206c6f6e6720737472696e672f092f09",
				Utils.hexStringFromByteArray( data.array() ) );
	}

	@Test
	public void references() throws SerealException {
		Boolean booleanValue = new Boolean(true);
		Integer integerValue = new Integer(12);
		byte[] bytesValue = new byte[] { 0x66, 0x6f, 0x6f };
		Map<String, Object> mapValue = new HashMap<String, Object>();
		Object[] arrayValue = new Object[0];
		String stringValue = "foo";
		ByteBuffer data;

		data = encoder.write(new Object[] {booleanValue, booleanValue});
		assertEquals("0x3d73726c0100282b023b3b",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {integerValue, integerValue});
		assertEquals("0x3d73726c0100282b020c0c",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {bytesValue, bytesValue});
		assertEquals("0x3d73726c0100282b0263666f6f2f09",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {stringValue, stringValue});
		assertEquals("0x3d73726c0100282b022703666f6f2f09",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {mapValue, mapValue});
		assertEquals("0x3d73726c0100282b0228aa00290a",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {arrayValue, arrayValue});
		assertEquals("0x3d73726c0100282b0228ab00290a",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();
	}
}
