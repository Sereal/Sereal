package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.junit.Before;
import org.junit.Test;

public class EncoderTest {

	private Encoder encoder;

	private Encoder defaultEncoder() {
		return new Encoder();
	}

	private Encoder v1Encoder() {
		return new Encoder(new EncoderOptions().protocolVersion(1));
	}

	private Encoder v2Encoder() {
		return new Encoder(new EncoderOptions().protocolVersion(2));
	}

	private Encoder v3Encoder() {
		return new Encoder(new EncoderOptions().protocolVersion(2));
	}

	@Before
	public void setup() {
		encoder = null;
	}

	@Test
	public void headerV1() throws SerealException, IOException {
		encoder = v1Encoder();

		ByteBuffer data = encoder.write(0);

		data.rewind();
		assertEquals( "Minimal header size incorrect", 7, data.limit() );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, data.getInt() );
		assertEquals( "Protocol version fail", 1, data.get() );
		assertEquals( "Header suffix not 0", 0, data.get() ); // is a varint, but should be 0

	}

	@Test
	public void headerV2() throws SerealException, IOException {
		encoder = v2Encoder();

		ByteBuffer data = encoder.write(0);

		data.rewind();
		assertEquals( "Minimal header size incorrect", 7, data.limit() );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, data.getInt() );
		assertEquals( "Protocol version fail", 2, data.get() );
		assertEquals( "Header suffix not 0", 0, data.get() ); // is a varint, but should be 0

	}

	@Test
	public void headerV3() throws SerealException, IOException {
		encoder = v3Encoder();

		ByteBuffer data = encoder.write(0);

		data.rewind();
		assertEquals( "Minimal header size incorrect", 7, data.limit() );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, data.getInt() );
		assertEquals( "Protocol version fail", 2, data.get() );
		assertEquals( "Header suffix not 0", 0, data.get() ); // is a varint, but should be 0

	}

	@Test
	public void shortBinaryV1() throws SerealException, IOException {
		encoder = v1Encoder();

		encoder.write( new Latin1String("foo").getBytes() );

		ByteBuffer data = encoder.getData();
		data.position( 6 ); // advance over the header

		byte[] short_binary = new byte[4]; // 1 for the tag, then 3 bytes
		data.get( short_binary );
		assertArrayEquals( "Short binary encoding fail", new byte[] { 0x63, 0x66, 0x6f, 0x6f }, short_binary );

	}

	@Test
	public void shortBinaryV2() throws SerealException, IOException {
		encoder = v2Encoder();

		encoder.write( new Latin1String("foo").getBytes() );

		ByteBuffer data = encoder.getData();
		data.position( 6 ); // advance over the header

		byte[] short_binary = new byte[4]; // 1 for the tag, then 3 bytes
		data.get( short_binary );
		assertArrayEquals( "Short binary encoding fail", new byte[] { 0x63, 0x66, 0x6f, 0x6f }, short_binary );

	}

	@Test
	public void allTypes() throws SerealException, IOException {
		encoder = defaultEncoder();

		encoder.write( new byte[] { 0x66, 0x6f, 0x6f } );
		encoder.write( Pattern.compile( "(?:foo)[0-9]{3}\\z", Pattern.CASE_INSENSITIVE ) );
		encoder.write( new Latin1String("Hello, Sereal!").getBytes() );
		encoder.write( 2395846 );
		encoder.write( -345 );

		encoder.getData();
	}

	@Test
	public void bytearrayCopyV1() throws SerealException, IOException {
		encoder = v1Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
		});

		ByteBuffer data = encoder.getData();

		// should end with 0x2f09 (x2) (0x2f is copy tag, 0x09 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0100282b037b546869732069732071756974652061206c6f6e6720737472696e672f092f09",
				Utils.hexStringFromByteArray( data.array() ) );
	}

	@Test
	public void bytearrayCopyV2() throws SerealException, IOException {
		encoder = v2Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
		});

		ByteBuffer data = encoder.getData();

		// should end with 0x2f04 (x2) (0x2f is copy tag, 0x04 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0200282b037b546869732069732071756974652061206c6f6e6720737472696e672f042f04",
				Utils.hexStringFromByteArray( data.array() ) );
	}

	@Test
	public void stringCopyV1() throws SerealException, IOException {
		encoder = v1Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new String("This is quite a long string"),
			new String("This is quite a long string"),
			new String("This is quite a long string"),
		});

		ByteBuffer data = encoder.getData();

		// should end with 0x2f09 (x2) (0x2f is copy tag, 0x09 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0100282b03271b546869732069732071756974652061206c6f6e6720737472696e672f092f09",
				Utils.hexStringFromByteArray( data.array() ) );
	}

	@Test
	public void stringCopyV2() throws SerealException, IOException {
		encoder = v2Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new String("This is quite a long string"),
			new String("This is quite a long string"),
			new String("This is quite a long string"),
		});

		ByteBuffer data = encoder.getData();

		// should end with 0x2f04 (x2) (0x2f is copy tag, 0x04 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0200282b03271b546869732069732071756974652061206c6f6e6720737472696e672f042f04",
				Utils.hexStringFromByteArray( data.array() ) );
	}

	@Test
	public void referencesV1() throws SerealException, IOException {
		encoder = v1Encoder();

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

	@Test
	public void referencesV2() throws SerealException, IOException {
		encoder = v2Encoder();

		Boolean booleanValue = new Boolean(true);
		Integer integerValue = new Integer(12);
		byte[] bytesValue = new byte[] { 0x66, 0x6f, 0x6f };
		Map<String, Object> mapValue = new HashMap<String, Object>();
		Object[] arrayValue = new Object[0];
		String stringValue = "foo";
		ByteBuffer data;

		data = encoder.write(new Object[] {booleanValue, booleanValue});
		assertEquals("0x3d73726c0200282b023b3b",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {integerValue, integerValue});
		assertEquals("0x3d73726c0200282b020c0c",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {bytesValue, bytesValue});
		assertEquals("0x3d73726c0200282b0263666f6f2f04",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {stringValue, stringValue});
		assertEquals("0x3d73726c0200282b022703666f6f2f04",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {mapValue, mapValue});
		assertEquals("0x3d73726c0200282b0228aa002905",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();

		data = encoder.write(new Object[] {arrayValue, arrayValue});
		assertEquals("0x3d73726c0200282b0228ab002905",
			     Utils.hexStringFromByteArray(data.array()));
		encoder.reset();
	}
}
