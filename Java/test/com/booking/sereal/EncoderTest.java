package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
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
		return new Encoder(new EncoderOptions().protocolVersion(3));
	}

	private Encoder v4Encoder() {
		return new Encoder(new EncoderOptions().protocolVersion(4));
	}

	private int getMagic(byte[] data) {
		return ((data[0] & 0xff) << 24) +
		       ((data[1] & 0xff) << 16) +
		       ((data[2] & 0xff) <<  8) +
		       ((data[3] & 0xff) <<  0);
	}

	@Before
	public void setup() {
		encoder = null;
	}

	@Test
	public void headerV1() throws SerealException {
		encoder = v1Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, getMagic(data) );
		assertEquals( "Protocol version fail", 1, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

	}

	@Test
	public void headerV2() throws SerealException {
		encoder = v2Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, getMagic(data) );
		assertEquals( "Protocol version fail", 2, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

	}

	@Test
	public void headerV3() throws SerealException {
		encoder = v3Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC_V3, getMagic(data) );
		assertEquals( "Protocol version fail", 3, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

	}

	@Test
	public void headerV4() throws SerealException {
		encoder = v4Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC_V3, getMagic(data) );
		assertEquals( "Protocol version fail", 4, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

	}

	@Test
	public void shortBinaryV1() throws SerealException {
		encoder = v1Encoder();

		encoder.write( new Latin1String("foo").getBytes() );

		byte[] data = encoder.getData();
		byte[] short_binary = Arrays.copyOfRange(data, 6, 10);
		assertArrayEquals( "Short binary encoding fail", new byte[] { 0x63, 0x66, 0x6f, 0x6f }, short_binary );

	}

	@Test
	public void shortBinaryV2() throws SerealException {
		encoder = v2Encoder();

		encoder.write( new Latin1String("foo").getBytes() );

		byte[] data = encoder.getData();
		byte[] short_binary = Arrays.copyOfRange(data, 6, 10);
		assertArrayEquals( "Short binary encoding fail", new byte[] { 0x63, 0x66, 0x6f, 0x6f }, short_binary );

	}

	@Test
	public void allTypes() throws SerealException {
		encoder = defaultEncoder();

		encoder.write( new byte[] { 0x66, 0x6f, 0x6f } );
		encoder.write( Pattern.compile( "(?:foo)[0-9]{3}\\z", Pattern.CASE_INSENSITIVE ) );
		encoder.write( new Latin1String("Hello, Sereal!").getBytes() );
		encoder.write( 2395846 );
		encoder.write( -345 );

		encoder.getData();
	}

	@Test
	public void bytearrayCopyV1() throws SerealException {
		encoder = v1Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
		});

		byte[] data = encoder.getData();

		// should end with 0x2f09 (x2) (0x2f is copy tag, 0x09 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0100282b037b546869732069732071756974652061206c6f6e6720737472696e672f092f09",
				Utils.hexStringFromByteArray( data ) );
	}

	@Test
	public void bytearrayCopyV2() throws SerealException {
		encoder = v2Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
			new Latin1String("This is quite a long string").getBytes(),
		});

		byte[] data = encoder.getData();

		// should end with 0x2f04 (x2) (0x2f is copy tag, 0x04 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0200282b037b546869732069732071756974652061206c6f6e6720737472696e672f042f04",
				Utils.hexStringFromByteArray( data ) );
	}

	@Test
	public void stringCopyV1() throws SerealException {
		encoder = v1Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new String("This is quite a long string"),
			new String("This is quite a long string"),
			new String("This is quite a long string"),
		});

		byte[] data = encoder.getData();

		// should end with 0x2f09 (x2) (0x2f is copy tag, 0x09 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0100282b03271b546869732069732071756974652061206c6f6e6720737472696e672f092f09",
				Utils.hexStringFromByteArray( data ) );
	}

	@Test
	public void stringCopyV2() throws SerealException {
		encoder = v2Encoder();

		// write 3 copies of a string (that should be copied)
		encoder.write( new Object[] {
			new String("This is quite a long string"),
			new String("This is quite a long string"),
			new String("This is quite a long string"),
		});

		byte[] data = encoder.getData();

		// should end with 0x2f04 (x2) (0x2f is copy tag, 0x04 is varint encoded offset of copy)
		assertEquals( "String was not copied", "0x3d73726c0200282b03271b546869732069732071756974652061206c6f6e6720737472696e672f042f04",
				Utils.hexStringFromByteArray( data ) );
	}

	@Test
	public void referencesV1() throws SerealException {
		encoder = v1Encoder();

		Boolean booleanValue = new Boolean(true);
		Integer integerValue = new Integer(12);
		byte[] bytesValue = new byte[] { 0x66, 0x6f, 0x6f };
		Map<String, Object> mapValue = new HashMap<String, Object>();
		Object[] arrayValue = new Object[0];
		String stringValue = "foo";
		byte[] data;

		data = encoder.write(new Object[] {booleanValue, booleanValue}).getData();
		assertEquals("0x3d73726c0100282b023b3b",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {integerValue, integerValue}).getData();
		assertEquals("0x3d73726c0100282b020c0c",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {bytesValue, bytesValue}).getData();
		assertEquals("0x3d73726c0100282b0263666f6f2f09",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {stringValue, stringValue}).getData();
		assertEquals("0x3d73726c0100282b022703666f6f2f09",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {mapValue, mapValue}).getData();
		assertEquals("0x3d73726c0100282b0228aa00290a",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {arrayValue, arrayValue}).getData();
		assertEquals("0x3d73726c0100282b0228ab00290a",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(makeList(integerValue, integerValue)).getData();
		assertEquals("0x3d73726c0100282b020c0c",
			     Utils.hexStringFromByteArray(data));
	}

	@Test
	public void referencesV2() throws SerealException {
		encoder = v2Encoder();

		Boolean booleanValue = new Boolean(true);
		Integer integerValue = new Integer(12);
		byte[] bytesValue = new byte[] { 0x66, 0x6f, 0x6f };
		Map<String, Object> mapValue = new HashMap<String, Object>();
		Object[] arrayValue = new Object[0];
		String stringValue = "foo";
		byte[] data;

		data = encoder.write(new Object[] {booleanValue, booleanValue}).getData();
		assertEquals("0x3d73726c0200282b023b3b",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {integerValue, integerValue}).getData();
		assertEquals("0x3d73726c0200282b020c0c",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {bytesValue, bytesValue}).getData();
		assertEquals("0x3d73726c0200282b0263666f6f2f04",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {stringValue, stringValue}).getData();
		assertEquals("0x3d73726c0200282b022703666f6f2f04",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {mapValue, mapValue}).getData();
		assertEquals("0x3d73726c0200282b0228aa002905",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {arrayValue, arrayValue}).getData();
		assertEquals("0x3d73726c0200282b0228ab002905",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(makeList(integerValue, integerValue)).getData();
		assertEquals("0x3d73726c0200282b020c0c",
			     Utils.hexStringFromByteArray(data));
	}

	@Test
	public void objects() throws SerealException {
		encoder = defaultEncoder();

		PerlObject fooObject1 = new PerlObject("Foo", makeMap("A", 7));
		PerlObject fooObject2 = new PerlObject("Foo", makeMap("B", 8));
		byte[] data;

		data = encoder.write(new Object[] {fooObject1}).getData();
		assertEquals("0x3df3726c0400282b012c2703466f6f282a0127014107",
			     Utils.hexStringFromByteArray(data));

		data = encoder.write(new Object[] {fooObject1, fooObject2}).getData();
		assertEquals("0x3df3726c0400282b022c2703466f6f282a01270141072d05282a0127014208",
			     Utils.hexStringFromByteArray(data));
	}

	private List<Object> makeList(Object... items) {
		return new ArrayList<Object>(Arrays.asList(items));
	}

	private Map<String, Object> makeMap(Object... items) {
		Map<String, Object> res = new HashMap<String, Object>();

		for (int i = 0; i < items.length; i += 2)
			res.put((String) items[i], items[i + 1]);

		return res;
	}
}
