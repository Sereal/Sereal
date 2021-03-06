package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.lang.ref.WeakReference;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.junit.Assert;
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

		encoder.close();
	}

	@Test
	public void headerV2() throws SerealException {
		encoder = v2Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC, getMagic(data) );
		assertEquals( "Protocol version fail", 2, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

		encoder.close();
	}

	@Test
	public void headerV3() throws SerealException {
		encoder = v3Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC_V3, getMagic(data) );
		assertEquals( "Protocol version fail", 3, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

		encoder.close();
	}

	@Test
	public void headerV4() throws SerealException {
		encoder = v4Encoder();

		byte[] data = encoder.write(0).getData();

		assertEquals( "Minimal header size incorrect", 7, data.length );
		assertEquals( "Header is not MAGIC", SerealHeader.MAGIC_V3, getMagic(data) );
		assertEquals( "Protocol version fail", 4, data[4] );
		assertEquals( "Header suffix not 0", 0, data[5] ); // is a varint, but should be 0

		encoder.close();
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

		encoder.close();
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

		Boolean booleanValue = Boolean.TRUE;
		Integer integerValue = Integer.valueOf(12);
		byte[] bytesValue = new byte[] { 0x66, 0x6f, 0x6f };
		Map<String, Object> mapValue = new HashMap<>();
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

		Boolean booleanValue = Boolean.TRUE;
		Integer integerValue = Integer.valueOf(12);
		byte[] bytesValue = new byte[] { 0x66, 0x6f, 0x6f };
		Map<String, Object> mapValue = new HashMap<>();
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

	@Test
	public void weakReferences() throws SerealException {
		String string = "abc";
		List<Object> emptyList = Collections.emptyList();

		encoder = defaultEncoder();

		byte[] data;

		data = encoder.write(new WeakReference<>(new PerlReference(string))).getData();
		assertEquals("0x3df3726c040030282703616263",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(new WeakReference<>(string)).getData();
		assertEquals("0x3df3726c040030282703616263",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(new WeakReference<>(emptyList)).getData();
		assertEquals("0x3df3726c040030282b00",
			Utils.hexStringFromByteArray(data));
	}

	@Test
	public void longInteger() throws SerealException {
		encoder = defaultEncoder();

		byte[] data;

		data = encoder.write(0L).getData();
		assertEquals("0x3df3726c040000",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(15L).getData();
		assertEquals("0x3df3726c04000f",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(16L).getData();
		assertEquals("0x3df3726c04002010",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(-16L).getData();
		assertEquals("0x3df3726c040010",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(-17L).getData();
		assertEquals("0x3df3726c04002121",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(Long.MIN_VALUE).getData();
		assertEquals("0x3df3726c040021ffffffffffffffffff01",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(Long.MAX_VALUE).getData();
		assertEquals("0x3df3726c040020ffffffffffffffff7f",
			Utils.hexStringFromByteArray(data));
	}

	@Test
	public void bigInteger() throws SerealException {
		encoder = defaultEncoder();

		byte[] data;

		data = encoder.write(BigInteger.valueOf(0)).getData();
		assertEquals("0x3df3726c040000",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(BigInteger.valueOf(15)).getData();
		assertEquals("0x3df3726c04000f",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(BigInteger.valueOf(16)).getData();
		assertEquals("0x3df3726c04002010",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(BigInteger.valueOf(-16)).getData();
		assertEquals("0x3df3726c040010",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(BigInteger.valueOf(-17)).getData();
		assertEquals("0x3df3726c04002121",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(BigInteger.valueOf(Long.MIN_VALUE)).getData();
		assertEquals("0x3df3726c040021ffffffffffffffffff01",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(BigInteger.valueOf(Long.MAX_VALUE)).getData();
		assertEquals("0x3df3726c040020ffffffffffffffff7f",
			Utils.hexStringFromByteArray(data));

		data = encoder.write(new BigInteger(1, TestUtils.byteArray(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff))).getData();
		assertEquals("0x3df3726c040020ffffffffffffffffff01",
			Utils.hexStringFromByteArray(data));
	}

	private List<Object> makeList(Object... items) {
		return new ArrayList<>(Arrays.asList(items));
	}

	private Map<String, Object> makeMap(Object... items) {
		Map<String, Object> res = new HashMap<>();

		for (int i = 0; i < items.length; i += 2)
			res.put((String) items[i], items[i + 1]);

		return res;
	}

	@Test
	public void testEncodeTooLargeHashMap() {
		Encoder encoder = new Encoder(new EncoderOptions().maxNumMapEntries(100));
		int size = 1000;
		Map<String, String> stringMap = new HashMap<>();
		for (int i = 0; i < size; i++) {
			stringMap.put(String.valueOf(i), "Value " + i + " of the map");
		}
		Exception exception = Assert.assertThrows(SerealException.class, () -> encoder.write(stringMap));
		assertEquals("Got input hash with 1000 entries, but the configured maximum is just 100", exception.getMessage());
	}

	@Test
	public void testEncodeTooLargeArrays() {
		Encoder encoder = new Encoder(new EncoderOptions().maxNumArrayEntries(100));
		int size = 1000;
		List<String> stringArray = new ArrayList<>();
		for (int i = 0; i < size; i++) {
			stringArray.add(new String("Value " + i + " of the array"));
		}
		Exception exception = Assert.assertThrows(SerealException.class, () -> encoder.write(stringArray));
		assertEquals("Got input array with 1000 entries, but the configured maximum is just 100", exception.getMessage());
	}

	@Test
	public void testEncodeLargeStrings() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions().maxStringLength(10));
		String shortString = "OK";
		encoder.write(shortString);

		String largeString = "Lorem ipsum dolor sit amet";
		Exception exception = Assert.assertThrows(SerealException.class, () -> encoder.write(largeString));
		assertEquals("Got input string with 26 characters, but the configured maximum is just 10", exception.getMessage());

		exception = Assert.assertThrows(SerealException.class, () -> encoder.write(new Object[]{
				new String("abc"),
				new String(largeString),
				new String("xyz"),
		}));
		assertEquals("Got input string with 26 characters, but the configured maximum is just 10", exception.getMessage());
	}
}
