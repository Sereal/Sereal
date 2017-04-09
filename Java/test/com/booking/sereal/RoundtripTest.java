package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Pattern;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Tests that [thing]->encode->decode->[thing] works successfully for all things.
 */
@RunWith(Parameterized.class)
public class RoundtripTest {
	String description;
	Encoder encoder;
	Decoder decoder;
	Random rand;

	@Parameters
	public static Collection<Object[]> data() {
		return Arrays.asList(new Object[][] {
			{ "default", new Encoder() },
			{ "v1", new Encoder(new EncoderOptions().protocolVersion(1)) },
			{ "v2", new Encoder(new EncoderOptions().protocolVersion(2)) },
			{ "v3", new Encoder(new EncoderOptions().protocolVersion(3)) },
			{ "v4", new Encoder(new EncoderOptions().protocolVersion(4)) },
		});
	}

	public RoundtripTest(String _description, Encoder _encoder) {
		description = _description;
		encoder = _encoder;
	}

	@Before
	public void setup() {
		decoder = new Decoder();
		rand = new Random();
	}

	@Test
	public void varintSmall() throws SerealException {
		for (int n = -100; n < 100; ++n) {
			decoder.setData(encoder.write(n).getData());
			assertTrue( "Varint not decoded correctly: " + n, ((Long) decoder.decode()) == n );
		}
	}

	@Test
	public void varintRandom() throws SerealException {
		for (int i = 0; i < 1000000; ++i) {
			int n = rand.nextInt( Integer.MAX_VALUE );
			decoder.setData(encoder.write(n).getData());
			assertTrue( "Varint not decoded correctly: " + n, ((Long) decoder.decode()) == n );
		}
	}


	@Test
	public void regex() throws SerealException {

		Pattern[] patterns = new Pattern[] { Pattern.compile( "foo" ), Pattern.compile( "foo", Pattern.DOTALL ),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE ), Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE | Pattern.COMMENTS ),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE | Pattern.COMMENTS | Pattern.CASE_INSENSITIVE ), Pattern.compile( "(?:foo)" ),
				Pattern.compile( "[0-9]{3}" ), Pattern.compile( "foo(bar)?" ), Pattern.compile( "(foo(bar))" ), };

		for(Pattern p : patterns) {
			decoder.setData(encoder.write(p).getData());
			Pattern actual = (Pattern) decoder.decode();

			Assert.assertEquals( "Pattern not equal: " + p.pattern() + " != " + actual.pattern(), p.pattern(), actual.pattern() );
			Assert.assertEquals( "Flags not equal: " + p.flags() + " != " + actual.flags(), p.flags(), actual.flags() );
		}

	}

	@Test
	public void byte_array() throws SerealException {

		int n = 10 * 1000;
		while( n-- > 0 ) {
			// make some random bytes
			byte[] pre = new byte[rand.nextInt( 100 )];
			rand.nextBytes( pre );

			decoder.setData(encoder.write(pre).getData());
			Object post = decoder.decode();
			Assert.assertTrue(post instanceof byte[]);
			Assert.assertArrayEquals(pre, (byte[]) post);
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void copy() throws SerealException {
		// write 3 copies of a string (that should be copied)
		String str = "This is quite a long string";

		encoder.write( new String[]{str, str, str} );
		decoder.setData(encoder.getData());
		// read all 3
		List<Object> o = (List<Object>) decoder.decode();
		assertEquals( "Number of objects", 3, o.size() );
		for(Object s : o) {
			assertEquals( str, s );
		}
	}

	@Test
	public void short_binary() throws SerealException {

		Latin1String str = new Latin1String( "Hello, Sereal!" );

		encoder.write( str.getBytes() );

		decoder.setData(encoder.getData());
		Object obj = decoder.decode();
		assertTrue(obj instanceof byte[]);
		assertEquals( str, new Latin1String((byte[]) obj) );
	}

	@Test
	public void booleans() throws SerealException {

		encoder.write( true );
		decoder.setData(encoder.getData());
		assertTrue( (Boolean) decoder.decode() );

		encoder = new Encoder();
		encoder.write( false );
		decoder.setData(encoder.getData());
		assertFalse( (Boolean) decoder.decode() );
	}
}
