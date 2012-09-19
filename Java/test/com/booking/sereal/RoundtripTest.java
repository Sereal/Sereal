package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.logging.Level;
import java.util.regex.Pattern;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests that [thing]->encode->decode->[thing] works successfully for all things.
 */
public class RoundtripTest {

	Encoder encoder;
	Decoder decoder;
	Random rand;

	@Before
	public void setup() {

		encoder = new Encoder( null );
		Map<String, Object> decoder_options = new HashMap<String, Object>();
		decoder_options.put("use_perl_refs", true); // so ref to int will give a Reference object and not just an int
		decoder = new Decoder( null );
		rand = new Random();
	}

	@Test
	public void varint() {

		int t = 1 * 1000 * 1000; // test a million random ints
		while( t-- > 0 ) {
			int n = rand.nextInt( Integer.MAX_VALUE );
			decoder.setData( ByteBuffer.wrap( encoder.write_varint( n ) ) );
			assertTrue( "Varint not decoded correctly: " + n, decoder.read_varint() == n );
		}

	}

	@Test
	public void zigzag() {

		// iterate like 0, -1, 1, -2, 2, ...
		for(long n = 0; n < 100 * 1000; n++) {
			decoder.setData( ByteBuffer.wrap( encoder.write_zigzag( n ) ) );
			assertEquals( "Zigzag not decoded correctly: " + n, decoder.read_zigzag(), n );
			decoder.setData( ByteBuffer.wrap( encoder.write_zigzag( -n ) ) );
			assertEquals( "Zigzag not decoded correctly: " + (-n), decoder.read_zigzag(), -n );
		}

	}

	@Test
	public void regex() {

		Pattern[] patterns = new Pattern[] { Pattern.compile( "foo" ), Pattern.compile( "foo", Pattern.DOTALL ),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE ), Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE | Pattern.COMMENTS ),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE | Pattern.COMMENTS | Pattern.CASE_INSENSITIVE ), Pattern.compile( "(?:foo)" ),
				Pattern.compile( "[0-9]{3}" ), Pattern.compile( "foo(bar)?" ), Pattern.compile( "(foo(bar))" ), };

		encoder.log.setLevel( Level.FINE );
		decoder.log.setLevel( Level.FINE );

		for(Pattern p : patterns) {

			try {
				encoder.reset();
				
				decoder.setData( encoder.write( p ) );
				Pattern actual = (Pattern) decoder.decode();

				Assert.assertEquals( "Pattern not equal: " + p.pattern() + " != " + actual.pattern(), p.pattern(), actual.pattern() );
				Assert.assertEquals( "Flags not equal: " + p.flags() + " != " + actual.flags(), p.flags(), actual.flags() );
			} catch (SerealException e) {
				fail( e.getMessage() );
			}

		}

	}

	@Test
	public void byte_array() {

		int n = 10 * 1000;
		while( n-- > 0 ) {

			encoder.reset();
			
			// make some random bytes
			byte[] pre = new byte[rand.nextInt( 100 )];
			rand.nextBytes( pre );

			try {
				decoder.setData( encoder.write( pre ) );
				byte[] post = (byte[]) decoder.decode();
				Assert.assertArrayEquals( pre, post );
			} catch (SerealException e) {
				fail( e.getMessage() );
			}
		}

	}

	@Test
	public void copy() {
		// write 3 copies of a string (that should be copied)
		String str = "This is quite a long string";
		try {
			encoder.write( new String[]{str, str, str} );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}
		decoder.setData( encoder.getData() );
		try {
			// read all 3
			Object[] o = (Object[]) decoder.decode();
			System.err.println(Utils.dump( o ));
			assertEquals( "Number of objects", 3, o.length );
			for(Object s : o) {
				assertEquals( str, s );
			}
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

	}

	@Test
	public void short_binary() {

		String str = "Hello, Sereal!";
		try {
			encoder.write_short_binary( str );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

		decoder.setData( encoder.getData() );
		try {
			assertEquals( str, decoder.decode() );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}
	}

	@Test
	public void booleans() {

		encoder.write_boolean( true );
		decoder.setData( encoder.getData() );
		try {
			assertTrue( (Boolean) decoder.decode() );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}

		encoder = new Encoder( null );
		encoder.write_boolean( false );
		decoder.setData( encoder.getData() );
		try {
			assertFalse( (Boolean) decoder.decode() );
		} catch (SerealException e) {
			fail( e.getMessage() );
		}
	}

}
