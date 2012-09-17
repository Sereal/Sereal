package com.booking.sereal;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.ByteBuffer;
import java.util.Random;
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

		encoder = new Encoder();
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
	public void regex() {

		Pattern[] patterns = new Pattern[]{
				Pattern.compile( "foo" ),
				Pattern.compile( "foo", Pattern.DOTALL),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE | Pattern.COMMENTS),
				Pattern.compile( "foo", Pattern.DOTALL | Pattern.MULTILINE | Pattern.COMMENTS | Pattern.CASE_INSENSITIVE),
				Pattern.compile( "(?:foo)" ),
				Pattern.compile( "[0-9]{3}" ),
				Pattern.compile( "foo(bar)?" ),
				Pattern.compile( "(foo(bar))" ),
		};
		
		for(Pattern p : patterns) {
			
			try {
				
				decoder.setData( ByteBuffer.wrap( encoder.write_regex( p ) ) );
				Pattern actual = decoder.read_regex();

				Assert.assertEquals( "Pattern not equal: " + p.pattern() + " != " + actual.pattern(), p.pattern(), actual.pattern() );
				Assert.assertEquals( "Flags not equal: " + p.flags() + " != " + actual.flags(), p.flags(), actual.flags() );
			} catch (SerealException e) {
				fail( e.getMessage() );
			}

		}

	}
	
	@Test
	public void binary() {
		
		int n = 10 * 1000;
		while( n --> 0 ) {

			// make some random bytes
			byte[] pre = new byte[ rand.nextInt( 100 ) ];
			rand.nextBytes( pre );
			
			decoder.setData( ByteBuffer.wrap(encoder.write_binary( pre )) );
			byte[] post = decoder.read_binary();
			Assert.assertArrayEquals( pre, post );
		}
		
		
	}

}
