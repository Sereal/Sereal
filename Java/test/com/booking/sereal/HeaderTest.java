package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.junit.Test;

public class HeaderTest {
	private static final String largeString = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

	private Encoder encoder = new Encoder();
	private Decoder decoder = new Decoder();

	@Test
	public void noHeader() throws SerealException, IOException {
		decoder.setData(encoder.write(1));

		assertFalse(decoder.hasHeader());
		assertEquals(0, decoder.headerSize());
		assertEquals(1L, decoder.decode());
	}

	@Test
	public void withSmallHeader() throws SerealException, IOException {
		decoder.setData(encoder.write(1, 77));

		assertTrue(decoder.hasHeader());
		assertEquals(2, decoder.headerSize());
		assertEquals(77L, decoder.decodeHeader());
		assertEquals(1L, decoder.decode());
	}

	@Test
	public void withLargerHeader() throws SerealException, IOException {
		decoder.setData(encoder.write(1, largeString));

		assertTrue(decoder.hasHeader());
		assertEquals(203, decoder.headerSize());
		assertEquals(largeString, decoder.decodeHeader());
		assertEquals(1L, decoder.decode());
	}
}
