package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Map;
import org.junit.Test;

public class HeaderTest {
	private static final byte[] realSerealDocument = new byte[] {
		0x3d, (byte) 0xf3, 0x72, 0x6c, 0x04, 0x0a, 0x01, 0x51, 0x66, 0x68, 0x65, 0x61, 0x64, 0x65, 0x72, 0x02, 0x51, 0x64, 0x62, 0x6f, 0x64, 0x79, 0x01,
	};
	private static final String largeString = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

	private Encoder encoder = new Encoder();
	private Decoder decoder = new Decoder();

	@Test
	public void noHeader() throws SerealException {
		decoder.setData(encoder.write(1).getData());

		assertFalse(decoder.hasHeader());
		assertEquals(0, decoder.headerSize());
		assertEquals(1L, decoder.decode());
	}

	@Test
	public void withSmallHeader() throws SerealException {
		decoder.setData(encoder.write(1, 77).getData());

		assertTrue(decoder.hasHeader());
		assertEquals(2, decoder.headerSize());
		assertEquals(77L, decoder.decodeHeader());
		assertEquals(1L, decoder.decode());
	}

	@Test
	public void withLargerHeader() throws SerealException {
		decoder.setData(encoder.write(1, largeString).getData());

		assertTrue(decoder.hasHeader());
		assertEquals(203, decoder.headerSize());
		assertEquals(largeString, decoder.decodeHeader());
		assertEquals(1L, decoder.decode());
	}

	@Test
	public void withActualSereal() throws SerealException {
		decoder.setData(realSerealDocument);

		assertTrue(decoder.hasHeader());
		assertEquals(9, decoder.headerSize());

		@SuppressWarnings("unchecked")
		Map header = (Map) decoder.decodeHeader(), body = (Map) decoder.decode();
		assertEquals(2L, header.get("header"));
		assertEquals(1L, body.get("body"));

		assertTrue(decoder.hasHeader());
		assertEquals(9, decoder.headerSize());
	}
}
