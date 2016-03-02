package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.junit.Test;

public class CompressionTest {
	private static final String data = "aaaaaaaaaaaaaaaaaaaaaaaa";

	@Test
	public void serealV1None() throws SerealException, IOException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(1));
		Decoder decoder = new Decoder(null);
		ByteBuffer encoded = encoder.write(data);

		assertEquals(0x01, encoded.get(4));
		assertEquals(32, encoded.limit());

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV1Snappy() throws SerealException, IOException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(1)
			.compressionType(EncoderOptions.CompressionType.SNAPPY));
		Decoder decoder = new Decoder(null);
		ByteBuffer encoded = encoder.write(data);

		assertEquals(0x11, encoded.get(4));
		assertEquals(14, encoded.limit());

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV2None() throws SerealException, IOException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(2));
		Decoder decoder = new Decoder(null);
		ByteBuffer encoded = encoder.write(data);

		assertEquals(0x02, encoded.get(4));
		assertEquals(32, encoded.limit());

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV2Snappy() throws SerealException, IOException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(2)
			.compressionType(EncoderOptions.CompressionType.SNAPPY));
		Decoder decoder = new Decoder(null);
		ByteBuffer encoded = encoder.write(data);

		assertEquals(0x22, encoded.get(4));
		assertEquals(15, encoded.limit());

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}
}
