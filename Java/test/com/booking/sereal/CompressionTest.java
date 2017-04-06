package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class CompressionTest {
	private static final String data = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
	private static final String smallData = "aa";

	@Test
	public void serealV1None() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(1));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x01, encoded[4]);
		assertEquals(173, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV1Snappy() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(1)
			.compressionThreshold(0)
			.compressionType(EncoderOptions.CompressionType.SNAPPY));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x11, encoded[4]);
		assertEquals(22, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV2None() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(2));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x02, encoded[4]);
		assertEquals(173, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV2Snappy() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(2)
			.compressionThreshold(0)
			.compressionType(EncoderOptions.CompressionType.SNAPPY));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x22, encoded[4]);
		assertEquals(24, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV3None() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(3));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x03, encoded[4]);
		assertEquals(173, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV3Snappy() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(3)
			.compressionThreshold(0)
			.compressionType(EncoderOptions.CompressionType.SNAPPY));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x23, encoded[4]);
		assertEquals(24, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealV3Zlib() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			.protocolVersion(3)
			.compressionThreshold(0)
			.compressionType(EncoderOptions.CompressionType.ZLIB));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x33, encoded[4]);
		assertEquals(24, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void aboveThreshold() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			// version-independent, but simplifies test
			.protocolVersion(3)
			.compressionThreshold(166)
			.compressionType(EncoderOptions.CompressionType.ZLIB));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x33, encoded[4]);
		assertEquals(24, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void belowThreshold() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			// version-independent, but simplifies test
			.protocolVersion(3)
			.compressionThreshold(167)
			.compressionType(EncoderOptions.CompressionType.ZLIB));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x03, encoded[4]);
		assertEquals(173, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void onlyCompressWhenSmallerSnappy() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			// version-independent, but simplifies test
			.protocolVersion(3)
			.compressionThreshold(0)
			.compressionType(EncoderOptions.CompressionType.SNAPPY));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(smallData).getData();

		assertEquals(0x03, encoded[4]);
		assertEquals(10, encoded.length);

		decoder.setData(encoded);
		assertEquals(smallData, decoder.decode());
	}

	@Test
	public void onlyCompressWhenSmallerZlib() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
			// version-independent, but simplifies test
			.protocolVersion(3)
			.compressionThreshold(0)
			.compressionType(EncoderOptions.CompressionType.ZLIB));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(smallData).getData();

		assertEquals(0x03, encoded[4]);
		assertEquals(10, encoded.length);

		decoder.setData(encoded);
		assertEquals(smallData, decoder.decode());
	}

	@Test
	public void serealZstd() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
				.protocolVersion(4)
				.compressionThreshold(0)
				.compressionType(EncoderOptions.CompressionType.ZSTD));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x44, encoded[4]);
		assertEquals(28, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}

	@Test
	public void serealZstdSmallData() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
				.protocolVersion(4)
				.compressionThreshold(0)
				.compressionType(EncoderOptions.CompressionType.ZSTD));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(smallData).getData();

		assertEquals(0x04, encoded[4]);
		assertEquals(10, encoded.length);

		decoder.setData(encoded);
		assertEquals(smallData, decoder.decode());
	}

	@Test
	public void serealZstdBelowThreshold() throws SerealException {
		Encoder encoder = new Encoder(new EncoderOptions()
				.protocolVersion(4)
				.compressionThreshold(167)
				.compressionType(EncoderOptions.CompressionType.ZSTD));
		Decoder decoder = new Decoder();
		byte[] encoded = encoder.write(data).getData();

		assertEquals(0x04, encoded[4]);
		assertEquals(173, encoded.length);

		decoder.setData(encoded);
		assertEquals(data, decoder.decode());
	}
}
