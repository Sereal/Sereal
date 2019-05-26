package com.booking.sereal;

import static org.junit.Assert.assertEquals;

import java.util.Random;
import org.junit.Test;

public class TokenCompressionTest {
  private static final String LONG_STRING;
  private static final byte[] RANDOM_BINARY = new byte[1024];

  static {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < 164; ++i) {
      sb.append('a');
    }
    LONG_STRING = sb.toString();

    Random rnd = new Random(1234);

    for (int i = 0; i < RANDOM_BINARY.length; ++i) {
      RANDOM_BINARY[i] = (byte) rnd.nextInt(256);
    }
  }

  @Test
  public void serealSnappyBelowThreshold() throws SerealException {
    checkBelowThreshold(EncoderOptions.CompressionType.SNAPPY, 0x04, 173);
  }

  @Test
  public void serealSnappyAboveThreshold() throws SerealException {
    checkAboveThreshold(EncoderOptions.CompressionType.SNAPPY, 0x24, 24);
  }

  @Test
  public void serealSnappyNoCompressUnlessSMaller() throws SerealException {
    checkNoCompressUnlessSmaller(EncoderOptions.CompressionType.SNAPPY, 0x04, 1033);
  }

  @Test
  public void serealZlibBelowThreshold() throws SerealException {
    checkBelowThreshold(EncoderOptions.CompressionType.ZLIB, 0x04, 173);
  }

  @Test
  public void serealZlibAboveThreshold() throws SerealException {
    checkAboveThreshold(EncoderOptions.CompressionType.ZLIB, 0x34, 24);
  }

  @Test
  public void serealZlibNoCompressUnlessSMaller() throws SerealException {
    checkNoCompressUnlessSmaller(EncoderOptions.CompressionType.ZLIB, 0x04, 1033);
  }

  @Test
  public void serealZstdBelowThreshold() throws SerealException {
    checkBelowThreshold(EncoderOptions.CompressionType.ZSTD, 0x04, 173);
  }

  @Test
  public void serealZstdAboveThreshold() throws SerealException {
    checkAboveThreshold(EncoderOptions.CompressionType.ZSTD, 0x44, 28);
  }

  @Test
  public void serealZstdNoCompressUnlessSMaller() throws SerealException {
    checkNoCompressUnlessSmaller(EncoderOptions.CompressionType.ZSTD, 0x04, 1033);
  }

  private void checkBelowThreshold(EncoderOptions.CompressionType compressionType, int expectedType, int expectedLength) throws SerealException {
    TokenEncoder encoder = new TokenEncoder(
      new EncoderOptions()
        .compressionType(compressionType)
        .compressionThreshold(LONG_STRING.length() + 3)
    );

    encoder.startDocument();
    encoder.appendString(LONG_STRING);
    encoder.endDocument();

    assertEquals((byte) expectedType, encoder.getDataReference().array[4]);
    assertEquals(expectedLength, encoder.getDataReference().length);
  }


  private void checkAboveThreshold(EncoderOptions.CompressionType compressionType, int expectedType, int expectedLength) throws SerealException {
    TokenEncoder encoder = new TokenEncoder(
      new EncoderOptions()
        .compressionType(compressionType)
        .compressionThreshold(LONG_STRING.length() + 2)
    );

    encoder.startDocument();
    encoder.appendString(LONG_STRING);
    encoder.endDocument();

    assertEquals((byte) expectedType, encoder.getDataReference().array[4]);
    assertEquals(expectedLength, encoder.getDataReference().length);
  }

  private void checkNoCompressUnlessSmaller(EncoderOptions.CompressionType compressionType, int expectedType, int expectedLength) throws SerealException {
    TokenEncoder encoder = new TokenEncoder(
      new EncoderOptions()
        .compressionType(compressionType)
        .compressionThreshold(0)
    );

    encoder.startDocument();
    encoder.appendBinary(RANDOM_BINARY);
    encoder.endDocument();

    assertEquals((byte) expectedType, encoder.getDataReference().array[4]);
    assertEquals(expectedLength, encoder.getDataReference().length);
  }
}
