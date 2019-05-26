package com.booking.sereal;

import static org.junit.Assert.assertEquals;

import java.nio.charset.StandardCharsets;
import org.junit.Test;

public class TokenHeaderAndCompressionTest {
  private static final String LONG_STRING;

  static {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < 164; ++i) {
      sb.append('a');
    }
    LONG_STRING = sb.toString();
  }

  @Test
  public void serealV1None() throws SerealException {
    check(1, EncoderOptions.CompressionType.NONE, false, 0x01, 173);
  }

  @Test
  public void serealV1Snappy() throws SerealException {
    check(1, EncoderOptions.CompressionType.SNAPPY, false, 0X11, 22);
  }

  @Test
  public void serealV2None() throws SerealException {
    check(2, EncoderOptions.CompressionType.NONE, false, 0X02, 173);
    check(2, EncoderOptions.CompressionType.NONE, true, 0X02, 175);
  }

  @Test
  public void serealV2Snappy() throws SerealException {
    check(2, EncoderOptions.CompressionType.SNAPPY, false, 0X22, 24);
    check(2, EncoderOptions.CompressionType.SNAPPY, true, 0X22, 26);
  }

  @Test
  public void serealV3None() throws SerealException {
    check(3, EncoderOptions.CompressionType.NONE, false, 0X03, 173);
    check(3, EncoderOptions.CompressionType.NONE, true, 0X03, 175);
  }

  @Test
  public void serealV3Snappy() throws SerealException {
    check(3, EncoderOptions.CompressionType.SNAPPY, false, 0X23, 24);
    check(3, EncoderOptions.CompressionType.SNAPPY, true, 0X23, 26);
  }

  @Test
  public void serealV3Zlib() throws SerealException {
    check(3, EncoderOptions.CompressionType.ZLIB, false, 0X33, 24);
    check(3, EncoderOptions.CompressionType.ZLIB, true, 0X33, 26);
  }

  @Test
  public void serealV4None() throws SerealException {
    check(4, EncoderOptions.CompressionType.NONE, false, 0X04, 173);
    check(4, EncoderOptions.CompressionType.NONE, true, 0X04, 175);
  }

  @Test
  public void serealV4Snappy() throws SerealException {
    check(4, EncoderOptions.CompressionType.SNAPPY, false, 0X24, 24);
    check(4, EncoderOptions.CompressionType.SNAPPY, true, 0X24, 26);
  }

  @Test
  public void serealV4Zlib() throws SerealException {
    check(4, EncoderOptions.CompressionType.ZLIB, false, 0X34, 24);
    check(4, EncoderOptions.CompressionType.ZLIB, true, 0X34, 26);
  }

  @Test
  public void serealV4Zstd() throws SerealException {
    check(4, EncoderOptions.CompressionType.ZSTD, false, 0X44,28);
    check(4, EncoderOptions.CompressionType.ZSTD, true, 0X44, 30);
  }

  private void check(int protocolVersion, EncoderOptions.CompressionType compressionType, boolean withHeader, int versionEncoding, int expectedLength) throws SerealException {
    TokenEncoder encoder = new TokenEncoder(new EncoderOptions()
      .protocolVersion(protocolVersion)
      .compressionThreshold(0)
      .compressionType(compressionType));

    assertEquals(protocolVersion, encoder.protocolVersion());

    if (withHeader) {
      encoder.startHeader();
      encoder.appendLong(10);
      encoder.endHeader();
    }

    encoder.startDocument();
    encoder.appendString(LONG_STRING);
    encoder.endDocument();

    assertEquals((byte) versionEncoding, encoder.getDataReference().array[4]);
    assertEquals(expectedLength, encoder.getDataReference().length);

    {
      TokenDecoder decoder = new TokenDecoder();

      setPaddedData(decoder, encoder);

      assertEquals(withHeader, decoder.hasHeader());
      assertEquals(withHeader ? 1 : 0, decoder.headerSize());
    }

    if (withHeader) {
      {
        TokenDecoder decoder = new TokenDecoder();

        setPaddedData(decoder, encoder);

        checkBody(decoder);
      }

      {
        TokenDecoder decoder = new TokenDecoder();

        setPaddedData(decoder, encoder);

        checkHeader(decoder);
      }

      {
        TokenDecoder decoder = new TokenDecoder();

        setPaddedData(decoder, encoder);

        checkHeader(decoder);
        checkBody(decoder);
      }

      {
        TokenDecoder decoder = new TokenDecoder();

        setPaddedData(decoder, encoder);

        checkBody(decoder);
        checkHeader(decoder);
      }
    } else {
      TokenDecoder decoder = new TokenDecoder();

      setPaddedData(decoder, encoder);

      checkBody(decoder);
    }
  }

  private void checkBody(TokenDecoder decoder) throws SerealException {
    decoder.prepareDecodeBody();

    assertEquals(SerealToken.UTF8, decoder.poisonNextToken());
    String string = new String(
      decoder.decoderBuffer(),
      decoder.binarySliceStart(),
      decoder.binarySliceLength(),
      StandardCharsets.UTF_8
    );
    assertEquals(LONG_STRING, string);

    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  private void checkHeader(TokenDecoder decoder) throws SerealException {
    decoder.prepareDecodeHeader();

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(10L, decoder.longValue());

    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }


  private void setPaddedData(TokenDecoder decoder, TokenEncoder encoder) {
    byte[] data = encoder.getData();
    byte[] padded = new byte[data.length + 6];
    for (int i = 0; i < padded.length; ++i) {
      padded[i] = (byte) (Math.random() * 256);
    }
    System.arraycopy(data, 0, padded, 3, data.length);
    decoder.setData(new ByteArray(padded, 3, data.length));
  }
}
