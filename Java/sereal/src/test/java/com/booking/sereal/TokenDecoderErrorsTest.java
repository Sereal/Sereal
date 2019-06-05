package com.booking.sereal;

import java.util.Arrays;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class TokenDecoderErrorsTest {
  TokenDecoder decoder = new TokenDecoder();

  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void noData() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("No data set");

    decoder.prepareDecodeHeader();
  }

  @Test
  public void shortDocument() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Invalid Sereal document: total size is too small");

    decoder.setData(Arrays.copyOf(v1Document(), 6));
    decoder.prepareDecodeHeader();
  }

  @Test
  public void invalidMagic() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Invalid Sereal header (3e73726c): doesn't match magic");

    byte[] data = v1Document();
    data[0] = 0x3e;

    decoder.setData(data);
    decoder.prepareDecodeHeader();
  }

  @Test
  public void invalidV1Magic() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Invalid Sereal header: magic v1 with protocol version 4");

    byte[] data = v1Document();
    data[4] = 0x04;

    decoder.setData(data);
    decoder.prepareDecodeHeader();
  }

  @Test
  public void invalidV3Magic() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Invalid Sereal header: magic v3 with protocol version 2");

    byte[] data = v3Document();
    data[4] = 0x02;

    decoder.setData(data);
    decoder.prepareDecodeHeader();
  }

  @Test
  public void invalidVersion() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Invalid Sereal header: unsupported protocol version 15");

    byte[] data = v1Document();
    data[4] = 0x0f;

    decoder.setData(data);
    decoder.prepareDecodeHeader();
  }

  @Test
  public void badEncoding() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Unsupported Sereal body encoding 15");

    byte[] data = v1Document();
    data[4] = (byte) 0xf1;

    decoder.setData(data);
    decoder.prepareDecodeHeader();
  }

  @Test
  public void badEncodingSnappyV1() throws SerealException {
    failEncoding(1, 2, "Unsupported encoding 2 (SNAPPY) for Sereal protocol 1");
  }

  @Test
  public void badEncodingZlibV1() throws SerealException {
    failEncoding(1, 3, "Unsupported encoding 3 (ZLIB) for Sereal protocol 1");
  }

  @Test
  public void badEncodingZstdV1() throws SerealException {
    failEncoding(1, 4, "Unsupported encoding 4 (ZSTD) for Sereal protocol 1");
  }

  @Test
  public void badEncodingLegacySnappyV2() throws SerealException {
    failEncoding(2, 1, "Unsupported encoding 1 (SNAPPY) for Sereal protocol 2");
  }

  @Test
  public void badEncodingZlibV2() throws SerealException {
    failEncoding(2, 3, "Unsupported encoding 3 (ZLIB) for Sereal protocol 2");
  }

  @Test
  public void badEncodingZstdV2() throws SerealException {
    failEncoding(2, 4, "Unsupported encoding 4 (ZSTD) for Sereal protocol 2");
  }

  @Test
  public void badEncodingLegacySnappyV3() throws SerealException {
    failEncoding(3, 1, "Unsupported encoding 1 (SNAPPY) for Sereal protocol 3");
  }

  @Test
  public void badEncodingZstdV3() throws SerealException {
    failEncoding(3, 4, "Unsupported encoding 4 (ZSTD) for Sereal protocol 3");
  }

  @Test
  public void badEncodingLegacySnappyV4() throws SerealException {
    failEncoding(4, 1, "Unsupported encoding 1 (SNAPPY) for Sereal protocol 4");
  }

  @Test
  public void noV1Hader() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Sereal user header not supported in protocol version 1");

    decoder.setData(v1Document());
    decoder.prepareDecodeHeader();
  }

  @Test
  public void emptyHeader() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Sereal user header not present");

    decoder.setData(v3Document());
    decoder.prepareDecodeHeader();
  }

  private void failEncoding(int protocol, int encoding, String errorMessage) throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage(errorMessage);

    byte[] data = protocol >= 3 ? v3Document() : v1Document();
    data[4] = (byte) ((encoding << 4) | protocol);

    decoder.setData(data);
    decoder.prepareDecodeHeader();
  }

  private static byte[] v1Document() {
    return TestUtils.byteArray(0x3d, 0x73, 0x72, 0x6c, 0x01, 0x0, 0x01);
  }

  private static byte[] v3Document() {
    return TestUtils.byteArray(0x3d, 0xf3, 0x72, 0x6c, 0x03, 0x0, 0x01);
  }
}
