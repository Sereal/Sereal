package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertThrows;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.junit.Test;
import org.xerial.snappy.Snappy;

/**
 * Regression tests for malformed documents: every one must be rejected with a
 * {@link SerealException}, without reading outside the document or allocating more than the input
 * (or the configured limits) justifies.
 */
public class MalformedInputTest {
  static byte[] hex(String s) {
    byte[] out = new byte[s.length() / 2];
    for (int i = 0; i < out.length; i++) {
      out[i] = (byte) Integer.parseInt(s.substring(2 * i, 2 * i + 2), 16);
    }
    return out;
  }

  private static Object decode(ByteArray data, DecoderOptions options) throws SerealException {
    Decoder decoder = new Decoder(options);
    decoder.setData(data);
    return decoder.decode();
  }

  private static Object decode(byte[] data) throws SerealException {
    return decode(new ByteArray(data), new DecoderOptions());
  }

  private static int drain(ByteArray data, DecoderOptions options) throws SerealException {
    TokenDecoder decoder = new TokenDecoder(options);
    decoder.setData(data);
    decoder.prepareDecodeBody();
    int count = 0;
    while (decoder.nextToken() != SerealToken.END) {
      count++;
    }
    return count;
  }

  private static void assertRejected(String packet) {
    assertRejected(hex(packet), new DecoderOptions());
  }

  private static void assertRejected(byte[] packet, DecoderOptions options) {
    assertThrows(SerealException.class, () -> decode(new ByteArray(packet), options));
    assertThrows(SerealException.class, () -> drain(new ByteArray(packet), options));
  }

  // a document followed by unrelated bytes in the same array
  private static ByteArray slice(byte[] document, int length) {
    byte[] secret = "|OTHER-REQUEST: password=hunter2|".getBytes(StandardCharsets.ISO_8859_1);
    byte[] buf = Arrays.copyOf(document, document.length + secret.length);
    System.arraycopy(secret, 0, buf, document.length, secret.length);
    return new ByteArray(buf, 0, length);
  }

  // ---- snappy bounds -----------------------------------------------------------------------

  @Test
  public void snappyLegacyHeaderSuffixPastEnd() {
    // v1 legacy snappy, header suffix size 0x20000000 points far past the 11-byte input
    assertRejected("3d73726c11808080800201");
  }

  @Test
  public void snappyCompressedLengthPastEnd() {
    // v2 incremental snappy, compressed length 0x7fffffff, snappy stream claims a 1GB literal
    assertRejected("3d73726c2200ffffffff07ffffffff0ffcffffff3f00000000");
    assertRejected("3df3726c2300ffffffff07ffffffff0ffcffffff3f00000000");
    // compressed length 0x7f, 0x10000000 and -1 (as an int) for a 2-byte stream
    assertRejected("3d73726c22007f0101");
    assertRejected("3d73726c220080808080010101");
    assertRejected("3d73726c2200ffffffff0f0101");
  }

  @Test
  public void snappyLiteralPastEnd() {
    // compressed length 67 for 4 bytes of input: a 64-byte literal would be read from the heap
    assertRejected("3d73726c22004340f03f263e4142");
  }

  @Test
  public void snappyCompressedLengthPastSliceEnd() throws Exception {
    byte[] body = {0x63, 'a', 'b', 'c'};
    byte[] compressed = Snappy.compress(body);
    byte[] doc = new byte[6 + 1 + compressed.length];
    System.arraycopy(hex("3d73726c2200"), 0, doc, 0, 6);
    doc[6] = (byte) compressed.length;
    System.arraycopy(compressed, 0, doc, 7, compressed.length);

    // the whole document decodes, also with unrelated bytes after it
    assertArrayEquals("abc".getBytes(StandardCharsets.ISO_8859_1),
        (byte[]) decode(slice(doc, doc.length), new DecoderOptions()));
    drain(slice(doc, doc.length), new DecoderOptions());

    // the compressed length runs past the end of the document, into the bytes after it
    ByteArray truncated = slice(doc, doc.length - 2);
    assertThrows(SerealException.class, () -> decode(truncated, new DecoderOptions()));
    assertThrows(SerealException.class, () -> drain(truncated, new DecoderOptions()));
  }

  @Test
  public void snappyUncompressedLengthOverLimit() throws Exception {
    byte[] compressed = Snappy.compress(new byte[4096]);
    byte[] doc = new byte[6 + 2 + compressed.length];
    System.arraycopy(hex("3d73726c2200"), 0, doc, 0, 6);
    doc[6] = (byte) (0x80 | (compressed.length & 0x7f));
    doc[7] = (byte) (compressed.length >> 7);
    System.arraycopy(compressed, 0, doc, 8, compressed.length);
    assertRejected(doc, new DecoderOptions().maxBufferSize(1024));
  }
}
