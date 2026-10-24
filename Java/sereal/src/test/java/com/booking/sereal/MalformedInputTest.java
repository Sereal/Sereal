package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.nio.charset.StandardCharsets;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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

  static byte[] concat(byte[]... parts) {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    for (byte[] part : parts) {
      out.write(part, 0, part.length);
    }
    return out.toByteArray();
  }

  static byte[] repeat(int b, int count) {
    byte[] out = new byte[count];
    Arrays.fill(out, (byte) b);
    return out;
  }

  static byte[] varint(long n) {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    while (Long.compareUnsigned(n, 0x7f) > 0) {
      out.write((int) ((n & 0x7f) | 0x80));
      n >>>= 7;
    }
    out.write((int) n);
    return out.toByteArray();
  }

  /**
   * An ARRAY of levels + 1 elements: an ARRAYREF_2 [1, 1], then levels ARRAYREF_2 elements each
   * holding two COPYs of the previous element (which are themselves COPYs), doubling the work per
   * level.
   */
  static byte[] copyExpansion(int levels) {
    ByteArrayOutputStream body = new ByteArrayOutputStream();
    body.write(0x2b);
    body.write(varint(levels + 1), 0, varint(levels + 1).length);
    int prev = body.size() + 1;
    body.write(0x42);
    body.write(0x01);
    body.write(0x01);
    for (int i = 0; i < levels; i++) {
      int cur = body.size() + 1;
      byte[] ref = varint(prev);
      body.write(0x42);
      body.write(0x2f);
      body.write(ref, 0, ref.length);
      body.write(0x2f);
      body.write(ref, 0, ref.length);
      prev = cur;
    }
    return concat(hex("3df3726c0300"), body.toByteArray());
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

  // ---- allocation bounds -------------------------------------------------------------------

  // fails the test when a container is presized beyond what the input could hold
  private static final class BoundedTypeMapper extends DefaultTypeMapper {
    private final int limit;
    private final boolean objectArray;

    BoundedTypeMapper(int limit, boolean objectArray) {
      this.limit = limit;
      this.objectArray = objectArray;
    }

    @Override
    public boolean useObjectArray() {
      return objectArray;
    }

    @Override
    public List<Object> makeArray(int size) {
      if (size < 0 || size > limit) {
        throw new AssertionError("array presized to " + size + " entries");
      }
      return super.makeArray(size);
    }

    @Override
    public Map<String, Object> makeMap(int size) {
      if (size < 0 || size > limit) {
        throw new AssertionError("map presized to " + size + " entries");
      }
      return super.makeMap(size);
    }
  }

  private static void assertContainerRejected(String packet) {
    byte[] data = hex(packet);
    for (boolean objectArray : new boolean[] {false, true}) {
      DecoderOptions options =
          new DecoderOptions().typeMapper(new BoundedTypeMapper(data.length, objectArray));
      assertThrows(SerealException.class, () -> decode(new ByteArray(data), options));
    }
    assertThrows(SerealException.class, () -> drain(new ByteArray(data), new DecoderOptions()));
  }

  @Test
  public void containerCountLargerThanInput() {
    // ARRAY with 0x7fffffff elements, then a single element
    assertContainerRejected("3df3726c03002bffffffff0701");
    // HASH with 0x7fffffff pairs, then a single pair
    assertContainerRejected("3df3726c03002affffffff07616101");
    // ARRAY / HASH with 0xffffffff entries: negative once narrowed to an int
    assertContainerRejected("3df3726c03002bffffffff0f");
    assertContainerRejected("3df3726c03002affffffff0f");
  }

  @Test
  public void hashCountOverflow() {
    // HASH with 0x80000001 pairs: twice that wraps to 2 in int arithmetic, so the single pair
    // that follows looked like a complete hash
    assertContainerRejected("3df3726c03002a8180808008616101");
  }

  @Test
  public void binaryLengthLargerThanInput() {
    // BINARY of 0x7fffffff and 0xffffffff bytes, with no data
    assertThrows(SerealException.class, () -> decode(hex("3df3726c030026ffffffff07")));
    assertThrows(SerealException.class, () -> decode(hex("3df3726c030026ffffffff0f")));
  }

  @Test
  public void binaryLongerThanMaxStringLength() throws Exception {
    // a complete 2000-byte BINARY, with maxStringLength 1000
    byte[] doc = new byte[6 + 3 + 2000];
    System.arraycopy(hex("3df3726c030026d00f"), 0, doc, 0, 9);
    assertThrows(SerealException.class,
        () -> decode(new ByteArray(doc), new DecoderOptions().maxStringLength(1000)));
    decode(new ByteArray(doc), new DecoderOptions().maxStringLength(2000));
  }

  @Test
  public void zstdCompressedLengthLargerThanInput() {
    // compressed length 0x7ffffff0 and 0x7fffffff for a 4-byte frame
    assertRejected("3df3726c4400f0ffffff0728b52ffd");
    assertRejected("3df3726c4400ffffffff0728b52ffd");
    // compressed length 0xffffffff: negative once narrowed to an int
    assertRejected("3df3726c4400ffffffff0f28b52ffd");
  }

  @Test
  public void zstdFrameContentSize() {
    // frame header declares 0x7fffff00 bytes of content
    assertRejected("3df3726c44000c28b52ffda000ffff7f010000");
    // frame header without a content size
    assertRejected("3df3726c44000928b52ffd0000010000");
  }

  @Test
  public void zlibUncompressedLengthLargerThanLimit() {
    // uncompressed length 0x7fffffff and 0xffffffff, 1 byte of compressed data
    assertRejected("3df3726c3300ffffffff070100");
    assertRejected("3df3726c3300ffffffff0f0100");
  }

  @Test
  public void compressedBodyLargerThanMaxBufferSize() throws Exception {
    for (EncoderOptions.CompressionType type : new EncoderOptions.CompressionType[] {
        EncoderOptions.CompressionType.SNAPPY, EncoderOptions.CompressionType.ZLIB,
        EncoderOptions.CompressionType.ZSTD}) {
      Encoder encoder = new Encoder(new EncoderOptions().protocolVersion(4)
          .compressionThreshold(0).compressionType(type));
      byte[] doc = encoder.write(new byte[4096]).getData();
      drain(new ByteArray(doc), new DecoderOptions());
      assertRejected(doc, new DecoderOptions().maxBufferSize(1024));
    }
  }

  // ---- COPY and recursion ------------------------------------------------------------------

  @Test(timeout = 10000)
  public void copyOfCopyExpansion() {
    // COPY targets that contain COPYs: forbidden, and exponential when followed
    assertThrows(SerealException.class, () -> decode(copyExpansion(18)));
  }

  @Test(timeout = 10000)
  public void copyOfCopyExpansionLarge() {
    // 141 bytes that expand to 2^26 copies
    assertThrows(SerealException.class, () -> decode(copyExpansion(26)));
  }

  @Test
  public void copyTargetNotBeforeTag() {
    // COPY to offset 0, to itself, and forward to the following element
    assertRejected("3df3726c03002f00");
    assertRejected("3df3726c03002f01");
    assertRejected("3df3726c0300422f0401");
    // COPY of the enclosing ARRAYREF_1 (TokenDecoder leaves following COPYs to its caller)
    assertThrows(SerealException.class, () -> decode(hex("3df3726c0300412f01")));
  }

  @Test
  public void stringCopyOfCopy() {
    // HASHREF_3 {a: 1, COPY 2: 2, COPY 5: 3}: the third key copies the second key's COPY tag
    assertRejected("3df3726c030053616101" + "2f0202" + "2f0503");
  }

  @Test
  public void stringCopyOfString() throws Exception {
    // HASHREF_3 {a: 1, COPY 2: 2, COPY 2: 3}: keys copying a plain string are fine
    @SuppressWarnings("unchecked")
    Map<String, Object> out = (Map<String, Object>) decode(hex("3df3726c030053616101" + "2f0202" + "2f0203"));
    assertEquals(3L, out.get("a"));
    drain(new ByteArray(hex("3df3726c030053616101" + "2f0202" + "2f0203")), new DecoderOptions());
  }

  @Test
  public void objectvClassNameLoop() {
    // OBJECTV whose class name offset points at a COPY of itself
    assertRejected("3df3726c03002d032f03");
  }

  @Test
  public void padRunIsSkippedWithoutRecursion() throws Exception {
    // a million PAD tags before the value: valid, and must not recurse per PAD
    byte[] doc = concat(hex("3df3726c0300"), repeat(0x3f, 1_000_000), hex("01"));
    assertEquals(1L, decode(doc));
    assertEquals(1, drain(new ByteArray(doc), new DecoderOptions()));
  }

  @Test
  public void userHeaderPadRun() throws Exception {
    // a user header made of a million PAD tags and a value
    byte[] doc = concat(hex("3df3726c03"), varint(1_000_002), hex("01"), repeat(0x3f, 1_000_000),
        hex("01"), hex("01"));
    Decoder decoder = new Decoder();
    decoder.setData(doc);
    assertEquals(1L, decoder.decodeHeader());
    assertEquals(1L, decoder.decode());

    TokenDecoder tokens = new TokenDecoder();
    tokens.setData(doc);
    tokens.prepareDecodeHeader();
    assertEquals(SerealToken.LONG, tokens.nextToken());
    assertEquals(SerealToken.END, tokens.nextToken());
  }

  @Test
  public void userHeaderRecursionLimit() {
    // a user header nesting a million REFN tags
    byte[] doc = concat(hex("3df3726c03"), varint(1_000_002), hex("01"), repeat(0x28, 1_000_000),
        hex("01"), hex("01"));
    for (DecoderOptions options : new DecoderOptions[] {
        new DecoderOptions(), new DecoderOptions().perlReferences(true)}) {
      Decoder decoder = new Decoder(options);
      decoder.setData(doc);
      assertThrows(SerealException.class, decoder::decodeHeader);
    }
  }

  @Test
  public void recursionLimitCountsEveryNestingTag() throws Exception {
    DecoderOptions limited = new DecoderOptions().maxRecursionDepth(100);
    DecoderOptions limitedPerl = new DecoderOptions().maxRecursionDepth(100).perlReferences(true);
    byte[] header = hex("3df3726c0300");
    // 200 nested REFN, WEAKEN REFN and OBJECT tags around a value
    byte[] refn = concat(header, repeat(0x28, 200), hex("01"));
    byte[] weaken = concat(header, repeat(0x30, 200), hex("2801"));
    ByteArrayOutputStream objects = new ByteArrayOutputStream();
    for (int i = 0; i < 200; i++) {
      objects.write(0x2c);
      objects.write(0x61);
      objects.write('a');
    }
    byte[] object = concat(header, objects.toByteArray(), hex("01"));
    for (byte[] doc : new byte[][] {refn, weaken, object}) {
      assertThrows(SerealException.class, () -> decode(new ByteArray(doc), limitedPerl));
      assertThrows(SerealException.class, () -> decode(new ByteArray(doc), limited));
      decode(new ByteArray(doc), new DecoderOptions());
    }
  }

  @Test
  public void repeatedStringsUseCopy() throws Exception {
    List<Object> rows = new ArrayList<>();
    for (int i = 0; i < 3; i++) {
      Map<String, Object> row = new LinkedHashMap<>();
      row.put("name", "Some::Class");
      row.put("text", "repeated text");
      row.put("object", new PerlObject("Some::Class", new PerlReference(row.get("text"))));
      rows.add(row);
    }
    for (int version = 1; version <= 6; version++) {
      byte[] doc = new Encoder(new EncoderOptions().protocolVersion(version)).write(rows).getData();
      assertTrue("uses COPY", indexOf(doc, (byte) 0x2f) >= 0);
      assertTrue("uses OBJECTV", indexOf(doc, (byte) 0x2d) >= 0);
      @SuppressWarnings("unchecked")
      List<Object> out = (List<Object>) decode(doc);
      assertEquals(3, out.size());
      for (Object o : out) {
        @SuppressWarnings("unchecked")
        Map<String, Object> row = (Map<String, Object>) o;
        assertEquals("Some::Class", row.get("name"));
        assertEquals("repeated text", row.get("text"));
        assertEquals("Some::Class", ((PerlObject) row.get("object")).getName());
      }
      drain(new ByteArray(doc), new DecoderOptions());
    }
  }

  private static int indexOf(byte[] haystack, byte needle) {
    for (int i = 6; i < haystack.length; i++) {
      if (haystack[i] == needle) {
        return i;
      }
    }
    return -1;
  }
}
