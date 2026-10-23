package com.booking.sereal;

import static com.booking.sereal.TestUtils.hex;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.SerealChecksum.Policy;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;

public class V6DecoderTest {
  private static final String NO_KEY_WARNING =
      "Decoding checksummed Sereal document without authentication: no matching checksum key is configured";
  private static final String IGNORE_WARNING = "Ignoring Sereal checksum authentication by request";

  private final List<String> warnings = new ArrayList<>();

  private DecoderOptions keyed() {
    byte[] k = V6Vectors.key();
    return new DecoderOptions()
        .checksumV0Key(k)
        .checksumV1Key(k)
        .checksumV2Key(k)
        .checksumWarningHandler(warnings::add);
  }

  private DecoderOptions unkeyed() {
    return new DecoderOptions().checksumWarningHandler(warnings::add);
  }

  private DecoderOptions key915() {
    byte[] k = V6Vectors.key915();
    return new DecoderOptions()
        .checksumV0Key(k)
        .checksumV1Key(k)
        .checksumV2Key(k)
        .checksumWarningHandler(warnings::add);
  }

  private static Object decode(DecoderOptions options, byte[] data) throws SerealException {
    Decoder decoder = new Decoder(options);
    decoder.setData(data);
    return decoder.decode();
  }

  private static Object decode(DecoderOptions options, String hexData) throws SerealException {
    return decode(options, hex(hexData));
  }

  private static byte[] concat(byte[] a, byte[] b) {
    byte[] out = Arrays.copyOf(a, a.length + b.length);
    System.arraycopy(b, 0, out, a.length, b.length);
    return out;
  }

  private static String latin1(byte[] b) {
    return new String(b, StandardCharsets.ISO_8859_1);
  }

  // byte[] has identity equality: turn byte strings into Strings so decoded structures compare
  @SuppressWarnings("unchecked")
  private static Object normalize(Object value) {
    if (value instanceof byte[]) {
      return latin1((byte[]) value);
    } else if (value instanceof Map) {
      Map<Object, Object> out = new HashMap<>();
      for (Map.Entry<Object, Object> e : ((Map<Object, Object>) value).entrySet()) {
        out.put(e.getKey(), normalize(e.getValue()));
      }
      return out;
    } else if (value instanceof List) {
      List<Object> out = new ArrayList<>();
      for (Object o : (List<Object>) value) {
        out.add(normalize(o));
      }
      return out;
    }
    return value;
  }

  private static void assertFails(String message, ThrowingRunnable runnable) {
    SerealException e = assertThrows(SerealException.class, runnable);
    assertTrue("'" + e.getMessage() + "' should contain '" + message + "'",
        e.getMessage().contains(message));
  }

  @Test
  public void samplePacketsDecode() throws SerealException {
    Object expected = normalize(decode(keyed(), V6Vectors.PLAIN_V6));
    String[] samples = {
      V6Vectors.RAW_V0KEY, V6Vectors.RAW_V1KEY, V6Vectors.RAW_V2KEY, V6Vectors.ZLIB_V2KEY,
      V6Vectors.ZSTD_V2KEY, V6Vectors.SNAPPY_INCR_V0KEY, V6Vectors.RAW_V1KEY_WITH_HEADER,
    };
    for (String sample : samples) {
      assertEquals(sample, expected, normalize(decode(keyed(), sample)));
    }

    Decoder decoder = new Decoder(keyed());
    decoder.setData(hex(V6Vectors.RAW_V1KEY_WITH_HEADER));
    assertTrue(decoder.hasHeader());
    Map<String, Object> header = new HashMap<>();
    header.put("hdr", "meta");
    header.put("n", 7L);
    assertEquals(header, normalize(decoder.decodeHeader()));
    assertEquals(expected, normalize(decoder.decode()));

    for (String sample : new String[] {
        V6Vectors.LEGACY_SNAPPY_TYPE0, V6Vectors.INCR_SNAPPY_TYPE2, V6Vectors.RAW_TYPE1_SHORT}) {
      assertEquals(sample, "k", normalize(decode(key915(), sample)));
    }

    byte[] garbage = concat(hex(V6Vectors.RAW_V2KEY_7), "garbage".getBytes(StandardCharsets.ISO_8859_1));
    assertEquals(7L, decode(keyed(), garbage));

    // a document that does not start at offset 0 of its array
    byte[] packet = hex(V6Vectors.RAW_V2KEY);
    byte[] shifted = concat(hex("aabbcc"), packet);
    decoder = new Decoder(keyed());
    decoder.setData(new ByteArray(shifted, 3, packet.length));
    assertEquals(expected, normalize(decoder.decode()));

    assertEquals(Collections.emptyList(), warnings);
  }

  @Test
  public void plainV6Body() throws SerealException {
    @SuppressWarnings("unchecked")
    Map<String, Object> body = (Map<String, Object>) decode(keyed(), V6Vectors.PLAIN_V6);
    assertEquals(-1000L, body.get("n"));
    assertEquals("sereal", normalize(body.get("name")));
    StringBuilder x40 = new StringBuilder();
    for (int i = 0; i < 40; i++) {
      x40.append('x');
    }
    // Perl's 2**40 is an NV: the Perl encoder emits it as FLOAT (22 00008053), not as an integer
    assertEquals(Arrays.asList(1L, -17L, 1099511627776f, x40.toString()), normalize(body.get("list")));
    assertEquals(new PerlVString("v1.2.3"), body.get("v"));
  }

  @Test
  public void policies() throws SerealException {
    byte[] wrong = new byte[16];
    Arrays.fill(wrong, (byte) 'A');

    assertEquals(7L, decode(unkeyed(), V6Vectors.RAW_V2KEY_7));
    assertEquals(Arrays.asList(NO_KEY_WARNING), warnings);
    warnings.clear();

    assertFails("requires a checksum key for type 2",
        () -> decode(unkeyed().checksumPolicy(Policy.DIE), V6Vectors.RAW_V2KEY_7));
    assertFails("verification failed",
        () -> decode(unkeyed().checksumV2Key(wrong), V6Vectors.RAW_V2KEY_7));
    assertFails("verification failed",
        () -> decode(unkeyed().checksumV2Key(wrong).checksumPolicy(Policy.DIE), V6Vectors.RAW_V2KEY_7));
    assertEquals(Collections.emptyList(), warnings);

    assertEquals(7L, decode(unkeyed().checksumV2Key(wrong).checksumPolicy(Policy.IGNORE), V6Vectors.RAW_V2KEY_7));
    assertEquals(Collections.emptyList(), warnings);

    assertEquals(7L,
        decode(unkeyed().checksumV2Key(wrong).checksumPolicy(Policy.IGNORE_WARN), V6Vectors.RAW_V2KEY_7));
    assertEquals(Arrays.asList(IGNORE_WARNING), warnings);
    warnings.clear();

    assertEquals(7L, decode(keyed(), V6Vectors.UNKNOWN_TYPE3));
    assertEquals(Arrays.asList(NO_KEY_WARNING), warnings);
    warnings.clear();

    assertFails("Unsupported Sereal checksum type 3",
        () -> decode(keyed().checksumPolicy(Policy.DIE), V6Vectors.UNKNOWN_TYPE3));

    assertEquals(7L, decode(keyed().checksumPolicy(Policy.DIE), V6Vectors.RAW_V2KEY_7));
    assertEquals(Collections.emptyList(), warnings);
  }

  @Test
  public void requireKey() throws SerealException {
    IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
        () -> new Decoder(new DecoderOptions().checksumPolicy(Policy.REQUIRE_KEY)));
    assertEquals("checksum policy REQUIRE_KEY requires at least one checksum key", e.getMessage());

    DecoderOptions options = unkeyed().checksumV2Key(V6Vectors.key()).checksumPolicy(Policy.REQUIRE_KEY);
    assertFails("checksum_policy 'require_key' requires a checksummed packet",
        () -> decode(options, V6Vectors.PLAIN_V6));
    assertEquals(7L, decode(options, V6Vectors.RAW_V2KEY_7));
    // a key for another type is not enough
    assertFails("requires a checksum key for type 2", () -> decode(
        unkeyed().checksumV0Key(V6Vectors.key()).checksumPolicy(Policy.REQUIRE_KEY), V6Vectors.RAW_V2KEY_7));
  }

  @Test
  public void tamper() throws SerealException {
    byte[] packet = hex(V6Vectors.RAW_V2KEY);
    packet[packet.length - 1] ^= 1;
    assertFails("verification failed", () -> decode(keyed(), packet));

    byte[] withHeader = hex(V6Vectors.RAW_V1KEY_WITH_HEADER);
    withHeader[10] ^= 1; // inside the header user data ("hdr")
    Decoder decoder = new Decoder(keyed());
    decoder.setData(withHeader);
    assertFails("verification failed", decoder::hasHeader);
    // a failed verification must not leave the header marked as parsed
    assertFails("verification failed", decoder::decodeHeader);
    assertFails("verification failed", decoder::decode);
  }

  @Test
  public void payloadLengthMismatch() {
    assertFails("Checksummed Sereal payload length mismatch", () -> decode(keyed(), V6Vectors.TRAILING_BYTE));
    assertFails("Checksummed Sereal payload length mismatch", () -> decode(keyed(), V6Vectors.TRAILING_PAD));
    assertFails("Unexpected end of data", () -> decode(keyed(), V6Vectors.PAYLOAD_LEN_0));
  }

  @Test
  public void ignoreStillValidatesFraming() {
    byte[] packet = hex(V6Vectors.RAW_V2KEY_7);
    packet[7] = 0x08;
    assertFails("Invalid checksum length 8 for checksum type 2",
        () -> decode(unkeyed().checksumPolicy(Policy.IGNORE), packet));
  }

  @Test
  public void versionByteWithHighBit() throws SerealException {
    assertFails("Checksummed Sereal documents require protocol version 6 or later",
        () -> decode(keyed(), V6Vectors.CHECKSUM_ON_V5));
    assertFails("unsupported protocol version 7", () -> decode(keyed(), V6Vectors.VERSION_7));
    assertFails("unsupported protocol version 0", () -> decode(keyed(), "3df3726c000007"));
    assertEquals(7L, decode(keyed(), V6Vectors.PLAIN_V6_7));
    assertEquals(7L, decode(keyed(), V6Vectors.PLAIN_V5_7));
  }

  @Test
  public void checksumBitOnV5() {
    // fails on the version check, before any key lookup or policy
    assertFails("Checksummed Sereal documents require protocol version 6 or later",
        () -> decode(unkeyed().checksumPolicy(Policy.DIE), V6Vectors.CHECKSUM_ON_V5));
    assertFails("Checksummed Sereal documents require protocol version 6 or later",
        () -> decode(unkeyed().checksumPolicy(Policy.IGNORE), V6Vectors.CHECKSUM_ON_V5));
  }

  @Test
  public void legacySnappyNeedsEnvelope() throws SerealException {
    assertFails("Non-incremental Snappy", () -> decode(key915(), V6Vectors.LEGACY_SNAPPY_UNSIGNED));
    assertEquals("k", normalize(decode(key915(), V6Vectors.LEGACY_SNAPPY_TYPE0)));
  }

  @Test
  public void negVarint() throws SerealException {
    assertEquals(-17L, decode(keyed(), V6Vectors.NEG17));
    assertEquals(-1000L, decode(keyed(), V6Vectors.NEG1000));
    assertEquals(Long.MIN_VALUE, decode(keyed(), V6Vectors.LONG_MIN));
    assertFails("Invalid NEG_VARINT magnitude 0", () -> decode(keyed(), V6Vectors.NEG_VARINT_ZERO));
    assertFails("Invalid NEG_VARINT magnitude 9223372036854775809",
        () -> decode(keyed(), V6Vectors.NEG_VARINT_TOO_BIG));
    assertFails("NEG_VARINT tag requires protocol version 6", () -> decode(keyed(), V6Vectors.NEG_VARINT_V5));
    assertEquals(-17L, decode(keyed(), V6Vectors.V5_ZIGZAG_NEG17));
  }

  @Test
  public void vstring() throws SerealException {
    PerlVString v = (PerlVString) decode(keyed(), V6Vectors.VSTRING_1_2_3);
    assertEquals(new PerlVString("v1.2.3"), v);
    assertFalse(v.isUtf8());
    assertEquals(new PerlVString("v65.66.67"), decode(keyed(), V6Vectors.VSTRING_65_66_67));
    PerlVString utf8 = (PerlVString) decode(keyed(), V6Vectors.VSTRING_300_1);
    assertEquals("v300.1", utf8.spelling());
    assertTrue(utf8.isUtf8());
    assertFalse(((PerlVString) decode(keyed(), V6Vectors.VSTRING_127)).isUtf8());
    assertTrue(((PerlVString) decode(keyed(), V6Vectors.VSTRING_128)).isUtf8());

    assertFails("Invalid extended VSTRING spelling length", () -> decode(keyed(), V6Vectors.VSTRING_EMPTY));
    assertFails("Invalid extended VSTRING spelling", () -> decode(keyed(), V6Vectors.VSTRING_BAD_SPELLING));
    assertFails("VSTRING extension ID does not match the decoded string encoding",
        () -> decode(keyed(), V6Vectors.VSTRING_V128_AS_BINARY));
    assertFails("VSTRING extension ID does not match the decoded string encoding",
        () -> decode(keyed(), V6Vectors.VSTRING_V127_AS_UTF8));
    assertFails("but the configured maximum is just 3",
        () -> decode(keyed().maxStringLength(3), V6Vectors.VSTRING_1_2_3));
    assertFails("Unexpected end of data", () -> decode(keyed(), "3df3726c06003e260676312e32"));
  }

  @Test
  public void undefDeadly() throws SerealException {
    assertEquals(null, decode(keyed(), V6Vectors.UNDEF_DEADLY));
    assertEquals(null, decode(keyed().undefDeadlyPolicy("undef"), V6Vectors.UNDEF_DEADLY));
    assertTrue(decode(keyed().preserveUndef(true), V6Vectors.UNDEF_DEADLY) instanceof PerlUndef);
    assertEquals(null, decode(keyed().undefDeadlyPolicy("warn")
        .undefDeadlyWarningHandler(warnings::add), V6Vectors.UNDEF_DEADLY));
    assertEquals(1, warnings.size());
    assertTrue(warnings.get(0).contains("UNDEF_DEADLY"));
    assertFails("Encountered UNDEF_DEADLY extended tag",
        () -> decode(keyed().undefDeadlyPolicy("die"), V6Vectors.UNDEF_DEADLY));
    assertFails("Unknown extended tag ID 0", () -> decode(keyed(), "3df3726c06003e00"));
    assertThrows(IllegalArgumentException.class, () -> keyed().undefDeadlyPolicy("ignore"));
    assertThrows(IllegalArgumentException.class, () -> keyed().undefDeadlyPolicy(null));
    assertThrows(IllegalArgumentException.class, () -> keyed().undefDeadlyWarningHandler(null));
    assertFails("Unknown extended tag ID 5", () -> decode(keyed(), V6Vectors.EXTEND_UNKNOWN_5));
    assertFails("EXTEND tag requires protocol version 6 or later", () -> decode(keyed(), V6Vectors.EXTEND_V5));
    assertFails("Unexpected end of data", () -> decode(keyed(), "3df3726c06003e"));
  }

  @Test
  public void yesNoFloat128() throws SerealException {
    assertEquals(Boolean.TRUE, decode(keyed(), V6Vectors.YES));
    assertEquals(Boolean.FALSE, decode(keyed(), V6Vectors.NO));
    assertEquals(Boolean.TRUE, decode(keyed(), V6Vectors.YES_TAG));
    assertEquals(Boolean.FALSE, decode(keyed(), V6Vectors.NO_TAG));
    assertFails("YES tag requires protocol version 5 or later", () -> decode(keyed(), "3df3726c040035"));
    assertFails("NO tag requires protocol version 5 or later", () -> decode(keyed(), "3df3726c040034"));

    assertEquals(new Float128(hex("000102030405060708090a0b0c0d0e0f")), decode(keyed(), V6Vectors.FLOAT128_V5));
    assertFails("Unexpected end of data", () -> decode(keyed(), "3df3726c05003800010203040506070809"));
    assertFails("FLOAT_128 tag requires protocol version 5 or later",
        () -> decode(keyed(), "3df3726c040038000102030405060708090a0b0c0d0e0f"));
  }

  @Test
  public void defaultWarningHandler() throws SerealException {
    PrintStream original = System.err;
    ByteArrayOutputStream captured = new ByteArrayOutputStream();
    System.setErr(new PrintStream(captured, true));
    try {
      assertEquals(7L, decode(new DecoderOptions(), V6Vectors.RAW_V2KEY_7));
    } finally {
      System.setErr(original);
    }
    assertTrue(captured.toString().startsWith("Sereal: Decoding checksummed"));
  }
}
