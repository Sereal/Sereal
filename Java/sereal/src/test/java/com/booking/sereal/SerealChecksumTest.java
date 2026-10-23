package com.booking.sereal;

import static com.booking.sereal.TestUtils.hex;
import static com.booking.sereal.TestUtils.toHex;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.SerealChecksum.Envelope;
import com.booking.sereal.SerealChecksum.Policy;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;

public class SerealChecksumTest {
  private static final String NO_KEY_WARNING =
      "Decoding checksummed Sereal document without authentication: no matching checksum key is configured";
  private static final String IGNORE_WARNING = "Ignoring Sereal checksum authentication by request";

  private static byte[] ascii(String s) {
    return s.getBytes(StandardCharsets.ISO_8859_1);
  }

  private static byte[] repeat(char c, int n) {
    byte[] b = new byte[n];
    Arrays.fill(b, (byte) c);
    return b;
  }

  private static byte[] concat(byte[] a, byte[] b) {
    byte[] out = Arrays.copyOf(a, a.length + b.length);
    System.arraycopy(b, 0, out, a.length, b.length);
    return out;
  }

  private static void assertMessage(String expected, Throwable e) {
    assertTrue("'" + e.getMessage() + "' should contain '" + expected + "'",
        e.getMessage().contains(expected));
  }

  @Test
  public void prepareKeyVectors() {
    assertEquals("37836edd9413d706d1067ef2e57a180f", toHex(SerealChecksum.prepareKey(ascii(""), ascii(""))));
    assertEquals("83182fefa01acd13bd33da94acac43b9", toHex(SerealChecksum.prepareKey(ascii("secret"), ascii(""))));
    assertEquals("7000483230ace57b62bcce4437cae6ea", toHex(SerealChecksum.prepareKey(ascii("secret"), ascii("salt"))));
    assertEquals("f5127cc67a45860d6f10630d765afb2f",
        toHex(SerealChecksum.prepareKey(ascii("0123456789abcdef"), ascii(""))));
    assertEquals("b46131cea89b4a55baf8305215f8c939",
        toHex(SerealChecksum.prepareKey(repeat('x', 100), repeat('y', 40))));
    assertEquals("37836edd9413d706d1067ef2e57a180f", toHex(SerealChecksum.prepareKey(null, null)));
  }

  @Test
  public void getChecksumSamples() throws SerealException {
    String[][] samples = {
      {V6Vectors.RAW_V0KEY, V6Vectors.RAW_V0KEY_TAG},
      {V6Vectors.RAW_V1KEY, V6Vectors.RAW_V1KEY_TAG},
      {V6Vectors.RAW_V2KEY, V6Vectors.RAW_V2KEY_TAG},
      {V6Vectors.ZLIB_V2KEY, V6Vectors.ZLIB_V2KEY_TAG},
      {V6Vectors.ZSTD_V2KEY, V6Vectors.ZSTD_V2KEY_TAG},
      {V6Vectors.SNAPPY_INCR_V0KEY, V6Vectors.SNAPPY_INCR_V0KEY_TAG},
      {V6Vectors.RAW_V1KEY_WITH_HEADER, V6Vectors.RAW_V1KEY_WITH_HEADER_TAG},
    };
    for (String[] sample : samples) {
      assertEquals(sample[1], toHex(SerealChecksum.getChecksum(hex(sample[0]))));
    }
    assertNull(SerealChecksum.getChecksum(hex(V6Vectors.PLAIN_V6)));
    assertArrayEquals(repeat('x', 16), SerealChecksum.getChecksum(hex(V6Vectors.UNKNOWN_TYPE3)));

    byte[] packet = hex(V6Vectors.RAW_V2KEY);
    byte[] shifted = concat(hex("aabbcc"), packet);
    assertEquals(V6Vectors.RAW_V2KEY_TAG, toHex(SerealChecksum.getChecksum(shifted, 3, packet.length)));
  }

  @Test
  public void validateChecksumSamples() throws SerealException {
    String[] samples = {
      V6Vectors.RAW_V0KEY, V6Vectors.RAW_V1KEY, V6Vectors.RAW_V2KEY, V6Vectors.ZLIB_V2KEY,
      V6Vectors.ZSTD_V2KEY, V6Vectors.SNAPPY_INCR_V0KEY, V6Vectors.RAW_V1KEY_WITH_HEADER,
      V6Vectors.RAW_V2KEY_7,
    };
    for (String sample : samples) {
      assertTrue(sample, SerealChecksum.validateChecksum(hex(sample), V6Vectors.key()));
      assertFalse(sample, SerealChecksum.validateChecksum(hex(sample), repeat('A', 16)));
    }
    String[] samples915 = {
      V6Vectors.LEGACY_SNAPPY_TYPE0, V6Vectors.INCR_SNAPPY_TYPE2, V6Vectors.RAW_TYPE1_SHORT,
    };
    for (String sample : samples915) {
      assertTrue(sample, SerealChecksum.validateChecksum(hex(sample), V6Vectors.key915()));
    }
    assertFalse(SerealChecksum.validateChecksum(hex(V6Vectors.PLAIN_V6), V6Vectors.key()));

    assertMessage("requires a key", assertThrows(SerealException.class,
        () -> SerealChecksum.validateChecksum(hex(V6Vectors.RAW_V2KEY), null)));
    assertMessage("exactly 16 bytes", assertThrows(SerealException.class,
        () -> SerealChecksum.validateChecksum(hex(V6Vectors.RAW_V2KEY), new byte[15])));
    assertMessage("Unsupported Sereal checksum type 3", assertThrows(SerealException.class,
        () -> SerealChecksum.validateChecksum(hex(V6Vectors.UNKNOWN_TYPE3), V6Vectors.key())));

    assertTrue(SerealChecksum.validateChecksum(hex(V6Vectors.TRAILING_BYTE), V6Vectors.key()));
    assertTrue(SerealChecksum.validateChecksum(
        concat(hex(V6Vectors.RAW_V2KEY_7), ascii("garbage")), V6Vectors.key()));

    byte[] packet = hex(V6Vectors.RAW_V2KEY_7);
    byte[] shifted = concat(hex("aabbcc"), packet);
    assertTrue(SerealChecksum.validateChecksum(shifted, 3, packet.length, V6Vectors.key()));

    assertMessage("require protocol version 6", assertThrows(SerealException.class,
        () -> SerealChecksum.validateChecksum(hex(V6Vectors.CHECKSUM_ON_V5), V6Vectors.key())));
  }

  @Test
  public void malformedEnvelopes() throws SerealException {
    byte[] truncated = hex("3df3726c860002");
    assertMessage("Unexpected end of data", assertThrows(SerealException.class,
        () -> SerealChecksum.read(truncated, 6, truncated.length)));

    byte[] longTag = hex("3df3726c860002ac02" + V6Vectors.KEY_HEX + "0107");
    assertMessage("checksum length exceeds", assertThrows(SerealException.class,
        () -> SerealChecksum.read(longTag, 6, longTag.length)));

    byte[] shortType2 = hex("3df3726c860002080102030405060708" + "0107");
    assertMessage("Invalid checksum length 8 for checksum type 2", assertThrows(SerealException.class,
        () -> SerealChecksum.read(shortType2, 6, shortType2.length)));

    byte[] longType0 = hex("3df3726c86000010" + V6Vectors.KEY_HEX + "0107");
    assertMessage("Invalid checksum length 16 for checksum type 0", assertThrows(SerealException.class,
        () -> SerealChecksum.read(longType0, 6, longType0.length)));

    byte[] longPayload = hex("3df3726c86000210" + V6Vectors.KEY_HEX + "0507");
    assertMessage("payload length exceeds", assertThrows(SerealException.class,
        () -> SerealChecksum.read(longPayload, 6, longPayload.length)));

    byte[] unknown = hex(V6Vectors.UNKNOWN_TYPE3);
    Envelope env = SerealChecksum.read(unknown, 6, unknown.length);
    assertFalse(env.knownType);
    assertEquals(3, env.type);
    assertEquals(16, env.tagEnd - env.tagStart);
    assertEquals(1, env.payloadEnd - env.payloadStart);

    byte[] seven = hex(V6Vectors.RAW_V2KEY_7);
    env = SerealChecksum.read(seven, 6, seven.length);
    assertTrue(env.knownType);
    assertEquals(8, env.tagStart);
    assertEquals(24, env.tagEnd);
    assertEquals(25, env.payloadStart);
    assertEquals(26, env.payloadEnd);
  }

  @Test
  public void protectMatchesPerl() {
    byte[] plain = hex(V6Vectors.PLAIN_V6);
    assertEquals(91, plain.length);
    assertArrayEquals(hex(V6Vectors.RAW_V0KEY), SerealChecksum.protect(plain, 91, 6, 0, V6Vectors.key()));
    assertArrayEquals(hex(V6Vectors.RAW_V1KEY), SerealChecksum.protect(plain, 91, 6, 1, V6Vectors.key()));
    assertArrayEquals(hex(V6Vectors.RAW_V2KEY), SerealChecksum.protect(plain, 91, 6, 2, V6Vectors.key()));
    // docLength bounds the input: trailing bytes in the buffer are not part of the document
    assertArrayEquals(hex(V6Vectors.RAW_V2KEY),
        SerealChecksum.protect(concat(plain, ascii("junk")), 91, 6, 2, V6Vectors.key()));
  }

  private static Envelope process(String packet, byte[][] keys, Policy policy, List<String> warnings)
      throws SerealException {
    byte[] data = hex(packet);
    return SerealChecksum.process(data, 0, 6, data.length, keys, policy, warnings::add);
  }

  @Test
  public void processPolicies() throws SerealException {
    byte[][] noKeys = new byte[3][];
    byte[][] withKey = {null, null, V6Vectors.key()};
    byte[][] wrongKey = {null, null, repeat('A', 16)};
    List<String> warnings = new ArrayList<>();

    assertNotNull(process(V6Vectors.RAW_V2KEY_7, noKeys, Policy.VERIFY_IF_AVAILABLE, warnings));
    assertEquals(Arrays.asList(NO_KEY_WARNING), warnings);

    warnings.clear();
    Envelope env = process(V6Vectors.RAW_V2KEY_7, withKey, Policy.VERIFY_IF_AVAILABLE, warnings);
    assertEquals(25, env.payloadStart);
    assertEquals(26, env.payloadEnd);
    assertEquals(0, warnings.size());

    assertMessage("requires a checksum key for type 2", assertThrows(SerealException.class,
        () -> process(V6Vectors.RAW_V2KEY_7, noKeys, Policy.DIE, warnings)));
    assertMessage("requires a checksum key for type 2", assertThrows(SerealException.class,
        () -> process(V6Vectors.RAW_V2KEY_7, noKeys, Policy.REQUIRE_KEY, warnings)));
    assertMessage("verification failed", assertThrows(SerealException.class,
        () -> process(V6Vectors.RAW_V2KEY_7, wrongKey, Policy.DIE, warnings)));
    assertMessage("verification failed", assertThrows(SerealException.class,
        () -> process(V6Vectors.RAW_V2KEY_7, wrongKey, Policy.VERIFY_IF_AVAILABLE, warnings)));
    assertEquals(0, warnings.size());

    assertNotNull(process(V6Vectors.RAW_V2KEY_7, wrongKey, Policy.IGNORE, warnings));
    assertEquals(0, warnings.size());

    assertNotNull(process(V6Vectors.RAW_V2KEY_7, wrongKey, Policy.IGNORE_WARN, warnings));
    assertEquals(Arrays.asList(IGNORE_WARNING), warnings);

    warnings.clear();
    assertMessage("Unsupported Sereal checksum type 3", assertThrows(SerealException.class,
        () -> process(V6Vectors.UNKNOWN_TYPE3, withKey, Policy.DIE, warnings)));
    assertEquals(0, warnings.size());

    assertNotNull(process(V6Vectors.UNKNOWN_TYPE3, withKey, Policy.VERIFY_IF_AVAILABLE, warnings));
    assertEquals(Arrays.asList(NO_KEY_WARNING), warnings);
  }

  @Test
  public void optionsRejectBadKeys() {
    assertMessage("checksumV2Key", assertThrows(IllegalArgumentException.class,
        () -> new DecoderOptions().checksumV2Key(new byte[15])));
    assertMessage("checksumV0Key", assertThrows(IllegalArgumentException.class,
        () -> new EncoderOptions().checksumV0Key(new byte[17])));
    assertMessage("exactly 16 bytes", assertThrows(IllegalArgumentException.class,
        () -> new DecoderOptions().checksumV1Key(new byte[0])));

    DecoderOptions decoderOptions = new DecoderOptions().checksumV1Key(V6Vectors.key());
    assertArrayEquals(V6Vectors.key(), decoderOptions.checksumV1Key());
    decoderOptions.checksumV1Key()[0] = 0;
    assertArrayEquals(V6Vectors.key(), decoderOptions.checksumV1Key());
    decoderOptions.checksumV1Key(null);
    assertNull(decoderOptions.checksumV1Key());

    // the setter copies its argument
    byte[] key = V6Vectors.key();
    EncoderOptions encoderOptions = new EncoderOptions().checksumV2Key(key);
    key[0] = 0;
    assertArrayEquals(V6Vectors.key(), encoderOptions.checksumV2Key());
    encoderOptions.checksumV2Key()[1] = 0;
    assertArrayEquals(V6Vectors.key(), encoderOptions.checksumV2Key());
    encoderOptions.checksumV2Key(null);
    assertNull(encoderOptions.checksumV2Key());

    assertEquals(Policy.VERIFY_IF_AVAILABLE, new DecoderOptions().checksumPolicy());
    assertEquals(Policy.VERIFY_IF_AVAILABLE, new EncoderOptions().checksumPolicy());
    assertEquals(Policy.DIE, new DecoderOptions().checksumPolicy(Policy.DIE).checksumPolicy());
    assertNotNull(new DecoderOptions().checksumWarningHandler());
    assertNotNull(new EncoderOptions().checksumWarningHandler());

    assertEquals(6, new EncoderOptions().protocolVersion(6).protocolVersion());
    assertEquals(5, new EncoderOptions().protocolVersion(5).protocolVersion());
    assertEquals(4, new EncoderOptions().protocolVersion());
    assertMessage("Unknown Sereal version 7", assertThrows(IllegalArgumentException.class,
        () -> new EncoderOptions().protocolVersion(7)));
  }
}
