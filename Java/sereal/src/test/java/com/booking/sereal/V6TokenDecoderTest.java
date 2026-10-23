package com.booking.sereal;

import static com.booking.sereal.TestUtils.hex;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.SerealChecksum.Policy;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;

public class V6TokenDecoderTest {
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

  // describe every token of the body (or header) up to and including END
  static List<String> tokens(TokenDecoder decoder) throws SerealException {
    List<String> out = new ArrayList<>();
    SerealToken token;
    do {
      token = decoder.nextToken();
      out.add(describe(decoder, token));
    } while (token != SerealToken.END);
    return out;
  }

  static String describe(TokenDecoder decoder, SerealToken token) {
    switch (token) {
      case LONG:
      case UNSIGNED_LONG:
        return token + ":" + decoder.longValue();
      case FLOAT:
        return token + ":" + decoder.floatValue();
      case DOUBLE:
        return token + ":" + decoder.doubleValue();
      case BINARY:
      case UTF8:
      case VSTRING:
      case OBJECT_START:
        return token + ":" + decoder.binaryIsUtf8() + ":" + new String(decoder.decoderBuffer(),
            decoder.binarySliceStart(), decoder.binarySliceLength(), StandardCharsets.ISO_8859_1);
      case FLOAT128:
        return token + ":" + TestUtils.toHex(Arrays.copyOfRange(decoder.decoderBuffer(),
            decoder.binarySliceStart(), decoder.binarySliceEnd()));
      case HASH_START:
      case ARRAY_START:
        return token + ":" + decoder.elementCount();
      default:
        return token.toString();
    }
  }

  private static List<String> body(DecoderOptions options, byte[] data) throws SerealException {
    TokenDecoder decoder = new TokenDecoder(options);
    decoder.setData(data);
    decoder.prepareDecodeBody();
    return tokens(decoder);
  }

  private static List<String> body(DecoderOptions options, String hexData) throws SerealException {
    return body(options, hex(hexData));
  }

  private static String single(DecoderOptions options, String hexData) throws SerealException {
    List<String> tokens = body(options, hexData);
    assertEquals(2, tokens.size());
    return tokens.get(0);
  }

  private static byte[] concat(byte[] a, byte[] b) {
    byte[] out = Arrays.copyOf(a, a.length + b.length);
    System.arraycopy(b, 0, out, a.length, b.length);
    return out;
  }

  private static void assertFails(String message, ThrowingRunnable runnable) {
    SerealException e = assertThrows(SerealException.class, runnable);
    assertTrue("'" + e.getMessage() + "' should contain '" + message + "'",
        e.getMessage().contains(message));
  }

  @Test
  public void samplePacketsDecode() throws SerealException {
    List<String> expected = body(keyed(), V6Vectors.PLAIN_V6);
    assertTrue(expected.contains("LONG:-1000"));
    assertTrue(expected.contains("LONG:-17"));
    assertTrue(expected.contains("VSTRING:false:v1.2.3"));
    String[] samples = {
      V6Vectors.RAW_V0KEY, V6Vectors.RAW_V1KEY, V6Vectors.RAW_V2KEY, V6Vectors.ZLIB_V2KEY,
      V6Vectors.ZSTD_V2KEY, V6Vectors.SNAPPY_INCR_V0KEY, V6Vectors.RAW_V1KEY_WITH_HEADER,
    };
    for (String sample : samples) {
      assertEquals(sample, expected, body(keyed(), sample));
    }

    TokenDecoder decoder = new TokenDecoder(keyed());
    decoder.setData(hex(V6Vectors.RAW_V1KEY_WITH_HEADER));
    assertTrue(decoder.hasHeader());
    decoder.prepareDecodeHeader();
    List<String> header = tokens(decoder);
    assertTrue(header.toString(), header.contains("BINARY:false:meta"));
    assertTrue(header.toString(), header.contains("LONG:7"));
    decoder.prepareDecodeBody();
    assertEquals(expected, tokens(decoder));

    for (String sample : new String[] {
        V6Vectors.LEGACY_SNAPPY_TYPE0, V6Vectors.INCR_SNAPPY_TYPE2, V6Vectors.RAW_TYPE1_SHORT}) {
      assertEquals(sample, "BINARY:false:k", single(key915(), sample));
    }

    byte[] garbage = concat(hex(V6Vectors.RAW_V2KEY_7), "garbage".getBytes(StandardCharsets.ISO_8859_1));
    assertEquals(Arrays.asList("LONG:7", "END"), body(keyed(), garbage));

    byte[] packet = hex(V6Vectors.RAW_V2KEY);
    decoder = new TokenDecoder(keyed());
    decoder.setData(new ByteArray(concat(hex("aabbcc"), packet), 3, packet.length));
    decoder.prepareDecodeBody();
    assertEquals(expected, tokens(decoder));

    assertEquals(Collections.emptyList(), warnings);
  }

  @Test
  public void policies() throws SerealException {
    byte[] wrong = new byte[16];
    Arrays.fill(wrong, (byte) 'A');

    assertEquals("LONG:7", single(unkeyed(), V6Vectors.RAW_V2KEY_7));
    assertEquals(Arrays.asList(NO_KEY_WARNING), warnings);
    warnings.clear();

    assertFails("requires a checksum key for type 2",
        () -> body(unkeyed().checksumPolicy(Policy.DIE), V6Vectors.RAW_V2KEY_7));
    assertFails("verification failed", () -> body(unkeyed().checksumV2Key(wrong), V6Vectors.RAW_V2KEY_7));
    assertFails("verification failed",
        () -> body(unkeyed().checksumV2Key(wrong).checksumPolicy(Policy.DIE), V6Vectors.RAW_V2KEY_7));
    assertEquals(Collections.emptyList(), warnings);

    assertEquals("LONG:7", single(unkeyed().checksumV2Key(wrong).checksumPolicy(Policy.IGNORE), V6Vectors.RAW_V2KEY_7));
    assertEquals(Collections.emptyList(), warnings);

    assertEquals("LONG:7",
        single(unkeyed().checksumV2Key(wrong).checksumPolicy(Policy.IGNORE_WARN), V6Vectors.RAW_V2KEY_7));
    assertEquals(Arrays.asList(IGNORE_WARNING), warnings);
    warnings.clear();

    assertEquals("LONG:7", single(keyed(), V6Vectors.UNKNOWN_TYPE3));
    assertEquals(Arrays.asList(NO_KEY_WARNING), warnings);
    warnings.clear();

    assertFails("Unsupported Sereal checksum type 3",
        () -> body(keyed().checksumPolicy(Policy.DIE), V6Vectors.UNKNOWN_TYPE3));
  }

  @Test
  public void requireKey() throws SerealException {
    DecoderOptions options = unkeyed().checksumV2Key(V6Vectors.key()).checksumPolicy(Policy.REQUIRE_KEY);
    assertFails("checksum_policy 'require_key' requires a checksummed packet",
        () -> body(options, V6Vectors.PLAIN_V6));
    assertEquals("LONG:7", single(options, V6Vectors.RAW_V2KEY_7));
  }

  @Test
  public void optionsAreHonoured() throws SerealException {
    IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
        () -> new TokenDecoder(new DecoderOptions().checksumPolicy(Policy.REQUIRE_KEY)));
    assertEquals("checksum policy REQUIRE_KEY requires at least one checksum key", e.getMessage());
    // the keys and the warning handler come from the options
    assertEquals("LONG:7", single(keyed().checksumPolicy(Policy.DIE), V6Vectors.RAW_V2KEY_7));
    assertEquals(Collections.emptyList(), warnings);
  }

  @Test
  public void tamper() throws SerealException {
    byte[] packet = hex(V6Vectors.RAW_V2KEY);
    packet[packet.length - 1] ^= 1;
    assertFails("verification failed", () -> body(keyed(), packet));

    byte[] withHeader = hex(V6Vectors.RAW_V1KEY_WITH_HEADER);
    withHeader[10] ^= 1;
    TokenDecoder decoder = new TokenDecoder(keyed());
    decoder.setData(withHeader);
    assertFails("verification failed", decoder::hasHeader);
    assertFails("verification failed", decoder::prepareDecodeHeader);
    assertFails("verification failed", decoder::prepareDecodeBody);
  }

  @Test
  public void payloadLengthMismatch() throws SerealException {
    for (String sample : new String[] {V6Vectors.TRAILING_BYTE, V6Vectors.TRAILING_PAD}) {
      TokenDecoder decoder = new TokenDecoder(keyed());
      decoder.setData(hex(sample));
      decoder.prepareDecodeBody();
      assertEquals(SerealToken.LONG, decoder.nextToken());
      assertEquals(7, decoder.longValue());
      // the nextToken() that would return END reports the mismatch
      assertFails("Checksummed Sereal payload length mismatch", decoder::nextToken);
    }
    assertFails("Unexpected end of data", () -> body(keyed(), V6Vectors.PAYLOAD_LEN_0));

    // the header of a checksummed document ends before the payload: no mismatch there
    TokenDecoder decoder = new TokenDecoder(keyed());
    decoder.setData(hex(V6Vectors.RAW_V1KEY_WITH_HEADER));
    decoder.prepareDecodeHeader();
    List<String> header = tokens(decoder);
    assertEquals("END", header.get(header.size() - 1));
  }

  @Test
  public void ignoreStillValidatesFraming() {
    byte[] packet = hex(V6Vectors.RAW_V2KEY_7);
    packet[7] = 0x08;
    assertFails("Invalid checksum length 8 for checksum type 2",
        () -> body(unkeyed().checksumPolicy(Policy.IGNORE), packet));
  }

  @Test
  public void versionByteWithHighBit() throws SerealException {
    assertFails("Checksummed Sereal documents require protocol version 6 or later",
        () -> body(keyed(), V6Vectors.CHECKSUM_ON_V5));
    assertFails("Checksummed Sereal documents require protocol version 6 or later",
        () -> body(unkeyed().checksumPolicy(Policy.DIE), V6Vectors.CHECKSUM_ON_V5));
    assertFails("unsupported protocol version 7", () -> body(keyed(), V6Vectors.VERSION_7 + "0007"));
    assertEquals("LONG:7", single(keyed(), V6Vectors.PLAIN_V6_7 + "3f"));
    assertEquals("LONG:7", single(keyed(), V6Vectors.PLAIN_V5_7 + "3f"));
  }

  @Test
  public void legacySnappyNeedsEnvelope() throws SerealException {
    assertFails("Unsupported encoding 1", () -> body(key915(), V6Vectors.LEGACY_SNAPPY_UNSIGNED));
    assertEquals("BINARY:false:k", single(key915(), V6Vectors.LEGACY_SNAPPY_TYPE0));
  }

  @Test
  public void negVarint() throws SerealException {
    assertEquals("LONG:-17", single(keyed(), V6Vectors.NEG17 + "3f3f"));
    assertEquals("LONG:-1000", single(keyed(), V6Vectors.NEG1000));
    assertEquals("LONG:" + Long.MIN_VALUE, single(keyed(), V6Vectors.LONG_MIN));
    assertFails("Invalid NEG_VARINT magnitude 0", () -> body(keyed(), V6Vectors.NEG_VARINT_ZERO + "00"));
    assertFails("Invalid NEG_VARINT magnitude 9223372036854775809",
        () -> body(keyed(), V6Vectors.NEG_VARINT_TOO_BIG));
    assertFails("NEG_VARINT tag requires protocol version 6", () -> body(keyed(), V6Vectors.NEG_VARINT_V5 + "00"));
    assertEquals("LONG:-17", single(keyed(), V6Vectors.V5_ZIGZAG_NEG17 + "3f"));
  }

  @Test
  public void vstring() throws SerealException {
    assertEquals("VSTRING:false:v1.2.3", single(keyed(), V6Vectors.VSTRING_1_2_3));
    assertEquals("VSTRING:true:v300.1", single(keyed(), V6Vectors.VSTRING_300_1));
    assertEquals("VSTRING:false:v127", single(keyed(), V6Vectors.VSTRING_127));
    assertEquals("VSTRING:true:v128", single(keyed(), V6Vectors.VSTRING_128));

    assertFails("Invalid extended VSTRING spelling length", () -> body(keyed(), V6Vectors.VSTRING_EMPTY));
    assertFails("Invalid extended VSTRING spelling", () -> body(keyed(), V6Vectors.VSTRING_BAD_SPELLING));
    assertFails("VSTRING extension ID does not match the decoded string encoding",
        () -> body(keyed(), V6Vectors.VSTRING_V128_AS_BINARY));
    assertFails("VSTRING extension ID does not match the decoded string encoding",
        () -> body(keyed(), V6Vectors.VSTRING_V127_AS_UTF8));
    assertFails("but the configured maximum is just 3",
        () -> body(keyed().maxStringLength(3), V6Vectors.VSTRING_1_2_3));
    assertFails("Unexpected end of data", () -> body(keyed(), "3df3726c06003e260676312e32"));
  }

  @Test
  public void undefDeadly() throws SerealException {
    assertEquals("UNDEF", single(keyed(), V6Vectors.UNDEF_DEADLY));
    assertEquals("UNDEF", single(keyed().undefDeadlyPolicy("undef"), V6Vectors.UNDEF_DEADLY));
    assertEquals("UNDEF", single(keyed().undefDeadlyPolicy("warn")
        .undefDeadlyWarningHandler(warnings::add), V6Vectors.UNDEF_DEADLY));
    assertEquals(1, warnings.size());
    assertTrue(warnings.get(0).contains("UNDEF_DEADLY"));
    assertFails("Encountered UNDEF_DEADLY extended tag",
        () -> body(keyed().undefDeadlyPolicy("die"), V6Vectors.UNDEF_DEADLY));
    assertFails("Unknown extended tag ID 0", () -> body(keyed(), "3df3726c06003e00"));
    assertFails("Unknown extended tag ID 5", () -> body(keyed(), V6Vectors.EXTEND_UNKNOWN_5));
    assertFails("EXTEND tag requires protocol version 6 or later", () -> body(keyed(), V6Vectors.EXTEND_V5));
  }

  @Test
  public void yesNoFloat128() throws SerealException {
    assertEquals("TRUE", single(keyed(), V6Vectors.YES + "3f"));
    assertEquals("FALSE", single(keyed(), V6Vectors.NO + "3f"));
    assertEquals("TRUE", single(keyed(), V6Vectors.YES_TAG + "3f"));
    assertEquals("FALSE", single(keyed(), V6Vectors.NO_TAG + "3f"));
    assertFails("YES tag requires protocol version 5 or later", () -> body(keyed(), "3df3726c0400353f"));

    TokenDecoder decoder = new TokenDecoder(keyed());
    decoder.setData(hex(V6Vectors.FLOAT128_V5));
    decoder.prepareDecodeBody();
    assertEquals(SerealToken.FLOAT128, decoder.nextToken());
    assertEquals(16, decoder.binarySliceLength());
    assertArrayEquals(hex("000102030405060708090a0b0c0d0e0f"), Arrays.copyOfRange(decoder.decoderBuffer(),
        decoder.binarySliceStart(), decoder.binarySliceEnd()));
    assertFalse(decoder.binaryIsUtf8());
    assertFails("Unexpected end of data", () -> body(keyed(), "3df3726c05003800010203040506070809"));
  }
}
