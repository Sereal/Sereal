package com.booking.sereal;

import static com.booking.sereal.TestUtils.hex;
import static com.booking.sereal.TestUtils.normalize;
import static com.booking.sereal.TestUtils.toHex;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.EncoderOptions.CompressionType;
import com.booking.sereal.SerealChecksum.Policy;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.Test;

public class V6EncoderTest {
  private static byte[] encode(int version, Object value) throws SerealException {
    return new Encoder(new EncoderOptions().protocolVersion(version)).write(value).getData();
  }

  // body bytes of a document without user header (header is 6 bytes)
  private static String body(int version, Object value) throws SerealException {
    byte[] data = encode(version, value);
    return toHex(Arrays.copyOfRange(data, 6, data.length));
  }

  static Map<String, Object> sampleBody() {
    StringBuilder x40 = new StringBuilder();
    for (int i = 0; i < 40; i++) {
      x40.append('x');
    }
    Map<String, Object> body = new LinkedHashMap<>();
    body.put("name", "sereal");
    body.put("n", -1000L);
    body.put("list", Arrays.asList(1L, -17L, 1099511627776L, x40.toString()));
    body.put("v", new PerlVString("v1.2.3"));
    return body;
  }

  static Map<String, Object> sampleHeader() {
    Map<String, Object> header = new HashMap<>();
    header.put("hdr", "meta");
    header.put("n", 7L);
    return header;
  }

  private static void assertMessage(String expected, Throwable e) {
    assertTrue("'" + e.getMessage() + "' should contain '" + expected + "'",
        e.getMessage().contains(expected));
  }

  @Test
  public void v6Header() throws SerealException {
    assertEquals(V6Vectors.PLAIN_V6_7, toHex(encode(6, 7L)));
    assertEquals(V6Vectors.PLAIN_V5_7, toHex(encode(5, 7L)));
  }

  @Test
  public void negVarint() throws SerealException {
    assertEquals("3c11", body(6, -17L));
    assertEquals("3ce807", body(6, -1000L));
    assertEquals("3c80808080808080808001", body(6, Long.MIN_VALUE));
    assertEquals("10", body(6, -16L));
    assertEquals("3c11", body(6, Integer.valueOf(-17)));
    assertEquals("2121", body(5, -17L));
    assertEquals("3ce807", body(6, BigInteger.valueOf(-1000)));
    assertEquals(V6Vectors.NEG1000, toHex(encode(6, -1000L)));
    assertEquals(V6Vectors.LONG_MIN, toHex(encode(6, Long.MIN_VALUE)));
  }

  @Test
  public void vstring() throws SerealException {
    assertEquals("3e260676312e322e33", body(6, new PerlVString("v1.2.3")));
    assertEquals("3e2706763330302e31", body(6, new PerlVString("v300.1")));
    assertEquals("3e270476313238", body(6, new PerlVString("v128")));
    assertEquals(V6Vectors.VSTRING_1_2_3, toHex(encode(6, new PerlVString("v1.2.3"))));
    assertMessage("v-strings require Sereal protocol version 6",
        assertThrows(SerealException.class, () -> encode(5, new PerlVString("v1.2.3"))));

    Decoder decoder = new Decoder();
    decoder.setData(encode(6, new PerlVString("v65.66.67")));
    assertEquals(new PerlVString("v65.66.67"), decoder.decode());
  }

  @Test
  public void float128() throws SerealException {
    byte[] bytes = hex("000102030405060708090a0b0c0d0e0f");
    assertEquals("38000102030405060708090a0b0c0d0e0f", body(5, new Float128(bytes)));
    assertEquals(V6Vectors.FLOAT128_V5, toHex(encode(5, new Float128(bytes))));
    assertMessage("FLOAT_128 requires Sereal protocol version 5",
        assertThrows(SerealException.class, () -> encode(4, new Float128(bytes))));

    Decoder decoder = new Decoder();
    decoder.setData(encode(6, new Float128(bytes)));
    assertEquals(new Float128(bytes), decoder.decode());
  }

  @Test
  public void checksumMatchesPerl() throws SerealException {
    byte[] data = new Encoder(new EncoderOptions().protocolVersion(6).checksumV2Key(V6Vectors.key()))
        .write(7L).getData();
    assertEquals(V6Vectors.RAW_V2KEY_7, toHex(data));
  }

  private static EncoderOptions keyOption(EncoderOptions options, int type, byte[] key) {
    switch (type) {
      case 0:
        return options.checksumV0Key(key);
      case 1:
        return options.checksumV1Key(key);
      default:
        return options.checksumV2Key(key);
    }
  }

  @Test
  public void checksumRoundtrip() throws SerealException {
    Object expectedBody = normalize(sampleBody());
    Object expectedHeader = normalize(sampleHeader());
    CompressionType[] compressions = {
      CompressionType.NONE, CompressionType.SNAPPY, CompressionType.ZLIB, CompressionType.ZSTD,
    };
    int[] encodings = {0, 2, 3, 4};

    for (int c = 0; c < compressions.length; c++) {
      for (int type = 0; type < 3; type++) {
        for (boolean withHeader : new boolean[] {false, true}) {
          String what = compressions[c] + "/type " + type + "/header " + withHeader;
          EncoderOptions options = keyOption(new EncoderOptions().protocolVersion(6), type, V6Vectors.key())
              .compressionType(compressions[c])
              .compressionThreshold(0);
          Encoder encoder = new Encoder(options);

          for (int round = 0; round < 2; round++) {
            if (withHeader) {
              encoder.write(sampleBody(), sampleHeader());
            } else {
              encoder.write(sampleBody());
            }
            byte[] data = encoder.getData();
            ByteArray reference = encoder.getDataReference();
            assertArrayEquals(what, data, Arrays.copyOfRange(reference.array, reference.start,
                reference.start + reference.length));

            assertEquals(what, 0x86 | (encodings[c] << 4), data[4] & 0xff);
            assertEquals(what, type == 0 ? 8 : 16, SerealChecksum.getChecksum(data).length);
            assertTrue(what, SerealChecksum.validateChecksum(data, V6Vectors.key()));

            DecoderOptions decoderOptions = new DecoderOptions()
                .checksumPolicy(Policy.DIE)
                .checksumWarningHandler(msg -> { throw new AssertionError(msg); });
            byte[][] keys = new byte[3][];
            keys[type] = V6Vectors.key();
            decoderOptions.checksumV0Key(keys[0]).checksumV1Key(keys[1]).checksumV2Key(keys[2]);
            Decoder decoder = new Decoder(decoderOptions);
            decoder.setData(data);
            if (withHeader) {
              assertEquals(what, expectedHeader, normalize(decoder.decodeHeader()));
            }
            assertEquals(what, expectedBody, normalize(decoder.decode()));
          }
        }
      }
    }
  }

  @Test
  public void constructionErrors() {
    IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> new Encoder(
        new EncoderOptions().protocolVersion(6).checksumV0Key(V6Vectors.key()).checksumV2Key(V6Vectors.key())));
    assertEquals("Specify only one of checksumV0Key, checksumV1Key or checksumV2Key", e.getMessage());

    e = assertThrows(IllegalArgumentException.class, () -> new Encoder(
        new EncoderOptions().protocolVersion(4).checksumV2Key(V6Vectors.key())));
    assertEquals("Checksum keys require Sereal protocol version 6 or later", e.getMessage());

    e = assertThrows(IllegalArgumentException.class, () -> new Encoder(
        new EncoderOptions().protocolVersion(6).checksumPolicy(Policy.REQUIRE_KEY)));
    assertEquals("checksum policy REQUIRE_KEY requires at least one checksum key", e.getMessage());
  }
}
