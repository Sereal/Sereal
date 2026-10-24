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
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;

public class V6TokenEncoderTest {
  private interface Body {
    void write(TokenEncoder encoder) throws SerealException;
  }

  private static byte[] encode(EncoderOptions options, Body body) throws SerealException {
    TokenEncoder encoder = new TokenEncoder(options);
    encoder.startDocument();
    body.write(encoder);
    encoder.endDocument();
    return encoder.getData();
  }

  private static String body(int version, Body body) throws SerealException {
    byte[] data = encode(new EncoderOptions().protocolVersion(version), body);
    return toHex(Arrays.copyOfRange(data, 6, data.length));
  }

  private static void assertMessage(String expected, Throwable e) {
    assertTrue("'" + e.getMessage() + "' should contain '" + expected + "'",
        e.getMessage().contains(expected));
  }

  // the design's sample body, written with the token API
  private static void sampleBody(TokenEncoder encoder) throws SerealException {
    StringBuilder x40 = new StringBuilder();
    for (int i = 0; i < 40; i++) {
      x40.append('x');
    }
    encoder.startHash(4);
    encoder.appendString("name");
    encoder.appendString("sereal");
    encoder.appendString("n");
    encoder.appendLong(-1000);
    encoder.appendString("list");
    encoder.startArray(4);
    encoder.appendLong(1);
    encoder.appendLong(-17);
    encoder.appendLong(1099511627776L);
    encoder.appendString(x40);
    encoder.endArray();
    encoder.appendString("v");
    encoder.appendVString(new PerlVString("v1.2.3"));
    encoder.endHash();
  }

  private static void sampleHeader(TokenEncoder encoder) throws SerealException {
    encoder.startHash(2);
    encoder.appendString("hdr");
    encoder.appendString("meta");
    encoder.appendString("n");
    encoder.appendLong(7);
    encoder.endHash();
  }

  @Test
  public void negVarint() throws SerealException {
    assertEquals("3c11", body(6, e -> e.appendLong(-17)));
    assertEquals("3ce807", body(6, e -> e.appendLong(-1000)));
    assertEquals("3c80808080808080808001", body(6, e -> e.appendLong(Long.MIN_VALUE)));
    assertEquals("10", body(6, e -> e.appendLong(-16)));
    assertEquals("2121", body(5, e -> e.appendLong(-17)));
  }

  @Test
  public void vstring() throws SerealException {
    assertEquals("3e260676312e322e33", body(6, e -> e.appendVString(new PerlVString("v1.2.3"))));
    byte[] spelling = "##v1.2.3##".getBytes(StandardCharsets.ISO_8859_1);
    assertEquals("3e260676312e322e33", body(6, e -> e.appendVString(spelling, 2, 6)));
    assertEquals("3e270476313238", body(6, e -> e.appendVString(new PerlVString("v128"))));
    byte[] v128 = "v128".getBytes(StandardCharsets.ISO_8859_1);
    assertEquals("3e270476313238", body(6, e -> e.appendVString(v128, 0, v128.length)));

    byte[] bad = "v1.2.3x".getBytes(StandardCharsets.ISO_8859_1);
    assertMessage("Invalid v-string spelling",
        assertThrows(SerealException.class, () -> body(6, e -> e.appendVString(bad, 0, bad.length))));
    assertMessage("Invalid v-string spelling",
        assertThrows(SerealException.class, () -> body(6, e -> e.appendVString(bad, 0, 0))));
    assertMessage("v-strings require Sereal protocol version 6",
        assertThrows(SerealException.class, () -> body(5, e -> e.appendVString(new PerlVString("v1.2.3")))));

    Decoder decoder = new Decoder();
    decoder.setData(encode(new EncoderOptions().protocolVersion(6), e -> e.appendVString(new PerlVString("v300.1"))));
    assertEquals(new PerlVString("v300.1"), decoder.decode());
  }

  @Test
  public void float128() throws SerealException {
    byte[] bytes = hex("000102030405060708090a0b0c0d0e0f");
    assertEquals("38000102030405060708090a0b0c0d0e0f", body(5, e -> e.appendFloat128(bytes)));
    assertEquals(V6Vectors.FLOAT128_V5, toHex(encode(new EncoderOptions().protocolVersion(5),
        e -> e.appendFloat128(bytes))));
    assertMessage("FLOAT_128 requires Sereal protocol version 5",
        assertThrows(SerealException.class, () -> body(4, e -> e.appendFloat128(bytes))));
    assertMessage("FLOAT_128 requires exactly 16 bytes",
        assertThrows(SerealException.class, () -> body(6, e -> e.appendFloat128(new byte[17]))));
  }

  @Test
  public void checksumMatchesPerl() throws SerealException {
    byte[] data = encode(new EncoderOptions().protocolVersion(6).checksumV2Key(V6Vectors.key()), e -> e.appendLong(7));
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
    Object expectedBody = normalize(V6EncoderTest.sampleBody());
    Object expectedHeader = normalize(V6EncoderTest.sampleHeader());
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
          TokenEncoder encoder = new TokenEncoder(options);

          for (int round = 0; round < 2; round++) {
            encoder.reset();
            if (withHeader) {
              encoder.startHeader();
              sampleHeader(encoder);
              encoder.endHeader();
            }
            encoder.startDocument();
            sampleBody(encoder);
            encoder.endDocument();

            byte[] data = encoder.getData();
            ByteArray reference = encoder.getDataReference();
            assertArrayEquals(what, data, Arrays.copyOfRange(reference.array, reference.start,
                reference.start + reference.length));

            assertEquals(what, 0x86 | (encodings[c] << 4), data[4] & 0xff);
            assertEquals(what, type == 0 ? 8 : 16, SerealChecksum.getChecksum(data).length);
            assertTrue(what, SerealChecksum.validateChecksum(data, V6Vectors.key()));

            byte[][] keys = new byte[3][];
            keys[type] = V6Vectors.key();
            DecoderOptions decoderOptions = new DecoderOptions()
                .checksumPolicy(Policy.DIE)
                .checksumV0Key(keys[0]).checksumV1Key(keys[1]).checksumV2Key(keys[2])
                .checksumWarningHandler(msg -> { throw new AssertionError(msg); });

            Decoder decoder = new Decoder(decoderOptions);
            decoder.setData(data);
            if (withHeader) {
              assertEquals(what, expectedHeader, normalize(decoder.decodeHeader()));
            }
            assertEquals(what, expectedBody, normalize(decoder.decode()));

            TokenDecoder tokenDecoder = new TokenDecoder(decoderOptions);
            tokenDecoder.setData(data);
            tokenDecoder.prepareDecodeBody();
            List<String> tokens = V6TokenDecoderTest.tokens(tokenDecoder);
            assertTrue(what, tokens.contains("VSTRING:false:v1.2.3"));
            assertTrue(what, tokens.contains("LONG:-1000"));
          }
        }
      }
    }
  }

  @Test
  public void constructionErrors() {
    IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> new TokenEncoder(
        new EncoderOptions().protocolVersion(6).checksumV0Key(V6Vectors.key()).checksumV1Key(V6Vectors.key())));
    assertEquals("Specify only one of checksumV0Key, checksumV1Key or checksumV2Key", e.getMessage());

    e = assertThrows(IllegalArgumentException.class, () -> new TokenEncoder(
        new EncoderOptions().protocolVersion(5).checksumV2Key(V6Vectors.key())));
    assertEquals("Checksum keys require Sereal protocol version 6 or later", e.getMessage());

    e = assertThrows(IllegalArgumentException.class, () -> new TokenEncoder(
        new EncoderOptions().protocolVersion(6).checksumPolicy(Policy.REQUIRE_KEY)));
    assertEquals("checksum policy REQUIRE_KEY requires at least one checksum key", e.getMessage());
  }
}
