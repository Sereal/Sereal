package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.nio.charset.StandardCharsets;
import org.junit.Test;

/**
 * SipHash vectors generated from the Perl Sereal::Decoder build (siphash24, siphash24_128 and
 * siphash36), key 000102...0f.
 */
public class SipHashTest {
  private static final byte[] KEY = new byte[16];

  static {
    for (int i = 0; i < 16; i++) {
      KEY[i] = (byte) i;
    }
  }

  private static final String[] DATA = {
    "", "a", "abcdefgh", "abcdefghi", "The quick brown fox jumps over the lazy dog",
  };

  private static final String[] HASH24 = {
    "310e0edd47db6f72",
    "ca4811a7e9e8a32b",
    "7044d491a3dd29c3",
    "a4ec9d6dc4e1a9b2",
    "e46f1fdc05612752",
  };

  private static final String[] HASH24_128 = {
    "a3817f04ba25a8e66df67214c7550293",
    "0ea4f2fb02ad21d92d5fc91684f26426",
    "8c93e201ac8e8396897e3133e447eda3",
    "247f7b6adc31aa1c1ec5cb1076ab0bd2",
    "7628c9301aa4412555e65227cd31964e",
  };

  private static final String[] HASH36_128 = {
    "37228a2b7a1e505b60ae19c48695eb27",
    "702e5fd82fb0b0c875cb8123c5b4282b",
    "3f6cb86db4351c887760e15c72ed0ac6",
    "5df18ae5838274bdd01091d4c6df0cdf",
    "b343e2a6f2cc0ddadd51e16b5851a1ef",
  };

  private static byte[] bytes(String s) {
    return s.getBytes(StandardCharsets.ISO_8859_1);
  }

  private static String hex(byte[] b) {
    StringBuilder sb = new StringBuilder();
    for (byte x : b) {
      sb.append(String.format("%02x", x & 0xff));
    }
    return sb.toString();
  }

  @Test
  public void vectors24() {
    for (int i = 0; i < DATA.length; i++) {
      assertEquals(DATA[i], HASH24[i], hex(SipHash.hash24(KEY, bytes(DATA[i]))));
    }
  }

  @Test
  public void vectors24_128() {
    for (int i = 0; i < DATA.length; i++) {
      assertEquals(DATA[i], HASH24_128[i], hex(SipHash.hash24_128(KEY, bytes(DATA[i]))));
    }
  }

  @Test
  public void vectors36_128() {
    for (int i = 0; i < DATA.length; i++) {
      assertEquals(DATA[i], HASH36_128[i], hex(SipHash.hash36_128(KEY, bytes(DATA[i]))));
    }
  }

  private static String streamed(int type, String... parts) {
    SipHash h = SipHash.forType(type, KEY);
    for (String part : parts) {
      byte[] b = bytes(part);
      h.update(b, 0, b.length);
    }
    return hex(h.digest());
  }

  @Test
  public void multiPartEqualsSinglePart() {
    assertEquals(HASH24[2], streamed(0, "abcd", "efgh"));
    assertEquals(HASH24_128[2], streamed(1, "abcd", "efgh"));
    assertEquals(HASH36_128[2], streamed(2, "abcd", "efgh"));
    assertEquals(HASH24[3], streamed(0, "abcdefgh", "i"));
    assertEquals(HASH24_128[3], streamed(1, "abcdefgh", "i"));
    assertEquals(HASH36_128[3], streamed(2, "abcdefgh", "i"));
    // odd splits that straddle block boundaries
    assertEquals(HASH36_128[4], streamed(2, "The q", "", "uick brown fox jum", "ps over the lazy dog"));
  }

  @Test
  public void tagLengths() {
    assertEquals(8, SipHash.tagLength(0));
    assertEquals(16, SipHash.tagLength(1));
    assertEquals(16, SipHash.tagLength(2));
    assertEquals(0, SipHash.tagLength(3));
  }

  @Test
  public void shortKeyRejected() {
    try {
      new SipHash(new byte[15], 2, 4, false);
      fail("expected IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // expected
    }
  }
}
