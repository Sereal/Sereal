package com.booking.sereal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.nio.charset.StandardCharsets;
import org.junit.Test;

public class VStringsTest {
  private static int classify(String spelling) {
    byte[] bytes = ("##" + spelling + "##").getBytes(StandardCharsets.ISO_8859_1);
    return VStrings.classify(bytes, 2, bytes.length - 4);
  }

  @Test
  public void classify() {
    String[] binary = {"v1.2.3", "1.2.3", "1.2", "1", "v", ".1", "v1_0.2", "01.002.003", "v_1", "v1_", "v127",
      "v18446744073709551621"};
    for (String s : binary) {
      assertEquals(s, VStrings.BINARY, classify(s));
    }
    String[] utf8 = {"v128", "v1.300", "v99999999999999999999999"};
    for (String s : utf8) {
      assertEquals(s, VStrings.UTF8, classify(s));
    }
    String[] invalid = {"v1..2", "1.", "x", "v1.2.3x", ""};
    for (String s : invalid) {
      assertEquals(s, VStrings.INVALID, classify(s));
    }
  }

  @Test
  public void perlVString() {
    PerlVString v = new PerlVString("v1.2.3");
    assertEquals("v1.2.3", v.spelling());
    assertArrayEquals("v1.2.3".getBytes(StandardCharsets.ISO_8859_1), v.spellingBytes());
    assertFalse(v.isUtf8());
    assertTrue(new PerlVString("v300.1").isUtf8());
    assertEquals(new PerlVString("v1.2.3"), v);
    assertEquals(new PerlVString("v1.2.3").hashCode(), v.hashCode());
    assertNotEquals(new PerlVString("1.2.3"), v);
    assertTrue(v.toString().contains("v1.2.3"));

    assertThrows(IllegalArgumentException.class, () -> new PerlVString("v1.2.3x"));
    assertThrows(IllegalArgumentException.class, () -> new PerlVString(""));
    assertThrows(IllegalArgumentException.class, () -> new PerlVString("v1.Ā"));
  }

  @Test
  public void float128() {
    byte[] bytes = new byte[16];
    for (int i = 0; i < 16; i++) {
      bytes[i] = (byte) i;
    }
    Float128 f = new Float128(bytes);
    bytes[0] = 99;
    assertEquals(0, f.bytes()[0]);
    f.bytes()[1] = 99;
    assertEquals(1, f.bytes()[1]);
    bytes[0] = 0;
    assertEquals(new Float128(bytes), f);
    assertEquals(new Float128(bytes).hashCode(), f.hashCode());
    assertTrue(f.toString().contains("000102030405060708090a0b0c0d0e0f"));

    assertThrows(IllegalArgumentException.class, () -> new Float128(new byte[15]));
    assertThrows(IllegalArgumentException.class, () -> new Float128(null));
  }
}
