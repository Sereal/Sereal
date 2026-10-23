package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import org.junit.Test;

public class TokenDecoderAccessorsTest {
  @Test
  public void currentToken() throws SerealException {
    TokenDecoder decoder = setupDecoder(123);

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(SerealToken.LONG, decoder.currentToken());

    assertEquals(SerealToken.END, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.currentToken());
  }

  @Test
  public void parsingOffsets() throws SerealException {
    // offsets are 1-based, from the start of the body, because that's what Sereal uses
    TokenDecoder decoder = setupDecoder(Arrays.asList(123, 1));

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(1, decoder.tokenOffset());
    assertEquals(1, decoder.currentOffset());

    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
    assertEquals(2, decoder.tokenOffset());
    assertEquals(3, decoder.currentOffset());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(4, decoder.tokenOffset());
    assertEquals(5, decoder.currentOffset());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(6, decoder.tokenOffset());
    assertEquals(6, decoder.currentOffset());

    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(6, decoder.currentOffset());

    assertEquals(SerealToken.END, decoder.poisonNextToken());
    assertEquals(6, decoder.currentOffset());
  }

  @Test
  public void strings() throws SerealException {
    {
      TokenDecoder decoder = setupDecoder("abc");

      assertEquals(SerealToken.UTF8, decoder.poisonNextToken());
      assertTrue(decoder.binaryIsUtf8());
      assertEquals(8, decoder.binarySliceStart());
      assertEquals(11, decoder.binarySliceEnd());
      assertEquals(3, decoder.binarySliceLength());
    }

    {
      TokenDecoder decoder = setupDecoder("abc".getBytes());

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertFalse(decoder.binaryIsUtf8());
      assertEquals(7, decoder.binarySliceStart());
      assertEquals(10, decoder.binarySliceEnd());
      assertEquals(3, decoder.binarySliceLength());
    }

    {
      TokenDecoder decoder = setupDecoder("0123456789012345678901234567890123456789".getBytes());

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertFalse(decoder.binaryIsUtf8());
      assertEquals(8, decoder.binarySliceStart());
      assertEquals(48, decoder.binarySliceEnd());
      assertEquals(40, decoder.binarySliceLength());
    }
  }

  @Test
  public void collections() throws SerealException {
    Object list = Arrays.asList(
      new HashMap<String, Long>() {{
        put("a", 1L);
        put("c", 2L);
      }},
      1234
    );
    TokenDecoder decoder = setupDecoder(list);

    assertEquals(1, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(1, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.HASH_START, decoder.poisonNextToken());
    assertEquals(4, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.UTF8, decoder.poisonNextToken());
    assertEquals(4, decoder.elementCount());
    assertTrue(decoder.isHashKey());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(4, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.UTF8, decoder.poisonNextToken());
    assertEquals(4, decoder.elementCount());
    assertTrue(decoder.isHashKey());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(4, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.HASH_END, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());
    assertFalse(decoder.isHashKey());

    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(1, decoder.elementCount());
    assertFalse(decoder.isHashKey());
  }

  @Test
  public void vstring() throws SerealException {
    // 3df3726c0600 3e 26 06 "v1.2.3"
    TokenDecoder decoder = setupDecoderFromHex(V6Vectors.VSTRING_1_2_3);

    assertEquals(SerealToken.VSTRING, decoder.poisonNextToken());
    assertEquals(SerealToken.VSTRING, decoder.currentToken());
    assertEquals(1, decoder.tokenOffset());
    assertFalse(decoder.binaryIsUtf8());
    assertEquals(9, decoder.binarySliceStart());
    assertEquals(15, decoder.binarySliceEnd());
    assertEquals(6, decoder.binarySliceLength());
    assertEquals("v1.2.3", new String(decoder.decoderBuffer(), decoder.binarySliceStart(),
        decoder.binarySliceLength(), java.nio.charset.StandardCharsets.ISO_8859_1));

    assertEquals(SerealToken.END, decoder.poisonNextToken());

    decoder = setupDecoderFromHex(V6Vectors.VSTRING_300_1);
    assertEquals(SerealToken.VSTRING, decoder.poisonNextToken());
    assertTrue(decoder.binaryIsUtf8());
  }

  @Test
  public void float128() throws SerealException {
    // 3df3726c0500 38 <16 bytes>
    TokenDecoder decoder = setupDecoderFromHex(V6Vectors.FLOAT128_V5);

    assertEquals(SerealToken.FLOAT128, decoder.poisonNextToken());
    assertEquals(1, decoder.tokenOffset());
    assertEquals(7, decoder.binarySliceStart());
    assertEquals(23, decoder.binarySliceEnd());
    assertEquals(16, decoder.binarySliceLength());
    assertEquals(15, decoder.decoderBuffer()[decoder.binarySliceEnd() - 1]);

    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  private TokenDecoder setupDecoderFromHex(String hex) throws SerealException {
    TokenDecoder decoder = new TokenDecoder();

    decoder.setData(TestUtils.hex(hex));
    decoder.prepareDecodeBody();

    return decoder;
  }

  private TokenDecoder setupDecoder(Object value) throws SerealException {
    Encoder encoder = new Encoder();
    TokenDecoder decoder = new TokenDecoder();

    encoder.write(value);
    byte[] originalData = encoder.getData();

    decoder.setData(originalData);
    decoder.prepareDecodeBody();

    return decoder;
  }
}
