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
