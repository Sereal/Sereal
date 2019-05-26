package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.regex.Pattern;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class TokenDecoderTest {
  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void decodeLong() throws SerealException {
    // small positive tag
    for (int smallPos = 0; smallPos < 16; ++smallPos) {
      TokenDecoder decoder = decoder(smallPos);

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(smallPos, decoder.longValue());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // small negative tag
    for (int smallNeg = -16; smallNeg < 0; ++smallNeg) {
      TokenDecoder decoder = decoder(32 + smallNeg);

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(smallNeg, decoder.longValue());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // varint
    {
      TokenDecoder decoder = decoder(0x20, 0xf6, 0x69);

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(13558, decoder.longValue());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // zig-zga
    {
      TokenDecoder decoder = decoder(0x21, 0xeb, 0xd3, 0x01);

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(-13558, decoder.longValue());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeString() throws SerealException {
    TokenDecoder decoder = decoder(0x27, 0x03, 0xEA, 0xB0, 0x81);

    assertEquals(SerealToken.UTF8, decoder.poisonNextToken());
    assertTrue(decoder.binaryIsUtf8());
    assertEquals("각", new String(
      decoder.decoderBuffer(),
      decoder.binarySliceStart(),
      decoder.binarySliceLength(),
      StandardCharsets.UTF_8)
    );
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeBytes() throws SerealException {
    byte[] binaryBytes = TestUtils.byteArray(
      0x21, 0x20, 0x22, 0xff, 0x25, 0x26, 0x27, 0x28, 0x29, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
      0x37, 0x38, 0x39, 0x40, 0x51, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x50, 0x51, 0x52,
      0x53
    );

    for (int len = 0; len < 32; ++len) {
      byte[] slice = Arrays.copyOf(binaryBytes, len);
      TokenDecoder decoder = decoderNested(0x60 + len, slice);

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertEquals(new String(slice, StandardCharsets.ISO_8859_1), new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.ISO_8859_1)
      );
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    {
      TokenDecoder decoder = decoderNested(0x26, 0x21, binaryBytes);

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertEquals(new String(binaryBytes, StandardCharsets.ISO_8859_1), new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.ISO_8859_1)
      );
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeFloat() throws SerealException {
    TokenDecoder decoder = decoder(0x22, 0x00, 0x00, 0xe4, 0x40);

    assertEquals(SerealToken.FLOAT, decoder.poisonNextToken());
    assertEquals(7.125f, decoder.floatValue(), 0.0);
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeDouble() throws SerealException {
    TokenDecoder decoder = decoder(0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x2c, 0x40);

    assertEquals(SerealToken.DOUBLE, decoder.poisonNextToken());
    assertEquals(14.03125d, decoder.doubleValue(), 0.0);
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeTrue() throws SerealException {
    TokenDecoder decoder = decoder(0x3a);

    assertEquals(SerealToken.FALSE, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeFalse() throws SerealException {
    TokenDecoder decoder = decoder(0x3b);

    assertEquals(SerealToken.TRUE, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeUndef() throws SerealException {
    TokenDecoder decoder = decoder(0x25);

    assertEquals(SerealToken.UNDEF, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeCanonicalUndef() throws SerealException {
    TokenDecoder decoder = decoder(0x39);

    assertEquals(SerealToken.CANONICAL_UNDEF, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeRefNext() throws SerealException {
    TokenDecoder decoder = decoder(0x28, 0x0a);

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(10, decoder.longValue());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeArray() throws SerealException {
    // simple array
    {
      TokenDecoder decoder = decoder(0x28, 0x2b, 0x02, 0x63, 0x61, 0x62, 0x63, 0x07);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
      assertEquals(2, decoder.elementCount());

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertEquals("abc", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(7, decoder.longValue());

      assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // empty array
    {
      TokenDecoder decoder = decoder(0x28, 0x2b, 0x00);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
      assertEquals(0, decoder.elementCount());
      assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeShortArray() throws SerealException {
    for (int len = 0; len < 16; ++len) {
      byte[] contents = new byte[len];
      for (int j = 0; j < len; ++j) {
        contents[j] = (byte) j;
      }
      TokenDecoder decoder = decoderNested(0x40 + len, contents);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
      assertEquals(len, decoder.elementCount());

      for (int j = 0; j < len; ++j) {
        assertEquals(SerealToken.LONG, decoder.poisonNextToken());
        assertEquals(j, decoder.longValue());
      }

      assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeHash() throws SerealException {
    // simple hash
    {
      TokenDecoder decoder = decoder(0x28, 0x2a, 0x02, 0x63, 0x61, 0x62, 0x63, 0x07, 0x62, 0x61, 0x62, 0x08);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.HASH_START, decoder.poisonNextToken());
      assertEquals(4, decoder.elementCount());

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertTrue(decoder.isHashKey());
      assertEquals("abc", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );
      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertFalse(decoder.isHashKey());
      assertEquals(7, decoder.longValue());

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertTrue(decoder.isHashKey());
      assertEquals("ab", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );
      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertFalse(decoder.isHashKey());
      assertEquals(8, decoder.longValue());

      assertEquals(SerealToken.HASH_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // empty hash
    {
      TokenDecoder decoder = decoder(0x28, 0x2a, 0x00);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.HASH_START, decoder.poisonNextToken());
      assertEquals(0, decoder.elementCount());
      assertEquals(SerealToken.HASH_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeShortHash() throws SerealException {
    for (int len = 0; len < 16; ++len) {
      byte[] contents = new byte[len * 3];
      for (int j = 0; j < len; ++j) {
        contents[j * 3] = (byte) 0x61;
        contents[j * 3 + 1] = (byte) ('a' + j);
        contents[j * 3 + 2] = (byte) j;
      }
      TokenDecoder decoder = decoderNested(0x50 + len, contents);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.HASH_START, decoder.poisonNextToken());
      assertEquals(len * 2, decoder.elementCount());

      for (int j = 0; j < len; ++j) {
        assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
        assertTrue(decoder.isHashKey());
        assertEquals(new String(TestUtils.byteArray('a' + j)), new String(
          decoder.decoderBuffer(),
          decoder.binarySliceStart(),
          decoder.binarySliceLength(),
          StandardCharsets.UTF_8)
        );
        assertEquals(SerealToken.LONG, decoder.poisonNextToken());
        assertFalse(decoder.isHashKey());
        assertEquals(j, decoder.longValue());
      }

      assertEquals(SerealToken.HASH_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeCopy() throws SerealException {
    TokenDecoder decoder = decoder(0x28, 0x2b, 0x02, 0x07, 0x2f, 0x04);

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(0, decoder.trackOffset());
    assertEquals(7, decoder.longValue());

    assertEquals(SerealToken.COPY, decoder.poisonNextToken());
    assertEquals(4, decoder.backreferenceOffset());

    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeCopyDeep() throws SerealException {
    TokenDecoder decoder = decoder(0x28, 0x2b, 0x02, 0x07, 0x2f, 0x04);

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(0, decoder.trackOffset());
    assertEquals(7, decoder.longValue());

    assertEquals(SerealToken.COPY, decoder.poisonNextToken());
    assertEquals(4, decoder.backreferenceOffset());

    decoder.startSubDecode(decoder.backreferenceOffset());
    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(0, decoder.trackOffset());
    assertEquals(4, decoder.tokenOffset());
    assertEquals(7, decoder.longValue());

    assertEquals(SerealToken.SUBDECODE_END, decoder.poisonNextToken());

    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeAlias() throws SerealException {
    TokenDecoder decoder = decoder(0x28, 0x2b, 0x02, 0x87, 0x2e, 0x04);

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(4, decoder.trackOffset());
    assertEquals(7, decoder.longValue());

    assertEquals(SerealToken.ALIAS, decoder.poisonNextToken());
    assertEquals(4, decoder.backreferenceOffset());

    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeRefp() throws SerealException {
    TokenDecoder decoder = decoder(0x28, 0x2b, 0x02, 0x87, 0x29, 0x04);

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());
    assertEquals(2, decoder.elementCount());

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(4, decoder.trackOffset());
    assertEquals(7, decoder.longValue());

    assertEquals(SerealToken.REFP, decoder.poisonNextToken());
    assertEquals(4, decoder.backreferenceOffset());

    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeObject() throws SerealException {
    // object with short binary tag
    {
      TokenDecoder decoder = decoder(0x28, 0x2c, 0x62, 0x41, 0x42, 0x07);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());

      assertEquals(SerealToken.OBJECT_START, decoder.poisonNextToken());
      assertEquals("AB", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(7, decoder.longValue());

      assertEquals(SerealToken.OBJECT_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // object with long binary tag
    {
      TokenDecoder decoder = decoder(0x28, 0x2c, 0x26, 0x02, 0x43, 0x44, 0x07);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());

      assertEquals(SerealToken.OBJECT_START, decoder.poisonNextToken());
      assertEquals("CD", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(7, decoder.longValue());

      assertEquals(SerealToken.OBJECT_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // object with string tag
    {
      TokenDecoder decoder = decoder(0x28, 0x2c, 0x27, 0x03, 0xEA, 0xB0, 0x81, 0x07);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());

      assertEquals(SerealToken.OBJECT_START, decoder.poisonNextToken());
      assertEquals("각", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(7, decoder.longValue());

      assertEquals(SerealToken.OBJECT_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // object with string copy tag
    {
      TokenDecoder decoder = decoder(0x43, 0x62, 0x41, 0x42, 0x61, 0x41, 0x2c, 0x2f, 0x02, 0x07);

      assertEquals(SerealToken.REFN, decoder.poisonNextToken());
      assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertEquals("AB", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.BINARY, decoder.poisonNextToken());
      assertEquals("A", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.OBJECT_START, decoder.poisonNextToken());
      assertEquals("AB", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.LONG, decoder.poisonNextToken());
      assertEquals(7, decoder.longValue());

      assertEquals(SerealToken.OBJECT_END, decoder.poisonNextToken());
      assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeObjectV() throws SerealException {
    TokenDecoder decoder = decoder(0x42, 0x2c, 0x62, 0x41, 0x42, 0x08, 0x2d, 0x03, 0x07);

    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.poisonNextToken());

    assertEquals(SerealToken.OBJECT_START, decoder.poisonNextToken());
    assertEquals("AB", new String(
      decoder.decoderBuffer(),
      decoder.binarySliceStart(),
      decoder.binarySliceLength(),
      StandardCharsets.UTF_8)
    );

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(8, decoder.longValue());

    assertEquals(SerealToken.OBJECT_END, decoder.poisonNextToken());

    assertEquals(SerealToken.OBJECT_START, decoder.poisonNextToken());
    assertEquals("AB", new String(
      decoder.decoderBuffer(),
      decoder.binarySliceStart(),
      decoder.binarySliceLength(),
      StandardCharsets.UTF_8)
    );

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(7, decoder.longValue());

    assertEquals(SerealToken.OBJECT_END, decoder.poisonNextToken());
    assertEquals(SerealToken.ARRAY_END, decoder.poisonNextToken());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeWeakref() throws SerealException {
    TokenDecoder decoder = decoder(0x30, 0x28, 0x0a);

    assertEquals(SerealToken.WEAKEN, decoder.poisonNextToken());
    assertEquals(SerealToken.REFN, decoder.poisonNextToken());
    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(10, decoder.longValue());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  @Test
  public void decodeRegexp() throws SerealException {
    // bytes regexp
    {
      TokenDecoder decoder = decoder(0x31, 0x63, 0x61, 0x62, 0x63, 0x61, 'm');

      assertEquals(SerealToken.REGEXP, decoder.poisonNextToken());
      assertFalse(decoder.binaryIsUtf8());

      assertEquals("abc", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals("m", new String(
        decoder.decoderBuffer(),
        decoder.regexpFlagsSliceStart(),
        decoder.regexpFlagsSliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }

    // string regexp
    {
      TokenDecoder decoder = decoder(0x31, 0x27, 0x03, 0x61, 0x62, 0x63, 0x63, 'm', 'x', 'u');

      assertEquals(SerealToken.REGEXP, decoder.poisonNextToken());
      assertTrue(decoder.binaryIsUtf8());

      assertEquals("abc", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals("mxu", new String(
        decoder.decoderBuffer(),
        decoder.regexpFlagsSliceStart(),
        decoder.regexpFlagsSliceLength(),
        StandardCharsets.UTF_8)
      );

      assertEquals(SerealToken.END, decoder.poisonNextToken());
    }
  }

  @Test
  public void decodeRegexpFlags() throws SerealException {
    assertEquals(Pattern.MULTILINE, TokenDecoder.decodeRegexpFlags("!m!".getBytes(), 1, 2));
    assertEquals(Pattern.DOTALL, TokenDecoder.decodeRegexpFlags("!s!".getBytes(), 1, 2));
    assertEquals(Pattern.COMMENTS, TokenDecoder.decodeRegexpFlags("!x!".getBytes(), 1, 2));
    assertEquals(Pattern.CASE_INSENSITIVE, TokenDecoder.decodeRegexpFlags("!i!".getBytes(), 1, 2));
    assertEquals(0, TokenDecoder.decodeRegexpFlags("!p!".getBytes(), 1, 2));

    assertEquals(Pattern.MULTILINE|Pattern.COMMENTS, TokenDecoder.decodeRegexpFlags("!mx!".getBytes(), 1, 3));
  }

  @Test
  public void decodeInvalidRegexpFlags() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Unknown regexp modifier: '$'");

    TokenDecoder.decodeRegexpFlags("$".getBytes(), 0, 1);
  }

  @Test
  public void skipPad() throws SerealException {
    TokenDecoder decoder = decoder(0x3f, 0x01, 0x3f);

    assertEquals(SerealToken.LONG, decoder.poisonNextToken());
    assertEquals(1, decoder.longValue());
    assertEquals(SerealToken.END, decoder.poisonNextToken());
  }

  private TokenDecoder decoder(int... ints) throws SerealException {
    return decoder(TestUtils.byteArray(ints));
  }

  private TokenDecoder decoderNested(Object... objects) throws SerealException {
    return decoder(TestUtils.byteArrayNested(objects));
  }

  private TokenDecoder decoder(byte... bytes) throws SerealException {
    byte[] body = Arrays.copyOf(TestUtils.byteArray(0x3d, 0xf3, 0x72, 0x6c, 0x04, 0x00), bytes.length + 6);

    System.arraycopy(bytes, 0, body, 6, bytes.length);

    TokenDecoder decoder = new TokenDecoder();

    decoder.setData(body);
    decoder.prepareDecodeBody();

    return decoder;
  }
}
