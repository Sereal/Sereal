package com.booking.sereal;

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import org.hamcrest.Matcher;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class TokenEncoderTest {
  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void encodeLong() throws SerealException {
    // small positive tag
    for (int smallPos = 0; smallPos < 16; ++smallPos) {
      TokenEncoder encoder = encoder();

      encoder.appendLong(smallPos);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(smallPos));
    }

    // small negative tag
    for (int smallNeg = -16; smallNeg < 0; ++smallNeg) {
      TokenEncoder encoder = encoder();

      encoder.appendLong(smallNeg);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(32 + smallNeg));
    }

    // boundary condition for positive
    {
      TokenEncoder encoder = encoder();

      encoder.appendLong(16);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x20, 0x10));
    }

    // boundary condition for negative
    {
      TokenEncoder encoder = encoder();

      encoder.appendLong(-17);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x21, 0x21));
    }

    // varint
    {
      TokenEncoder encoder = encoder();

      encoder.appendLong(13558);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x20, 0xf6, 0x69));
    }

    // zig-zga
    {
      TokenEncoder encoder = encoder();

      encoder.appendLong(-13558);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x21, 0xeb, 0xd3, 0x01));
    }
  }

  @Test
  public void encodeBoolean() throws SerealException {
    {
      TokenEncoder encoder = encoder();

      encoder.appendBoolean(false);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x3a));
    }

    {
      TokenEncoder encoder = encoder();

      encoder.appendBoolean(true);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x3b));
    }
  }

  @Test
  public void encodeUndef() throws SerealException {
    {
      TokenEncoder encoder = encoder();

      encoder.appendUndef();
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x25));
    }

    {
      TokenEncoder encoder = encoder();

      encoder.appendCanonicalUndef();
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x39));
    }
  }

  @Test
  public void encodeFloat() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.appendFloat(7.125f);
    assertEquals(1, encoder.trackOffsetLastValue());

    assertThat(bodyBytes(encoder), expectedBytes(0x22, 0x00, 0x00, 0xe4, 0x40));
  }

  @Test
  public void encodeDouble() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.appendDouble(14.03125d);
    assertEquals(1, encoder.trackOffsetLastValue());

    assertThat(bodyBytes(encoder), expectedBytes(0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x2c, 0x40));
  }

  @Test
  public void encodeString() throws SerealException {
    String longAscii;
    {
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < 125; ++i) {
        sb.append('a');
      }
      sb.append('b');
      longAscii = sb.toString();
    }

    // short ASCII string
    {
      TokenEncoder encoder = encoder();

      encoder.appendString("abc");
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x27, 0x03, 0x61, 0x62, 0x63));
    }

    // Short Unicode string
    {
      TokenEncoder encoder = encoder();

      encoder.appendString("각");
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x27, 0x03, 0xEA, 0xB0, 0x81));
    }

    // String that is shorter than max encoded length and requires padding
    {
      TokenEncoder encoder = encoder();

      encoder.appendString(longAscii);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytesNested(0x27, 0xfe, 0x00, longAscii.getBytes()));
    }

    // char array
    {
      TokenEncoder encoder = encoder();

      encoder.appendString(longAscii.toCharArray());
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytesNested(0x27, 0xfe, 0x00, longAscii.getBytes()));
    }

    // char array range
    {
      TokenEncoder encoder = encoder();

      encoder.appendString(longAscii.toCharArray(), 2, longAscii.length() - 4);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytesNested(0x27, 0xfa, 0x00, Arrays.copyOfRange(longAscii.getBytes(), 2, longAscii.length() - 2)));
    }
  }

  @Test
  public void encodeRawUtf8() throws SerealException {
    byte[] bytes = TestUtils.byteArray(0xea, 0xEA, 0xB0, 0x81, 0xea);

    TokenEncoder encoder = encoder();

    encoder.appendUTF8(bytes, 1, 3);
    assertEquals(1, encoder.trackOffsetLastValue());

    assertThat(bodyBytes(encoder), expectedBytes(0x27, 0x03, 0xEA, 0xB0, 0x81));
  }

  @Test
  public void encodeBytes() throws SerealException {
    byte[] binaryBytes = TestUtils.byteArray(
      0x01, 0x00, 0x02, 0xff, 0x05, 0x06, 0x07, 0x08, 0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
      0x17, 0x18, 0x19, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x30, 0x31, 0x32,
      0x33
    );

    // short byte string
    for (int len = 0; len < 32; len++) {
      TokenEncoder encoder = encoder();

      encoder.appendBinary(binaryBytes, 0, len);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytesNested(0x60 + len, Arrays.copyOf(binaryBytes, len)));
    }

    // short bytes string boundary condition
    {
      TokenEncoder encoder = encoder();

      encoder.appendBinary(binaryBytes, 0, 32);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytesNested(0x26, 0x20, Arrays.copyOf(binaryBytes, 32)));
    }

    // long byte string
    {
      TokenEncoder encoder = encoder();

      encoder.appendBinary(binaryBytes);
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytesNested(0x26, 0x21, binaryBytes));
    }
  }

  @Test
  public void encodeHash() throws SerealException {
    // hash with unknown key count, small count
    {
      TokenEncoder encoder = encoder();

      encoder.startHash();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.appendBinary("abc".getBytes());
      assertEquals(4, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(8, encoder.trackOffsetLastValue());
      encoder.endHash();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2a, 0x01, 0x63, 0x61, 0x62, 0x63, 0x07));
    }

    // hash with unknown key count, small count boundary
    {
      TokenEncoder encoder = encoder();
      List<byte[]> hashContents = new ArrayList<>();

      encoder.startHash();
      for (int i = 0; i < 127; ++i) {
        encoder.appendBinary(new byte[]{ 'A' });
        encoder.appendLong(7);
        hashContents.add(TestUtils.byteArray(0x61, 0x41, 0x07));
      }
      encoder.endHash();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2a, 0x7f, hashContents));
    }

    // hash with unknown key count, small count boundary
    {
      TokenEncoder encoder = encoder();
      List<byte[]> hashContents = new ArrayList<>();

      encoder.startHash();
      for (int i = 0; i < 128; ++i) {
        encoder.appendBinary(new byte[]{ 'A' });
        encoder.appendLong(7);
        hashContents.add(TestUtils.byteArray(0x61, 0x41, 0x07));
      }
      encoder.endHash();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2a, 0x80, 0x01, hashContents));
    }

    // hash with known key count, small count
    for (int len = 0; len < 16; ++len) {
      TokenEncoder encoder = encoder();
      List<byte[]> hashContents = new ArrayList<>();

      encoder.startHash(len);
      for (int i = 0; i < len; ++i) {
        encoder.appendBinary(new byte[]{ 'A' });
        encoder.appendLong(7);
        hashContents.add(TestUtils.byteArray(0x61, 0x41, 0x07));
      }
      encoder.endHash();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x50 + len, hashContents));
    }

    // hash with known key count, small count boundary
    {
      TokenEncoder encoder = encoder();
      List<byte[]> hashContents = new ArrayList<>();

      encoder.startHash(16);
      for (int i = 0; i < 16; ++i) {
        encoder.appendBinary(new byte[]{ 'A' });
        encoder.appendLong(7);
        hashContents.add(TestUtils.byteArray(0x61, 0x41, 0x07));
      }
      encoder.endHash();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2a, 0x10, hashContents));
    }
  }

  @Test
  public void encodeHashInvalidCount() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Bad value count");

    TokenEncoder encoder = encoder();

    encoder.startHash(2);
    encoder.appendLong(7);
    encoder.appendLong(8);
    encoder.endHash();
  }


  @Test
  public void encodeHashOddCount() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Odd value count in hash");

    TokenEncoder encoder = encoder();

    encoder.startHash(2);
    encoder.appendLong(7);
    encoder.appendLong(8);
    encoder.appendLong(8);
    encoder.endHash();
  }

  @Test
  public void encodeHashValue() throws SerealException {
    {
      TokenEncoder encoder = encoder();

      encoder.startHashValue();
      encoder.appendBinary(new byte[]{ 'A' });
      encoder.appendLong(7);
      encoder.endHash();

      // this would break the Perl decoder, but it's a test anyway
      assertThat(bodyBytes(encoder), expectedBytes(0x2a, 0x01, 0x61, 0x41, 0x07));
    }

    {
      TokenEncoder encoder = encoder();

      encoder.startHashValue(2);
      encoder.appendBinary(new byte[]{ 'A' });
      encoder.appendLong(7);
      encoder.appendBinary(new byte[]{ 'B' });
      encoder.appendLong(8);
      encoder.endHash();

      // this would break the Perl decoder, but it's a test anyway
      assertThat(bodyBytes(encoder), expectedBytes(0x2a, 0x02, 0x61, 0x41, 0x07, 0x61, 0x42, 0x08));
    }
  }

  @Test
  public void encodeHashValueInvalidCount() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Bad value count");

    TokenEncoder encoder = encoder();

    encoder.startHashValue(2);
    encoder.appendLong(7);
    encoder.appendLong(8);
    encoder.endHash();
  }

  @Test
  public void encodeArray() throws SerealException {
    // array with unknown value count, small count
    {
      TokenEncoder encoder = encoder();

      encoder.startArray();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.appendBinary("abc".getBytes());
      assertEquals(4, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(8, encoder.trackOffsetLastValue());
      encoder.endArray();

      assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x2b, 0x02, 0x63, 0x61, 0x62, 0x63, 0x07));
    }

    // array with unknown value count, small count boundary
    {
      TokenEncoder encoder = encoder();
      byte[] arrayContents = new byte[127];

      encoder.startArray();
      for (int i = 0; i < 127; ++i) {
        encoder.appendLong(7);
        arrayContents[i] = 0x07;
      }
      encoder.endArray();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2b, 0x7f, arrayContents));
    }

    // array with unknown value count, small count boundary
    {
      TokenEncoder encoder = encoder();
      byte[] arrayContents = new byte[129];

      encoder.startArray();
      for (int i = 0; i < 129; ++i) {
        encoder.appendLong(7);
        arrayContents[i] = 0x07;
      }
      encoder.endArray();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2b, 0x81, 0x01, arrayContents));
    }

    // array with known key count, small count
    for (int len = 0; len < 16; ++len) {
      TokenEncoder encoder = encoder();
      byte[] arrayContents = new byte[len];

      encoder.startArray(len);
      for (int i = 0; i < len; ++i) {
        encoder.appendLong(i);
        arrayContents[i] = (byte) i;
      }
      encoder.endArray();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x40 + len, arrayContents));
    }

    // array with known key count, small count boundary
    {
      TokenEncoder encoder = encoder();
      byte[] arrayContents = new byte[16];

      encoder.startArray(16);
      for (int i = 0; i < 16; ++i) {
        encoder.appendLong(i);
        arrayContents[i] = (byte) i;
      }
      encoder.endArray();

      assertThat(bodyBytes(encoder), expectedBytesNested(0x28, 0x2b, 0x10, arrayContents));
    }
  }

  @Test
  public void encodeArrayInvalidCount() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Bad value count");

    TokenEncoder encoder = encoder();

    encoder.startArray(4);
    encoder.appendLong(7);
    encoder.appendLong(8);
    encoder.appendLong(9);
    encoder.endArray();
  }

  @Test
  public void encodeArrayValue() throws SerealException {
    {
      TokenEncoder encoder = encoder();

      encoder.startArrayValue();
      encoder.appendLong(7);
      encoder.endArray();

      // this would break the Perl decoder, but it's a test anyway
      assertThat(bodyBytes(encoder), expectedBytes(0x2b, 0x01, 0x07));
    }

    {
      TokenEncoder encoder = encoder();

      encoder.startArrayValue(3);
      encoder.appendLong(7);
      encoder.appendLong(8);
      encoder.appendLong(9);
      encoder.endArray();

      // this would break the Perl decoder, but it's a test anyway
      assertThat(bodyBytes(encoder), expectedBytes(0x2b, 0x03, 0x07, 0x08, 0x09));
    }
  }

  @Test
  public void encodeArrayValueInvalidCount() throws SerealException {
    exceptionRule.expect(SerealException.class);
    exceptionRule.expectMessage("Bad value count");

    TokenEncoder encoder = encoder();

    encoder.startArrayValue(4);
    encoder.appendLong(7);
    encoder.appendLong(8);
    encoder.appendLong(9);
    encoder.endArray();
  }

  @Test
  public void encodeObject() throws SerealException {
    // object with byte classname
    {
      TokenEncoder encoder = encoder();

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.startObject("abc".getBytes());
      assertEquals(2, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(7, encoder.trackOffsetLastValue());
      encoder.endObject();

      assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x2c, 0x63, 0x61, 0x62, 0x63, 0x07));
    }

    // object with string classname
    {
      TokenEncoder encoder = encoder();

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.startObject("abc");
      assertEquals(2, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(8, encoder.trackOffsetLastValue());
      encoder.endObject();

      assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x2c, 0x27, 0x03, 0x61, 0x62, 0x63, 0x07));
    }

    // object with char[] classname
    {
      TokenEncoder encoder = encoder();

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.startObject("abc".toCharArray());
      assertEquals(2, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(8, encoder.trackOffsetLastValue());
      encoder.endObject();

      assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x2c, 0x27, 0x03, 0x61, 0x62, 0x63, 0x07));
    }

    // object with copy tag classname
    {
      TokenEncoder encoder = encoder();

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.startObject(1);
      assertEquals(2, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(5, encoder.trackOffsetLastValue());
      encoder.endObject();

      assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x2c, 0x2f, 0x01, 0x07));
    }

    // object with classname offset classname
    {
      TokenEncoder encoder = encoder();

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      encoder.startObjectV(1);
      assertEquals(2, encoder.trackOffsetLastValue());
      encoder.appendLong(7);
      assertEquals(4, encoder.trackOffsetLastValue());
      encoder.endObject();

      assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x2d, 0x01, 0x07));
    }
  }

  @Test
  public void encodeWeakenSimple() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.appendWeaken();
    encoder.appendRefNext();
    encoder.appendLong(10);

    assertThat(bodyBytes(encoder), expectedBytes(0x30, 0x28, 0x0a));
  }

  @Test
  public void encodeWeakenChecked() throws SerealException {
    {
      TokenEncoder encoder = encoder();

      encoder.startWeaken(false);
      encoder.appendRefNext();
      encoder.appendLong(10);
      encoder.endWeaken();

      assertThat(bodyBytes(encoder), expectedBytes(0x30, 0x28, 0x0a));
    }

    {
      TokenEncoder encoder = encoder();

      encoder.startWeaken(true);
      encoder.appendRefNext();
      encoder.appendLong(10);
      encoder.endWeaken();

      assertThat(bodyBytes(encoder), expectedBytes(0x30, 0x3f,  0x28, 0x0a));
    }

    {
      TokenEncoder encoder = encoder();

      encoder.startWeaken(true);
      encoder.appendLong(10);
      encoder.endWeaken();

      assertThat(bodyBytes(encoder), expectedBytes(0x30, 0x28, 0x0a));
    }
  }

  @Test
  public void encodeRefNext() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.appendRefNext();
    assertEquals(1, encoder.trackOffsetLastValue());
    encoder.appendLong(10);
    assertEquals(2, encoder.trackOffsetLastValue());

    assertThat(bodyBytes(encoder), expectedBytes(0x28, 0x0a));
  }

  @Test
  public void encodeRefPrevious() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.startArray(2);
    assertEquals(1, encoder.trackOffsetLastValue());
    encoder.appendLong(7);
    assertEquals(2, encoder.trackOffsetLastValue());
    encoder.appendRefPrevious(2);
    assertEquals(3, encoder.trackOffsetLastValue());
    encoder.endArray();

    assertThat(bodyBytes(encoder), expectedBytes(0x42, 0x87, 0x29, 0x02));
  }

  @Test
  public void encodeCopy() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.startArray(2);
    assertEquals(1, encoder.trackOffsetLastValue());
    encoder.appendLong(7);
    assertEquals(2, encoder.trackOffsetLastValue());
    encoder.appendCopy(2);
    assertEquals(3, encoder.trackOffsetLastValue());
    encoder.endArray();

    assertThat(bodyBytes(encoder), expectedBytes(0x42, 0x07, 0x2f, 0x02));
  }

  @Test
  public void encodeAlias() throws SerealException {
    TokenEncoder encoder = encoder();

    encoder.startArray(2);
    assertEquals(1, encoder.trackOffsetLastValue());
    encoder.appendLong(7);
    assertEquals(2, encoder.trackOffsetLastValue());
    encoder.appendAlias(2);
    assertEquals(3, encoder.trackOffsetLastValue());
    encoder.endArray();

    assertThat(bodyBytes(encoder), expectedBytes(0x42, 0x87, 0x2e, 0x02));
  }

  @Test
  public void encodeRegexp() throws SerealException {
    // bytes regexp
    {
      TokenEncoder encoder = encoder();

      encoder.appendRegexpBinary("abc".getBytes(), "m".getBytes());
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x31, 0x63, 0x61, 0x62, 0x63, 0x61, 'm'));
    }

    // string regexp
    {
      TokenEncoder encoder = encoder();

      encoder.appendRegexpString("abc", "m".getBytes());
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x31, 0x27, 0x03, 0x61, 0x62, 0x63, 0x61, 'm'));
    }

    // Pattern object
    {
      TokenEncoder encoder = encoder();

      encoder.appendRegexp(Pattern.compile("abc", Pattern.MULTILINE));
      assertEquals(1, encoder.trackOffsetLastValue());

      assertThat(bodyBytes(encoder), expectedBytes(0x31, 0x27, 0x03, 0x61, 0x62, 0x63, 0x61, 'm'));
    }
  }

  @Test
  public void reset() throws SerealException {
    TokenEncoder encoder = new TokenEncoder();

    encoder.poisonBuffer();

    {
      encoder.startDocument();
      encoder.appendString("abc");
      encoder.endDocument();

      assertThat(bodyBytes(encoder), expectedBytes(0x27, 0x03, 0x61, 0x62, 0x63));
    }

    encoder.reset();
    encoder.poisonBuffer();

    {
      encoder.startDocument();
      encoder.appendLong(70);
      encoder.endDocument();

      assertThat(bodyBytes(encoder), expectedBytes(0x20, 70));
    }
  }

  @Test
  public void trackOffsets() throws SerealException {
    TokenEncoder encoder = new TokenEncoder();

    {
      encoder.startHeader();
      assertEquals(1, encoder.trackOffsetNextValue());

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      assertEquals(2, encoder.trackOffsetNextValue());

      encoder.startArrayValue(2);
      assertEquals(2, encoder.trackOffsetLastValue());
      assertEquals(4, encoder.trackOffsetNextValue());

      encoder.appendString("ab");
      assertEquals(4, encoder.trackOffsetLastValue());
      assertEquals(8, encoder.trackOffsetNextValue());

      encoder.appendLong(1234);
      assertEquals(8, encoder.trackOffsetLastValue());
      assertEquals(11, encoder.trackOffsetNextValue());

      encoder.endArray();
      assertEquals(8, encoder.trackOffsetLastValue());
      assertEquals(11, encoder.trackOffsetNextValue());

      encoder.endHeader();
    }

    {
      encoder.startDocument();
      assertEquals(1, encoder.trackOffsetNextValue());

      encoder.appendRefNext();
      assertEquals(1, encoder.trackOffsetLastValue());
      assertEquals(2, encoder.trackOffsetNextValue());

      encoder.startArrayValue(2);
      assertEquals(2, encoder.trackOffsetLastValue());
      assertEquals(4, encoder.trackOffsetNextValue());

      encoder.appendString("ab");
      assertEquals(4, encoder.trackOffsetLastValue());
      assertEquals(8, encoder.trackOffsetNextValue());

      encoder.appendLong(1234);
      assertEquals(8, encoder.trackOffsetLastValue());
      assertEquals(11, encoder.trackOffsetNextValue());

      encoder.endArray();
      assertEquals(8, encoder.trackOffsetLastValue());
      assertEquals(11, encoder.trackOffsetNextValue());

      encoder.endDocument();
    }
  }

  private static TokenEncoder encoder() throws SerealException {
    TokenEncoder encoder = new TokenEncoder();

    encoder.poisonBuffer();
    encoder.startDocument();

    return encoder;
  }

  private static byte[] bodyBytes(TokenEncoder encoder) throws SerealException {
    assertTrue(encoder.isComplete());
    encoder.endDocument();
    byte[] document = encoder.getData();

    return Arrays.copyOfRange(document, 6, document.length);
  }

  private static Matcher<byte[]> expectedBytes(int... ints) {
    return equalTo(TestUtils.byteArray(ints));
  }

  private static Matcher<byte[]> expectedBytesNested(Object... ints) {
    return equalTo(TestUtils.byteArrayNested(ints));
  }
}
