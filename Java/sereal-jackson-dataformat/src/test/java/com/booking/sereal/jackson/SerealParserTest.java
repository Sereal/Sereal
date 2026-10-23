package com.booking.sereal.jackson;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.DecoderOptions;
import com.booking.sereal.EncoderOptions;
import com.booking.sereal.Float128;
import com.booking.sereal.PerlVString;
import com.booking.sereal.SerealChecksum;
import com.booking.sereal.SerealException;
import com.booking.sereal.TokenEncoder;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonLocation;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import org.junit.Test;

public class SerealParserTest {
  @FunctionalInterface
  private interface Emit {
    void emit(TokenEncoder decoder) throws SerealException;
  }

  private static final SerealFactory FACTORY = new SerealFactory();

  @Test
  public void parseInt() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendLong(1777));

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(1777, parser.getIntValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseUnsignedInt() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendUnsignedLong(0xfeffffffffffffffL));

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(new BigInteger("18374686479671623679"), parser.getBigIntegerValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseFloat() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendFloat(1.25f));

    assertEquals(JsonToken.VALUE_NUMBER_FLOAT, parser.nextToken());
    assertEquals(1.25f, parser.getFloatValue(), 0.0);

    assertNull(parser.nextToken());
  }

  @Test
  public void parseDouble() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendDouble(8.125d));

    assertEquals(JsonToken.VALUE_NUMBER_FLOAT, parser.nextToken());
    assertEquals(8.125d, parser.getDoubleValue(), 0.0);

    assertNull(parser.nextToken());
  }

  @Test
  public void parseString() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendString("각"));

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("각", parser.getText());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseBinary() throws SerealException, IOException {
    byte[] bytes = new byte[] { (byte) 0xEA, (byte) 0xB0, (byte) 0x81 };

    SerealParser parser = setupParser(encoder -> encoder.appendBinary(bytes));

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals(new String(bytes, StandardCharsets.ISO_8859_1), parser.getText());
    assertArrayEquals(bytes, parser.getBinaryValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseUndef() throws SerealException, IOException {
    SerealParser parser = setupParser(TokenEncoder::appendUndef);

    assertEquals(JsonToken.VALUE_NULL, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseCanonicalUndef() throws SerealException, IOException {
    SerealParser parser = setupParser(TokenEncoder::appendCanonicalUndef);

    assertEquals(JsonToken.VALUE_NULL, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseTrue() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendBoolean(true));

    assertEquals(JsonToken.VALUE_TRUE, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseFalse() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> encoder.appendBoolean(false));

    assertEquals(JsonToken.VALUE_FALSE, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseArray() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.startArray(2);
      encoder.appendLong(1);
      encoder.appendBoolean(true);
      encoder.endArray();
    });

    assertEquals(JsonToken.START_ARRAY, parser.nextToken());

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(1, parser.getIntValue());

    assertEquals(JsonToken.VALUE_TRUE, parser.nextToken());

    assertEquals(JsonToken.END_ARRAY, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseHash() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.startHash();
      encoder.appendBinary("key".getBytes());
      encoder.appendLong(123);
      encoder.endHash();
    });

    assertEquals(JsonToken.START_OBJECT, parser.nextToken());

    assertEquals(JsonToken.FIELD_NAME, parser.nextToken());
    assertEquals("key", parser.getCurrentName());

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(123, parser.getIntValue());
    assertEquals("key", parser.getCurrentName());

    assertEquals(JsonToken.END_OBJECT, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseRefNext() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.appendRefNext();
      encoder.appendLong(123);
    });

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(123, parser.getIntValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseWeaken() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.appendWeaken();
      encoder.appendRefNext();
      encoder.appendLong(123);
    });

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(123, parser.getIntValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parsePerlObject() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.appendRefNext();
      encoder.startObject("Klass");
      encoder.appendLong(123);
      encoder.endObject();
    });

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(123, parser.getIntValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseCopy() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.startArray();
      encoder.appendString("defg");

      int stringOffset = encoder.trackOffsetLastValue();

      encoder.appendCopy(stringOffset);
      encoder.endArray();
    });

    assertEquals(JsonToken.START_ARRAY, parser.nextToken());

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("defg", parser.getText());

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("defg", parser.getText());

    assertEquals(JsonToken.END_ARRAY, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseCopyHashKey() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.startArray();

      encoder.startHash();
      encoder.appendString("defg");
      int stringOffset = encoder.trackOffsetLastValue();
      encoder.appendLong(7);
      encoder.endHash();

      encoder.startHash();
      encoder.appendCopy(stringOffset);
      encoder.appendLong(7);
      encoder.endHash();
      encoder.endArray();
    });

    assertEquals(JsonToken.START_ARRAY, parser.nextToken());

    assertEquals(JsonToken.START_OBJECT, parser.nextToken());
    assertEquals(JsonToken.FIELD_NAME, parser.nextToken());
    assertEquals("defg", parser.getText());
    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(7, parser.getLongValue());
    assertEquals(JsonToken.END_OBJECT, parser.nextToken());

    assertEquals(JsonToken.START_OBJECT, parser.nextToken());
    assertEquals(JsonToken.FIELD_NAME, parser.nextToken());
    assertEquals("defg", parser.getText());
    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(7, parser.getLongValue());
    assertEquals(JsonToken.END_OBJECT, parser.nextToken());

    assertEquals(JsonToken.END_ARRAY, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseAlias() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.startArray();
      encoder.appendString("defg");

      int stringOffset = encoder.trackOffsetLastValue();

      encoder.appendAlias(stringOffset);
      encoder.endArray();
    });

    assertEquals(JsonToken.START_ARRAY, parser.nextToken());

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("defg", parser.getText());

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("defg", parser.getText());

    assertEquals(JsonToken.END_ARRAY, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseRefPrevious() throws SerealException, IOException {
    SerealParser parser = setupParser(encoder -> {
      encoder.startArray();
      encoder.appendRefNext();
      encoder.appendString("defg");

      int stringOffset = encoder.trackOffsetLastValue();

      encoder.appendRefPrevious(stringOffset);
      encoder.endArray();
    });

    assertEquals(JsonToken.START_ARRAY, parser.nextToken());

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("defg", parser.getText());

    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("defg", parser.getText());

    assertEquals(JsonToken.END_ARRAY, parser.nextToken());
    assertNull(parser.nextToken());
  }

  @Test
  public void parseStream() throws SerealException, IOException {
    {
      TokenEncoder encoder = new TokenEncoder();

      encoder.startDocument();
      encoder.appendLong(1789);
      encoder.endDocument();

      byte[] originalData = encoder.getData();
      SerealParser parser = (SerealParser) FACTORY
        .createParser(new ByteArrayInputStream(originalData));

      assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
      assertEquals(1789, parser.getIntValue());

      assertNull(parser.nextToken());
    }

    {
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < 10000; ++i) {
        sb.append('x');
      }

      TokenEncoder encoder = new TokenEncoder();

      encoder.startDocument();
      encoder.appendString(sb);
      encoder.endDocument();

      byte[] originalData = encoder.getData();
      SerealParser parser = (SerealParser) FACTORY
        .createParser(new ByteArrayInputStream(originalData));

      assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
      assertEquals(sb.toString(), parser.getText());

      assertNull(parser.nextToken());
    }
  }

  @Test
  public void getFieldNameAndText() throws SerealException, IOException {
    TokenEncoder tokenEncoder = new TokenEncoder();

    tokenEncoder.startDocument();
    tokenEncoder.startHash();
    tokenEncoder.appendBinary("key1".getBytes());
    tokenEncoder.appendLong(123);
    tokenEncoder.appendString("key2");
    tokenEncoder.startArray();
    tokenEncoder.appendDouble(234.25);
    tokenEncoder.endArray();
    tokenEncoder.endHash();
    tokenEncoder.endDocument();

    SerealParser parser = (SerealParser) FACTORY.createParser(tokenEncoder.getData());

    assertNull(parser.getCurrentName());
    assertNull(parser.getText());

    assertEquals(JsonToken.START_OBJECT, parser.nextToken());
    assertNull(parser.getCurrentName());
    assertEquals("{", parser.getText());

    assertEquals(JsonToken.FIELD_NAME, parser.nextToken());
    assertEquals("key1", parser.getCurrentName());
    assertEquals("key1", parser.getText());

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals("key1", parser.getCurrentName());
    assertEquals("123", parser.getText());

    assertEquals(JsonToken.FIELD_NAME, parser.nextToken());
    assertEquals("key2", parser.getCurrentName());
    assertEquals("key2", parser.getText());

    assertEquals(JsonToken.START_ARRAY, parser.nextToken());
    assertEquals("key2", parser.getCurrentName());
    assertEquals("[", parser.getText());

    assertEquals(JsonToken.VALUE_NUMBER_FLOAT, parser.nextToken());
    assertNull(parser.getCurrentName());
    assertEquals("234.25", parser.getText());

    assertEquals(JsonToken.END_ARRAY, parser.nextToken());
    assertEquals("key2", parser.getCurrentName());
    assertEquals("]", parser.getText());

    assertEquals(JsonToken.END_OBJECT, parser.nextToken());
    assertNull("key2", parser.getCurrentName());
    assertEquals("}", parser.getText());
  }

  @Test
  public void getLocations() throws SerealException, IOException {
    TokenEncoder tokenEncoder = new TokenEncoder();

    tokenEncoder.startDocument();
    tokenEncoder.startHash();
    tokenEncoder.appendBinary("key1".getBytes());
    tokenEncoder.appendLong(123);
    tokenEncoder.endHash();
    tokenEncoder.endDocument();

    SerealParser parser = (SerealParser) FACTORY.createParser(tokenEncoder.getData());

    {
      assertEquals(JsonToken.START_OBJECT, parser.nextToken());
      JsonLocation tokenLocation = parser.getTokenLocation();
      JsonLocation currentLocation = parser.getCurrentLocation();

      // offsets are 2 and 3 because of the (implicit) REFN
      assertEquals(0, tokenLocation.getCharOffset());
      assertEquals(2, tokenLocation.getByteOffset());
      assertEquals(0, tokenLocation.getLineNr());
      assertEquals(2, tokenLocation.getColumnNr());
      assertEquals(0, currentLocation.getCharOffset());
      assertEquals(3, currentLocation.getByteOffset());
      assertEquals(0, currentLocation.getLineNr());
      assertEquals(3, currentLocation.getColumnNr());
    }
  }

  @Test
  public void getNumber() throws SerealException, IOException {
    {
      SerealParser parser = setupParser(encoder -> encoder.appendLong(1234));

      assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
      assertEquals(JsonParser.NumberType.LONG, parser.getNumberType());
      assertThat(parser.getNumberValue(), instanceOf(Long.class));
      assertEquals(1234L, parser.getNumberValue());
      assertEquals(1234, parser.getIntValue());
      assertEquals(1234L, parser.getLongValue());
      assertEquals(1234.0F, parser.getFloatValue(), 0.0);
      assertEquals(1234.0D, parser.getDoubleValue(), 0.0);
      assertEquals(new BigInteger("1234"), parser.getBigIntegerValue());
      assertEquals(new BigDecimal("1234"), parser.getDecimalValue());
    }

    {
      SerealParser parser = setupParser(encoder -> encoder.appendFloat(1234.25f));

      assertEquals(JsonToken.VALUE_NUMBER_FLOAT, parser.nextToken());
      assertEquals(JsonParser.NumberType.FLOAT, parser.getNumberType());
      assertThat(parser.getNumberValue(), instanceOf(Float.class));
      assertEquals(1234.25F, parser.getNumberValue());
      assertEquals(1234, parser.getIntValue());
      assertEquals(1234L, parser.getLongValue());
      assertEquals(1234.25F, parser.getFloatValue(), 0.0);
      assertEquals(1234.25D, parser.getDoubleValue(), 0.0);
      assertEquals(new BigInteger("1234"), parser.getBigIntegerValue());
      assertEquals(new BigDecimal("1234.25"), parser.getDecimalValue());
    }

    {
      SerealParser parser = setupParser(encoder -> encoder.appendDouble(1234.125d));

      assertEquals(JsonToken.VALUE_NUMBER_FLOAT, parser.nextToken());
      assertEquals(JsonParser.NumberType.DOUBLE, parser.getNumberType());
      assertThat(parser.getNumberValue(), instanceOf(Double.class));
      assertEquals(1234.125D, parser.getNumberValue());
      assertEquals(1234, parser.getIntValue());
      assertEquals(1234L, parser.getLongValue());
      assertEquals(1234.125F, parser.getFloatValue(), 0.0);
      assertEquals(1234.125D, parser.getDoubleValue(), 0.0);
      assertEquals(new BigInteger("1234"), parser.getBigIntegerValue());
      assertEquals(new BigDecimal("1234.125"), parser.getDecimalValue());
    }

    {
      SerealParser parser = setupParser(encoder -> encoder.appendUnsignedLong(0xfeffffffffffffffL));

      assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
      assertEquals(JsonParser.NumberType.BIG_INTEGER, parser.getNumberType());
      assertThat(parser.getNumberValue(), instanceOf(BigInteger.class));
      assertEquals(new BigInteger("18374686479671623679"), parser.getNumberValue());
      assertEquals(-1, parser.getIntValue());
      assertEquals(-72057594037927937L, parser.getLongValue());
      assertEquals(1.8374686479671624E19F, parser.getFloatValue(), 0.0);
      assertEquals(1.8374686479671624E19D, parser.getDoubleValue(), 0.0);
      assertEquals(new BigInteger("18374686479671623679"), parser.getBigIntegerValue());
      assertEquals(new BigDecimal("18374686479671623679"), parser.getDecimalValue());
    }
  }

  // Perl Sereal::Encoder->new({checksum_v2_key => "0123456789abcdef"})->encode(7)
  private static final String RAW_V2KEY_7 = "3df3726c860002100f8ddc80c103af28700d68f7d168aa3b0107";
  private static final byte[] KEY = "0123456789abcdef".getBytes(StandardCharsets.ISO_8859_1);

  private static byte[] hex(String s) {
    byte[] out = new byte[s.length() / 2];
    for (int i = 0; i < out.length; i++) {
      out[i] = (byte) Integer.parseInt(s.substring(2 * i, 2 * i + 2), 16);
    }
    return out;
  }

  @Test
  public void parseVString() throws SerealException, IOException {
    SerealParser parser = setupParser(new EncoderOptions().protocolVersion(6), encoder -> {
      encoder.startHash(1);
      encoder.appendString("v");
      encoder.appendVString(new PerlVString("v1.2.3"));
      encoder.endHash();
    });

    assertEquals(JsonToken.START_OBJECT, parser.nextToken());
    assertEquals(JsonToken.FIELD_NAME, parser.nextToken());
    assertEquals("v", parser.getCurrentName());
    assertEquals(JsonToken.VALUE_STRING, parser.nextToken());
    assertEquals("v1.2.3", parser.getText());
    assertEquals(JsonToken.END_OBJECT, parser.nextToken());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseFloat128() throws SerealException, IOException {
    byte[] bytes = new byte[16];
    for (int i = 0; i < 16; i++) {
      bytes[i] = (byte) i;
    }
    SerealParser parser = setupParser(new EncoderOptions().protocolVersion(5), encoder -> encoder.appendFloat128(bytes));

    assertEquals(JsonToken.VALUE_EMBEDDED_OBJECT, parser.nextToken());
    assertNull(parser.getText());
    assertEquals(new Float128(bytes), parser.getEmbeddedObject());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseNegVarint() throws SerealException, IOException {
    SerealParser parser = setupParser(new EncoderOptions().protocolVersion(6), encoder -> encoder.appendLong(-1000));

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(-1000, parser.getLongValue());

    assertNull(parser.nextToken());
  }

  @Test
  public void parseChecksummedDocument() throws IOException {
    SerealFactory keyed = new SerealFactory(null, new DecoderOptions().checksumV2Key(KEY));
    JsonParser parser = keyed.createParser(hex(RAW_V2KEY_7));

    assertEquals(JsonToken.VALUE_NUMBER_INT, parser.nextToken());
    assertEquals(7, parser.getIntValue());
    assertNull(parser.nextToken());

    byte[] wrongKey = "AAAAAAAAAAAAAAAA".getBytes(StandardCharsets.ISO_8859_1);
    SerealFactory strict = new SerealFactory(null,
        new DecoderOptions().checksumV2Key(wrongKey).checksumPolicy(SerealChecksum.Policy.DIE));
    JsonParseException e = assertThrows(JsonParseException.class, () -> strict.createParser(hex(RAW_V2KEY_7)));
    assertTrue(e.getCause().getMessage(), e.getCause().getMessage().contains("verification failed"));
  }

  private SerealParser setupParser(Emit e) throws IOException, SerealException {
    return setupParser(new EncoderOptions(), e);
  }

  private SerealParser setupParser(EncoderOptions options, Emit e) throws IOException, SerealException {
    TokenEncoder encoder = new TokenEncoder(options);

    encoder.startDocument();
    e.emit(encoder);
    encoder.endDocument();

    byte[] originalData = encoder.getData();

    return (SerealParser) FACTORY.createParser(originalData);
  }
}
