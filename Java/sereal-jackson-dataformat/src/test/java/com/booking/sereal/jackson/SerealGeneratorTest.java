package com.booking.sereal.jackson;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import com.booking.sereal.SerealException;
import com.booking.sereal.SerealToken;
import com.booking.sereal.TokenDecoder;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.junit.Test;

public class SerealGeneratorTest {
  @FunctionalInterface
  private interface Check {
    void check(TokenDecoder decoder) throws SerealException;
  }

  private static final SerealFactory FACTORY = new SerealFactory();

  private final ByteArrayOutputStream bos = new ByteArrayOutputStream();
  private final SerealGenerator generator = newGenerator(bos);

  @Test
  public void writeObjectSanity() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeEndObject();

    TokenDecoder decoder = decoderFor(generator);

    assertEquals(SerealToken.REFN, decoder.nextToken());
    assertEquals(SerealToken.HASH_START, decoder.nextToken());

    assertEquals(SerealToken.HASH_END, decoder.nextToken());
    assertEquals(SerealToken.END, decoder.nextToken());
  }

  @Test
  public void writeArraySanity() throws IOException, SerealException {
    generator.writeStartArray();
    generator.writeEndArray();

    TokenDecoder decoder = decoderFor(generator);

    assertEquals(SerealToken.REFN, decoder.nextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.nextToken());

    assertEquals(SerealToken.ARRAY_END, decoder.nextToken());
    assertEquals(SerealToken.END, decoder.nextToken());
  }

  @Test
  public void writeString() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeString("value");
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.UTF8, decoder.nextToken());
      assertEquals("value", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8
      ));
    });
  }

  @Test
  public void writeSubString() throws IOException, SerealException {
    char[] chars = "value".toCharArray();

    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeString(chars, 1, 3);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.UTF8, decoder.nextToken());
      assertEquals("alu", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8
      ));
    });
  }

  @Test
  public void writeRawUTF8() throws IOException, SerealException {
    byte[] utf8Bytes = "\u1010".getBytes(StandardCharsets.UTF_8);

    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeUTF8String(utf8Bytes, 0, utf8Bytes.length);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.UTF8, decoder.nextToken());
      assertArrayEquals(utf8Bytes, Arrays.copyOfRange(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceEnd()
      ));
    });
  }

  @Test
  public void writeBinary() throws IOException, SerealException {
    byte[] bytes = new byte[] { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06 };

    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeBinary(bytes, 1, 3);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.BINARY, decoder.nextToken());
      assertArrayEquals(Arrays.copyOfRange(bytes, 1, 4), Arrays.copyOfRange(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceEnd()
      ));
    });
  }

  @Test
  public void writeNumberInt() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNumber(7734);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.LONG, decoder.nextToken());
      assertEquals(7734, decoder.longValue());
    });
  }

  @Test
  public void writeNumberLong() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNumber(773467865987897L);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.LONG, decoder.nextToken());
      assertEquals(773467865987897L, decoder.longValue());
    });
  }

  @Test
  public void writeNumberFloat() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNumber(3.125f);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.FLOAT, decoder.nextToken());
      assertEquals(3.125f, decoder.floatValue(), 0.0);
    });
  }

  @Test
  public void writeNumberDouble() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNumber(8.25d);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.DOUBLE, decoder.nextToken());
      assertEquals(8.25d, decoder.doubleValue(), 0.0);
    });
  }

  @Test
  public void writeNumberBigInteger() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNumber(new BigInteger("000709834209832901809284094231832408231098230941"));
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.UTF8, decoder.nextToken());
      assertEquals("709834209832901809284094231832408231098230941", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8
      ));
    });
  }

  @Test
  public void writeNumberBigDecimal() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNumber(new BigDecimal("000903820982109814098240841048048.7326983721497000"));
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.UTF8, decoder.nextToken());
      assertEquals("903820982109814098240841048048.7326983721497000", new String(
        decoder.decoderBuffer(),
        decoder.binarySliceStart(),
        decoder.binarySliceLength(),
        StandardCharsets.UTF_8
      ));
    });
  }

  @Test
  public void writeBoolean() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeBoolean(true);
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.TRUE, decoder.nextToken());
    });
  }

  @Test
  public void writeNull() throws IOException, SerealException {
    generator.writeStartObject();
    generator.writeFieldName("field");
    generator.writeNull();
    generator.writeEndObject();

    checkHashValue(generator, decoder -> {
      assertEquals(SerealToken.CANONICAL_UNDEF, decoder.nextToken());
    });
  }

  @Test
  public void writeArray() throws IOException, SerealException {
    generator.writeStartArray();
    generator.writeNumber(77);
    generator.writeNull();
    generator.writeEndArray();

    TokenDecoder decoder = decoderFor(generator);

    assertEquals(SerealToken.REFN, decoder.nextToken());
    assertEquals(SerealToken.ARRAY_START, decoder.nextToken());

    assertEquals(SerealToken.LONG, decoder.nextToken());
    assertEquals(77, decoder.longValue());

    assertEquals(SerealToken.CANONICAL_UNDEF, decoder.nextToken());

    assertEquals(SerealToken.ARRAY_END, decoder.nextToken());
    assertEquals(SerealToken.END, decoder.nextToken());
  }

  private TokenDecoder decoderFor(SerealGenerator generator) throws IOException, SerealException {
    TokenDecoder decoder = new TokenDecoder();
    generator.close();

    decoder.setData(bos.toByteArray());
    decoder.prepareDecodeBody();

    return decoder;
  }

  private void checkHashValue(SerealGenerator generator, Check check) throws IOException, SerealException {
    TokenDecoder decoder = decoderFor(generator);

    assertEquals(SerealToken.REFN, decoder.nextToken());
    assertEquals(SerealToken.HASH_START, decoder.nextToken());

    assertEquals(SerealToken.UTF8, decoder.nextToken());
    assertEquals("field", new String(
      decoder.decoderBuffer(),
      decoder.binarySliceStart(),
      decoder.binarySliceLength(),
      StandardCharsets.UTF_8
    ));

    check.check(decoder);

    assertEquals(SerealToken.HASH_END, decoder.nextToken());
    assertEquals(SerealToken.END, decoder.nextToken());
  }

  private static SerealGenerator newGenerator(OutputStream os) {
    try {
      return (SerealGenerator) FACTORY.createGenerator(os);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
