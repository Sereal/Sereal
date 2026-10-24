package com.booking.sereal.jackson;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import com.booking.sereal.DecoderOptions;
import com.booking.sereal.Encoder;
import com.booking.sereal.EncoderOptions;
import com.booking.sereal.SerealException;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;

/** Malformed documents must fail with a {@link JsonParseException}, quickly. */
public class MalformedInputParserTest {
  private static final SerealObjectMapper MAPPER = new SerealObjectMapper();

  private static byte[] hex(String s) {
    byte[] out = new byte[s.length() / 2];
    for (int i = 0; i < out.length; i++) {
      out[i] = (byte) Integer.parseInt(s.substring(2 * i, 2 * i + 2), 16);
    }
    return out;
  }

  private static byte[] concat(byte[]... parts) {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    for (byte[] part : parts) {
      out.write(part, 0, part.length);
    }
    return out.toByteArray();
  }

  private static byte[] repeat(int b, int count) {
    byte[] out = new byte[count];
    Arrays.fill(out, (byte) b);
    return out;
  }

  // consumes the whole token stream, reading every value
  private static int stream(SerealObjectMapper mapper, byte[] data) throws IOException {
    try (JsonParser parser = mapper.getFactory().createParser(data)) {
      int count = 0;
      JsonToken token;
      while ((token = parser.nextToken()) != null) {
        count++;
        if (token == JsonToken.VALUE_STRING || token == JsonToken.FIELD_NAME) {
          parser.getText();
        } else if (token == JsonToken.VALUE_EMBEDDED_OBJECT) {
          parser.getEmbeddedObject();
        }
      }
      return count;
    }
  }

  private static void assertRejected(byte[] data) {
    assertThrows(JsonParseException.class, () -> stream(MAPPER, data));
    assertThrows(JsonParseException.class, () -> MAPPER.readTree(data));
  }

  private static void assertRejected(String packet) {
    assertRejected(hex(packet));
  }

  // ---- COPY, REFP and recursion ------------------------------------------------------------

  @Test(timeout = 10000)
  public void refpToEnclosingArray() {
    // tracked ARRAY [REFP 1]: the reference points at the array being parsed
    assertRejected("3df3726c0300ab012901");
  }

  @Test(timeout = 10000)
  public void copyOfEnclosingArray() {
    // ARRAYREF_1 [COPY 1]
    assertRejected("3df3726c0300412f01");
  }

  @Test(timeout = 10000)
  public void copyOfItself() {
    assertRejected("3df3726c03002f01");
  }

  @Test(timeout = 10000)
  public void copyOfCopyExpansion() {
    // ARRAY of ARRAYREF_2 [COPY prev, COPY prev] levels: the COPY targets contain COPYs
    ByteArrayOutputStream body = new ByteArrayOutputStream();
    int levels = 40;
    body.write(0x2b);
    body.write(levels + 1);
    int prev = body.size() + 1;
    body.write(0x42);
    body.write(0x01);
    body.write(0x01);
    for (int i = 0; i < levels; i++) {
      int cur = body.size() + 1;
      body.write(0x42);
      body.write(0x2f);
      body.write(prev);
      body.write(0x2f);
      body.write(prev);
      prev = cur;
    }
    assertRejected(concat(hex("3df3726c0300"), body.toByteArray()));
  }

  @Test(timeout = 10000)
  public void refnRunOverRecursionLimit() {
    // a million REFN tags around a value
    assertRejected(concat(hex("3df3726c0300"), repeat(0x28, 1_000_000), hex("01")));
  }

  @Test(timeout = 10000)
  public void subDecodeNestingLimit() throws IOException {
    // ARRAY [ [1], [REFP 3], [REFP 5], ... ]: each element references the previous one, so
    // following them nests 30 sub-decodes (offsets stay below 128, so each is a 1-byte varint)
    ByteArrayOutputStream body = new ByteArrayOutputStream();
    int levels = 30;
    body.write(0x2b);
    body.write(levels + 1);
    int prev = body.size() + 1;
    body.write(0xc1); // tracked ARRAYREF_1
    body.write(0x01);
    for (int i = 0; i < levels; i++) {
      int cur = body.size() + 1;
      body.write(0xc1);
      body.write(0x29);
      body.write(prev);
      prev = cur;
    }
    byte[] doc = concat(hex("3df3726c0300"), body.toByteArray());
    stream(MAPPER, doc);
    SerealObjectMapper limited =
        new SerealObjectMapper(new EncoderOptions(), new DecoderOptions().maxRecursionDepth(10));
    assertThrows(JsonParseException.class, () -> stream(limited, doc));
  }

  @Test
  public void repeatedStringsUseCopy() throws IOException, SerealException {
    List<Object> rows = new ArrayList<>();
    for (int i = 0; i < 3; i++) {
      Map<String, Object> row = new LinkedHashMap<>();
      row.put("name", "repeated name");
      row.put("text", "repeated text");
      rows.add(row);
    }
    // the core Encoder emits COPY tags for repeated strings, hash keys included
    byte[] doc = new Encoder(new EncoderOptions()).write(rows).getData();
    JsonNode tree = MAPPER.readTree(doc);
    assertEquals(3, tree.size());
    for (JsonNode row : tree) {
      assertEquals("repeated name", row.get("name").asText());
      assertEquals("repeated text", row.get("text").asText());
    }
  }

  // ---- bounded reads -----------------------------------------------------------------------

  @Test
  public void stringsPastTheEnd() {
    // BINARY of 16 bytes and SHORT_BINARY_31, with 2 bytes present
    assertRejected("3df3726c030026106162");
    assertRejected("3df3726c03007f6162");
    // STR_UTF8 of 16 bytes, 2 present
    assertRejected("3df3726c030027106162");
  }

  @Test
  public void invalidRegexpPattern() {
    // REGEXP with the pattern "(" and no modifiers
    assertRejected("3df3726c030031612860");
  }
}
