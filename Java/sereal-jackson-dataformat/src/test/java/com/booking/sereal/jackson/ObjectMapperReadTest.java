package com.booking.sereal.jackson;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.Encoder;
import com.booking.sereal.PerlObject;
import com.booking.sereal.SerealException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.junit.Test;

public class ObjectMapperReadTest {
  private static final TypeReference<Map<String, ScalarFields>> MAP_STRING_SCALARFIELDS =
    new TypeReference<Map<String, ScalarFields>>() {};

  private static class ScalarFields {
    public String stringField;
    public long longField;
    public float floatField;
    public double doubleField;
    public boolean booleanField;
    public byte[] bytesField;
    public int intField;
    public BigInteger bigIntegerField;
    public BigDecimal bigDecimalField;
  }

  private static class ArrayFields {
    public List<String> stringList;
    public String[] stringArray;
    public List<ScalarFields> scalarFieldsList;
    public ScalarFields[] scalarFieldsArray;
  }

  private static class ObjectFields {
    public ScalarFields scalarFields;
  }

  private static class PatternFields {
    public Pattern patternField;
  }

  @Test
  public void scalarFields() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("stringField", "abc");
      put("longField", 7);
      put("floatField", 6.75f);
      put("doubleField", 5.375d);
      put("booleanField", true);
      put("bytesField", new byte[] { 'd', 'e', 'f' });
      put("intField", 8);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(7, decoded.longField);
    assertEquals("abc", decoded.stringField);
    assertEquals(6.75, decoded.floatField, 0.0001);
    assertEquals(5.375, decoded.doubleField, 0.0001);
    assertTrue(decoded.booleanField);
    assertTrue(Arrays.equals("def".getBytes(), decoded.bytesField));
    assertEquals(8, decoded.intField);
  }

  @Test
  public void listField() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("stringList", Arrays.asList("abc", "def"));
    }};
    ArrayFields decoded = decodeAs(hash, ArrayFields.class);

    assertEquals(Arrays.asList("abc", "def"), decoded.stringList);
  }

  @Test
  public void arrayField() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("stringArray", new String[] {"abc", "def"});
    }};
    ArrayFields decoded = decodeAs(hash, ArrayFields.class);

    assertArrayEquals(new String[] {"abc", "def"}, decoded.stringArray);
  }

  @Test
  public void objectListField() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("scalarFieldsList", Arrays.asList(
        new HashMap<String, Object>() {{
          put("stringField", "abc");
        }},
        new HashMap<String, Object>() {{
          put("longField", 7L);
        }}
      ));
    }};
    ArrayFields decoded = decodeAs(hash, ArrayFields.class);

    assertEquals(2, decoded.scalarFieldsList.size());
    assertEquals("abc", decoded.scalarFieldsList.get(0).stringField);
    assertEquals(7L, decoded.scalarFieldsList.get(1).longField);
  }

  @Test
  public void objectArrayField() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("scalarFieldsArray", Arrays.asList(
        new HashMap<String, Object>() {{
          put("stringField", "abc");
        }},
        new HashMap<String, Object>() {{
          put("longField", 7L);
        }}
      ));
    }};
    ArrayFields decoded = decodeAs(hash, ArrayFields.class);

    assertEquals(2, decoded.scalarFieldsArray.length);
    assertEquals("abc", decoded.scalarFieldsArray[0].stringField);
    assertEquals(7L, decoded.scalarFieldsArray[1].longField);
  }

  @Test
  public void copyTagFieldName() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("scalarFieldsList", Arrays.asList(
        new HashMap<String, Object>() {{
          put("stringField", "abc");
        }},
        new HashMap<String, Object>() {{
          // this gets emitted as a COPY tag
          put("stringField", "def");
        }}
      ));
    }};
    ArrayFields decoded = decodeAs(hash, ArrayFields.class);

    assertEquals(2, decoded.scalarFieldsList.size());
    assertEquals("abc", decoded.scalarFieldsList.get(0).stringField);
    assertEquals("def", decoded.scalarFieldsList.get(1).stringField);
  }

  @Test
  public void nestedObject() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("scalarFields", new HashMap<String, Object>() {{
        put("stringField", "xyz");
      }});
    }};
    ObjectFields decoded = decodeAs(hash, ObjectFields.class);

    assertEquals("xyz", decoded.scalarFields.stringField);
  }

  @Test
  public void perlObject() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("scalarFields", new PerlObject(
        "ScalarFields",
        new HashMap<String, Object>() {{
          put("stringField", "xyz");
        }})
      );
    }};
    ObjectFields decoded = decodeAs(hash, ObjectFields.class);

    assertEquals("xyz", decoded.scalarFields.stringField);
  }

  @Test
  public void refp() throws SerealException, IOException {
    Map<String, Object> inner = new HashMap<String, Object>() {{
      put("stringField", "x");
    }};
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("a", inner);
      put("b", inner);
    }};
    Map<String, ScalarFields> decoded = decodeAs(hash, MAP_STRING_SCALARFIELDS);

    {
      ScalarFields sf = decoded.get("a");

      assertEquals(sf.stringField, "x");
    }

    {
      ScalarFields sf = decoded.get("b");

      assertEquals(sf.stringField, "x");
    }
  }

  @Test
  public void regexpField() throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("patternField", Pattern.compile("xyz", Pattern.DOTALL|Pattern.MULTILINE));
    }};
    PatternFields decoded = decodeAs(hash, PatternFields.class);

    assertEquals("xyz", decoded.patternField.pattern());
    assertEquals(Pattern.DOTALL|Pattern.MULTILINE, decoded.patternField.flags());
  }

  @Test
  public void coerceLong() throws SerealException, IOException {
    checkLongCoercion(2, (long) 2);
    checkLongCoercion(3, (int) 3);
    checkLongCoercion(4, "4");
    checkLongCoercion(5, new byte[] { '5' });
    checkLongCoercion(6, 6.125f);
    checkLongCoercion(7, 7.125d);
  }

  @Test
  public void coerceFloat() throws SerealException, IOException {
    checkFloatCoercion(2, (long) 2);
    checkFloatCoercion(3, (int) 3);
    checkFloatCoercion(4, "4");
    checkFloatCoercion(5, new byte[] { '5' });
    checkFloatCoercion(6.125f, 6.125f);
    checkFloatCoercion(7.25f, 7.25d);
  }

  @Test
  public void coerceDouble() throws SerealException, IOException {
    checkDoubleCoercion(2, (long) 2);
    checkDoubleCoercion(3, (int) 3);
    checkDoubleCoercion(4, "4");
    checkDoubleCoercion(5, new byte[] { '5' });
    checkDoubleCoercion(6.125f, 6.125f);
    checkDoubleCoercion(7.25f, 7.25d);
  }

  @Test
  public void coerceString() throws SerealException, IOException {
    checkStringCoercion("2", (long) 2);
    checkStringCoercion("3", (int) 3);
    checkStringCoercion("4", "4");
    checkStringCoercion("5", new byte[] { '5' });
    checkStringCoercion("6.125", 6.125f);
    checkStringCoercion("7.25", 7.25d);
  }

  @Test
  public void coerceBoolean() throws SerealException, IOException {
    checkBooleanCoercion(true, (long) 2);
    checkBooleanCoercion(false, (long) 0);
    checkBooleanCoercion(true, (int) 3);
    checkBooleanCoercion(true, "4");
    checkBooleanCoercion(false, "");
    checkBooleanCoercion(false, "0");
    checkBooleanCoercion(true, new byte[] { '5' });
    checkBooleanCoercion(false, new byte[0]);
    checkBooleanCoercion(false, new byte[] { '0' });
    checkBooleanCoercion(true, 6.125f);
    checkBooleanCoercion(true, 7.25d);
    checkBooleanCoercion(false, null);
    checkBooleanCoercion(true, true);
    checkBooleanCoercion(false, false);
  }

  @Test
  public void coerceBytes() throws SerealException, IOException {
    checkBytesCoercion("2", (long) 2);
    checkBytesCoercion("3", (int) 3);
    checkBytesCoercion("4", "4");
    checkBytesCoercion("5", new byte[] { '5' });
    checkBytesCoercion("6.125", 6.125f);
    checkBytesCoercion("7.25", 7.25d);
    checkBytesCoercion("", false);
    checkBytesCoercion("1", true);
  }

  @Test
  public void coerceBigInteger() throws SerealException, IOException {
    checkBigIntegerCoercion("2", (long) 2);
    checkBigIntegerCoercion("3", (int) 3);
    checkBigIntegerCoercion("4", "4");
    checkBigIntegerCoercion("5", new byte[] { '5' });
    checkBigIntegerCoercion("6", 6.125f);
    checkBigIntegerCoercion("7", 7.125d);
  }

  @Test
  public void coerceBigDecimal() throws SerealException, IOException {
    checkBigDecimalCoercion("2", (long) 2);
    checkBigDecimalCoercion("3", (int) 3);
    checkBigDecimalCoercion("4", "4");
    checkBigDecimalCoercion("5", new byte[] { '5' });
    checkBigDecimalCoercion("6.125", 6.125f);
    checkBigDecimalCoercion("7.125", 7.125d);
  }

  private static <T> T decodeAs(Object value, Class<T> klass) throws SerealException, IOException {
    Encoder encoder = new Encoder();
    ObjectMapper objectMapper = new SerealObjectMapper();

    encoder.write(value);
    byte[] originalData = encoder.getData();

    return objectMapper.readValue(originalData, klass);
  }

  private static <T> T decodeAs(Object value, TypeReference<T> typeReference) throws SerealException, IOException {
    Encoder encoder = new Encoder();
    ObjectMapper objectMapper = new SerealObjectMapper();

    encoder.write(value);
    byte[] originalData = encoder.getData();

    return objectMapper.readValue(originalData, typeReference);
  }

  private void checkLongCoercion(long expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("longField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(expected, decoded.longField);
  }

  private void checkFloatCoercion(float expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("floatField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(expected, decoded.floatField, 0.0001);
  }

  private void checkDoubleCoercion(double expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("doubleField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(expected, decoded.doubleField, 0.0001);
  }

  private void checkStringCoercion(String expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("stringField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(expected, decoded.stringField);
  }

  private void checkBytesCoercion(String expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("bytesField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(expected, new String(decoded.bytesField));
  }

  private void checkBooleanCoercion(boolean expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("booleanField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(expected, decoded.booleanField);
  }

  private void checkBigIntegerCoercion(String expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("bigIntegerField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(new BigInteger(expected), decoded.bigIntegerField);
  }

  private void checkBigDecimalCoercion(String expected, Object value) throws SerealException, IOException {
    Map<String, Object> hash = new HashMap<String, Object>() {{
      put("bigDecimalField", value);
    }};
    ScalarFields decoded = decodeAs(hash, ScalarFields.class);

    assertEquals(new BigDecimal(expected), decoded.bigDecimalField);
  }
}
