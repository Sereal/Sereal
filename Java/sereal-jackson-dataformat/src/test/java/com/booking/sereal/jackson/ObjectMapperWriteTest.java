package com.booking.sereal.jackson;

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.Decoder;
import com.booking.sereal.SerealException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.junit.Test;

public class ObjectMapperWriteTest {
  private static class StringField {
    public String stringField;
  }

  private static class CharsField {
    public char[] charsField;
  }

  private static class LongField {
    public long longField;
  }

  private static class FloatField {
    public float floatField;
  }

  private static class DoubleField {
    public double doubleField;
  }

  private static class BooleanField {
    public boolean booleanField;
  }

  private static class BytesField {
    public byte[] bytesField;
  }

  private static class IntField {
    public int intField;
  }

  private static class BigIntegerField {
    public BigInteger bigIntegerField;
  }

  private static class BigDecimalField {
    public BigDecimal bigDecimalField;
  }

  private static class PatternField {
    public Pattern patternField;
  }

  private static class ArrayFields {
    public List<String> stringList;
  }

  @Test
  public void nullField() throws JsonProcessingException, SerealException {
    StringField object = new StringField();
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("stringField", null)));
  }

  @Test
  public void stringField() throws JsonProcessingException, SerealException {
    StringField object = new StringField();
    object.stringField = "abc";
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("stringField", "abc")));
  }

  @Test
  public void charsField() throws JsonProcessingException, SerealException {
    CharsField object = new CharsField();
    object.charsField = "def".toCharArray();
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("charsField", "def")));
  }

  @Test
  public void bytesField() throws JsonProcessingException, SerealException {
    BytesField object = new BytesField();
    object.bytesField = "abc".getBytes();
    Map<String, Object> hash = roundTrip(object);

    assertTrue(Arrays.equals("abc".getBytes(), (byte[]) hash.get("bytesField")));
  }

  @Test
  public void intField() throws JsonProcessingException, SerealException {
    IntField object = new IntField();
    object.intField = 77;
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("intField", 77L)));
  }

  @Test
  public void longField() throws JsonProcessingException, SerealException {
    LongField object = new LongField();
    object.longField = 987;
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("longField", 987L)));
  }

  @Test
  public void floatField() throws JsonProcessingException, SerealException {
    FloatField object = new FloatField();
    object.floatField = 88.7f;
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("floatField", 88.7f)));
  }

  @Test
  public void doubleField() throws JsonProcessingException, SerealException {
    DoubleField object = new DoubleField();
    object.doubleField = 99.5;
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("doubleField", 99.5)));
  }

  @Test
  public void booleanField() throws JsonProcessingException, SerealException {
    BooleanField object = new BooleanField();
    object.booleanField = true;
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("booleanField", true)));
  }

  @Test
  public void bigIntegrField() throws JsonProcessingException, SerealException {
    BigIntegerField object = new BigIntegerField();
    object.bigIntegerField = new BigInteger("123");
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("bigIntegerField", "123")));
  }

  @Test
  public void bigDecimalField() throws JsonProcessingException, SerealException {
    BigDecimalField object = new BigDecimalField();
    object.bigDecimalField = new BigDecimal("123.4");
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("bigDecimalField", "123.4")));
  }

  @Test
  public void patternField() throws JsonProcessingException, SerealException {
    PatternField object = new PatternField();
    object.patternField = Pattern.compile("abc", Pattern.MULTILINE);
    Map<String, Object> hash = roundTrip(object);
    Pattern value = (Pattern) hash.get("patternField");

    assertThat(value.pattern(), equalTo("abc"));
    assertThat(value.flags(), equalTo(Pattern.MULTILINE));
  }

  @Test
  public void arrayField() throws JsonProcessingException, SerealException {
    ArrayFields object = new ArrayFields();
    object.stringList = Arrays.asList("abc", "def");
    Map<String, Object> hash = roundTrip(object);

    assertThat(hash, equalTo(Collections.singletonMap("stringList", Arrays.asList("abc", "def"))));
  }

  @SuppressWarnings("unchecked")
  private Map<String, Object> roundTrip(Object object) throws JsonProcessingException, SerealException {
    Decoder decoder = new Decoder();
    ObjectMapper objectMapper = new SerealObjectMapper();
    byte[] bytes = objectMapper.writeValueAsBytes(object);

    decoder.setData(bytes);

    return (Map<String, Object>) decoder.decode();
  }
}
