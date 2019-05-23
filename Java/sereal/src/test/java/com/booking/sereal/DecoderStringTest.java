package com.booking.sereal;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class DecoderStringTest {

  @Test
  public void decode() throws Exception {
    Encoder e = new Encoder();
    Decoder dString = new Decoder(new DecoderOptions().forceJavaStringForByteArrayValues(true));
    Decoder dBytes = new Decoder(new DecoderOptions().forceJavaStringForByteArrayValues(false));

    StringBuilder big = new StringBuilder(1000);
    for (int i = 0; i < 1000; i++) {
      big.append("abc");
    }
    String bigString = big.toString();

    Map<String,Object> data = new HashMap<>();
    String keyShort = "hello ✓ world";
    String keyLong = "abc";
    data.put(keyShort, new Latin1String("asd"));
    data.put(keyLong, new Latin1String(bigString));

    byte[] encoded = e.write(data).getData();
    dString.setData(encoded);
    dBytes.setData(encoded);
    @SuppressWarnings("unchecked")
    HashMap<String,Object> decodedString = (HashMap<String, Object>) dString.decode();
    @SuppressWarnings("unchecked")
    HashMap<String,Object> decodedBytes = (HashMap<String, Object>) dBytes.decode();

    assertEquals(decodedBytes.get(keyShort).getClass(),byte[].class);
    assertEquals(decodedString.get(keyShort).getClass(),String.class);
    assertEquals(new String((byte[]) decodedBytes.get(keyShort)), decodedString.get(keyShort));


    assertEquals(decodedBytes.get(keyLong).getClass(),byte[].class);
    assertEquals(decodedString.get(keyLong).getClass(),String.class);
    assertEquals(new String((byte[]) decodedBytes.get(keyLong)), decodedString.get(keyLong));
  }
}