package com.booking.sereal;

public class TestUtils {
  public static byte[] byteArray(int... ints) {
    byte[] bytes = new byte[ints.length];

    for (int i = 0; i < ints.length; ++i) {
      if (ints[i] < -128 || ints[i] > 255) {
        throw new IllegalArgumentException(String.format("Value %d at index %d is out of range", ints[i], i));
      }
      bytes[i] = (byte) ints[i];
    }

    return bytes;
  }
}
