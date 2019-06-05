package com.booking.sereal;

import java.util.Arrays;
import java.util.List;

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

  public static byte[] byteArrayNested(Object... ints) {
    byte[] bytes = new byte[Math.max(ints.length, 10)];
    int index = 0;

    for (int i = 0; i < ints.length; ++i) {
      Object value = ints[i];

      if (value instanceof byte[]) {
        byte[] bytesValue = (byte[]) value;

        while (bytes.length < index + bytesValue.length) {
          bytes = Arrays.copyOf(bytes, bytes.length * 3 / 2);
        }
        System.arraycopy(bytesValue, 0, bytes, index, bytesValue.length);
        index += bytesValue.length;
      } else if (value instanceof Number) {
        long longValue = ((Number) value).longValue();

        if (longValue < -128 || longValue > 255) {
          throw new IllegalArgumentException(
            String.format("Value %d at index %d is out of range", longValue, i));
        }
        bytes[index++] = (byte) longValue;
      } else if (value instanceof Character) {
        char charValue = (Character) value;

        if (charValue > 255) {
          throw new IllegalArgumentException(
            String.format("Value %d at index %d is out of range", (int) charValue, i));
        }
        bytes[index++] = (byte) charValue;
      } else if (value instanceof List) {
        byte[] bytesValue = byteArrayNested(((List) value).toArray());

        while (bytes.length < index + bytesValue.length) {
          bytes = Arrays.copyOf(bytes, bytes.length * 3 / 2);
        }
        System.arraycopy(bytesValue, 0, bytes, index, bytesValue.length);
        index += bytesValue.length;
      } else {
        throw new IllegalArgumentException("Don't know how to handle value of type " + value.getClass().getName());
      }
    }

    return Arrays.copyOf(bytes, index);
  }
}
