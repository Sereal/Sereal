package com.booking.sereal;

import java.util.Arrays;

/**
 * The 16 raw bytes of a Perl quadmath {@code __float128} (Sereal FLOAT_128 tag, protocol version
 * 5+). Java has no 128-bit float type, so the value is carried uninterpreted.
 */
public final class Float128 {
  private final byte[] bytes;

  /**
   * @param bytes exactly 16 bytes (copied)
   * @throws IllegalArgumentException if {@code bytes} is not 16 bytes long
   */
  public Float128(byte[] bytes) {
    if (bytes == null || bytes.length != 16) {
      throw new IllegalArgumentException("FLOAT_128 requires exactly 16 bytes");
    }
    this.bytes = bytes.clone();
  }

  /** @return a copy of the 16 bytes */
  public byte[] bytes() {
    return bytes.clone();
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof Float128 && Arrays.equals(bytes, ((Float128) o).bytes);
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(bytes);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("Float128(");
    for (byte b : bytes) {
      sb.append(String.format("%02x", b & 0xff));
    }
    return sb.append(')').toString();
  }
}
