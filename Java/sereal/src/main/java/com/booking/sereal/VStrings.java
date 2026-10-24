package com.booking.sereal;

/**
 * V-string spelling rules for the protocol version 6 EXTEND VSTRING tags (mirrors Perl's
 * {@code scan_vstring}).
 *
 * <p>A spelling matches {@code ^v?[0-9_]*(\.[0-9][0-9_]*)*$}: a component after a dot must start
 * with a digit. Each component's value is its digits
 * (underscores ignored, leading zeros allowed) read as an unsigned 64-bit integer with wraparound;
 * an empty component is 0. The decoded string is UTF-8 (id {@code 0x27}) iff any component is
 * greater than 127, otherwise binary (id {@code 0x26}).
 */
final class VStrings {
  private VStrings() {}

  static final int INVALID = -1;
  static final int BINARY = 0;
  static final int UTF8 = 1;

  /**
   * @param s buffer holding the spelling
   * @param off start of the spelling
   * @param len length of the spelling
   * @return {@link #INVALID}, {@link #BINARY} or {@link #UTF8}
   */
  static int classify(byte[] s, int off, int len) {
    if (len <= 0) {
      return INVALID;
    }
    int pos = off;
    int end = off + len;
    if (s[pos] == 'v') {
      pos++;
    }
    boolean utf8 = false;
    boolean first = true;
    while (true) {
      // components after a '.' must start with a digit
      if (!first && (pos == end || !isDigit(s[pos]))) {
        return INVALID;
      }
      long value = 0;
      while (pos < end && (isDigit(s[pos]) || s[pos] == '_')) {
        if (s[pos] != '_') {
          value = value * 10 + (s[pos] - '0'); // wraps like a C UV
        }
        pos++;
      }
      if (Long.compareUnsigned(value, 127) > 0) {
        utf8 = true;
      }
      if (pos == end) {
        return utf8 ? UTF8 : BINARY;
      }
      if (s[pos] != '.') {
        return INVALID;
      }
      pos++;
      first = false;
    }
  }

  private static boolean isDigit(byte b) {
    return b >= '0' && b <= '9';
  }
}
