package com.booking.sereal;

import java.nio.charset.StandardCharsets;

/**
 * A Perl v-string (e.g. {@code v1.2.3}), carried by the protocol version 6 EXTEND VSTRING tags.
 *
 * <p>The value is the spelling, not Perl's reconstructed string of code points. Whether Perl would
 * flag the string as UTF-8 is derived from the spelling (any component greater than 127).
 */
public final class PerlVString {
  private final String spelling;
  private final boolean utf8;

  /**
   * @param spelling the v-string spelling, e.g. {@code "v1.2.3"}
   * @throws IllegalArgumentException if the spelling is not a valid v-string
   */
  public PerlVString(String spelling) {
    if (spelling == null) {
      throw new IllegalArgumentException("Invalid v-string spelling");
    }
    for (int i = 0; i < spelling.length(); i++) {
      if (spelling.charAt(i) > 0x7f) {
        throw new IllegalArgumentException("Invalid v-string spelling");
      }
    }
    byte[] bytes = spelling.getBytes(StandardCharsets.ISO_8859_1);
    int kind = VStrings.classify(bytes, 0, bytes.length);
    if (kind == VStrings.INVALID) {
      throw new IllegalArgumentException("Invalid v-string spelling");
    }
    this.spelling = spelling;
    this.utf8 = kind == VStrings.UTF8;
  }

  /** @return the spelling */
  public String spelling() {
    return spelling;
  }

  /** @return the spelling as ISO-8859-1 bytes */
  public byte[] spellingBytes() {
    return spelling.getBytes(StandardCharsets.ISO_8859_1);
  }

  /** @return {@code true} if Perl flags the decoded string as UTF-8 (a component above 127) */
  public boolean isUtf8() {
    return utf8;
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof PerlVString && spelling.equals(((PerlVString) o).spelling);
  }

  @Override
  public int hashCode() {
    return spelling.hashCode();
  }

  @Override
  public String toString() {
    return "PerlVString(" + spelling + ")";
  }
}
