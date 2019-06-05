package com.booking.sereal;

/**
 * Enumeration for Sereal token types, used by {@link com.booking.sereal.TokenDecoder}.
 */
public enum SerealToken {
  /** Returned when there is no token. */
  NONE,

  /**
   * An integer value up to 64 bits.
   */
  LONG,

  /**
   * A positive integer value larger than {@link java.lang.Long#MAX_VALUE}.
   * <p>
   * Java does not have unsigned values, so the value is returned as a signed (negative) {@code long}
   * with the same bit pattern as the unsigned value.
   */
  UNSIGNED_LONG,

  /**
   * A binary/ISO-8859-1 string.
   */
  BINARY,

  /**
   * An array value.
   * <p>
   * This token will always by preceded by a {@code REFN} token.
   */
  ARRAY_START,

  /**
   * Returned after the last element of an array.
   */
  ARRAY_END,

  /**
   * An hash value.
   * <p>
   * This token will always by preceded by a {@code REFN} token.
   */
  HASH_START,

  /**
   * Returned after the last element of an hash.
   */
  HASH_END,

  /**
   * Returned after the last element of the Sereal document.
   */
  END,

  /**
   * A 32-bit IEEE-754 floating point number.
   */
  FLOAT,

  /**
   * A 64-bit IEEE-754 floating point number.
   */
  DOUBLE,

  /**
   * Canonical {@code true} value.
   */
  TRUE,

  /**
   * Canonical {@code false} value.
   */
  FALSE,

  /**
   * An {@code undef} value.
   */
  UNDEF,

  /**
   * An UTF-8 encoded string.
   */
  UTF8,

  /**
   * A Perl reference, pointing to the object following the {@code REFN} token.
   */
  REFN,

  /**
   * A Perl reference, pointing to an object already encountered at a previous offset.
   */
  REFP,

  /**
   * A Perl object.
   * <p>
   * This token will always by preceded by a {@code REFN} token.
   */
  OBJECT_START,

  /**
   * Returned after the end of a Perl object.
   */
  OBJECT_END,

  /**
   * Sereal copy tag, referring to a previous value.
   */
  COPY,

  /**
   * Sereal alias tag, referring to a previous value.
   */
  ALIAS,

  /**
   * Marks the following reference as a weak reference.
   */
  WEAKEN,

  /**
   * Regular expression value.
   */
  REGEXP,

  /**
   * Canonical {@code undef} value.
   */
  CANONICAL_UNDEF,

  /**
   * Returned after the end of a sub-decode.
   */
  SUBDECODE_END;
}
