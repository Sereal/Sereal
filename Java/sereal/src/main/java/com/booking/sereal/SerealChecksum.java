package com.booking.sereal;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.function.Consumer;

/**
 * Keyed checksum envelopes (Sereal protocol version 6).
 *
 * <p>A protected document sets bit 7 of the version-type byte and carries, right after the header
 * suffix and before any compression framing:
 *
 * <pre>
 *   &lt;CHECKSUM-TYPE u8&gt; &lt;CHECKSUM-LENGTH varint&gt; &lt;CHECKSUM bytes&gt; &lt;PAYLOAD-LENGTH varint&gt; &lt;PAYLOAD&gt;
 * </pre>
 *
 * <p>The tag is a SipHash over the bytes from the magic through the checksum-length varint, then the
 * payload-length varint and the payload (the tag itself is excluded). Checksum type
 * {@link #SIPHASH_2_4_64} uses an 8-byte tag, {@link #SIPHASH_2_4_128} and {@link #SIPHASH_3_6_128}
 * use 16-byte tags. Keys are always exactly 16 bytes; use {@link #prepareKey(byte[], byte[])} to
 * derive one from an arbitrary secret the same way Perl's {@code prepare_checksum_key} does.
 */
public final class SerealChecksum {
  private SerealChecksum() {}

  /** SipHash-2-4, 64-bit tag. */
  public static final int SIPHASH_2_4_64 = 0;
  /** SipHash-2-4-128, 128-bit tag. */
  public static final int SIPHASH_2_4_128 = 1;
  /** SipHash-3-6-128, 128-bit tag (Perl's default). */
  public static final int SIPHASH_3_6_128 = 2;

  static final int KEY_LENGTH = 16;

  static final String NO_KEY_WARNING =
      "Decoding checksummed Sereal document without authentication: no matching checksum key is configured";
  static final String IGNORE_WARNING = "Ignoring Sereal checksum authentication by request";
  static final String PAYLOAD_LENGTH_MISMATCH = "Checksummed Sereal payload length mismatch";

  /** How a decoder treats checksum envelopes; mirrors Perl's {@code checksum_policy}. */
  public enum Policy {
    /**
     * Verify when a key for the document's checksum type is configured; otherwise decode the
     * document unauthenticated and report a warning. Unsigned documents are accepted. (default)
     */
    VERIFY_IF_AVAILABLE,
    /**
     * Signed documents must be verifiable: an unknown checksum type or a missing key is an error.
     * Unsigned documents are accepted.
     */
    DIE,
    /** Like {@link #DIE}, but unsigned documents are rejected too. Requires at least one key. */
    REQUIRE_KEY,
    /** Skip verification (the envelope framing is still validated). */
    IGNORE,
    /** Like {@link #IGNORE}, but report a warning for every signed document. */
    IGNORE_WARN,
  }

  /** Parsed envelope: offsets are absolute indexes into the document's array. */
  static final class Envelope {
    final int type;
    final boolean knownType;
    final int tagStart;
    final int tagEnd;
    final int payloadStart;
    final int payloadEnd;

    Envelope(int type, int tagStart, int tagEnd, int payloadStart, int payloadEnd) {
      this.type = type;
      this.knownType = tagLength(type) != 0;
      this.tagStart = tagStart;
      this.tagEnd = tagEnd;
      this.payloadStart = payloadStart;
      this.payloadEnd = payloadEnd;
    }
  }

  /**
   * Normalize an arbitrary secret to a 16-byte checksum key, compatible with Perl's
   * {@code Sereal::Decoder::prepare_checksum_key($key, $opt_key)}.
   *
   * @param key the secret ({@code null} is the empty string)
   * @param optKey an optional salt ({@code null} is the empty string)
   * @return a fresh 16-byte key
   */
  public static byte[] prepareKey(byte[] key, byte[] optKey) {
    byte[] k1 = sipHash24_128(
        "Call me Ishmael.".getBytes(StandardCharsets.ISO_8859_1),
        "Out damned spot!".getBytes(StandardCharsets.ISO_8859_1),
        optKey);
    return sipHash24_128(k1, "Luke, I am your father.".getBytes(StandardCharsets.ISO_8859_1), key);
  }

  private static byte[] sipHash24_128(byte[] key, byte[] prefix, byte[] data) {
    SipHash h = SipHash.forType(SIPHASH_2_4_128, key);
    h.update(prefix, 0, prefix.length);
    if (data != null) {
      h.update(data, 0, data.length);
    }
    return h.digest();
  }

  /**
   * Return the checksum tag of a document without verifying it.
   *
   * @param data the document
   * @return a copy of the tag bytes, or {@code null} when the document is not checksummed
   * @throws SerealException if the header or the envelope framing is malformed
   */
  public static byte[] getChecksum(byte[] data) throws SerealException {
    return getChecksum(data, 0, data.length);
  }

  /**
   * Return the checksum tag of a document without verifying it.
   *
   * @param data buffer holding the document
   * @param offset where the document starts
   * @param length bytes available from {@code offset}
   * @return a copy of the tag bytes, or {@code null} when the document is not checksummed
   * @throws SerealException if the header or the envelope framing is malformed
   */
  public static byte[] getChecksum(byte[] data, int offset, int length) throws SerealException {
    Envelope env = inspect(data, offset, length);
    if (env == null) {
      return null;
    }
    byte[] tag = new byte[env.tagEnd - env.tagStart];
    System.arraycopy(data, env.tagStart, tag, 0, tag.length);
    return tag;
  }

  /**
   * Verify the checksum of a document without decoding it.
   *
   * @param data the document
   * @param key the 16-byte key for the document's checksum type
   * @return {@code true} if the tag matches, {@code false} on a mismatch or for an unsigned document
   * @throws SerealException for malformed framing, an unknown checksum type, a {@code null} key on a
   *     checksummed document or a key that is not 16 bytes
   */
  public static boolean validateChecksum(byte[] data, byte[] key) throws SerealException {
    return validateChecksum(data, 0, data.length, key);
  }

  /**
   * Verify the checksum of a document without decoding it.
   *
   * @param data buffer holding the document
   * @param offset where the document starts
   * @param length bytes available from {@code offset}
   * @param key the 16-byte key for the document's checksum type
   * @return {@code true} if the tag matches, {@code false} on a mismatch or for an unsigned document
   * @throws SerealException for malformed framing, an unknown checksum type, a {@code null} key on a
   *     checksummed document or a key that is not 16 bytes
   */
  public static boolean validateChecksum(byte[] data, int offset, int length, byte[] key)
      throws SerealException {
    if (key != null && key.length != KEY_LENGTH) {
      throw new SerealException("The checksum key must be exactly 16 bytes");
    }
    Envelope env = inspect(data, offset, length);
    if (env == null) {
      return false;
    }
    if (!env.knownType) {
      throw new SerealException("Unsupported Sereal checksum type " + env.type);
    }
    if (key == null) {
      throw new SerealException("Validating a checksummed Sereal document requires a key");
    }
    return matches(data, offset, env, key);
  }

  // parse the header of a standalone document; null when it carries no envelope
  private static Envelope inspect(byte[] data, int offset, int length) throws SerealException {
    if (offset < 0 || length < 0 || offset > data.length || length > data.length - offset) {
      throw new SerealException("Sereal checksum offset is outside the input");
    }
    int end = offset + length;
    if (length < 5) {
      throw new SerealException("Invalid Sereal header: too few bytes");
    }
    int magic =
        ((data[offset] & 0xff) << 24)
            + ((data[offset + 1] & 0xff) << 16)
            + ((data[offset + 2] & 0xff) << 8)
            + (data[offset + 3] & 0xff);
    if (magic != SerealHeader.MAGIC && magic != SerealHeader.MAGIC_V3) {
      throw new SerealException(
          String.format("Invalid Sereal header (%08x): doesn't match magic", magic));
    }
    int versionAndType = data[offset + 4] & 0xff;
    int version = versionAndType & SerealHeader.SRL_PROTOCOL_VERSION_MASK;
    int encoding = (versionAndType >> 4) & SerealHeader.SRL_ENCODING_BASE_MASK;
    boolean hasChecksum = (versionAndType & 0x80) != 0;
    if (version < 1 || version > 6) {
      throw new SerealException(
          String.format("Invalid Sereal header: unsupported protocol version %d", version));
    }
    if (encoding > SerealHeader.SRL_ENCODING_ZSTD) {
      throw new SerealException("Unsupported encoding: unknown");
    }
    if (hasChecksum && version < 6) {
      throw new SerealException("Checksummed Sereal documents require protocol version 6 or later");
    }
    if (!hasChecksum) {
      return null;
    }
    int[] pos = {offset + 5};
    long suffixSize = readVarint(data, pos, end);
    if (Long.compareUnsigned(suffixSize, end - pos[0]) > 0) {
      throw new SerealException("Sereal header suffix exceeds the available input");
    }
    return read(data, pos[0] + (int) suffixSize, end);
  }

  /**
   * @param type checksum type
   * @return the tag length for a known checksum type, 0 for an unknown one
   */
  static int tagLength(int type) {
    return SipHash.tagLength(type);
  }

  /**
   * Parse a checksum envelope.
   *
   * @param data the document's array
   * @param pos first byte after the header suffix
   * @param end end of the available input
   * @return the envelope (type, tag and payload bounds)
   * @throws SerealException if the framing is truncated or inconsistent
   */
  static Envelope read(byte[] data, int pos, int end) throws SerealException {
    if (pos >= end) {
      throw new SerealException("Unexpected end of data at byte " + pos);
    }
    int type = data[pos++] & 0xff;
    int[] cursor = {pos};
    long tagLength = readVarint(data, cursor, end);
    pos = cursor[0];
    if (Long.compareUnsigned(tagLength, end - pos) > 0) {
      throw new SerealException("Sereal checksum length exceeds the available input");
    }
    int expected = tagLength(type);
    if (expected != 0 && tagLength != expected) {
      throw new SerealException(
          "Invalid checksum length " + tagLength + " for checksum type " + type);
    }
    int tagStart = pos;
    int tagEnd = pos + (int) tagLength;
    cursor[0] = tagEnd;
    long payloadLength = readVarint(data, cursor, end);
    int payloadStart = cursor[0];
    if (Long.compareUnsigned(payloadLength, end - payloadStart) > 0) {
      throw new SerealException("Checksummed Sereal payload length exceeds the available input");
    }
    return new Envelope(type, tagStart, tagEnd, payloadStart, payloadStart + (int) payloadLength);
  }

  /**
   * Compare the envelope's tag with the SipHash of the protected bytes (constant time).
   *
   * @param data the document's array
   * @param docStart index of the magic
   * @param env a parsed envelope of a known type
   * @param key 16-byte key
   * @return {@code true} if the tag matches
   */
  static boolean matches(byte[] data, int docStart, Envelope env, byte[] key) {
    SipHash h = SipHash.forType(env.type, key);
    h.update(data, docStart, env.tagStart - docStart);
    h.update(data, env.tagEnd, env.payloadStart - env.tagEnd);
    h.update(data, env.payloadStart, env.payloadEnd - env.payloadStart);
    byte[] actual = h.digest();
    byte[] expected = new byte[env.tagEnd - env.tagStart];
    System.arraycopy(data, env.tagStart, expected, 0, expected.length);
    return MessageDigest.isEqual(actual, expected);
  }

  /**
   * Read the envelope at {@code pos} and apply the checksum policy to it.
   *
   * @param data the document's array
   * @param docStart index of the magic
   * @param pos first byte after the header suffix
   * @param end end of the available input
   * @param keys three key slots indexed by checksum type ({@code null} = no key)
   * @param policy the decoder's policy
   * @param warn receives warnings (unauthenticated decoding, ignored checksums)
   * @return the envelope; the caller decodes {@code [payloadStart, payloadEnd)}
   * @throws SerealException for malformed framing or when the policy rejects the document
   */
  static Envelope process(byte[] data, int docStart, int pos, int end, byte[][] keys, Policy policy,
      Consumer<String> warn) throws SerealException {
    Envelope env = read(data, pos, end);
    boolean strict = policy == Policy.DIE || policy == Policy.REQUIRE_KEY;
    byte[] key = env.knownType ? keys[env.type] : null;
    boolean hasKey = key != null;

    if (strict && !env.knownType) {
      throw new SerealException("Unsupported Sereal checksum type " + env.type);
    }
    if (strict && !hasKey) {
      throw new SerealException("Checksummed Sereal document requires a checksum key for type " + env.type);
    }
    boolean verify = hasKey && (policy == Policy.VERIFY_IF_AVAILABLE || strict);
    if (policy == Policy.IGNORE_WARN) {
      warn.accept(IGNORE_WARNING);
    } else if (policy == Policy.VERIFY_IF_AVAILABLE && !hasKey) {
      warn.accept(NO_KEY_WARNING);
    }
    if (verify && !matches(data, docStart, env, key)) {
      throw new SerealException("Sereal packet checksum verification failed");
    }
    return env;
  }

  /**
   * Build a checksummed document from an unprotected one.
   *
   * @param doc the unprotected document (header, then the possibly compressed body)
   * @param docLength bytes of {@code doc} that belong to the document
   * @param headerLength bytes up to and including the header suffix
   * @param type checksum type
   * @param key 16-byte key
   * @return a new array holding the protected document
   */
  static byte[] protect(byte[] doc, int docLength, int headerLength, int type, byte[] key) {
    int tagLength = tagLength(type);
    if (tagLength == 0) {
      throw new IllegalArgumentException("Unsupported Sereal checksum type " + type);
    }
    int payloadLength = docLength - headerLength;
    int tagStart = headerLength + 1 + varintLength(tagLength);
    int tagEnd = tagStart + tagLength;
    int payloadStart = tagEnd + varintLength(payloadLength);
    byte[] out = new byte[payloadStart + payloadLength];

    System.arraycopy(doc, 0, out, 0, headerLength);
    out[4] |= SerealHeader.SRL_ENCODING_CHECKSUM_FLAG;
    out[headerLength] = (byte) type;
    encodeVarint(tagLength, out, headerLength + 1);
    encodeVarint(payloadLength, out, tagEnd);
    System.arraycopy(doc, headerLength, out, payloadStart, payloadLength);

    SipHash h = SipHash.forType(type, key);
    h.update(out, 0, tagStart);
    h.update(out, tagEnd, out.length - tagEnd);
    byte[] tag = h.digest();
    System.arraycopy(tag, 0, out, tagStart, tagLength);
    return out;
  }

  /**
   * Validate and copy a checksum key option.
   *
   * @param key the key, or {@code null} to clear it
   * @param option option name for the error message
   * @return a copy of the key, or {@code null}
   */
  static byte[] checkKey(byte[] key, String option) {
    if (key == null) {
      return null;
    }
    if (key.length != KEY_LENGTH) {
      throw new IllegalArgumentException("The '" + option
          + "' key must be exactly 16 bytes; use SerealChecksum.prepareKey() to normalize it");
    }
    return key.clone();
  }

  static byte[] copyKey(byte[] key) {
    return key == null ? null : key.clone();
  }

  /** The default warning handler: mirrors Perl's {@code warn}. */
  static void defaultWarning(String msg) {
    System.err.println("Sereal: " + msg);
  }

  // bounded unsigned varint reader; pos[0] is advanced past the varint
  private static long readVarint(byte[] data, int[] pos, int end) throws SerealException {
    long value = 0;
    int p = pos[0];
    for (int shift = 0; ; shift += 7) {
      if (p >= end) {
        throw new SerealException("Unexpected end of data at byte " + p);
      }
      if (shift > 63) {
        throw new SerealException("Varint too long at byte " + p);
      }
      byte b = data[p++];
      value |= (long) (b & 0x7f) << shift;
      if (b >= 0) {
        break;
      }
    }
    pos[0] = p;
    return value;
  }

  /**
   * @param n value, treated as unsigned
   * @return number of bytes of its varint encoding
   */
  static int varintLength(long n) {
    int length = 0;

    while (Long.compareUnsigned(n, 127) > 0) {
      n >>>= 7;
      length++;
    }

    return length + 1;
  }

  /**
   * Write {@code n} (treated as unsigned) as a varint.
   *
   * @return the position after the varint
   */
  static int encodeVarint(long n, byte[] buffer, int pos) {
    while (Long.compareUnsigned(n, 127) > 0) {
      buffer[pos++] = (byte) ((n & 127) | 128);
      n >>>= 7;
    }
    buffer[pos++] = (byte) n;

    return pos;
  }
}
