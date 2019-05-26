package com.booking.sereal;

import com.booking.sereal.EncoderOptions.CompressionType;
import com.github.luben.zstd.Zstd;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.regex.Pattern;
import java.util.zip.Deflater;
import org.xerial.snappy.Snappy;

/**
 * A low-level stream encoder for Sereal.
 * <p>
 * Using this class correctly requires understanding of the Sereal specification and how the Perl encoder
 * handles the produced Sereal.
 * <p>
 * Example:
 * <pre>
 * {@code
 *   encoder.startDocument();
 *
 *   encoder.startArray(2);
 *   encoder.appendString("Hello, Sereal");
 *   encoder.appendLong(4);
 *   encoder.endArray();
 *
 *   encoder.endDocument();
 *
 *   byte[] data = encoder.getData();
 * }
 * </pre>
 */
public class TokenEncoder {
  private static class Context {
    private final Context outer;
    private final int type;
    private final int expectedCount;
    private final int position;
    private int count;

    Context(Context outer, int type, int position, int expectedCount) {
      this.outer = outer;
      this.type = type;
      this.position = position;
      this.expectedCount = expectedCount;
    }
  }

  private static final int CONTEXT_ROOT = 0;
  private static final int CONTEXT_HASH = 1;
  private static final int CONTEXT_ARRAY = 2;
  private static final int CONTEXT_OBJECT = 3;
  private static final int CONTEXT_WEAKEN = 4;

  private static final EncoderOptions DEFAULT_OPTIONS = new EncoderOptions();
  private static final byte[] EMPTY_BYTE_ARRAY = new byte[0];
  private final int MAX_VARINT_LENGTH = 10;
  private final byte[] HEADER =
    new byte[] {
      (byte) (SerealHeader.MAGIC >> 24),
      (byte) (SerealHeader.MAGIC >> 16),
      (byte) (SerealHeader.MAGIC >> 8),
      (byte) (SerealHeader.MAGIC),
    };
  private final byte[] HEADER_V3 =
    new byte[] {
      (byte) (SerealHeader.MAGIC_V3 >> 24),
      (byte) (SerealHeader.MAGIC_V3 >> 16),
      (byte) (SerealHeader.MAGIC_V3 >> 8),
      (byte) (SerealHeader.MAGIC_V3),
    };

  private final byte protocolVersion, encoding;
  private final CompressionType compressionType;
  private final CharsetEncoder utf8Encoder = StandardCharsets.UTF_8.newEncoder();
  private final Deflater deflater;
  private final int compressionThreshold;
  private Context currentContext;
  private byte[] bytes = new byte[1024], compressedBytes = EMPTY_BYTE_ARRAY;
  private int size = 0, compressedSize = 0, trackOffset = 0;
  private int headerSize, headerOffset;
  private final int zstdCompressionLevel;
  private boolean hasHeader;

  /** Create an new {@code TokenEncoder} with default options. */
  public TokenEncoder() {
    this(DEFAULT_OPTIONS);
  }

  /** Create an new {@code TokenEncoder} with the specified options. */
  public TokenEncoder(EncoderOptions options) {
    protocolVersion = (byte) options.protocolVersion();
    compressionType = options.compressionType();
    zstdCompressionLevel = options.zstdCompressionLevel();
    compressionThreshold = (int) options.compressionThreshold();

    if (compressionType == CompressionType.SNAPPY) {
      encoding = protocolVersion == 1 ? SerealHeader.SRL_ENCODING_SNAPPY_LEGACY : SerealHeader.SRL_ENCODING_SNAPPY;
    } else {
      encoding = compressionType.encoding;
    }

    if (encoding == 3) {
      deflater = new Deflater(options.zlibCompressionLevel());
    } else {
      deflater = null;
    }

    start();
  }

  /** Sereal protocol version used by this encoder. */
  public int protocolVersion() {
    return protocolVersion;
  }

  /** {@code true} after the root element has been completely written. */
  public boolean isComplete() {
    return currentContext.type == CONTEXT_ROOT && currentContext.count == 1;
  }

  /**
   * Sereal document offset of last written token.
   * <p>
   * The returned value can be used for the target of {@link TokenEncoder#appendCopy(int)},
   * {@link TokenEncoder#appendRefPrevious(int)} or {@link TokenEncoder#appendAlias(int)}.
   */
  public int trackOffsetLastValue() {
    return trackOffset - headerOffset;
  }

  /**
   * Sereal document offset of the next token that will be written.
   * <p>
   * The returned value can be used for the target of {@link TokenEncoder#appendCopy(int)},
   * {@link TokenEncoder#appendRefPrevious(int)} or {@link TokenEncoder#appendAlias(int)}.
   */
  public int trackOffsetNextValue() {
    return size - headerOffset;
  }

  /** Reset internal state as it was right after construction. */
  public void reset() {
    size = compressedSize = headerSize = headerOffset = trackOffset = 0;
    hasHeader = false;
    start();
  }

  /**
   * Get a reference to the encoded document.
   * <p>
   * The contents of the buffer willl become invalid after calling any of the mutator methods.
   */
  public ByteArray getDataReference() {
    if (compressedSize != 0) {
      return new ByteArray(compressedBytes, compressedSize);
    } else {
      return new ByteArray(bytes, size);
    }
  }

  /** Get a copy of the encoded document. */
  public byte[] getData() {
    if (compressedSize != 0) {
      return Arrays.copyOf(compressedBytes, compressedSize);
    } else {
      return Arrays.copyOf(bytes, size);
    }
  }

  private static void prepareHeader(
    byte[] originBytes, byte[] compressedBytes, int headerSize, int sizeLength) {
    System.arraycopy(originBytes, 0, compressedBytes, 0, headerSize);
    if (sizeLength > 0) {
      // varint-encoded 0, filling all space
      for (int i = headerSize; i < headerSize + sizeLength - 1; i++) {
        compressedBytes[i] = (byte) 0x80;
      }
      compressedBytes[headerSize + sizeLength - 1] = 0;
    }
  }

  private static void finishHeader(
    byte[] compressedBytes, long compressedSize, int headerSize, int sizeLength) {
    int after = encodeVarint(compressedSize, compressedBytes, headerSize);
    if (after != headerSize + sizeLength) {
      compressedBytes[after - 1] |= (byte) 0x80;
    }
  }

  private void start() {
    if (protocolVersion >= 3) {
      appendBytesUnsafe(HEADER_V3);
    } else {
      appendBytesUnsafe(HEADER);
    }
    appendByteUnsafe((byte) ((encoding << 4) | protocolVersion));
  }

  /**
   * Set up the encoder to emit data to the Sereal header.
   * <p>
   * Sereal header is optional, and if present it must be emitted before the main document.
   */
  public void startHeader() throws SerealException {
    if (protocolVersion == 1) {
      throw new SerealException("Can't encode user header in Sereal protocol version 1");
    }

    currentContext = new Context(null, CONTEXT_ROOT, size, 1);
    // be optimistic about encoded header size
    size += 2; // one for the size, one for 8bit bitfield
    // because offsets start at 1
    headerOffset = size - 1;
  }

  /**
   * Complete encoding of the Sereal header.
   */
  public void endHeader() throws SerealException {
    if (currentContext.type != CONTEXT_ROOT) {
      throw new SerealException("Mismatched begin/end calls");
    }
    checkCount(currentContext);
    hasHeader = true;
    headerSize = size;

    int originalSize = currentContext.position;
    int suffixSize = (size - originalSize - 1);
    if (suffixSize < 128) {
      bytes[originalSize] = (byte) suffixSize;
      bytes[originalSize + 1] = 0x01;
    } else {
      // we were too optimistic
      int sizeLength = varintLength(suffixSize);

      // make space
      ensureAvailable(sizeLength - 1);
      System.arraycopy(
        bytes,
        originalSize + 2,
        bytes,
        originalSize + sizeLength + 1,
        suffixSize - 1);
      size += sizeLength - 1;
      headerSize = size;

      // now write size and 8bit bitfield
      encodeVarint(suffixSize, bytes, originalSize);
      bytes[originalSize + sizeLength] = 0x01;
    }
  }

  /**
   * Set up the encoder to emit data for the Sereal body.
   */
  public void startDocument() throws SerealException {
    if (!hasHeader) {
      appendByteUnsafe((byte) 0x00);
      headerSize = size;
    } else {
      if (headerSize != size) {
        throw new SerealException("Can't append data between header and body");
      }
    }

    currentContext = new Context(null, CONTEXT_ROOT, -1, 1);
    if (protocolVersion > 1) {
      // because offsets start at 1
      headerOffset = headerSize - 1;
    } else {
      headerOffset = 0;
    }
  }

  /**
   * Complete encoding of the Sereal body.
   */
  public void endDocument() throws SerealException {
    if (currentContext.type != CONTEXT_ROOT) {
      throw new SerealException("Mismatched begin/end calls");
    }
    checkCount(currentContext);
    if (!compressionType.equals(CompressionType.NONE) && size - headerSize > compressionThreshold) {
      if (compressionType.equals(CompressionType.SNAPPY)) {
        compressSnappy();
      } else if (compressionType.equals(CompressionType.ZLIB)) {
        compressZlib();
      } else if (compressionType.equals(CompressionType.ZSTD)) {
        compressZstd();
      }
    } else {
      // we did not do compression after all
      markNotCompressed();
    }
  }

  private void checkCount(Context context) throws SerealException {
    if (context.expectedCount != -1 && context.count != context.expectedCount) {
      throw new SerealException("Bad value count");
    }
  }

  /**
   * Append a Sereal {@code REFN} tag.
   */
  public void appendRefNext() throws SerealException {
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_REFN);
  }

  /**
   * Append a Sereal {@code WEAKEN} tag.
   * <p>
   * The next value appended needs to be some kind of reference.
   */
  public void appendWeaken() throws SerealException {
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_WEAKEN);
  }

  /**
   * Convenience method to check or force the next emitted value to be a weak reference.
   * <p>
   * After emitting the value, {@link TokenEncoder#endWeaken()} must be called.
   * <p>
   * If the emitted value is not a reference and {@code forceReference} is {@code false}, an exception is thrown.
   * <p>
   * If the emitted value is not a reference and {@code forceReference} is {@code true}, the value
   * is forced into a reference by using {@code REFN}.
   */
  public void startWeaken(boolean forceReference) throws SerealException {
    ensureAvailable(2);
    appendByteUnsafe(SerealHeader.SRL_HDR_WEAKEN);

    if (forceReference) {
      appendByteUnsafe(SerealHeader.SRL_HDR_PAD);
    }
    currentContext = new Context(currentContext, CONTEXT_WEAKEN, size - 1,1);
  }

  /**
   * Completes writing a weak reference started with {@link TokenEncoder#startWeaken(boolean)}.
   */
  public void endWeaken() throws SerealException {
    if (currentContext.type != CONTEXT_WEAKEN) {
      throw new SerealException("Mismatched begin/end calls");
    }
    checkCount(currentContext);
    if (bytes[currentContext.position] == SerealHeader.SRL_HDR_PAD) {
      if (!isRefTag(bytes[currentContext.position + 1])) {
        bytes[currentContext.position] = SerealHeader.SRL_HDR_REFN;
      }
    } else {
      if (!isRefTag(bytes[currentContext.position + 1])) {
        throw new SerealException("Internal error while encoding weak reference");
      }
    }
    currentContext = currentContext.outer;
    currentContext.count++;
  }

  /**
   * Append a Sereal {@code REFP} tag.
   *
   * @param offset Sereal document offset of the target of the reference.
   */
  public void appendRefPrevious(int offset) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_REFP);
    appendVarint(offset);
    setTrackBit(offset);
  }

  /**
   * Append a Sereal {@code ALIAS} tag.
   *
   * @param offset Sereal document offset of the target of the alias.
   */
  public void appendAlias(int offset) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_ALIAS);
    appendVarint(offset);
    setTrackBit(offset);
  }

  /**
   * Append a Sereal {@code COPY} tag.
   *
   * @param offset Sereal document offset of the target of the copy.
   */
  public void appendCopy(int offset) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_COPY);
    appendVarint(offset);
  }

  /**
   * Append an integer value.
   * <p>
   * Depending on the value, uses one one of {@code POS_*}, {@code NEG_*}, {@code VARINT} or {@code ZIGZAG} Sereal tags.
   *
   * @param l Value to be appended.
   */
  public void appendLong(long l) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    if (l < 0) {
      if (l > -17) {
        appendByte((byte) (SerealHeader.SRL_HDR_NEG_LOW | (l + 32)));
      } else {
        appendZigZag(l);
      }
    } else {
      if (l < 16) {
        appendByte((byte) (SerealHeader.SRL_HDR_POS_LOW | l));
      } else {
        appendByte(SerealHeader.SRL_HDR_VARINT);
        appendVarint(l);
      }
    }
  }

  /**
   * Append a Sereal {@code FLOAT} tag.
   */
  public void appendFloat(float f) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(5);
    appendByteUnsafe(SerealHeader.SRL_HDR_FLOAT);
    int floatBits = Float.floatToIntBits(f);
    for (int i = 0; i < 4; ++i) {
      appendByteUnsafe((byte) (floatBits & 0xff));
      floatBits >>= 8;
    }
  }

  /**
   * Append a Sereal {@code DOUBLE} tag.
   */
  public void appendDouble(double d) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(9);
    appendByteUnsafe(SerealHeader.SRL_HDR_DOUBLE);
    long doubleBits = Double.doubleToLongBits(d);
    for (int i = 0; i < 8; ++i) {
      appendByteUnsafe((byte) (doubleBits & 0xff));
      doubleBits >>= 8;
    }
  }

  /**
   * Append a Sereal {@code TRUE} or {@code FALSE} tag.
   */
  public void appendBoolean(boolean b) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(b ? SerealHeader.SRL_HDR_TRUE : SerealHeader.SRL_HDR_FALSE);
  }

  /**
   * Append a Sereal {@code UNDEF} tag.
   */
  public void appendUndef() throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_UNDEF);
  }

  /**
   * Append a Sereal {@code CANONICAL_UNDEF} tag.
   */
  public void appendCanonicalUndef() throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_CANONICAL_UNDEF);
  }

  /**
   * Append an binary/ISO 8859-1 value.
   * <p>
   * Depending on the value, uses one of {@code SHORT_BINARY_*} or {@code BINARY} Sereal tags.
   *
   * @param bytes Value to be appended.
   */
  public void appendBinary(byte[] bytes) throws SerealException {
    appendBinary(bytes, 0, bytes.length);
  }

  /**
   * Append an binary/ISO 8859-1 value.
   * <p>
   * Depending on the value, uses one of {@code SHORT_BINARY_*} or {@code BINARY} Sereal tags.
   *
   * @param bytes Value to be appended.
   * @param offset Index of the first byte to append.
   * @param length Number of bytes to append.
   */
  public void appendBinary(byte[] bytes, int offset, int length) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendBinaryInternal(bytes, offset, length);
  }

  private void appendBinaryInternal(byte[] bytes, int offset, int length) throws SerealException {
    if (length <= SerealHeader.SRL_MASK_SHORT_BINARY_LEN) {
      appendShortBinary(bytes, offset, length);
    } else {
      appendLongBinary(bytes, offset, length);
    }
  }

  /**
   * Append a Sereal {@code UTF8} tag.
   *
   * @param string the value to be appended
   */
  public void appendString(CharSequence string) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendCharBuffer(CharBuffer.wrap(string));
  }

  /**
   * Append a Sereal {@code UTF8} tag.
   *
   * @param string Value to be appended.
   */
  public void appendString(char[] string) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendCharBuffer(CharBuffer.wrap(string));
  }

  /**
   * Append a Sereal {@code UTF8} tag.
   *
   * @param string Value to be appended.
   * @param offset Index of the first character to append.
   * @param length Number of characters to append.
   */
  public void appendString(char[] string, int offset, int length) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendCharBuffer(CharBuffer.wrap(string, offset, length));
  }

  private void appendCharBuffer(CharBuffer string) throws SerealException {
    int maxLength = string.length() * 3;
    int varintLenght = varintLength(maxLength);
    ensureAvailable( maxLength + varintLenght + 1);
    appendByteUnsafe(SerealHeader.SRL_HDR_STR_UTF8);
    utf8Encoder.reset();
    int stringStart = size + varintLenght;
    ByteBuffer out = ByteBuffer.wrap(bytes, stringStart, bytes.length - stringStart);
    CoderResult result = utf8Encoder.encode(string, out, true);
    if (result.isError()) {
      throw new SerealException(result.toString());
    }
    int actualLength = out.position() - stringStart;
    for (int varintEnd = encodeVarint(actualLength, bytes, size); varintEnd < stringStart; ++varintEnd) {
      bytes[varintEnd - 1] |= (byte) 0x80;
      bytes[varintEnd] = (byte) 0x00;
    }
    size += varintLenght + actualLength;
  }

  /**
   * Append a Sereal {@code UTF8} tag.
   * <p>
   * The passed-in value is assumed to be valid UTF-8, no check is performed.
   *
   * @param utf8 Value to be appended.
   * @param offset Index of the first byte to append.
   * @param length Number of bytes to append.
   */
  public void appendUTF8(byte[] utf8, int offset, int length) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable( length + MAX_VARINT_LENGTH + 1);
    appendByteUnsafe(SerealHeader.SRL_HDR_STR_UTF8);
    appendVarint(length);
    appendBytesUnsafe(utf8, offset, length);
  }

  /**
   * Append a Sereal {@code REGEXP} tag.
   * <p>
   * Pattern flags other than {@link java.util.regex.Pattern#MULTILINE}, {@link java.util.regex.Pattern#DOTALL},
   * {@link java.util.regex.Pattern#CASE_INSENSITIVE} and {@link java.util.regex.Pattern#COMMENTS} are
   * silently ignored.
   */
  public void appendRegexp(Pattern pattern) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_REGEXP);
    appendCharBuffer(CharBuffer.wrap(pattern.pattern()));
    ensureAvailable(5);
    int nextFlag = size + 1;
    int flags = pattern.flags();

    if ((flags & Pattern.MULTILINE) != 0) {
      bytes[nextFlag++] = 'm';
    }
    if ((flags & Pattern.DOTALL) != 0) {
      bytes[nextFlag++] = 's';
    }
    if ((flags & Pattern.CASE_INSENSITIVE) != 0) {
      bytes[nextFlag++] = 'i';
    }
    if ((flags & Pattern.COMMENTS) != 0) {
      bytes[nextFlag++] = 'x';
    }

    bytes[size] = (byte) ((nextFlag - size - 1) | SerealHeader.SRL_HDR_SHORT_BINARY);
    size = nextFlag;
  }

  /**
   * Append a Sereal {@code REGEXP} tag.
   * <p>
   * The pattern is encoded using a Sereal {@code UTF8} tag.
   */
  public void appendRegexpString(String pattern, byte[] flags) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_REGEXP);
    appendCharBuffer(CharBuffer.wrap(pattern));
    appendBinaryInternal(flags, 0, flags.length);
  }

  /**
   * Append a Sereal {@code REGEXP} tag.
   * <p>
   * The pattern is encoded using a Sereal {@code BINARY} or {@code SHORT_BINARY_*} tag.
   */
  public void appendRegexpBinary(byte[] pattern, byte[] flags) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_REGEXP);
    appendBinaryInternal(pattern, 0, pattern.length);
    appendBinaryInternal(flags, 0, flags.length);
  }

  /**
   * Start writing an hash reference.
   * <p>
   * Whenever possible, use {@link TokenEncoder#startHash(int)}.
   */
  public void startHash() throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    appendByteUnsafe(SerealHeader.SRL_HDR_REFN);
    appendByteUnsafe(SerealHeader.SRL_HDR_HASH);
    currentContext = new Context(currentContext, CONTEXT_HASH, size,-1);
    appendByteUnsafe((byte) 0x0);
  }

  /**
   * Start writing an hash reference.
   * <p>
   * Depending on the key count, uses one of {@code HASHREF_*} or {@code REFN + HASH} Sereal tags.
   *
   * @param count Number of keys in the hash.
   */
  public void startHash(int count) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    if (count <= 15) {
      appendByteUnsafe((byte) (SerealHeader.SRL_HDR_HASHREF | count));
    } else {
      appendByteUnsafe(SerealHeader.SRL_HDR_REFN);
      appendByteUnsafe(SerealHeader.SRL_HDR_HASH);
      appendVarint(count);
    }
    currentContext = new Context(currentContext, CONTEXT_HASH, size, count);
  }

  /**
   * Start writing an hash value.
   * <p>
   * Whenever possible, use {@link TokenEncoder#startHashValue(int)}.
   */
  public void startHashValue() throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    appendByteUnsafe(SerealHeader.SRL_HDR_HASH);
    currentContext = new Context(currentContext, CONTEXT_HASH, size,-1);
    appendByteUnsafe((byte) 0x0);
  }

  /**
   * Start writing an hash value.
   * <p>
   * Uses a Sereal {@code HASH} tag without a preceding {@code REFN}.
   *
   * @param count Number of keys in the hash.
   */
  public void startHashValue(int count) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    appendByteUnsafe(SerealHeader.SRL_HDR_HASH);
    appendVarint(count);
    currentContext = new Context(currentContext, CONTEXT_HASH, size, count);
  }

  /**
   * Complete writing an hash value or hash reference.
   */
  public void endHash() throws SerealException {
    if (currentContext.type != CONTEXT_HASH) {
      throw new SerealException("Mismatched begin/end calls");
    }
    if ((currentContext.count & 0x1) != 0) {
      throw new SerealException("Odd value count in hash");
    }
    currentContext.count >>= 1;
    checkCount(currentContext);
    if (currentContext.expectedCount == -1) {
      if (currentContext.count > 127) {
        int lenght = varintLength(currentContext.count);
        ensureAvailable(lenght);
        System.arraycopy(bytes, currentContext.position + 1,
            bytes, currentContext.position + lenght,
            size - currentContext.position - 1);
        size += lenght - 1;
      }
      encodeVarint(currentContext.count, bytes, currentContext.position);
    }
    currentContext = currentContext.outer;
  }

  /**
   * Start writing an array reference.
   * <p>
   * Whenever possible, use {@link TokenEncoder#startArray(int)}.
   */
  public void startArray() throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    appendByteUnsafe(SerealHeader.SRL_HDR_REFN);
    appendByteUnsafe(SerealHeader.SRL_HDR_ARRAY);
    currentContext = new Context(currentContext, CONTEXT_ARRAY, size,-1);
    appendByteUnsafe((byte) 0x0);
  }

  /**
   * Start writing an array reference.
   * <p>
   * Depending on the value count, uses one of {@code ARRAYREF_*} or {@code REFN + ARRAY} Sereal tags.
   *
   * @param count Number of elements in the array.
   */
  public void startArray(int count) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    if (count <= 15) {
      appendByteUnsafe((byte) (SerealHeader.SRL_HDR_ARRAYREF | count));
    } else {
      appendByteUnsafe(SerealHeader.SRL_HDR_REFN);
      appendByteUnsafe(SerealHeader.SRL_HDR_ARRAY);
      appendVarint(count);
    }
    currentContext = new Context(currentContext, CONTEXT_ARRAY, size, count);
  }

  /**
   * Start writing an array value.
   * <p>
   * Whenever possible, use {@link TokenEncoder#startArrayValue(int)}.
   */
  public void startArrayValue() throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    appendByteUnsafe(SerealHeader.SRL_HDR_ARRAY);
    currentContext = new Context(currentContext, CONTEXT_ARRAY, size,-1);
    appendByteUnsafe((byte) 0x0);
  }

  /**
   * Start writing an array value.
   * <p>
   * Uses a Sereal {@code ARRAY} tag without a preceding {@code REFN}.
   */
  public void startArrayValue(int count) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    ensureAvailable(3);
    appendByteUnsafe(SerealHeader.SRL_HDR_ARRAY);
    currentContext = new Context(currentContext, CONTEXT_ARRAY, size, count);
    appendVarint(count);
  }

  /**
   * Complete writing an array value or array reference.
   */
  public void endArray() throws SerealException {
    if (currentContext.type != CONTEXT_ARRAY) {
      throw new SerealException("Mismatched begin/end calls");
    }
    checkCount(currentContext);
    if (currentContext.expectedCount == -1) {
      if (currentContext.count > 127) {
        int lenght = varintLength(currentContext.count);
        ensureAvailable(lenght);
        System.arraycopy(bytes, currentContext.position + 1,
            bytes, currentContext.position + lenght,
            size - currentContext.position - 1);
        size += lenght - 1;
      }
      encodeVarint(currentContext.count, bytes, currentContext.position);
    }
    currentContext = currentContext.outer;
  }

  /**
   * Start writing an object value.
   * <p>
   * Uses a Sereal {@code OBJECT} tag, followed by the class as a {@code SHORT_BINARY_*} or {@code BINARY} tag.
   */
  public void startObject(byte[] className) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_OBJECT);
    appendBinaryInternal(className, 0, className.length);
    currentContext = new Context(currentContext, CONTEXT_OBJECT, size,1);
  }

  /**
   * Start writing an object value.
   * <p>
   * Uses a Sereal {@code OBJECT} tag, followed by the class as an {@code UTF8} tag.
   */
  public void startObject(CharSequence className) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_OBJECT);
    appendCharBuffer(CharBuffer.wrap(className));
    currentContext = new Context(currentContext, CONTEXT_OBJECT, size,1);
  }

  /**
   * Start writing an object value.
   * <p>
   * Uses a Sereal {@code OBJECT} tag, followed by the class as an {@code UTF8} tag.
   */
  public void startObject(char[] className) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_OBJECT);
    appendCharBuffer(CharBuffer.wrap(className));
    currentContext = new Context(currentContext, CONTEXT_OBJECT, size,1);
  }

  /**
   * Start writing an object value.
   * <p>
   * Uses a Sereal {@code OBJECT} tag, followed by the class as a {@code COPY} tag.
   */
  public void startObject(int classnameOffset) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_OBJECT);
    appendByte(SerealHeader.SRL_HDR_COPY);
    appendVarint(classnameOffset);
    currentContext = new Context(currentContext, CONTEXT_OBJECT, size,1);
  }

  /**
   * Start writing an object value.
   * <p>
   * Uses a Sereal {@code OBJECTV} tag.
   */
  public void startObjectV(int classnameOffset) throws SerealException {
    currentContext.count++;
    trackOffset = size;
    appendByte(SerealHeader.SRL_HDR_OBJECTV);
    appendVarint(classnameOffset);
    currentContext = new Context(currentContext, CONTEXT_OBJECT, size,1);
  }

  /**
   * Complete writing an object value.
   */
  public void endObject() throws SerealException {
    if (currentContext.type != CONTEXT_OBJECT) {
      throw new SerealException("Mismatched begin/end calls");
    }
    checkCount(currentContext);
    currentContext = currentContext.outer;
  }

  private void markNotCompressed() {
    compressedSize = 0;
    bytes[4] &= (byte) 0xf;
  }

  private void compressSnappy() throws SerealException {
    int maxSize = Snappy.maxCompressedLength(size - headerSize);
    int sizeLength = encoding == SerealHeader.SRL_ENCODING_SNAPPY ? varintLength(maxSize) : 0;

    // I don't think there is any point in overallocating here
    if ((headerSize + sizeLength + maxSize) > compressedBytes.length) {
      compressedBytes = new byte[headerSize + sizeLength + maxSize];
    }

    prepareHeader(bytes, compressedBytes, headerSize, sizeLength);

    int compressed;
    try {
      compressed =
        Snappy.compress(
          bytes, headerSize, size - headerSize, compressedBytes, headerSize + sizeLength);
    } catch (IOException e) {
      throw new SerealException(e);
    }
    compressedSize = headerSize + sizeLength + compressed;
    if (compressedSize > size) {
      markNotCompressed();
      return;
    }

    if (encoding == 2) {
      finishHeader(compressedBytes, compressed, headerSize, sizeLength);
    }
  }

  // from miniz.c
  private int zlibMaxSize(int sourceLen) {
    return Math.max(
      128 + (sourceLen * 110) / 100, 128 + sourceLen + ((sourceLen / (31 * 1024)) + 1) * 5);
  }

  private void compressZlib() {
    deflater.reset();

    int sourceSize = size - headerSize;
    int maxSize = zlibMaxSize(sourceSize);
    int sizeLength = varintLength(sourceSize);
    int sizeLength2 = varintLength(maxSize);
    int pos = 0;

    // I don't think there is any point in overallocating here
    if ((headerSize + sizeLength + sizeLength2 + maxSize) > compressedBytes.length) {
      compressedBytes = new byte[headerSize + sizeLength + sizeLength2 + maxSize];
    }

    System.arraycopy(bytes, 0, compressedBytes, 0, headerSize);
    pos += headerSize;
    pos = encodeVarint(sourceSize, compressedBytes, pos);

    // varint-encoded 0, filling all space
    int encodedSizePos = pos;
    for (int max = pos + sizeLength2 - 1; pos < max; ) {
      compressedBytes[pos++] = (byte) 128;
    }
    compressedBytes[pos++] = 0;

    deflater.setInput(bytes, headerSize, sourceSize);
    deflater.finish();

    int compressed = deflater.deflate(compressedBytes, pos, compressedBytes.length - pos);
    compressedSize = headerSize + sizeLength + sizeLength2 + compressed;
    if (compressedSize > size) {
      markNotCompressed();
      return;
    }

    int after = encodeVarint(compressed, compressedBytes, encodedSizePos);
    if (after != headerSize + sizeLength + sizeLength2) {
      compressedBytes[after - 1] |= (byte) 0x80;
    }
  }

  private void compressZstd() throws SerealException {
    long maxSize = Zstd.compressBound(size - headerSize);
    int sizeLength = varintLength(maxSize);

    if (headerSize + sizeLength + maxSize > Integer.MAX_VALUE) {
      throw new SerealException(
        "Compressed data size exceeds integer MAX_VALUE: " + (headerSize + maxSize));
    }
    if (headerSize + sizeLength + maxSize > compressedBytes.length) {
      compressedBytes = new byte[(int) (headerSize + sizeLength + maxSize)];
    }

    prepareHeader(bytes, compressedBytes, headerSize, sizeLength);

    long compressed =
      Zstd.compressUsingDict(
        compressedBytes,
        headerSize + sizeLength,
        bytes,
        headerSize,
        size - headerSize,
        new byte[0],
        zstdCompressionLevel);
    if (Zstd.isError(compressed)) {
      throw new SerealException(Zstd.getErrorName(compressed));
    }
    compressedSize = headerSize + sizeLength + (int) compressed;
    if (compressedSize > size) {
      markNotCompressed();
      return;
    }

    finishHeader(compressedBytes, compressed, headerSize, sizeLength);
  }

  private void appendShortBinary(byte[] latin1, int offset, int length) throws SerealException {
    // length of string
    appendByte((byte) (length | SerealHeader.SRL_HDR_SHORT_BINARY));

    // save it
    appendBytes(latin1, offset, length);
  }

  private void appendLongBinary(byte[] latin1, int offset, int lenght) {
    // length of string
    appendByte(SerealHeader.SRL_HDR_BINARY);
    appendBytesWithLength(latin1, offset, lenght);
  }

  private void appendBytesWithLength(byte[] in, int offset, int length) {
    appendVarint(length);
    appendBytes(in, offset, length);
  }

  private void appendVarint(long n) {
    ensureAvailable(MAX_VARINT_LENGTH);
    size = encodeVarint(n, bytes, size);
  }

  private void appendZigZag(long n) {
    ensureAvailable(MAX_VARINT_LENGTH + 1);
    appendByteUnsafe(SerealHeader.SRL_HDR_ZIGZAG);
    size = encodeVarint((n << 1) ^ (n >> 63), bytes, size); // note the signed right shift
  }

  private void setTrackBit(int offset) {
    bytes[offset + headerOffset] |= (byte) 0x80;
  }

  private static boolean isRefTag(byte tag) {
    // the first branch is the common case, the other two branchs are unlikely
    if (tag == SerealHeader.SRL_HDR_REFN ||
        tag == SerealHeader.SRL_HDR_REFP) {
      return true;
    } else if ((tag & SerealHeader.SRL_HDR_ARRAYREF) == SerealHeader.SRL_HDR_ARRAYREF) {
      return true;
    } else if ((tag & SerealHeader.SRL_HDR_HASHREF) == SerealHeader.SRL_HDR_HASHREF) {
      return true;
    }

    return false;
  }

  private static int varintLength(long n) {
    int length = 0;

    while (n > 127) {
      n >>= 7;
      length++;
    }

    return length + 1;
  }

  private static int encodeVarint(long n, byte[] buffer, int pos) {
    while (n > 127) {
      buffer[pos++] = (byte) ((n & 127) | 128);
      n >>= 7;
    }
    buffer[pos++] = (byte) n;

    return pos;
  }

  private void ensureAvailable(int required) {
    long total = required + size;

    if (total > bytes.length) {
      bytes = Arrays.copyOf(bytes, (int) (total * 3 / 2));
    }
  }

  private void appendBytes(byte[] data, int offset, int lenght) {
    ensureAvailable(lenght);
    appendBytesUnsafe(data, offset, lenght);
  }

  private void appendBytesUnsafe(byte[] data) {
    System.arraycopy(data, 0, bytes, size, data.length);
    size += data.length;
  }

  private void appendBytesUnsafe(byte[] data, int offset, int length) {
    System.arraycopy(data, offset, bytes, size, length);
    size += length;
  }

  private void appendByte(byte data) {
    ensureAvailable(1);
    appendByteUnsafe(data);
  }

  private void appendByteUnsafe(byte data) {
    bytes[size] = data;
    size++;
  }

  // used for testing
  void poisonBuffer() {
    for (int i = size; i < bytes.length; ++i) {
      bytes[i] = (byte) (Math.random() * 256);
    }
  }
}
