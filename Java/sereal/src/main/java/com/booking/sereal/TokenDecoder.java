package com.booking.sereal;

import com.github.luben.zstd.Zstd;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;
import org.xerial.snappy.Snappy;

/**
 * A low-level stream decoder for Sereal.
 * <p>
 * It provides a token stream, with accessor to retrieve token data (e.g. the integer value associated with a {@code LONG}
 * token).
 * <p>
 * The decoder performs some basic sanity checks, but it does not perform higher-level validation, such as checking
 * that hash keys are strings, array values are scalars, or weaken tokens are followed by a reference.
 * <p>
 * Example:
 * <pre>
 * {@code
 *   decoder.setData(bytes);
 *   decoder.prepareDecodeBody();
 *
 *   while (decoder.nextToken() != SerealToken.END) {
 *     if (decoder.nextToken() == SerealToken.LONG) {
 *       System.outprintln("Long value " + decoder.longValue() + " at offset " + decoder.tokenOffset());
 *     }
 *   }
 * }
 * </pre>
 */
public class TokenDecoder {
  private static class Context {
    private final Context outer;
    private final int type;
    private int total, remaining;
    private int originalPosition;

    Context(Context outer, int type, int remaining) {
      this.outer = outer;
      this.type = type;
      this.remaining = this.total = remaining;
    }

    Context(Context outer, int type, int remaining, int originalPosition) {
      this.outer = outer;
      this.type = type;
      this.remaining = this.total = remaining;
      this.originalPosition = originalPosition;
    }
  }

  private static final DecoderOptions DEFAULT_OPTIONS = new DecoderOptions();

  private static final int CONTEXT_ROOT = 0;
  private static final int CONTEXT_HASH = 1;
  private static final int CONTEXT_ARRAY = 2;
  private static final int CONTEXT_OBJECT = 3;
  private static final int CONTEXT_SUBDECODE = 4;

  private byte[] data;
  private int position, end;
  private byte[] bodyData;
  private int bodyPosition, bodySize;
  private ByteArray originalData;
  private int protocolVersion = -1;
  private int encoding = -1;
  private int baseOffset = Integer.MAX_VALUE;
  private int userHeaderPosition = -1;
  private int userHeaderSize = -1;
  private Inflater inflater;
  private byte[] bigintBuffer;

  private Context currentContext;
  private SerealToken currentToken = SerealToken.NONE;
  private boolean isSecondTime;
  private int trackOffset, tokenOffset;
  private long longValue;
  private float floatValue;
  private double doubleValue;
  private int binarySliceStart, binarySliceEnd;
  private boolean binaryIsUtf8;
  private int backreferenceOffset;

  /** Create a new {@code TokenDecoder} with default options. */
  public TokenDecoder() {
    this(DEFAULT_OPTIONS);
  }

  /** Create an new {@code TokenDecoder} with the specified options. */
  public TokenDecoder(DecoderOptions options) {
  }

  private void checkHeader() throws SerealException {
    if ((end - position) < 4) {
      throw new SerealException("Invalid Sereal header: too few bytes");
    }

    int magic =
        ((data[position] & 0xff) << 24)
            + ((data[position + 1] & 0xff) << 16)
            + ((data[position + 2] & 0xff) << 8)
            + (data[position + 3] & 0xff);
    position += 4;
    if (magic != SerealHeader.MAGIC && magic != SerealHeader.MAGIC_V3) {
      throw new SerealException(
          String.format("Invalid Seareal header (%08x): doesn't match magic", magic));
    }
  }

  private void checkHeaderSuffix() {
    long suffixSize = readVarint();
    long basePosition = position;

    userHeaderSize = 0;
    userHeaderPosition = position;
    if (suffixSize > 0) {
      byte bitfield = data[position++];

      if ((bitfield & 0x01) == 0x01) {
        userHeaderPosition = position;
        userHeaderSize = (int) suffixSize - 1;
      }
    }

    // skip everything in the optional suffix part
    position = (int) (basePosition + suffixSize);
  }

  private void checkNoEOD() throws SerealException {
    if ((end - position) <= 0) {
      throw new SerealException("Unexpected end of data at byte " + position);
    }
  }

  private void checkProtoAndFlags() throws SerealException {
    if ((end - position) < 1) {
      throw new SerealException("Invalid Sereal header: no protocol/version byte");
    }

    int protoAndFlags = data[position++];
    protocolVersion = protoAndFlags & 15; // 4 bits for version

    if (protocolVersion < 1 || protocolVersion > 4) {
      throw new SerealException(
          String.format("Invalid Sereal header: unsupported protocol version %d", protocolVersion));
    }

    encoding = (protoAndFlags & ~15) >> 4;
    if (encoding == 4 && protocolVersion < 4) {
      throw new SerealException(
          "Unsupported encoding zstd for protocol version " + protocolVersion);
    } else if (encoding < 0 || encoding > 4) {
      throw new SerealException("Unsupported encoding: unknown");
    }
  }

  /**
   * {@code true} if the document has a Sereal header.
   */
  public boolean hasHeader() throws SerealException {
    parseHeader();

    return userHeaderSize > 0;
  }

  /**
   * Size of the Sereal header, if present, 0 otherwise.
   */
  public int headerSize() throws SerealException {
    parseHeader();

    return userHeaderSize > 0 ? userHeaderSize : 0;
  }

  /**
   * Set up the decoder to iterate over the Sereal header.
   * <p>
   * This function will fail if {@link TokenDecoder#hasHeader()} returned {@code false}.
   */
  public void prepareDecodeHeader() throws SerealException {
    parseHeader();

    if (protocolVersion == 1) {
      throw new SerealException("Sereal user header not supported in protocol version 1");
    }
    if (userHeaderSize <= 0) {
      throw new SerealException("Sereal user header not present");
    }

    data = originalData.array;
    position = userHeaderPosition;
    end = userHeaderPosition + userHeaderSize;

    // because offsets start at 1
    baseOffset = position - 1;

    currentContext = new Context(null, CONTEXT_ROOT, 1);
  }

  private void parseHeader() throws SerealException {
    if (userHeaderSize >= 0) {
      return;
    }

    checkHeader();
    checkProtoAndFlags();
    checkHeaderSuffix();

    if (encoding == 0) {
      bodyData = data;
      bodyPosition = position;
      bodySize = end;
    }
  }

  /**
   * Set up the decoder to iterate over the Sereal document body.
   */
  public void prepareDecodeBody() throws SerealException {
    if (data == null) {
      throw new SerealException("No data set");
    }

    parseHeader();

    if (bodyData == null) {
      if (encoding != 0) {
        position = userHeaderPosition + userHeaderSize;
        end = originalData.length;
        if (encoding == SerealHeader.SRL_ENCODING_SNAPPY_LEGACY || encoding == SerealHeader.SRL_ENCODING_SNAPPY) {
          uncompressSnappy();
        } else if (encoding == SerealHeader.SRL_ENCODING_ZLIB) {
          uncompressZlib();
        } else if (encoding == SerealHeader.SRL_ENCODING_ZSTD) {
          uncompressZstd();
        }
      } else {
        throw new SerealException("Corrupted internal state");
      }
    }

    data = bodyData;
    position = bodyPosition;
    end = bodySize;

    if (protocolVersion == 1) {
      baseOffset = 0;
    } else {
      // because offsets start at 1
      baseOffset = position - 1;
    }

    currentContext = new Context(null, CONTEXT_ROOT, 1);
  }

  private void uncompressSnappy() throws SerealException {
    int len = originalData.length - (position - originalData.start);
    int pos = encoding == SerealHeader.SRL_ENCODING_SNAPPY_LEGACY ? position : originalData.start;

    if (encoding == SerealHeader.SRL_ENCODING_SNAPPY) {
      len = (int) readVarint();
    }
    byte[] uncompressed;
    try {
      if (!Snappy.isValidCompressedBuffer(
          originalData.array, position, len))
        throw new SerealException("Invalid snappy data");
      uncompressed =
          new byte
              [pos
                  + Snappy.uncompressedLength(
                      originalData.array, position, len)];
      Snappy.uncompress(
          originalData.array, position, len, uncompressed, pos);
    } catch (IOException e) {
      throw new SerealException(e);
    }
    this.bodyData = uncompressed;
    this.bodyPosition = pos;
    this.bodySize = uncompressed.length;
  }

  private void uncompressZlib() throws SerealException {
    if (inflater == null) {
      inflater = new Inflater();
    }
    inflater.reset();

    long uncompressedLength = readVarint();
    long compressedLength = readVarint();
    inflater.setInput(originalData.array, position, (int) compressedLength);
    byte[] uncompressed = new byte[(int) uncompressedLength];
    try {
      int inflatedSize = inflater.inflate(uncompressed);
      if (inflatedSize != uncompressedLength || !inflater.finished()) {
        throw new SerealException("Error in zlib-compressed data");
      }
    } catch (DataFormatException e) {
      throw new SerealException(e);
    }
    this.bodyData = uncompressed;
    this.bodyPosition = 0;
    this.bodySize = uncompressed.length;
  }

  private void uncompressZstd() throws SerealException {
    int len = (int) readVarint();

    byte[] compressedData = Arrays.copyOfRange(originalData.array, position, position + len);
    long decompressedSize = Zstd.decompressedSize(compressedData);
    if (decompressedSize > Integer.MAX_VALUE) {
      throw new SerealException("Decompressed size exceeds integer MAX_VALUE: " + decompressedSize);
    }

    byte[] uncompressed = new byte[(int) decompressedSize];
    long status = Zstd.decompress(uncompressed, compressedData);
    if (Zstd.isError(status)) {
      throw new SerealException(Zstd.getErrorName(status));
    }
    this.bodyData = uncompressed;
    this.bodyPosition = 0;
    this.bodySize = uncompressed.length;
  }

  private void readBinary() {
    int length = (int) readVarint();
    binaryIsUtf8 = false;
    binarySliceStart = position;
    binarySliceEnd = position + length;
    position += length;
  }

  // top bit set (0x80) means next byte is 7 bits more more varint
  private long readVarint() {
    long uv = 0;
    int lshift = 0;

    byte b = data[position++];
    while ((position < end) && (b < 0)) {
      uv |= ((long) b & 127) << lshift; // add 7 bits
      lshift += 7;
      b = data[position++];
    }
    uv |= (long) b << lshift; // add final (or first if there is only 1)

    return uv;
  }

  /**
   * Iterate over the Sereal document returning the next token.
   * <p>
   * Depending on the token returned, accessors can be used to retrieve additional
   * information about the token.
   *
   * @return The next token
   */
  public SerealToken nextToken() throws SerealException {
    if (currentContext.remaining == 0) {
      Context popped = currentContext;
      currentContext = popped.outer;
      switch (popped.type) {
        case CONTEXT_ROOT:
          return (currentToken = SerealToken.END);
        case CONTEXT_ARRAY:
          return (currentToken = SerealToken.ARRAY_END);
        case CONTEXT_HASH:
          return (currentToken = SerealToken.HASH_END);
        case CONTEXT_OBJECT:
          return (currentToken = SerealToken.OBJECT_END);
        case CONTEXT_SUBDECODE:
          position = popped.originalPosition;
          return (currentToken = SerealToken.SUBDECODE_END);
      }
    }

    checkNoEOD();

    byte tag = data[position++];

    tokenOffset = position - 1 - baseOffset;
    trackOffset = 0;
    if ((tag & SerealHeader.SRL_HDR_TRACK_FLAG) != 0) {
      tag = (byte) (tag & ~SerealHeader.SRL_HDR_TRACK_FLAG);
      trackOffset = tokenOffset;
    }

    currentContext.remaining--;

    if (tag <= SerealHeader.SRL_HDR_POS_HIGH) {
      longValue = (long) tag;
      return (currentToken = SerealToken.LONG);
    } else if (tag <= SerealHeader.SRL_HDR_NEG_HIGH) {
      longValue = (long) (tag - 32);
      return (currentToken = SerealToken.LONG);
    } else if ((tag & SerealHeader.SRL_HDR_SHORT_BINARY_LOW) == SerealHeader.SRL_HDR_SHORT_BINARY_LOW) {
      readShortBinary(tag);
      return (currentToken = SerealToken.BINARY);
    } else if ((tag & SerealHeader.SRL_HDR_HASHREF) == SerealHeader.SRL_HDR_HASHREF) {
      if (isSecondTime) {
        isSecondTime = false;
        trackOffset = 0;
        int numKeys = tag & 0xf;
        currentContext = new Context(currentContext, CONTEXT_HASH, numKeys * 2);
        return (currentToken = SerealToken.HASH_START);
      } else {
        isSecondTime = true;
        --position;
        currentContext.remaining++;
        return (currentToken = SerealToken.REFN);
      }
    } else if ((tag & SerealHeader.SRL_HDR_ARRAYREF) == SerealHeader.SRL_HDR_ARRAYREF) {
      if (isSecondTime) {
        isSecondTime = false;
        trackOffset = 0;
        int length = tag & 0xf;
        currentContext = new Context(currentContext, CONTEXT_ARRAY, length);
        return (currentToken = SerealToken.ARRAY_START);
      } else {
        isSecondTime = true;
        --position;
        currentContext.remaining++;
        return (currentToken = SerealToken.REFN);
      }
    } else {
      switch (tag) {
        case SerealHeader.SRL_HDR_VARINT:
          longValue = readVarint();
          return (currentToken = (longValue >= 0 ? SerealToken.LONG : SerealToken.UNSIGNED_LONG));
        case SerealHeader.SRL_HDR_ZIGZAG:
          longValue = readZigzag();
          return (currentToken = SerealToken.LONG);
        case SerealHeader.SRL_HDR_FLOAT:
          int floatBits =
              ((data[position + 3] & 0xff) << 24)
                  + ((data[position + 2] & 0xff) << 16)
                  + ((data[position + 1] & 0xff) << 8)
                  + (data[position] & 0xff);
          position += 4;
          floatValue = Float.intBitsToFloat(floatBits);
          return (currentToken = SerealToken.FLOAT);
        case SerealHeader.SRL_HDR_DOUBLE:
          long doubleBits =
              ((long) (data[position + 7] & 0xff) << 56)
                  + ((long) (data[position + 6] & 0xff) << 48)
                  + ((long) (data[position + 5] & 0xff) << 40)
                  + ((long) (data[position + 4] & 0xff) << 32)
                  + ((long) (data[position + 3] & 0xff) << 24)
                  + ((long) (data[position + 2] & 0xff) << 16)
                  + ((long) (data[position + 1] & 0xff) << 8)
                  + ((long) (data[position] & 0xff));
          position += 8;
          doubleValue = Double.longBitsToDouble(doubleBits);
          return (currentToken = SerealToken.DOUBLE);
        case SerealHeader.SRL_HDR_TRUE:
          return (currentToken = SerealToken.TRUE);
        case SerealHeader.SRL_HDR_FALSE:
          return (currentToken = SerealToken.FALSE);
        case SerealHeader.SRL_HDR_UNDEF:
          return (currentToken = SerealToken.UNDEF);
        case SerealHeader.SRL_HDR_CANONICAL_UNDEF:
          return (currentToken = SerealToken.CANONICAL_UNDEF);
        case SerealHeader.SRL_HDR_BINARY:
          readBinary();
          return (currentToken = SerealToken.BINARY);
        case SerealHeader.SRL_HDR_STR_UTF8:
          readUTF8();
          return (currentToken = SerealToken.UTF8);
        case SerealHeader.SRL_HDR_REFN:
          currentContext.remaining++;
          return (currentToken = SerealToken.REFN);
        case SerealHeader.SRL_HDR_REFP:
          backreferenceOffset = (int) readVarint();
          return (currentToken = SerealToken.REFP);
        case SerealHeader.SRL_HDR_OBJECT:
          currentContext = new Context(currentContext, CONTEXT_OBJECT, 1);
          readString();
          return (currentToken = SerealToken.OBJECT_START);
        case SerealHeader.SRL_HDR_OBJECTV:
          currentContext = new Context(currentContext, CONTEXT_OBJECT, 1);
          readStringCopy();
          return (currentToken = SerealToken.OBJECT_START);
        case SerealHeader.SRL_HDR_COPY:
          backreferenceOffset = (int) readVarint();
          return (currentToken = SerealToken.COPY);
        case SerealHeader.SRL_HDR_ALIAS:
          backreferenceOffset = (int) readVarint();
          return (currentToken = SerealToken.ALIAS);
        case SerealHeader.SRL_HDR_WEAKEN:
          currentContext.remaining++;
          return (currentToken = SerealToken.WEAKEN);
        case SerealHeader.SRL_HDR_HASH:
          int numKeys = (int) readVarint();
          currentContext = new Context(currentContext, CONTEXT_HASH, numKeys * 2);
          return (currentToken = SerealToken.HASH_START);
        case SerealHeader.SRL_HDR_ARRAY:
          int length = (int) readVarint();
          currentContext = new Context(currentContext, CONTEXT_ARRAY, length);
          return (currentToken = SerealToken.ARRAY_START);
        case SerealHeader.SRL_HDR_REGEXP:
          readRegexp();
          return (currentToken = SerealToken.REGEXP);
        case SerealHeader.SRL_HDR_PAD:
          currentContext.remaining++;
          return nextToken();
        default:
          throw new SerealException("Tag not supported: " + tag);
      }
    }
  }

  /**
   * Starts a nested decoding context at the specified position.
   * <p>
   * Position must point at the start of a Sereal tag.
   * <p>
   * After the tag has been completely parsed (including any nested values), {@link #nextToken}
   * returns {@link SerealToken#SUBDECODE_END}, and the decoder position is restored to the
   * one before this call.
   *
   * @param offset Sereal document offset.
   */
  public void startSubDecode(int offset) throws SerealException {
    currentContext = new Context(currentContext, CONTEXT_SUBDECODE, 1, position);
    position = offset + baseOffset;
  }

  /** After a call to {@link #nextToken}, return the current token. */
  public SerealToken currentToken() {
    return currentToken;
  }

  /**
   * The element count of the current parsing context.
   * <p>
   * Between {@link SerealToken#ARRAY_START} and {@link SerealToken#ARRAY_END} returns the number of elements in the array.
   * <p>
   * Between {@link SerealToken#HASH_START} and {@link SerealToken#HASH_END} returns the number of
   * keys + values in the hash (always a multiple of 2).
   * <p>
   * Between {@link SerealToken#OBJECT_START} and {@link SerealToken#OBJECT_END} returns 1.
   * <p>
   * At the top-level of a sub-parse, returns 1.
   */
  public int elementCount() {
    return currentContext.total;
  }

  /**
   * Whether the offset of the current tag is the target of a reference.
   * <p>
   * When it returns a non-zero value, it is the offset of a Sereal tag that is going to be references
   * by a later {@link SerealToken#ALIAS} or {@link SerealToken#REFP} token.
   */
  public int trackOffset() {
    return trackOffset;
  }

  /**
   * The offset of the last token returned by {@link TokenDecoder#nextToken()}.
   * <p>
   * Defined for all tokens except for {@link SerealToken#ARRAY_END}, {@link SerealToken#HASH_END}
   * and {@link SerealToken#OBJECT_END}.
   *
   * @return a token offset that can be used for {@link #startSubDecode(int)}
   */
  public int tokenOffset() {
    return tokenOffset;
  }

  /**
   * Decoder position in the Sereal document.
   * <p>
   * Defined for all tokens.
   */
  public int currentOffset() {
    return position - 1 - baseOffset;
  }

  /**
   * Current {@code long} value.
   * <p>
   * Defined for {@link SerealToken#LONG} and {@link SerealToken#UNSIGNED_LONG}.
   */
  public long longValue() {
    return longValue;
  }

  /**
   * Current {@code long} value as a {@link java.math.BigInteger}.
   * <p>
   * Defined for {@link SerealToken#LONG} and {@link SerealToken#UNSIGNED_LONG}.
   */
  public BigInteger bigintValue() {
    if (currentToken == SerealToken.UNSIGNED_LONG) {
      if (bigintBuffer == null) {
        bigintBuffer = new byte[8];
      }
      long temp = longValue;
      for (int i = 7; i >= 0; --i) {
        bigintBuffer[i] = (byte) (temp & 0xff);
        temp >>= 8;
      }
      return new BigInteger(1, bigintBuffer);
    } else {
      return BigInteger.valueOf(longValue);
    }
  }

  /**
   * Current {@code float} value.
   * <p>
   * Defined for {@link SerealToken#FLOAT}.
   */
  public float floatValue() {
    return floatValue;
  }

  /**
   * Current {@code double} value.
   * <p>
   * Defined for {@link SerealToken#DOUBLE}.
   */
  public double doubleValue() {
    return doubleValue;
  }

  /**
   * All binary/string values are slices of this array.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START} and {@link SerealToken#REGEXP}.
   */
  public byte[] decoderBuffer() {
    return data;
  }

  /**
   * Start offset of the current binary/string in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START} and {@link SerealToken#REGEXP}.
   */
  public int binarySliceStart() {
    return binarySliceStart;
  }

  /**
   * End offset of the current binary/string in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START} and {@link SerealToken#REGEXP}.
   */
  public int binarySliceEnd() {
    return binarySliceEnd;
  }

  /**
   * Length of the current binary/string.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START} and {@link SerealToken#REGEXP}.
   */
  public int binarySliceLength() {
    return binarySliceEnd - binarySliceStart;
  }

  /**
   * Start offset of the current regexp flags in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#REGEXP}.
   */
  public int regexpFlagsSliceStart() {
    return (int) longValue;
  }

  /**
   * End offset of the current regexp flags in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#REGEXP}.
   */
  public int regexpFlagsSliceEnd() {
    return backreferenceOffset;
  }

  /**
   * Length of the regexp flags string.
   * <p>
   * Defined for {@link SerealToken#REGEXP}.
   */
  public int regexpFlagsSliceLength() {
    return regexpFlagsSliceEnd() - regexpFlagsSliceStart();
  }

  /**
   * {@code true} if the byte slice between {@link TokenDecoder#binarySliceStart()}/{@link TokenDecoder#binarySliceEnd()} is UTF-8 encoded.
   * <p>
   * Always {@code true} for {@link SerealToken#UTF8}, always {@code false} for {@link SerealToken#BINARY}.
   */
  public boolean binaryIsUtf8() {
    return binaryIsUtf8;
  }

  /**
   * Offset into the Sereal document.
   * <p>
   * Defined for {@link SerealToken#REFP}, {@link SerealToken#ALIAS} and {@link SerealToken#COPY} token.
   */
  public int backreferenceOffset() {
    return backreferenceOffset;
  }

  /** {@code true} if the current token is a hash key, {@code false} otherwise. */
  public boolean isHashKey() {
    return currentContext.type == CONTEXT_HASH && (currentContext.remaining & 1) == 1;
  }

  private void readShortBinary(byte tag) {
    int length = tag & SerealHeader.SRL_MASK_SHORT_BINARY_LEN;
    binaryIsUtf8 = false;
    binarySliceStart = position;
    binarySliceEnd = position + length;
    position += length;
  }

  private void readStringCopy() throws SerealException {
    int originalPosition = (int) readVarint();
    int currentPosition = position; // remember where we parked

    position = originalPosition + baseOffset;
    readString();
    position = currentPosition; // go back to where we were
  }

  private void readUTF8() {
    int length = (int) readVarint();
    binaryIsUtf8 = true;
    binarySliceStart = position;
    binarySliceEnd = position + length;
    position += length;
  }

  private long readZigzag() {
    long n = readVarint();

    return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
  }

  private void readRegexp() throws SerealException {
    readString();

    boolean savedBinaryIfUtf8 = binaryIsUtf8;
    int savedBinarySliceStart = binarySliceStart, savedBinarySliceEnd = binarySliceEnd;

    readString();

    longValue = binarySliceStart;
    backreferenceOffset = binarySliceEnd;

    binaryIsUtf8 = savedBinaryIfUtf8;
    binarySliceStart = savedBinarySliceStart;
    binarySliceEnd = savedBinarySliceEnd;
  }

  private void readString() throws SerealException {
    checkNoEOD();

    byte tag = data[position++];

    if ((tag & SerealHeader.SRL_HDR_SHORT_BINARY_LOW) == SerealHeader.SRL_HDR_SHORT_BINARY_LOW) {
      readShortBinary(tag);
    } else if (tag == SerealHeader.SRL_HDR_BINARY) {
      readBinary();
    } else if (tag == SerealHeader.SRL_HDR_STR_UTF8) {
      readUTF8();
    } else if (tag == SerealHeader.SRL_HDR_COPY) {
      readStringCopy();
    } else {
      throw new SerealException("Tag " + tag + " is not a string tag");
    }
  }

  /**
   * Set the Sereal data to be decoded.
   * <p>
   * After calling this method, blob is owned by the decoder until the next call to {@code setData} or {@link #reset()}.
   *
   * @param blob Sereal data.
   */
  public void setData(ByteArray blob) {
    reset();
    originalData = blob;
    data = originalData.array;
    end = originalData.start + originalData.length;
    position = originalData.start;
  }

  /**
   * Set the Sereal data to be decoded.
   * <p>
   * After calling this method, blob is owned by the decoder until the next call to {@code setData} or {@link #reset()}.
   *
   * @param blob Sereal data.
   */
  public void setData(byte[] blob) {
    reset();
    originalData = new ByteArray(blob);
    data = blob;
    end = blob.length;
    position = 0;
  }

  /** Discard all internal state. */
  public void reset() {
    currentToken = SerealToken.NONE;
    currentContext = null;
    originalData = null;
    data = bodyData = null;
    protocolVersion = encoding = -1;
    baseOffset = Integer.MAX_VALUE;
    userHeaderPosition = userHeaderSize = -1;
    isSecondTime = false;
  }

  /**
   * Decode a string of bytes as Perl regexp modifiers, and return the corresponding {@link java.util.regex.Pattern} flags.
   * <p>
   * It only handles {@code m, s, i, x} flags.
   *
   * @param buffer Buffer containing the modifier characters.
   * @param start Start of the range containing the modifier characters.
   * @param end Start of the range containing the modifier characters.
   * @return The decoded modifiers as {@link java.util.regex.Pattern} flags
   * @throws SerealException on an unrecognized modifier
   */
  public static int decodeRegexpFlags(byte[] buffer, int start, int end) throws SerealException {
    int flags = 0;

    for (int i = start; i < end; ++i) {
      byte value = buffer[i];
      switch (value) {
        case 'm':
          flags = flags | Pattern.MULTILINE;
          break;
        case 's':
          flags = flags | Pattern.DOTALL;
          break;
        case 'i':
          flags = flags | Pattern.CASE_INSENSITIVE;
          break;
        case 'x':
          flags = flags | Pattern.COMMENTS;
          break;
        case 'p':
          // ignored
          break;
        default:
          throw new SerealException(String.format("Unknown regexp modifier: '%c'", value));
      }
    }

    return flags;
  }

  // used for testing
  SerealToken poisonNextToken() throws SerealException {
    poisonFields();
    return nextToken();
  }

  private void poisonFields() {
    trackOffset = (int) (Math.random() * Integer.MAX_VALUE);
    tokenOffset = (int) (Math.random() * Integer.MAX_VALUE);
    longValue = (long) (Math.random() * Integer.MAX_VALUE);
    floatValue = (float) (Math.random() * Float.MAX_VALUE);
    doubleValue = Math.random() * Double.MAX_VALUE;
    binarySliceStart = (int) (Math.random() * Integer.MAX_VALUE);
    binarySliceEnd = (int) (Math.random() * Integer.MAX_VALUE);;
    binaryIsUtf8 = Math.random() > 0.5;
    backreferenceOffset = (int) (Math.random() * Integer.MAX_VALUE);;
  }
}
