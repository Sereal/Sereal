package com.booking.sereal;

import com.github.luben.zstd.Zstd;
import com.github.luben.zstd.ZstdException;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.function.Consumer;
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
    // a sub-decode started for a COPY token
    private boolean isCopy;

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

  private static class AllowedEncoding {
    private final EncoderOptions.CompressionType compressionType;
    private final int encoding, minVersion, maxVersion;

    AllowedEncoding(EncoderOptions.CompressionType compressionType, int encoding, int minVersion, int maxVersion) {
      this.compressionType = compressionType;
      this.encoding = encoding;
      this.minVersion = minVersion;
      this.maxVersion = maxVersion;
    }
  }

  private static final DecoderOptions DEFAULT_OPTIONS = new DecoderOptions();
  private static final AllowedEncoding[] ALLOWED_ENCODINGS = new AllowedEncoding[] {
    new AllowedEncoding(EncoderOptions.CompressionType.NONE, 0, 1, Integer.MAX_VALUE),
    new AllowedEncoding(EncoderOptions.CompressionType.SNAPPY, 1, 1, 1),
    new AllowedEncoding(EncoderOptions.CompressionType.SNAPPY, 2, 2, Integer.MAX_VALUE),
    new AllowedEncoding(EncoderOptions.CompressionType.ZLIB, 3, 3, Integer.MAX_VALUE),
    new AllowedEncoding(EncoderOptions.CompressionType.ZSTD, 4, 4, Integer.MAX_VALUE),
  };

  private static final int CONTEXT_ROOT = 0;
  private static final int CONTEXT_HASH = 1;
  private static final int CONTEXT_ARRAY = 2;
  private static final int CONTEXT_OBJECT = 3;
  private static final int CONTEXT_SUBDECODE = 4;

  private final byte[][] checksumKeys;
  private final SerealChecksum.Policy checksumPolicy;
  private final Consumer<String> checksumWarningHandler;
  private final String undefDeadlyPolicy;
  private final Consumer<String> undefDeadlyWarningHandler;
  // only applied to v-string spellings
  private final int maxStringLength;
  private final int maxBufferSize;

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
  // checksum envelope (protocol version 6): the body lives in [payloadStart, payloadEnd)
  private boolean hasChecksum;
  private int payloadStart = -1;
  private int payloadEnd = -1;
  // true while iterating over an uncompressed checksummed body, which must end at payloadEnd
  private boolean checkPayloadEnd;
  private Inflater inflater;
  private byte[] bigintBuffer;

  private Context currentContext;
  // number of enclosing sub-decodes following a COPY
  private int copyDepth;
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

  /**
   * Create an new {@code TokenDecoder} with the specified options.
   *
   * @param options {@link DecoderOptions} to use
   */
  public TokenDecoder(DecoderOptions options) {
    checksumKeys = options.checksumKeys();
    checksumPolicy = options.checksumPolicy();
    checksumWarningHandler = options.checksumWarningHandler();
    undefDeadlyPolicy = options.undefDeadlyPolicy();
    undefDeadlyWarningHandler = options.undefDeadlyWarningHandler();
    maxStringLength = options.maxStringLength();
    maxBufferSize = options.maxBufferSize();
    Decoder.checkChecksumOptions(checksumKeys, checksumPolicy);
  }

  private void checkHeaderSuffix() throws SerealException {
    long suffixLength = readVarint();
    if (suffixLength < 0 || suffixLength > end - position) {
      throw new SerealException("Sereal header suffix exceeds the available input");
    }
    int suffixSize = (int) suffixLength;

    userHeaderSize = 0;
    userHeaderPosition = position + suffixSize;
    if (protocolVersion >= 2 && suffixSize > 0) {
      byte bitfield = data[position++];

      if ((bitfield & 0x01) == 0x01) {
        userHeaderPosition = position;
        userHeaderSize = suffixSize - 1;
      }
    }
  }

  private void checkNoEOD() throws SerealException {
    if ((end - position) <= 0) {
      throw new SerealException("Unexpected end of data at byte " + position);
    }
  }

  private void checkMagicProtoAndFlags() throws SerealException {
    int magic =
        ((data[position] & 0xff) << 24)
            + ((data[position + 1] & 0xff) << 16)
            + ((data[position + 2] & 0xff) << 8)
            + (data[position + 3] & 0xff);
    int versionAndType = data[position + 4] & 0xff;
    protocolVersion = versionAndType & SerealHeader.SRL_PROTOCOL_VERSION_MASK; // 4 bits for version
    hasChecksum = (versionAndType & SerealHeader.SRL_ENCODING_CHECKSUM_FLAG & 0xff) != 0;
    if (magic != SerealHeader.MAGIC && magic != SerealHeader.MAGIC_V3) {
      throw new SerealException(
        String.format("Invalid Sereal header (%08x): doesn't match magic", magic));
    } else if (protocolVersion < 1 || protocolVersion > 6) {
      throw new SerealException(
          String.format("Invalid Sereal header: unsupported protocol version %d", protocolVersion));
    } else if (magic == SerealHeader.MAGIC && protocolVersion > 2) {
      throw new SerealException(
        String.format("Invalid Sereal header: magic v1 with protocol version %d", protocolVersion));
    } else if (magic == SerealHeader.MAGIC_V3 && protocolVersion < 3) {
      throw new SerealException(
        String.format("Invalid Sereal header: magic v3 with protocol version %d", protocolVersion));
    }

    if (hasChecksum && protocolVersion < 6) {
      throw new SerealException("Checksummed Sereal documents require protocol version 6 or later");
    }
    if (!hasChecksum && checksumPolicy == SerealChecksum.Policy.REQUIRE_KEY) {
      throw new SerealException("checksum_policy 'require_key' requires a checksummed packet");
    }

    encoding = (versionAndType >> 4) & SerealHeader.SRL_ENCODING_BASE_MASK;
    if (encoding > 4) {
      throw new SerealException("Unsupported Sereal body encoding " + encoding);
    }
    for (AllowedEncoding allowedEncoding : ALLOWED_ENCODINGS) {
      // inside an envelope the payload is delimited, so legacy Snappy is allowed again
      if (hasChecksum && encoding == SerealHeader.SRL_ENCODING_SNAPPY_LEGACY) {
        break;
      }
      if (allowedEncoding.encoding == encoding) {
        if (protocolVersion < allowedEncoding.minVersion || protocolVersion > allowedEncoding.maxVersion) {
          throw new SerealException(String.format("Unsupported encoding %d (%s) for Sereal protocol %d",
            encoding, allowedEncoding.compressionType, protocolVersion));
        }
      }
    }
    position += 5;
  }

  /**
   * @return {@code true} if the document has a Sereal header.
   *
   * @throws SerealException header cannot be parsed.
   */
  public boolean hasHeader() throws SerealException {
    parseHeader();

    return userHeaderSize > 0;
  }

  /**
   * @return Size of the Sereal header, if present, 0 otherwise.
   *
   * @throws SerealException header cannot be parsed.
   */
  public int headerSize() throws SerealException {
    parseHeader();

    return userHeaderSize > 0 ? userHeaderSize : 0;
  }

  /**
   * Set up the decoder to iterate over the Sereal header.
   * <p>
   * This function will fail if {@link TokenDecoder#hasHeader()} returned {@code false}.
   *
   * @throws SerealException Token cannot be decoded.
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
    checkPayloadEnd = false;

    // because offsets start at 1
    baseOffset = position - 1;

    currentContext = new Context(null, CONTEXT_ROOT, 1);
    copyDepth = 0;
  }

  private void parseHeader() throws SerealException {
    if (originalData == null) {
      throw new SerealException("No data set");
    }
    if (userHeaderSize >= 0) {
      return;
    }
    if (originalData.length < 7) {
      throw new SerealException("Invalid Sereal document: total size is too small");
    }

    data = originalData.array;
    end = originalData.start + originalData.length;
    position = originalData.start;

    try {
      checkMagicProtoAndFlags();
      checkHeaderSuffix();

      int bodyStart = userHeaderPosition + userHeaderSize;
      if (hasChecksum) {
        // verify before exposing header data or decompressing anything
        SerealChecksum.Envelope envelope = SerealChecksum.process(data, originalData.start, bodyStart, end,
            checksumKeys, checksumPolicy, checksumWarningHandler);
        payloadStart = envelope.payloadStart;
        payloadEnd = envelope.payloadEnd;
      } else {
        payloadStart = bodyStart;
        payloadEnd = end;
      }
    } catch (SerealException | RuntimeException e) {
      // leave the header unparsed, so that a later call cannot skip the checks
      userHeaderPosition = userHeaderSize = -1;
      throw e;
    }
  }

  /**
   * Set up the decoder to iterate over the Sereal document body.
   *
   * @throws SerealException Token cannot be decoded.
   */
  public void prepareDecodeBody() throws SerealException {
    parseHeader();

    if (bodyData == null) {
      if (encoding != 0) {
        data = originalData.array;
        position = payloadStart;
        end = payloadEnd;
        if (encoding == SerealHeader.SRL_ENCODING_SNAPPY_LEGACY || encoding == SerealHeader.SRL_ENCODING_SNAPPY) {
          uncompressSnappy();
        } else if (encoding == SerealHeader.SRL_ENCODING_ZLIB) {
          uncompressZlib();
        } else if (encoding == SerealHeader.SRL_ENCODING_ZSTD) {
          uncompressZstd();
        }
      } else {
        bodyData = originalData.array;
        bodyPosition = payloadStart;
        bodySize = payloadEnd;
      }
    }

    data = bodyData;
    position = bodyPosition;
    end = bodySize;
    checkPayloadEnd = hasChecksum && encoding == 0;

    if (protocolVersion == 1) {
      baseOffset = 0;
    } else {
      // because offsets start at 1
      baseOffset = position - 1;
    }

    currentContext = new Context(null, CONTEXT_ROOT, 1);
    copyDepth = 0;
  }

  private void uncompressSnappy() throws SerealException {
    long compressedLength = end - position;
    int pos = encoding == SerealHeader.SRL_ENCODING_SNAPPY_LEGACY ? position : originalData.start;

    if (encoding == SerealHeader.SRL_ENCODING_SNAPPY) {
      compressedLength = readVarint();
    }
    checkCompressedEnd(compressedLength);
    // snappy-java's native code trusts offset and length: keep them inside the document
    if (position < 0 || position > end || compressedLength < 0 || compressedLength > end - position) {
      throw new SerealException("Snappy compressed data exceeds the available input");
    }
    int len = (int) compressedLength;
    byte[] uncompressed;
    try {
      if (!Snappy.isValidCompressedBuffer(
          originalData.array, position, len))
        throw new SerealException("Invalid snappy data");
      int uncompressedLength = Snappy.uncompressedLength(originalData.array, position, len);
      if (uncompressedLength < 0 || (long) pos + uncompressedLength > maxBufferSize) {
        throw new SerealException("The expected uncompressed size is larger than the allowed maximum size");
      }
      uncompressed = new byte[pos + uncompressedLength];
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
    if (uncompressedLength < 0 || uncompressedLength > maxBufferSize) {
      throw new SerealException("The expected uncompressed size is larger than the allowed maximum size");
    }
    long compressedLength = readVarint();
    checkCompressedEnd(compressedLength);
    if (compressedLength < 0 || compressedLength > end - position) {
      throw new SerealException("Zlib compressed data exceeds the available input");
    }
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
    long compressedLength = readVarint();
    checkCompressedEnd(compressedLength);
    if (compressedLength < 0 || compressedLength > end - position) {
      throw new SerealException("Zstd compressed data exceeds the available input");
    }
    int len = (int) compressedLength;

    byte[] compressedData = Arrays.copyOfRange(originalData.array, position, position + len);
    long decompressedSize;
    try {
      decompressedSize = Zstd.decompressedSize(compressedData);
    } catch (ZstdException e) {
      throw new SerealException(e);
    }
    if (decompressedSize <= 0) {
      throw new SerealException("Invalid zstd frame: unknown or empty decompressed size");
    }
    if (decompressedSize > maxBufferSize) {
      throw new SerealException("The expected uncompressed size is larger than the allowed maximum size");
    }

    byte[] uncompressed = new byte[(int) decompressedSize];
    long status;
    try {
      status = Zstd.decompress(uncompressed, compressedData);
    } catch (ZstdException e) {
      throw new SerealException(e);
    }
    if (Zstd.isError(status)) {
      throw new SerealException(Zstd.getErrorName(status));
    }
    this.bodyData = uncompressed;
    this.bodyPosition = 0;
    this.bodySize = uncompressed.length;
  }

  // in a checksummed document the compressed data must end exactly at the payload end
  private void checkCompressedEnd(long compressedLength) throws SerealException {
    if (hasChecksum && compressedLength != payloadEnd - position) {
      throw new SerealException(SerealChecksum.PAYLOAD_LENGTH_MISMATCH);
    }
  }

  private void checkMinVersion(int version, String message) throws SerealException {
    if (protocolVersion < version) {
      throw new SerealException(message);
    }
  }

  private void checkAvailable(long length) throws SerealException {
    if (length > end - position) {
      throw new SerealException("Unexpected end of data at byte " + end);
    }
  }

  // NEG_VARINT: the magnitude of a negative integer, 1..2^63 (2^63 is Long.MIN_VALUE)
  private long readNegVarint() throws SerealException {
    checkNoEOD();
    long magnitude = readVarint();
    if (magnitude == 0 || Long.compareUnsigned(magnitude, Long.MIN_VALUE) > 0) {
      throw new SerealException("Invalid NEG_VARINT magnitude " + Long.toUnsignedString(magnitude));
    }
    return -magnitude;
  }

  private SerealToken readExtend() throws SerealException {
    checkMinVersion(6, "EXTEND tag requires protocol version 6 or later");
    checkNoEOD();
    int id = data[position++] & 0xff;
    switch (id) {
      case SerealHeader.SRL_EXT_UNDEF_DEADLY:
        if ("die".equals(undefDeadlyPolicy)) {
          throw new SerealException("Encountered UNDEF_DEADLY extended tag");
        }
        if ("warn".equals(undefDeadlyPolicy)) {
          undefDeadlyWarningHandler.accept("Encountered UNDEF_DEADLY extended tag");
        }
        return SerealToken.UNDEF;
      case SerealHeader.SRL_EXT_VSTRING_BINARY:
        readVString(false);
        return SerealToken.VSTRING;
      case SerealHeader.SRL_EXT_VSTRING_UTF8:
        readVString(true);
        return SerealToken.VSTRING;
      default:
        throw new SerealException("Unknown extended tag ID " + id);
    }
  }

  private void readVString(boolean utf8) throws SerealException {
    checkNoEOD();
    long length = readVarint();
    if (maxStringLength != 0 && Long.compareUnsigned(length, maxStringLength) > 0) {
      throw new SerealException("Got input string with " + Long.toUnsignedString(length)
          + " characters, but the configured maximum is just " + maxStringLength);
    }
    if (length < 0) {
      throw new SerealException("Unexpected end of data at byte " + end);
    }
    checkAvailable(length);
    if (length == 0) {
      throw new SerealException("Invalid extended VSTRING spelling length");
    }
    int kind = VStrings.classify(data, position, (int) length);
    if (kind == VStrings.INVALID) {
      throw new SerealException("Invalid extended VSTRING spelling");
    }
    if ((kind == VStrings.UTF8) != utf8) {
      throw new SerealException("VSTRING extension ID does not match the decoded string encoding");
    }
    binaryIsUtf8 = utf8;
    binarySliceStart = position;
    binarySliceEnd = position + (int) length;
    position += (int) length;
  }

  private void readBinary() throws SerealException {
    int length = readLength();
    binaryIsUtf8 = false;
    binarySliceStart = position;
    binarySliceEnd = position + length;
    position += length;
  }

  // a string length, which must fit in the remaining input
  private int readLength() throws SerealException {
    long length = readVarint();
    if (length < 0) {
      throw new SerealException("Invalid length " + Long.toUnsignedString(length) + " at byte " + position);
    }
    checkAvailable(length);
    return (int) length;
  }

  // an element count, bounded by the remaining input at bytesPerElement bytes each
  private int readCount(int bytesPerElement) throws SerealException {
    long count = readVarint();
    if (count < 0 || count > (end - position) / bytesPerElement) {
      throw new SerealException("Invalid element count " + Long.toUnsignedString(count) + " at byte " + position);
    }
    return (int) count;
  }

  // top bit set (0x80) means next byte is 7 bits more more varint; at most 10 bytes
  private long readVarint() throws SerealException {
    long uv = 0;

    for (int lshift = 0; ; lshift += 7) {
      if (position >= end) {
        throw new SerealException("Unexpected end of data at byte " + position);
      }
      if (lshift > 63) {
        throw new SerealException("Varint too long at byte " + position);
      }
      byte b = data[position++];
      uv |= ((long) b & 127) << lshift; // add 7 bits
      if (b >= 0) {
        return uv;
      }
    }
  }

  /**
   * Iterate over the Sereal document returning the next token.
   * <p>
   * Depending on the token returned, accessors can be used to retrieve additional
   * information about the token.
   *
   * @return The next token
   *
   * @throws SerealException Token cannot be decoded.
   */
  public SerealToken nextToken() throws SerealException {
    if (currentContext.remaining == 0) {
      Context popped = currentContext;
      currentContext = popped.outer;
      switch (popped.type) {
        case CONTEXT_ROOT:
          if (checkPayloadEnd && position != payloadEnd) {
            // undo the pop, so the error is reported again on a retry
            currentContext = popped;
            throw new SerealException(SerealChecksum.PAYLOAD_LENGTH_MISMATCH);
          }
          return (currentToken = SerealToken.END);
        case CONTEXT_ARRAY:
          return (currentToken = SerealToken.ARRAY_END);
        case CONTEXT_HASH:
          return (currentToken = SerealToken.HASH_END);
        case CONTEXT_OBJECT:
          return (currentToken = SerealToken.OBJECT_END);
        case CONTEXT_SUBDECODE:
          if (popped.isCopy) {
            copyDepth--;
          }
          position = popped.originalPosition;
          return (currentToken = SerealToken.SUBDECODE_END);
      }
    }

    checkNoEOD();

    byte tag = data[position++];
    // skip padding iteratively: a long run of PAD tags must not recurse
    while ((byte) (tag & ~SerealHeader.SRL_HDR_TRACK_FLAG) == SerealHeader.SRL_HDR_PAD) {
      checkNoEOD();
      tag = data[position++];
    }

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
          checkAvailable(4);
          int floatBits =
              ((data[position + 3] & 0xff) << 24)
                  + ((data[position + 2] & 0xff) << 16)
                  + ((data[position + 1] & 0xff) << 8)
                  + (data[position] & 0xff);
          position += 4;
          floatValue = Float.intBitsToFloat(floatBits);
          return (currentToken = SerealToken.FLOAT);
        case SerealHeader.SRL_HDR_DOUBLE:
          checkAvailable(8);
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
        case SerealHeader.SRL_HDR_YES:
          checkMinVersion(5, "YES tag requires protocol version 5 or later");
          return (currentToken = SerealToken.TRUE);
        case SerealHeader.SRL_HDR_NO:
          checkMinVersion(5, "NO tag requires protocol version 5 or later");
          return (currentToken = SerealToken.FALSE);
        case SerealHeader.SRL_HDR_FLOAT_128:
          checkMinVersion(5, "FLOAT_128 tag requires protocol version 5 or later");
          checkAvailable(16);
          binaryIsUtf8 = false;
          binarySliceStart = position;
          binarySliceEnd = position + 16;
          position += 16;
          return (currentToken = SerealToken.FLOAT128);
        case SerealHeader.SRL_HDR_NEG_VARINT:
          checkMinVersion(6, "NEG_VARINT tag requires protocol version 6");
          longValue = readNegVarint();
          return (currentToken = SerealToken.LONG);
        case SerealHeader.SRL_HDR_EXTEND:
          return (currentToken = readExtend());
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
          backreferenceOffset = readBackreference(position - 1);
          return (currentToken = SerealToken.REFP);
        case SerealHeader.SRL_HDR_OBJECT:
          currentContext = new Context(currentContext, CONTEXT_OBJECT, 1);
          readString();
          return (currentToken = SerealToken.OBJECT_START);
        case SerealHeader.SRL_HDR_OBJECTV:
          currentContext = new Context(currentContext, CONTEXT_OBJECT, 1);
          // the class name offset may point at an OBJECT's class name, itself possibly a COPY
          readStringCopy(position - 1, true);
          return (currentToken = SerealToken.OBJECT_START);
        case SerealHeader.SRL_HDR_COPY:
          backreferenceOffset = readCopyTarget(position - 1);
          // a COPY target may not contain a COPY, except for hash keys and class names
          if (copyDepth > 0 && !isHashKey()) {
            throw new SerealException("Nested COPY at offset " + tokenOffset + " inside another COPY");
          }
          return (currentToken = SerealToken.COPY);
        case SerealHeader.SRL_HDR_ALIAS:
          backreferenceOffset = readBackreference(position - 1);
          return (currentToken = SerealToken.ALIAS);
        case SerealHeader.SRL_HDR_WEAKEN:
          currentContext.remaining++;
          return (currentToken = SerealToken.WEAKEN);
        case SerealHeader.SRL_HDR_HASH:
          // every key and value takes at least one byte
          int numKeys = readCount(2);
          currentContext = new Context(currentContext, CONTEXT_HASH, numKeys * 2);
          return (currentToken = SerealToken.HASH_START);
        case SerealHeader.SRL_HDR_ARRAY:
          int length = readCount(1);
          currentContext = new Context(currentContext, CONTEXT_ARRAY, length);
          return (currentToken = SerealToken.ARRAY_START);
        case SerealHeader.SRL_HDR_REGEXP:
          readRegexp();
          return (currentToken = SerealToken.REGEXP);
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
   *
   * @throws SerealException Token cannot be decoded.
   */
  public void startSubDecode(int offset) throws SerealException {
    long target = (long) offset + baseOffset;
    if (offset < 1 || target < 0 || target >= end) {
      throw new SerealException("Sub-decode offset " + offset + " is outside the document");
    }
    currentContext = new Context(currentContext, CONTEXT_SUBDECODE, 1, position);
    if (currentToken == SerealToken.COPY && offset == backreferenceOffset) {
      currentContext.isCopy = true;
      copyDepth++;
    }
    position = (int) target;
  }

  /**
   * @return After a call to {@link #nextToken}, return the current token.
   */
  public SerealToken currentToken() {
    return currentToken;
  }

  /**
   * @return The element count of the current parsing context.
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
   * @return Whether the offset of the current tag is the target of a reference.
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
   * @return Decoder position in the Sereal document.
   * <p>
   * Defined for all tokens.
   */
  public int currentOffset() {
    return position - 1 - baseOffset;
  }

  /**
   * @return Current {@code long} value.
   * <p>
   * Defined for {@link SerealToken#LONG} and {@link SerealToken#UNSIGNED_LONG}.
   */
  public long longValue() {
    return longValue;
  }

  /**
   * @return Current {@code long} value as a {@link java.math.BigInteger}.
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
   * @return Current {@code float} value.
   * <p>
   * Defined for {@link SerealToken#FLOAT}.
   */
  public float floatValue() {
    return floatValue;
  }

  /**
   * @return Current {@code double} value.
   * <p>
   * Defined for {@link SerealToken#DOUBLE}.
   */
  public double doubleValue() {
    return doubleValue;
  }

  /**
   * All binary/string values are slices of this array.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START}, {@link SerealToken#REGEXP},
   * {@link SerealToken#VSTRING} and {@link SerealToken#FLOAT128}.
   *
   * @return Token binary data
   */
  public byte[] decoderBuffer() {
    return data;
  }

  /**
   * @return Start offset of the current binary/string in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START}, {@link SerealToken#REGEXP},
   * {@link SerealToken#VSTRING} and {@link SerealToken#FLOAT128}.
   */
  public int binarySliceStart() {
    return binarySliceStart;
  }

  /**
   * @return End offset of the current binary/string in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START}, {@link SerealToken#REGEXP},
   * {@link SerealToken#VSTRING} and {@link SerealToken#FLOAT128}.
   */
  public int binarySliceEnd() {
    return binarySliceEnd;
  }

  /**
   * @return Length of the current binary/string.
   * <p>
   * Defined for {@link SerealToken#UTF8}, {@link SerealToken#BINARY}, {@link SerealToken#OBJECT_START}, {@link SerealToken#REGEXP},
   * {@link SerealToken#VSTRING} and {@link SerealToken#FLOAT128}.
   */
  public int binarySliceLength() {
    return binarySliceEnd - binarySliceStart;
  }

  /**
   * @return Start offset of the current regexp flags in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#REGEXP}.
   */
  public int regexpFlagsSliceStart() {
    return (int) longValue;
  }

  /**
   * @return End offset of the current regexp flags in {@link TokenDecoder#decoderBuffer()}.
   * <p>
   * Defined for {@link SerealToken#REGEXP}.
   */
  public int regexpFlagsSliceEnd() {
    return backreferenceOffset;
  }

  /**
   * @return Length of the regexp flags string.
   * <p>
   * Defined for {@link SerealToken#REGEXP}.
   */
  public int regexpFlagsSliceLength() {
    return regexpFlagsSliceEnd() - regexpFlagsSliceStart();
  }

  /**
   * @return {@code true} if the byte slice between {@link TokenDecoder#binarySliceStart()}/{@link TokenDecoder#binarySliceEnd()} is UTF-8 encoded.
   * <p>
   * Always {@code true} for {@link SerealToken#UTF8}, always {@code false} for {@link SerealToken#BINARY}
   * and {@link SerealToken#FLOAT128}. For {@link SerealToken#VSTRING} the slice is the ASCII spelling and this
   * tells whether Perl flags the decoded string as UTF-8.
   */
  public boolean binaryIsUtf8() {
    return binaryIsUtf8;
  }

  /**
   * @return Offset into the Sereal document.
   * <p>
   * Defined for {@link SerealToken#REFP}, {@link SerealToken#ALIAS} and {@link SerealToken#COPY} token.
   */
  public int backreferenceOffset() {
    return backreferenceOffset;
  }

  /** @return {@code true} if the current token is a hash key, {@code false} otherwise. */
  public boolean isHashKey() {
    Context context = currentContext;
    while (context.type == CONTEXT_SUBDECODE) {
      context = context.outer;
    }
    return context.type == CONTEXT_HASH && (context.remaining & 1) == 1;
  }

  private void readShortBinary(byte tag) throws SerealException {
    int length = tag & SerealHeader.SRL_MASK_SHORT_BINARY_LEN;
    checkAvailable(length);
    binaryIsUtf8 = false;
    binarySliceStart = position;
    binarySliceEnd = position + length;
    position += length;
  }

  /**
   * Re-read the string at a previous offset, for a COPY used as a class name, and for OBJECTV
   * class names.
   *
   * @param tag position of the COPY or OBJECTV tag
   * @param allowCopyTarget whether the target may itself be a COPY (only for OBJECTV, whose
   *     target is an OBJECT's class name)
   */
  private void readStringCopy(int tag, boolean allowCopyTarget) throws SerealException {
    int originalOffset = allowCopyTarget ? readBackreference(tag) : readCopyTarget(tag);
    int currentPosition = position; // remember where we parked

    position = originalOffset + baseOffset;
    readString();
    position = currentPosition; // go back to where we were
  }

  // read a REFP/ALIAS/OBJECTV offset, which must point strictly before the tag at position tag
  private int readBackreference(int tag) throws SerealException {
    long offset = readVarint();
    long target = baseOffset + offset;
    if (offset < 1 || target < 0 || target >= tag) {
      throw new SerealException("Backreference " + offset + " at byte " + tag + " must precede its own tag");
    }
    return (int) offset;
  }

  // a COPY offset is a backreference whose target is not itself a COPY
  private int readCopyTarget(int tag) throws SerealException {
    int offset = readBackreference(tag);
    if ((byte) (data[offset + baseOffset] & ~SerealHeader.SRL_HDR_TRACK_FLAG) == SerealHeader.SRL_HDR_COPY) {
      throw new SerealException("COPY at byte " + tag + " refers to another COPY");
    }
    return offset;
  }

  private void readUTF8() throws SerealException {
    int length = readLength();
    binaryIsUtf8 = true;
    binarySliceStart = position;
    binarySliceEnd = position + length;
    position += length;
  }

  private long readZigzag() throws SerealException {
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
      readStringCopy(position - 1, false);
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
  }

  /**
   * Set the Sereal data to be decoded.
   * <p>
   * After calling this method, blob is owned by the decoder until the next call to {@code setData} or {@link #reset()}.
   *
   * @param blob Sereal data.
   */
  public void setData(byte[] blob) {
    setData(new ByteArray(blob));
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
    hasChecksum = checkPayloadEnd = false;
    payloadStart = payloadEnd = -1;
    isSecondTime = false;
    copyDepth = 0;
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

  /**
   * Close the decoder to recycle the resources.
   * <p>
   * Returns the native memory used by inflater explicitly before the garbage collector.
   */
  public void close() {
    if (inflater != null) {
      inflater.end();
      inflater = null;
    }
  }
}
