package com.booking.sereal;

import com.booking.sereal.impl.RefpMap;
import com.github.luben.zstd.Zstd;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import org.xerial.snappy.Snappy;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

/**
 * Sereal decoder with Perl-like interface.
 * <p>
 * This class can be used to decoder Perl-like data-structures: (boxed) primitive types, strings, arrays
 * and maps.
 */
public class Decoder implements SerealHeader {

  private static final DecoderOptions DEFAULT_OPTIONS = new DecoderOptions();
  private static final Charset charset_utf8 = Charset.forName("UTF-8");
  private static final Charset charset_latin1 = Charset.forName("ISO-8859-1");
  private final boolean perlRefs;
  private final boolean perlAlias;
  private final boolean preserveUndef;
  private final boolean refuseSnappy;
  private final boolean refuseZlib;
  private final boolean refuseZstd;
  private final boolean preferLatin1;
  private final boolean forceJavaStringForByteArrayValues;
  private final boolean refuseObjects;
  private final boolean stripObjects;
  private final TypeMapper typeMapper;
  private final boolean useObjectArray;
  private final int decodeBufferSize;
  private final int maxSize;

  private final int maxRecursionDepth;
  private final int maxNumMapEntries;
  private final int maxNumArrayEntries;
  private final int maxStringLength;

  private final byte[][] checksumKeys;
  private final SerealChecksum.Policy checksumPolicy;
  private final Consumer<String> checksumWarningHandler;

  private byte[] data;
  private int position, end;
  private ByteArray originalData;
  // where we track items for REFP purposes
  private RefpMap tracked = new RefpMap();
  private int protocolVersion = -1;
  private int encoding = -1;
  private int baseOffset = Integer.MAX_VALUE;
  private long userHeaderPosition = -1;
  private long userHeaderSize = -1;
  // checksum envelope (protocol version 6): the body lives in [payloadStart, payloadEnd)
  private boolean hasChecksum;
  private int payloadStart = -1;
  private int payloadEnd = -1;
  private Inflater inflater;

  private int recursionDepth = 0;
  // set while decoding the target of a COPY tag, which may not contain another COPY
  private boolean inCopy;

  /** Create a new Decoder with default options. */
  public Decoder() {
    this(DEFAULT_OPTIONS);
  }

  /**
   * Create a new Decoder with the specified options.
   *
   * @param options {@link DecoderOptions} to apply.
   */
  public Decoder(DecoderOptions options) {
    perlRefs = options.perlReferences();
    perlAlias = options.perlAliases();
    preserveUndef = options.preserveUndef();
    refuseSnappy = options.refuseSnappy();
    refuseZlib = options.refuseZlib();
    refuseZstd = options.refuseZstd();
    preferLatin1 = options.preferLatin1();
    forceJavaStringForByteArrayValues = options.forceJavaStringForByteArrayValues();
    refuseObjects = options.refuseObjects();
    stripObjects = options.stripObjects();
    typeMapper = options.typeMapper();
    useObjectArray = typeMapper.useObjectArray();
    decodeBufferSize = options.bufferSize();
    maxSize = options.maxBufferSize();

    maxRecursionDepth = options.maxRecursionDepth();
    maxNumMapEntries = options.maxNumMapEntries();
    maxNumArrayEntries = options.maxNumArrayEntries();
    maxStringLength = options.maxStringLength();

    checksumKeys = options.checksumKeys();
    checksumPolicy = options.checksumPolicy();
    checksumWarningHandler = options.checksumWarningHandler();
    checkChecksumOptions(checksumKeys, checksumPolicy);
  }

  static void checkChecksumOptions(byte[][] keys, SerealChecksum.Policy policy) {
    if (policy == SerealChecksum.Policy.REQUIRE_KEY
        && keys[0] == null && keys[1] == null && keys[2] == null) {
      throw new IllegalArgumentException("checksum policy REQUIRE_KEY requires at least one checksum key");
    }
  }

  private void checkHeader() throws SerealException {

    if ((end - position) < 4) {
      throw new SerealException("Invalid Sereal header: too few bytes");
    }

    int magic =
        ((int) (data[position] & 0xff) << 24)
            + ((int) (data[position + 1] & 0xff) << 16)
            + ((int) (data[position + 2] & 0xff) << 8)
            + ((int) (data[position + 3] & 0xff) << 0);
    position += 4;
    if (magic != MAGIC && magic != MAGIC_V3) {
      throw new SerealException(
          String.format("Invalid Sereal header (%08x): doesn't match magic", magic));
    }
  }

  private void checkHeaderSuffix() throws SerealException {
    long suffix_size = read_varint();
    long basePosition = position;
    if (suffix_size < 0 || suffix_size > end - position) {
      throw new SerealException("Sereal header suffix exceeds the available input");
    }

    userHeaderSize = 0;
    // protocol version 1 never looks inside the suffix
    if (protocolVersion >= 2 && suffix_size > 0) {
      byte bitfield = data[position++];

      if ((bitfield & 0x01) == 0x01) {
        userHeaderPosition = position;
        userHeaderSize = suffix_size - 1;
      }
    }

    // skip everything in the optional suffix part
    position = (int) (basePosition + suffix_size);
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

    int protoAndFlags = data[position++] & 0xff;
    protocolVersion = protoAndFlags & SRL_PROTOCOL_VERSION_MASK; // 4 bits for version
    hasChecksum = (protoAndFlags & SRL_ENCODING_CHECKSUM_FLAG & 0xff) != 0;

    if (protocolVersion < 1 || protocolVersion > 6) {
      throw new SerealException(
          String.format("Invalid Sereal header: unsupported protocol version %d", protocolVersion));
    }
    if (hasChecksum && protocolVersion < 6) {
      throw new SerealException("Checksummed Sereal documents require protocol version 6 or later");
    }
    if (!hasChecksum && checksumPolicy == SerealChecksum.Policy.REQUIRE_KEY) {
      throw new SerealException("checksum_policy 'require_key' requires a checksummed packet");
    }

    encoding = (protoAndFlags >> 4) & SRL_ENCODING_BASE_MASK;
    if (encoding == 1 && protocolVersion >= 2 && !hasChecksum) {
      // without an envelope, legacy Snappy data runs to the end of the input
      throw new SerealException("Non-incremental Snappy compression requires protocol version 1");
    } else if ((encoding == 1 || encoding == 2) && refuseSnappy) {
      throw new SerealException("Unsupported encoding: Snappy");
    } else if (encoding == 3 && refuseZlib) {
      throw new SerealException("Unsupported encoding: Zlib");
    } else if (encoding == 4 && refuseZstd) {
      throw new SerealException("Unsupported encoding: Zstd");
    } else if (encoding == 4 && protocolVersion < 4) {
      throw new SerealException(
          "Unsupported encoding zstd for protocol version " + protocolVersion);
    } else if (encoding < 0 || encoding > 4) {
      throw new SerealException("Unsupported encoding: unknown");
    }
  }

  /**
   * Indicates if the Sereal document has an header.
   *
   * @return {@code true} if the Sereal document has an header, {@code false} otherwise
   *
   * @throws SerealException if header cannot be parsed.
   */
  public boolean hasHeader() throws SerealException {
    parseHeader();

    return userHeaderSize > 0;
  }

  /**
   * Size of the Sereal header.
   *
   * @return Size of the Sereal header, 0 if there is no header.
   *
   * @throws SerealException if header cannot be parsed.
   */
  public long headerSize() throws SerealException {
    parseHeader();

    return userHeaderSize > 0 ? userHeaderSize : 0;
  }

  /**
   * Decode the Sereal document header and returns the decoded value.
   *
   * @return returns the header decoded value.
   *
   * @throws SerealException if header cannot be parsed.
   */
  public Object decodeHeader() throws SerealException {
    parseHeader();

    if (userHeaderSize <= 0) throw new SerealException("Sereal user header not present");
    byte[] originalData = data;
    int originalPosition = position, originalSize = end;
    recursionDepth = 0;
    inCopy = false;
    try {
      data = originalData;
      end = (int) (userHeaderPosition + userHeaderSize);
      position = (int) userHeaderPosition;

      return readSingleValue();
    } catch (StackOverflowError error) {
      throw new SerealException("StackOverflowError: Reached recursion limit during deserialization");
    } finally {
      data = originalData;
      end = originalSize;
      position = originalPosition;
      resetTracked();
    }
  }

  private void parseHeader() throws SerealException {
    if (userHeaderSize >= 0) return;

    try {
      checkHeader();
      checkProtoAndFlags();
      checkHeaderSuffix();

      if (hasChecksum) {
        // verify before exposing header data or decompressing anything
        SerealChecksum.Envelope envelope = SerealChecksum.process(data, originalData.start, position, end,
            checksumKeys, checksumPolicy, checksumWarningHandler);
        payloadStart = envelope.payloadStart;
        payloadEnd = envelope.payloadEnd;
      } else {
        payloadStart = position;
        payloadEnd = end;
      }
    } catch (SerealException | RuntimeException e) {
      // leave the header unparsed, so that a later call cannot skip the checks
      userHeaderPosition = userHeaderSize = -1;
      position = originalData.start;
      end = originalData.start + originalData.length;
      throw e;
    }
  }

  /**
   * Decode the Sereal document body and returns the decoded value.
   *
   * @return the decoded value.
   *
   * @throws SerealException if data cannot be parsed.
   */
  public Object decode() throws SerealException {

    recursionDepth = 0;
    inCopy = false;

    if (data == null) {
      throw new SerealException("No data set");
    }

    parseHeader();

    position = payloadStart;
    end = payloadEnd;
    if (encoding != 0) {
      if (encoding == 1 || encoding == 2) uncompressSnappy();
      else if (encoding == 3) uncompressZlib();
      else if (encoding == 4) uncompressZstd();
      if (protocolVersion == 1) baseOffset = 0;
      else
        // because offsets start at 1
        baseOffset = -1;
    } else {
      if (protocolVersion == 1) baseOffset = 0;
      else
        // because offsets start at 1
        baseOffset = position - 1;
    }
    Object out;
    try {
      out = readSingleValue();
    } catch (StackOverflowError error) {
      throw new SerealException("StackOverflowError: Reached recursion limit during deserialization");
    }
    if (hasChecksum && encoding == 0 && position != payloadEnd) {
      throw new SerealException(SerealChecksum.PAYLOAD_LENGTH_MISMATCH);
    }

    return out;
  }

  private void uncompressSnappy() throws SerealException {
    long compressedLength = end - position;
    int pos = protocolVersion == 1 ? position : originalData.start;

    if (encoding == 2) {
      compressedLength = read_varint();
    }
    checkCompressedEnd(compressedLength);
    // snappy-java's native code trusts offset and length: keep them inside the document
    if (position < 0 || position > end || compressedLength < 0 || compressedLength > end - position) {
      throw new SerealException("Snappy compressed data exceeds the available input");
    }
    int len = (int) compressedLength;

    byte[] uncompressed;
    try {
      if (!Snappy.isValidCompressedBuffer(originalData.array, position, len)) {
        throw new SerealException("Invalid snappy data");
      }
      int uncompressedLength = Snappy.uncompressedLength(originalData.array, position, len);
      if (uncompressedLength < 0 || (long) pos + uncompressedLength > this.maxSize) {
        throw new SerealException("The expected uncompressed size is larger than the allowed maximum size");
      }
      uncompressed = new byte[pos + uncompressedLength];
      Snappy.uncompress(originalData.array, position, len, uncompressed, pos);
    } catch (IOException e) {
      throw new SerealException(e);
    }
    this.data = uncompressed;
    this.position = pos;
    this.end = uncompressed.length;
  }

  private void uncompressZlib() throws SerealException {
    if (inflater == null) {
      inflater = new Inflater();
    }
    inflater.reset();

    long uncompressedLength = read_varint();
    if (uncompressedLength > this.maxSize) {
      throw new SerealException("The expected uncompressed size is larger than the allowed maximum size");
    }

    long compressedLength = read_varint();
    checkCompressedEnd(compressedLength);
    inflater.setInput(originalData.array, position, (int) compressedLength);
    try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
      byte[] buffer = new byte[this.decodeBufferSize];
      while (!inflater.finished()) {
        if (outputStream.size() > this.maxSize) {
          throw new SerealException("The uncompressed size is larger than the allowed maximum size");
        } else if (outputStream.size() > uncompressedLength) {
          throw new SerealException("The uncompressed size is larger than the expected size");
        }
        int count = inflater.inflate(buffer);
        if (count == 0 ) {
          break;
        }
        outputStream.write(buffer, 0, count);
      }
      this.data = outputStream.toByteArray();
    } catch (DataFormatException | IOException e) {
      throw new SerealException(e);
    }
    this.position = 0;
    this.end = this.data.length;
  }

  private void uncompressZstd() throws SerealException {
    long compressedLength = read_varint();
    checkCompressedEnd(compressedLength);
    if (compressedLength < 0 || compressedLength > end - position) {
      throw new SerealException("Zstd compressed data exceeds the available input");
    }
    int len = (int) compressedLength;

    byte[] compressedData = Arrays.copyOfRange(originalData.array, position, position + len);
    long decompressedSize = Zstd.decompressedSize(compressedData);

    if (decompressedSize <= 0) {
      throw new SerealException("Invalid zstd frame: unknown or empty decompressed size");
    }
    if (decompressedSize > this.maxSize) {
      throw new SerealException("The expected uncompressed size is larger than the allowed maximum size");
    }
    if (decompressedSize > Integer.MAX_VALUE) {
      throw new SerealException("Decompressed size exceeds integer MAX_VALUE: " + decompressedSize);
    }
    byte[] uncompressed = new byte[(int) decompressedSize];
    long status = Zstd.decompress(uncompressed, compressedData);
    if (Zstd.isError(status)) {
      throw new SerealException(Zstd.getErrorName(status));
    }
    this.data = uncompressed;
    this.position = 0;
    this.end = uncompressed.length;
  }

  // in a checksummed document the compressed data must end exactly at the payload end
  private void checkCompressedEnd(long compressedLength) throws SerealException {
    if (hasChecksum && compressedLength != payloadEnd - position) {
      throw new SerealException(SerealChecksum.PAYLOAD_LENGTH_MISMATCH);
    }
  }

  /**
   * Decode a Sereal ARRAY tag to a native Java arrya
   *
   * @param length number of items in the array
   * @param track we might need to track since array elements could refer to us
   */
  private Object[] readNativeArray(int length, int track) throws SerealException {
    if (maxNumArrayEntries != 0 && length > maxNumArrayEntries) {
      throw new SerealException("Got input array with " + length + " entries, but the configured maximum is just " + maxNumArrayEntries);
    }
    checkAvailable(length); // every element takes at least one byte

    Object[] out = new Object[length];
    if (track != 0) { // track ourself
      track_stuff(track, out);
    }

    for (int i = 0; i < length; i++) {
      out[i] = readSingleValue();
    }

    return out;
  }

  /**
   * Decode a Sereal ARRAY tag to a native Java List
   *
   * @param length number of items in the list
   * @param track we might need to track since array elements could refer to us
   */
  private List<Object> readList(int length, int track) throws SerealException {

    if (maxNumArrayEntries != 0 && length > maxNumArrayEntries) {
      throw new SerealException("Got input array with " + length + " entries, but the configured maximum is just " + maxNumArrayEntries);
    }
    checkAvailable(length); // every element takes at least one byte

    List<Object> out = typeMapper.makeArray(length);
    if (track != 0) { // track ourself
      track_stuff(track, out);
    }

    for (int i = 0; i < length; i++) {
      out.add(readSingleValue());
    }

    return out;
  }

  /**
   * Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name
   *
   * <p>For some reason we call them Latin1Strings.
   */
  private byte[] read_binary() throws SerealException {
    int length = readLength();
    if (maxStringLength != 0 && length > maxStringLength) {
      throw new SerealException("Got input string with " + length + " characters, but the configured maximum is just " + maxStringLength);
    }
    checkAvailable(length);
    byte[] out = Arrays.copyOfRange(data, position, position + length);

    position += length;

    return out;
  }

  private Map<String, Object> readMap(int num_keys, int track) throws SerealException {

    if (maxNumMapEntries != 0 && num_keys > maxNumMapEntries) {
      throw new SerealException("Got input hash with " + num_keys + " entries, but the configured maximum is just " + maxNumMapEntries);
    }
    checkAvailable(2L * num_keys); // every key and value takes at least one byte

    Map<String, Object> hash = typeMapper.makeMap((int) num_keys);
    if (track != 0) { // track ourself
      track_stuff(track, hash);
    }

    for (int i = 0; i < num_keys; i++) {
      String key = readString();
      Object val = readSingleValue();
      hash.put(key, val);
    }

    return hash;
  }

  // a count or length, which must fit in an int
  private int readLength() throws SerealException {
    long length = read_varint();
    if (length < 0 || length > Integer.MAX_VALUE) {
      throw new SerealException("Invalid length " + Long.toUnsignedString(length) + " at byte " + position);
    }
    return (int) length;
  }

  private Object get_tracked_item() {
    long offset = read_varint();
    return tracked.get(offset);
  }

  // top bit set (0x80) means next byte is 7 bits more more varint
  private long read_varint() {

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

  private Object readSingleValue() throws SerealException {

    checkNoEOD();

    byte tag = data[position++];
    // skip padding iteratively: a long run of PAD tags must not recurse
    while ((byte) (tag & ~SRL_HDR_TRACK_FLAG) == SRL_HDR_PAD) {
      checkNoEOD();
      tag = data[position++];
    }

    int track = 0;
    if ((tag & SRL_HDR_TRACK_FLAG) != 0) {
      tag = (byte) (tag & ~SRL_HDR_TRACK_FLAG);
      track = position - 1 - baseOffset;
    }

    Object out;

    if (tag <= SRL_HDR_POS_HIGH) {
      out = (long) tag;
    } else if (tag <= SRL_HDR_NEG_HIGH) {
      out = (long) (tag - 32);
    } else if ((tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW) {
      byte[] short_binary = read_short_binary(tag);
      if (forceJavaStringForByteArrayValues) {
        out = new String(short_binary);
      } else {
        out = preferLatin1 ? new Latin1String(short_binary) : short_binary;
      }
    } else if ((tag & SRL_HDR_HASHREF) == SRL_HDR_HASHREF) {
      depthIncrement();

      Map<String, Object> hash = readMap(tag & 0xf, track);
      if (perlRefs) {
        out = new PerlReference(hash);
      } else {
        out = hash;
      }

      depthDecrement();
    } else if ((tag & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF) {
      depthIncrement();

      Object arr;
      if (useObjectArray) {
        arr = readNativeArray(tag & 0xf, track);
      } else {
        arr = readList(tag & 0xf, track);
      }
      if (perlRefs) {
        out = new PerlReference(arr);
      } else {
        out = arr;
      }

      depthDecrement();
    } else {
      switch (tag) {
        case SRL_HDR_VARINT:
          long l = read_varint();
          if (l >= 0) {
            out = l;
          } else {
            // long int greater than Long.MAX_VALUE wrapped around to negative: return a BigInteger
            byte[] buffer = new byte[8];
            for (int i = 7; i >= 0; --i) {
              buffer[i] = (byte) (l & 0xff);
              l >>= 8;
            }
            out = new BigInteger(1, buffer);
          }
          break;
        case SRL_HDR_ZIGZAG:
          long zz = read_zigzag();
          out = zz;
          break;
        case SRL_HDR_FLOAT:
          int floatBits =
              ((int) (data[position + 3] & 0xff) << 24)
                  + ((int) (data[position + 2] & 0xff) << 16)
                  + ((int) (data[position + 1] & 0xff) << 8)
                  + ((int) (data[position] & 0xff) << 0);
          position += 4;
          float f = Float.intBitsToFloat(floatBits);
          out = f;
          break;
        case SRL_HDR_DOUBLE:
          long doubleBits =
              ((long) (data[position + 7] & 0xff) << 56)
                  + ((long) (data[position + 6] & 0xff) << 48)
                  + ((long) (data[position + 5] & 0xff) << 40)
                  + ((long) (data[position + 4] & 0xff) << 32)
                  + ((long) (data[position + 3] & 0xff) << 24)
                  + ((long) (data[position + 2] & 0xff) << 16)
                  + ((long) (data[position + 1] & 0xff) << 8)
                  + ((long) (data[position] & 0xff) << 0);
          position += 8;
          double d = Double.longBitsToDouble(doubleBits);
          out = d;
          break;
        case SRL_HDR_TRUE:
          out = true;
          break;
        case SRL_HDR_FALSE:
          out = false;
          break;
        case SRL_HDR_YES:
          checkMinVersion(5, "YES tag requires protocol version 5 or later");
          out = Boolean.TRUE;
          break;
        case SRL_HDR_NO:
          checkMinVersion(5, "NO tag requires protocol version 5 or later");
          out = Boolean.FALSE;
          break;
        case SRL_HDR_FLOAT_128:
          checkMinVersion(5, "FLOAT_128 tag requires protocol version 5 or later");
          checkAvailable(16);
          out = new Float128(Arrays.copyOfRange(data, position, position + 16));
          position += 16;
          break;
        case SRL_HDR_NEG_VARINT:
          checkMinVersion(6, "NEG_VARINT tag requires protocol version 6");
          out = readNegVarint();
          break;
        case SRL_HDR_EXTEND:
          out = readExtend();
          break;
        case SRL_HDR_UNDEF:
          if (preserveUndef) out = new PerlUndef();
          else out = null;
          break;
        case SRL_HDR_CANONICAL_UNDEF:
          if (preserveUndef) out = PerlUndef.CANONICAL;
          else out = null;
          break;
        case SRL_HDR_BINARY:
          byte[] bytes = read_binary();
          if (forceJavaStringForByteArrayValues) {
            out = new String(bytes);
          } else {
            out = preferLatin1 ? new Latin1String(bytes) : bytes;
          }
          break;
        case SRL_HDR_STR_UTF8:
          String utf8 = read_UTF8();
          out = utf8;
          break;
        case SRL_HDR_REFN:
          if (perlRefs) {
            PerlReference refn = new PerlReference(null);
            // track early for weak references
            if (track != 0) { // track ourself
              track_stuff(track, refn);
            }
            depthIncrement();
            refn.setValue(readSingleValue());
            depthDecrement();
            out = refn;
          } else {
            depthIncrement();
            out = readSingleValue();
            depthDecrement();
          }
          break;
        case SRL_HDR_REFP:
          long offset_prev = read_varint();
          Object prv_value = tracked.get(offset_prev);
          if (prv_value == RefpMap.NOT_FOUND) {
            throw new SerealException("REFP to offset " + offset_prev + ", which is not tracked");
          }
          Object prev = perlRefs ? new PerlReference(prv_value) : prv_value;
          out = prev;
          break;
        case SRL_HDR_OBJECT:
          if (refuseObjects)
            throw new SerealException(
                String.format(
                    "Encountered object in input, but the 'refuseObject' option is in effect at offset %d of input",
                    position));
          depthIncrement();
          Object obj = readObject();
          depthDecrement();
          out = obj;
          break;
        case SRL_HDR_OBJECTV:
          if (refuseObjects)
            throw new SerealException(
                String.format(
                    "Encountered object in input, but the 'refuseObject' option is in effect at offset %d of input",
                    position));
          // the class name offset may point at an OBJECT's class name, itself possibly a COPY
          String className = readStringCopy(position - 1, true);
          depthIncrement();
          out = readObject(className);
          depthDecrement();
          break;
        case SRL_HDR_COPY:
          depthIncrement();
          Object copy = read_copy();
          depthDecrement();
          out = copy;
          break;
        case SRL_HDR_ALIAS:
          Object value = get_tracked_item();

          if (perlAlias) {
            out = new PerlAlias(value);
          } else {
            out = value;
          }
          break;
        case SRL_HDR_WEAKEN:
          // so the next thing HAS to be a ref (afaict) which means we can track it
          depthIncrement();
          if (perlRefs) {
            PerlReference placeHolder = new PerlReference(null);
            // track early for weak references
            if (track != 0) { // track ourself
              track_stuff(track, placeHolder);
            }
            placeHolder.setValue(((PerlReference) readSingleValue()).getValue());
            WeakReference<PerlReference> wref = new WeakReference<PerlReference>(placeHolder);
            out = wref;
          } else {
            Object ref = readSingleValue();
            // track early for weak references
            if (track != 0) { // track ourself
              track_stuff(track, ref);
            }
            WeakReference<Object> wref = new WeakReference<Object>(ref);
            out = wref;
          }
          depthDecrement();
          break;
        case SRL_HDR_HASH:
          depthIncrement();
          Object hash = readMap(readLength(), track);
          out = hash;
          depthDecrement();
          break;
        case SRL_HDR_ARRAY:
          depthIncrement();
          if (useObjectArray) {
            out = readNativeArray(readLength(), track);
          } else {
            out = readList(readLength(), track);
          }
          depthDecrement();
          break;
        case SRL_HDR_REGEXP:
          Pattern pattern = read_regex();
          out = pattern;
          break;
        default:
          throw new SerealException("Tag not supported: " + tag);
      }
    }

    if (track != 0) { // we double-track arrays ATM (but they just overwrite)
      track_stuff(track, out);
    }

    return out;
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
    long magnitude = read_varint();
    if (magnitude == 0 || Long.compareUnsigned(magnitude, Long.MIN_VALUE) > 0) {
      throw new SerealException("Invalid NEG_VARINT magnitude " + Long.toUnsignedString(magnitude));
    }
    return -magnitude;
  }

  private Object readExtend() throws SerealException {
    checkMinVersion(6, "EXTEND tag requires protocol version 6 or later");
    checkNoEOD();
    int id = data[position++] & 0xff;
    switch (id) {
      case SRL_EXT_DEADLY:
        throw new SerealException("Encountered DEADLY extended tag");
      case SRL_EXT_VSTRING_BINARY:
        return readVString(false);
      case SRL_EXT_VSTRING_UTF8:
        return readVString(true);
      default:
        throw new SerealException("Unknown extended tag ID " + id);
    }
  }

  private PerlVString readVString(boolean utf8) throws SerealException {
    checkNoEOD();
    long length = read_varint();
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
    String spelling = new String(data, position, (int) length, charset_latin1);
    position += (int) length;
    return new PerlVString(spelling);
  }

  /**
   * Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length
   *
   * @param tag the Sereal SHORT_BINARY_* tag
   */
  private byte[] read_short_binary(byte tag) {
    int length = tag & SRL_MASK_SHORT_BINARY_LEN;
    byte[] buf = Arrays.copyOfRange(data, position, position + length);
    position += length;
    return buf;
  }

  /**
   * From the spec: Sometimes it is convenient to be able to reuse a previously emitted sequence in
   * the packet to reduce duplication. For instance a data structure with many hashes with the same
   * keys. The COPY tag is used for this. Its argument is a varint which is the offset of a
   * previously emitted tag, and decoders are to behave as though the tag it references was inserted
   * into the packet stream as a replacement for the COPY tag.
   *
   * <p>Note, that in this case the track flag is not set. It is assumed the decoder can jump back
   * to reread the tag from its location alone.
   *
   * <p>Copy tags are forbidden from referring to another COPY tag, and are also forbidden from
   * referring to anything containing a COPY tag, with the exception that a COPY tag used as a value
   * may refer to an tag that uses a COPY tag for a classname or hash key.
   */
  private Object read_copy() throws SerealException {
    int copyTag = position - 1;
    int originalPosition = copyTarget(copyTag);
    if (inCopy) {
      throw new SerealException("Nested COPY at byte " + copyTag + " inside another COPY");
    }
    int currentPosition = position, currentEnd = end; // remember where we parked

    position = originalPosition;
    // the copied value, including anything nested in it, must precede the COPY tag
    end = copyTag;
    inCopy = true;
    Object copy = readSingleValue();
    inCopy = false;
    end = currentEnd;
    position = currentPosition; // go back to where we were

    return copy;
  }

  /**
   * Re-read the string at a previous offset, for a COPY used as a hash key or class name, and for
   * OBJECTV class names.
   *
   * @param tag position of the COPY or OBJECTV tag
   * @param allowCopyTarget whether the target may itself be a COPY (only for OBJECTV, whose
   *     target is an OBJECT's class name)
   */
  private String readStringCopy(int tag, boolean allowCopyTarget) throws SerealException {
    int originalPosition = copyTarget(tag);
    if (!allowCopyTarget && data[originalPosition] == SRL_HDR_COPY) {
      throw new SerealException("COPY at byte " + tag + " refers to another COPY");
    }
    int currentPosition = position; // remember where we parked

    position = originalPosition;
    String copy = readString();
    position = currentPosition; // go back to where we were

    return copy;
  }

  // read a backreference offset, which must point strictly before the tag at position tag
  private int copyTarget(int tag) throws SerealException {
    long offset = read_varint();
    long target = baseOffset + offset;
    if (offset < 1 || target < 0 || target >= tag) {
      throw new SerealException("COPY target " + offset + " at byte " + tag + " must precede its own tag");
    }
    return (int) target;
  }

  private String read_UTF8() throws SerealException {
    int length = (int) read_varint();
    int originalPosition = position;

    position += length;

    if (maxStringLength != 0 && length > maxStringLength) {
      throw new SerealException("Got input string with " + length + " characters, but the configured maximum is just " + maxStringLength);
    }

    return new String(data, originalPosition, length, charset_utf8);
  }

  private long read_zigzag() {

    long n = read_varint();

    return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
  }

  private Pattern read_regex() throws SerealException {

    int flags = 0;
    Object str = readSingleValue();
    String regex;
    if (str instanceof CharSequence) {
      regex = ((CharSequence) str).toString();
    } else if (str instanceof byte[]) {
      regex = (new Latin1String((byte[]) str)).toString();
    } else {
      throw new SerealException("Regex has to be built from a char or byte sequence");
    }

    // now read modifiers
    byte tag = data[position++];
    if ((tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW) {
      int length = tag & SRL_MASK_SHORT_BINARY_LEN;
      while (length-- > 0) {
        byte value = data[position++];
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
            throw new SerealException("Unknown regex modifier: " + value);
        }
      }
    } else {
      throw new SerealException(
          "Expecting SRL_HDR_SHORT_BINARY for modifiers of regexp, got: " + tag);
    }

    return Pattern.compile(regex, flags);
  }

  private Object readObject() throws SerealException {
    Object className = readString();

    return readObject(className.toString());
  }

  private Object readObject(String className) throws SerealException {
    Object structure = readSingleValue();
    if (stripObjects) return structure;
    Object object = typeMapper.makeObject(className, structure);
    return object;
  }

  private String readString() throws SerealException {
    checkNoEOD();

    byte tag = data[position++];

    if ((tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW) {
      int length = tag & SRL_MASK_SHORT_BINARY_LEN;
      String string = new String(data, position, length, charset_latin1);

      position += length;

      return string;
    } else if (tag == SRL_HDR_BINARY) {
      int length = (int) read_varint();
      String string = new String(data, position, length, charset_latin1);

      position += length;

      return string;
    } else if (tag == SRL_HDR_STR_UTF8) {
      return read_UTF8();
    } else if (tag == SRL_HDR_COPY) {
      return readStringCopy(position - 1, false);
    } else {
      throw new SerealException("Tag " + tag + " is not a string tag");
    }
  }

  /**
   * Set the Sereal data to be decoded.
   * <p>
   * The caller must not modify the data while it is owned by the decoder.
   *
   * @param blob Sereal data to decode.
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
   * The caller must not modify the data while it is owned by the decoder.
   *
   * @param blob Sereal data to decode.
   */
  public void setData(byte[] blob) {
    reset();
    originalData = new ByteArray(blob);
    data = blob;
    end = blob.length;
    position = 0;
  }

  private void track_stuff(int pos, Object ref) {
    tracked.put(pos, ref);
  }

  private void reset() {
    originalData = null;
    data = null;
    protocolVersion = encoding = -1;
    baseOffset = Integer.MAX_VALUE;
    userHeaderPosition = userHeaderSize = -1;
    hasChecksum = false;
    payloadStart = payloadEnd = -1;
    resetTracked();
  }

  private void resetTracked() {
    tracked.clear();
  }

  private void depthIncrement() throws SerealException {
    if (recursionDepth++ > maxRecursionDepth) {
      throw new SerealException("Reached recursion limit (" + maxRecursionDepth + ") during deserialization");
    }
  }

  private void depthDecrement() {
    recursionDepth--;
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
