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
  private Inflater inflater;

  private int recursionDepth = 0;

  /** Create a new Decoder with default options. */
  public Decoder() {
    this(DEFAULT_OPTIONS);
  }

  /** Create a new Decoder with the specified options. */
  public Decoder(DecoderOptions options) {
    perlRefs = options.perlReferences();
    perlAlias = options.perlAliases();
    preserveUndef = options.preserveUndef();
    refuseSnappy = options.refuseSnappy();
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
          String.format("Invalid Seareal header (%08x): doesn't match magic", magic));
    }
  }

  private void checkHeaderSuffix() {
    long suffix_size = read_varint();
    long basePosition = position;

    userHeaderSize = 0;
    if (suffix_size > 0) {
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

    int protoAndFlags = data[position++];
    protocolVersion = protoAndFlags & 15; // 4 bits for version

    if (protocolVersion < 0 || protocolVersion > 4) {
      throw new SerealException(
          String.format("Invalid Sereal header: unsupported protocol version %d", protocolVersion));
    }

    encoding = (protoAndFlags & ~15) >> 4;
    if ((encoding == 1 || encoding == 2) && refuseSnappy) {
      throw new SerealException("Unsupported encoding: Snappy");
    } else if (encoding == 4 && protocolVersion < 4) {
      throw new SerealException(
          "Unsupported encoding zstd for protocol version " + protocolVersion);
    } else if (encoding < 0 || encoding > 4) {
      throw new SerealException("Unsupported encoding: unknown");
    }
  }

  /** {@code true} if the Sereal document has an header. */
  public boolean hasHeader() throws SerealException {
    parseHeader();

    return userHeaderSize > 0;
  }

  /** Size of the Sereal header, 0 if there is no header. */
  public long headerSize() throws SerealException {
    parseHeader();

    return userHeaderSize > 0 ? userHeaderSize : 0;
  }

  /**
   * Decode the Sereal document header and returns the decoded value.
   */
  public Object decodeHeader() throws SerealException {
    parseHeader();

    if (userHeaderSize <= 0) throw new SerealException("Sereal user header not present");
    byte[] originalData = data;
    int originalPosition = position, originalSize = end;
    try {
      data = originalData;
      end = (int) (userHeaderPosition + userHeaderSize);
      position = (int) userHeaderPosition;

      return readSingleValue();
    } finally {
      data = originalData;
      end = originalSize;
      position = originalPosition;
      resetTracked();
    }
  }

  private void parseHeader() throws SerealException {
    if (userHeaderSize >= 0) return;

    checkHeader();
    checkProtoAndFlags();
    checkHeaderSuffix();
  }

  /**
   * Decode the Sereal document body and returns the decoded value.
   */
  public Object decode() throws SerealException {

    recursionDepth = 0;

    if (data == null) {
      throw new SerealException("No data set");
    }

    parseHeader();

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
    Object out = readSingleValue();

    return out;
  }

  private void uncompressSnappy() throws SerealException {
    int len = originalData.length - (position - originalData.start);
    int pos = protocolVersion == 1 ? position : originalData.start;

    if (encoding == 2) {
      len = (int) read_varint();
    }

    byte[] uncompressed;
    try {
      if (!Snappy.isValidCompressedBuffer(originalData.array, position, len)) {
        throw new SerealException("Invalid snappy data");
      }
      int uncompressedLength = Snappy.uncompressedLength(originalData.array, position, len);
      if (uncompressedLength > this.maxSize) {
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
    int len = (int) read_varint();

    byte[] compressedData = Arrays.copyOfRange(originalData.array, position, position + len);
    long decompressedSize = Zstd.decompressedSize(compressedData);

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

  /**
   * Decode a Sereal ARRAY tag to anative Java arrya
   *
   * @param length number of items in the array
   * @param track we might need to track since array elements could refer to us
   */
  private Object[] readNativeArray(int length, int track) throws SerealException {
    if (maxNumArrayEntries != 0 && length > maxNumArrayEntries) {
      throw new SerealException("Got input array with " + length + " entries, but the configured maximum is just " + maxNumArrayEntries);
    }

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
   * Decode a Sereal ARRAY tag to anative Java List
   *
   * @param length number of items in the list
   * @param track we might need to track since array elements could refer to us
   */
  private List<Object> readList(int length, int track) throws SerealException {

    if (maxNumArrayEntries != 0 && length > maxNumArrayEntries) {
      throw new SerealException("Got input array with " + length + " entries, but the configured maximum is just " + maxNumArrayEntries);
    }

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
  private byte[] read_binary() {
    int length = (int) read_varint();
    byte[] out = Arrays.copyOfRange(data, position, position + length);

    position += length;

    return out;
  }

  private Map<String, Object> readMap(int num_keys, int track) throws SerealException {

    if (maxNumMapEntries != 0 && num_keys > maxNumMapEntries) {
      throw new SerealException("Got input hash with " + num_keys + " entries, but the configured maximum is just " + maxNumMapEntries);
    }

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
          depthIncrement();

          if (perlRefs) {
            PerlReference refn = new PerlReference(null);
            // track early for weak references
            if (track != 0) { // track ourself
              track_stuff(track, refn);
            }
            refn.setValue(readSingleValue());
            out = refn;
          } else {
            out = readSingleValue();
          }

          depthDecrement();
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
          Object obj = readObject();
          out = obj;
          break;
        case SRL_HDR_OBJECTV:
          if (refuseObjects)
            throw new SerealException(
                String.format(
                    "Encountered object in input, but the 'refuseObject' option is in effect at offset %d of input",
                    position));
          String className = readStringCopy();
          out = readObject(className);
          break;
        case SRL_HDR_COPY:
          Object copy = read_copy();
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
          break;
        case SRL_HDR_HASH:
          Object hash = readMap((int) read_varint(), track);
          out = hash;
          break;
        case SRL_HDR_ARRAY:
          if (useObjectArray) {
            out = readNativeArray((int) read_varint(), track);
          } else {
            out = readList((int) read_varint(), track);
          }
          break;
        case SRL_HDR_REGEXP:
          Pattern pattern = read_regex();
          out = pattern;
          break;
        case SRL_HDR_PAD:
          return readSingleValue();
        default:
          throw new SerealException("Tag not supported: " + tag);
      }
    }

    if (track != 0) { // we double-track arrays ATM (but they just overwrite)
      track_stuff(track, out);
    }

    return out;
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

    int originalPosition = (int) read_varint();
    int currentPosition = position; // remember where we parked

    position = originalPosition + baseOffset;
    Object copy = readSingleValue();
    position = currentPosition; // go back to where we were

    return copy;
  }

  private String readStringCopy() throws SerealException {
    int originalPosition = (int) read_varint();
    int currentPosition = position; // remember where we parked

    position = originalPosition + baseOffset;
    String copy = readString();
    position = currentPosition; // go back to where we were

    return copy;
  }

  private String read_UTF8() {
    int length = (int) read_varint();
    int originalPosition = position;

    position += length;

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
      return readStringCopy();
    } else {
      throw new SerealException("Tag " + tag + " is not a string tag");
    }
  }

  /**
   * Set the Sereal data to be decoded.
   * <p>
   * The caller must not modify the data while it is owned by the decoder.
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
    resetTracked();
  }

  private void resetTracked() {
    tracked.clear();
  }

  private void depthIncrement() throws SerealException {
    ++recursionDepth;

    if (recursionDepth > maxRecursionDepth) {
      throw new SerealException("Reached recursion limit (" + maxRecursionDepth + ") during deserialization");
    }
  }

  private void depthDecrement() {
    recursionDepth--;
  }
}
