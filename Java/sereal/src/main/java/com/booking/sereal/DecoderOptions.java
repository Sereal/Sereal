package com.booking.sereal;

import java.util.function.Consumer;

public class DecoderOptions {
  private boolean perlRefs = false;
  private boolean perlAlias = false;
  private boolean preserveUndef = false;
  private boolean preferLatin1 = false;
  private boolean refuseSnappy = false;
  private boolean refuseObjects = false;
  private boolean refuseZlib = false;
  private boolean refuseZstd = false;
  private boolean stripObjects = false;
  private boolean forceJavaStringForByteArrayValues = false;

  private int maxRecursionDepth = 10_000;
  private int maxNumMapEntries = 0;
  private int maxNumArrayEntries = 0;
  private int maxStringLength = 0;

  private byte[] checksumV0Key;
  private byte[] checksumV1Key;
  private byte[] checksumV2Key;
  private SerealChecksum.Policy checksumPolicy = SerealChecksum.Policy.VERIFY_IF_AVAILABLE;
  private Consumer<String> checksumWarningHandler = SerealChecksum::defaultWarning;

  // Size to use on the buffer used to read the data. Defaults to 1KB
  private int decodeBufferSize = 1024;
  // Maximum size allowed for the data. Defaults to 100MB
  private int maxSize = 100 * 1024 * 1024;

  private TypeMapper typeMapper = new DefaultTypeMapper();

  public boolean perlReferences() {
    return perlRefs;
  }

  public boolean perlAliases() {
    return perlAlias;
  }

  public boolean preserveUndef() {
    return preserveUndef;
  }

  public boolean preferLatin1() {
    return preferLatin1;
  }

  /**
   * If set, the decoder will refuse Snappy-compressed input data. This can be
   * desirable for robustness.
   *
   * @return {@code True} if Snappy is refused, {@code False} otherwise
   */
  public boolean refuseSnappy() {
    return refuseSnappy;
  }

  /**
   * If set, the decoder will refuse deserializing any objects in the input stream and
   * instead throw an exception.
   *
   * @return {@code True} if Objects are refused, {@code False} otherwise
   */
  public boolean refuseObjects() {
    return refuseObjects;
  }

  /**
   * If set, the decoder will refuse Zlib-compressed input data. This can be
   * desirable for robustness.
   *
   * @return {@code True} if Zlib is refused, {@code False} otherwise
   */
  public boolean refuseZlib() {
    return refuseZlib;
  }

  /**
   * If set, the decoder will refuse Zstd-compressed input data. This can be
   * desirable for robustness.
   *
   * @return {@code True} if Zstd is refused, {@code False} otherwise
   */
  public boolean refuseZstd() {
    return refuseZstd;
  }

  public boolean stripObjects() {
    return stripObjects;
  }

  public boolean forceJavaStringForByteArrayValues() {
    return forceJavaStringForByteArrayValues;
  }

  public TypeMapper typeMapper() {
    return typeMapper;
  }

  public int bufferSize() {
    return decodeBufferSize;
  }

  public int maxBufferSize() {
    return maxSize;
  }

  /**
   * {@link Decoder} is recursive. If you pass it a Sereal document that is deeply
   * nested, it will eventually exhaust the Java stack. Therefore, there is a limit on
   * the depth of recursion that is accepted. It defaults to 10000 nested calls. You
   * may choose to override this value with the {@link DecoderOptions#maxRecursionDepth(int)} option.
   * Beware that setting it too high can cause hard crashes.
   *
   * Do note that the setting is somewhat approximate. Setting it to 10000 may break at
   * somewhere between 9997 and 10003 nested structures depending on their types.
   *
   * It's also important to note that this value doesn't mirror the Perl implementation. Perl and Java
   * Sereal wrappers are implemented in a different way. That means that the recursion depth marked
   * as max in Perl may be different than the one needed in Java. The same applies for the Encoder
   * and Decoder options.
   *
   * The Java stack can be controlled with different flags (for example -Xss) to change it's size.
   *
   * @return maximum recursion depth
   */
  public int maxRecursionDepth() {
    return maxRecursionDepth;
  }

  /**
   * If set to a non-zero value (default: 0), then {@link Decoder} will refuse
   * to deserialize any hash/dictionary (or hash-based object) with more than
   * that number of entries. This is to be able to respond quickly to any future
   * hash-collision attacks on Perl's hash function. Chances are, you don't want
   * or need this. For a gentle introduction to the topic from the cryptographic
   * point of view, see <a href="http://en.wikipedia.org/wiki/Collision_attack">Collision attack</a>.
   *
   * This value can be override with {@link DecoderOptions#maxNumMapEntries(int)} option
   *
   * @return maximum number of map entries
   */
  public int maxNumMapEntries() {
    return maxNumMapEntries;
  }

  /**
   * If set to a non-zero value (default: 0), then {@link Decoder} will refuse
   * to deserialize any array with more than that number of entries.
   * This is to be able to respond quickly to any future memory exhaustion attacks on
   * Sereal.
   *
   * This value can be override with {@link DecoderOptions#maxNumArrayEntries(int)} option
   *
   * @return maximum number of array entries
   */
  public int maxNumArrayEntries() {
    return maxNumArrayEntries;
  }

  /**
   * If set to a non-zero value (default: 0), then {@link Decoder} will refuse
   * to deserialize any string with more than that number of characters.
   * This is to be able to respond quickly to any future memory exhaustion attacks on
   * Sereal.
   *
   * This value can be override with {@link DecoderOptions#maxStringLength(int)} option
   *
   * @return maximum supported string length
   */
  public int maxStringLength() {
    return maxStringLength;
  }

  /**
   * Key for checksum type 0 (SipHash-2-4, 64-bit tag).
   *
   * @return a copy of the key, or {@code null} if not set
   */
  public byte[] checksumV0Key() {
    return SerealChecksum.copyKey(checksumV0Key);
  }

  /**
   * Key for checksum type 1 (SipHash-2-4-128).
   *
   * @return a copy of the key, or {@code null} if not set
   */
  public byte[] checksumV1Key() {
    return SerealChecksum.copyKey(checksumV1Key);
  }

  /**
   * Key for checksum type 2 (SipHash-3-6-128).
   *
   * @return a copy of the key, or {@code null} if not set
   */
  public byte[] checksumV2Key() {
    return SerealChecksum.copyKey(checksumV2Key);
  }

  /**
   * How checksum envelopes are handled. Defaults to {@link SerealChecksum.Policy#VERIFY_IF_AVAILABLE}.
   *
   * @return the checksum policy
   */
  public SerealChecksum.Policy checksumPolicy() {
    return checksumPolicy;
  }

  /**
   * Receives checksum warnings (unauthenticated decoding, ignored checksums). The default handler
   * prints {@code "Sereal: " + msg} to {@code System.err}.
   *
   * @return the warning handler
   */
  public Consumer<String> checksumWarningHandler() {
    return checksumWarningHandler;
  }

  // the three key slots indexed by checksum type (not copied)
  byte[][] checksumKeys() {
    return new byte[][] {checksumV0Key, checksumV1Key, checksumV2Key};
  }

  public DecoderOptions perlReferences(boolean perlReferences) {
    this.perlRefs = perlReferences;

    return this;
  }

  public DecoderOptions perlAliases(boolean perlAliases) {
    this.perlAlias = perlAliases;

    return this;
  }

  public DecoderOptions forceJavaStringForByteArrayValues(boolean forceJavaStringForByteArrayValues) {
    this.forceJavaStringForByteArrayValues = forceJavaStringForByteArrayValues;

    return this;
  }

  public DecoderOptions preserveUndef(boolean preserveUndef) {
    this.preserveUndef = preserveUndef;

    return this;
  }

  public DecoderOptions preferLatin1(boolean preferLatin1) {
    this.preferLatin1 = preferLatin1;

    return this;
  }

  public DecoderOptions refuseSnappy(boolean refuseSnappy) {
    this.refuseSnappy = refuseSnappy;

    return this;
  }

  public DecoderOptions refuseObjects(boolean refuseObjects) {
    this.refuseObjects = refuseObjects;

    return this;
  }

  public DecoderOptions stripObjects(boolean stripObjects) {
    this.stripObjects = stripObjects;

    return this;
  }

  public DecoderOptions refuseZlib(boolean refuseZlib) {
    this.refuseZlib = refuseZlib;

    return this;
  }

  public DecoderOptions refuseZstd(boolean refuseZstd) {
    this.refuseZstd = refuseZstd;

    return this;
  }

  public DecoderOptions typeMapper(TypeMapper typeMapper) {
    this.typeMapper = typeMapper;

    return this;
  }

  public DecoderOptions bufferSize(int bufferSize) {
    this.decodeBufferSize = bufferSize;

    return this;
  }

  public DecoderOptions maxBufferSize(int maxSize) {
    this.maxSize = maxSize;

    return this;
  }

  public DecoderOptions maxRecursionDepth(int maxRecursionDepth) {
    this.maxRecursionDepth = maxRecursionDepth;

    return this;
  }

  public DecoderOptions maxNumMapEntries(int maxNumMapEntries) {
    this.maxNumMapEntries = maxNumMapEntries;

    return this;
  }

  public DecoderOptions maxNumArrayEntries(int maxNumArrayEntries) {
    this.maxNumArrayEntries = maxNumArrayEntries;

    return this;
  }

  public DecoderOptions maxStringLength(int maxStringLength) {
    this.maxStringLength = maxStringLength;

    return this;
  }

  /**
   * Set the key for checksum type 0 (SipHash-2-4, 64-bit tag). Keys must be exactly 16 bytes; use
   * {@link SerealChecksum#prepareKey(byte[], byte[])} to derive one from an arbitrary secret.
   *
   * @param key 16-byte key (copied), or {@code null} to clear it
   * @return DecoderOptions
   */
  public DecoderOptions checksumV0Key(byte[] key) {
    this.checksumV0Key = SerealChecksum.checkKey(key, "checksumV0Key");

    return this;
  }

  /**
   * Set the key for checksum type 1 (SipHash-2-4-128). Keys must be exactly 16 bytes.
   *
   * @param key 16-byte key (copied), or {@code null} to clear it
   * @return DecoderOptions
   */
  public DecoderOptions checksumV1Key(byte[] key) {
    this.checksumV1Key = SerealChecksum.checkKey(key, "checksumV1Key");

    return this;
  }

  /**
   * Set the key for checksum type 2 (SipHash-3-6-128). Keys must be exactly 16 bytes.
   *
   * @param key 16-byte key (copied), or {@code null} to clear it
   * @return DecoderOptions
   */
  public DecoderOptions checksumV2Key(byte[] key) {
    this.checksumV2Key = SerealChecksum.checkKey(key, "checksumV2Key");

    return this;
  }

  public DecoderOptions checksumPolicy(SerealChecksum.Policy checksumPolicy) {
    if (checksumPolicy == null) {
      throw new IllegalArgumentException("checksumPolicy must not be null");
    }
    this.checksumPolicy = checksumPolicy;

    return this;
  }

  public DecoderOptions checksumWarningHandler(Consumer<String> checksumWarningHandler) {
    if (checksumWarningHandler == null) {
      throw new IllegalArgumentException("checksumWarningHandler must not be null");
    }
    this.checksumWarningHandler = checksumWarningHandler;

    return this;
  }
}
