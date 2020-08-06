package com.booking.sereal;

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
   * @return {@code True} if Snappy is refused, {@False} otherwise
   */
  public boolean refuseSnappy() {
    return refuseSnappy;
  }

  /**
   * If set, the decoder will refuse deserializing any objects in the input stream and
   * instead throw an exception.
   *
   * @return {@code True} if Objects are refused, {@False} otherwise
   */
  public boolean refuseObjects() {
    return refuseObjects;
  }

  /**
   * If set, the decoder will refuse Zlib-compressed input data. This can be
   * desirable for robustness.
   *
   * @return {@code True} if Zlib is refused, {@False} otherwise
   */
  public boolean refuseZlib() {
    return refuseZlib;
  }

  /**
   * If set, the decoder will refuse Zstd-compressed input data. This can be
   * desirable for robustness.
   *
   * @return {@code True} if Zstd is refused, {@False} otherwise
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
   * nested, it will eventually exhaust the C stack. Therefore, there is a limit on
   * the depth of recursion that is accepted. It defaults to 10000 nested calls. You
   * may choose to override this value with the {@link DecoderOptions#maxRecursionDepth(int)} option.
   * Beware that setting it too high can cause hard crashes.
   *
   * Do note that the setting is somewhat approximate. Setting it to 10000 may break at
   * somewhere between 9997 and 10003 nested structures depending on their types.
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
   * point of view, see <a href="http://en.wikipedia.org/wiki/Collision_attack"/>.
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
}
