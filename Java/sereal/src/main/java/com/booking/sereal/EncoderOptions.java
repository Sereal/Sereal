package com.booking.sereal;

public class EncoderOptions {
  private boolean perlRefs = false;
  private boolean perlAlias = false;
  private int protocolVersion = 4;
  private CompressionType compressionType = CompressionType.NONE;
  private long compressionThreshold = 1024;
  private int zlibCompressionLevel = 6;
  private int zstdCompressionLevel = 3;

  private int maxRecursionDepth = 10_000;
  private int maxNumMapEntries = 10_000;
  private int maxNumArrayEntries = 10_000;

  public boolean perlReferences() {
    return perlRefs;
  }

  public boolean perlAliases() {
    return perlAlias;
  }

  public int protocolVersion() {
    return protocolVersion;
  }

  public CompressionType compressionType() {
    return compressionType;
  }

  public long compressionThreshold() {
    return compressionThreshold;
  }

  public int zlibCompressionLevel() {
    return zlibCompressionLevel;
  }

  public int zstdCompressionLevel() {
    return zstdCompressionLevel;
  }

  /**
   * {@link Encoder} is recursive. If you pass it an Object that is deeply
   * nested, it will eventually exhaust the C stack. Therefore, there is a limit on
   * the depth of recursion that is accepted. It defaults to 10000 nested calls. You
   * may choose to override this value with the {@link EncoderOptions#maxRecursionDepth(int)} option.
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
   * If set to a non-zero value (default: 10000), then {@link Encoder} will refuse
   * to deserialize any hash/dictionary (or hash-based object) with more than
   * that number of entries. This is to be able to respond quickly to any future
   * hash-collision attacks on Perl's hash function. Chances are, you don't want
   * or need this. For a gentle introduction to the topic from the cryptographic
   * point of view, see <a href="http://en.wikipedia.org/wiki/Collision_attack"/>.
   *
   * This value can be override with {@link EncoderOptions#maxNumMapEntries(int)} option
   *
   * @return maximum number of map entries
   */
  public int maxNumMapEntries() {
    return maxNumMapEntries;
  }

  /**
   * If set to a non-zero value (default: 10000), then {@link Encoder} will refuse
   * to deserialize any array with more than that number of entries.
   * This is to be able to respond quickly to any future memory exhaustion attacks on
   * Sereal.
   *
   * This value can be override with {@link EncoderOptions#maxNumArrayEntries(int)} option
   *
   * @return maximum number of array entries
   */
  public int maxNumArrayEntries() {
    return maxNumArrayEntries;
  }

  public EncoderOptions perlReferences(boolean perlReferences) {
    this.perlRefs = perlReferences;

    return this;
  }

  public EncoderOptions perlAliases(boolean perlAliases) {
    this.perlAlias = perlAliases;

    return this;
  }

  public EncoderOptions protocolVersion(int protocolVersion) {
    if (protocolVersion < 1 || protocolVersion > 4) {
      throw new IllegalArgumentException("Unknown Sereal version " + protocolVersion);
    }
    this.protocolVersion = protocolVersion;

    return this;
  }

  public EncoderOptions compressionType(CompressionType compressionType) {
    if (protocolVersion < compressionType.minProtocolVersion) {
      throw new IllegalArgumentException("Compression " + compressionType + " not supported in Sereal protocol" + protocolVersion);
    }
    this.compressionType = compressionType;

    return this;
  }

  public EncoderOptions compressionThreshold(long compressionThreshold) {
    this.compressionThreshold = compressionThreshold;

    return this;
  }

  public EncoderOptions zlibCompressionLevel(int zlibCompressionLevel) {
    this.zlibCompressionLevel = zlibCompressionLevel;

    return this;
  }

  public EncoderOptions zstdCompressionLevel(int zstdCompressionLevel) {
    this.zstdCompressionLevel = zstdCompressionLevel;
    return this;
  }


  public EncoderOptions maxRecursionDepth(int maxRecursionDepth) {
    this.maxRecursionDepth = maxRecursionDepth;

    return this;
  }

  public EncoderOptions maxNumMapEntries(int maxNumMapEntries) {
    this.maxNumMapEntries = maxNumMapEntries;

    return this;
  }

  public EncoderOptions maxNumArrayEntries(int maxNumArrayEntries) {
    this.maxNumArrayEntries = maxNumArrayEntries;

    return this;
  }

  public enum CompressionType {
    NONE(1, SerealHeader.SRL_ENCODING_NONE),
    SNAPPY(1, -1),
    ZLIB(3, SerealHeader.SRL_ENCODING_ZLIB),
    ZSTD(4, SerealHeader.SRL_ENCODING_ZSTD);

    final byte minProtocolVersion;
    final byte encoding;

    CompressionType(int minProtocolVersion, int encoding) {
      this.minProtocolVersion = (byte) minProtocolVersion;
      this.encoding = (byte) encoding;
    }
  }
}
