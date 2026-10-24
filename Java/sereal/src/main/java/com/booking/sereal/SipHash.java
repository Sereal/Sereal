package com.booking.sereal;

/**
 * Streaming SipHash (64-bit and 128-bit output, configurable compression/finalisation rounds) as
 * used by the protocol version 6 keyed checksum envelope.
 *
 * <p>Checksum type 0 is SipHash-2-4 with a 64-bit tag, type 1 is SipHash-2-4-128 and type 2 is
 * SipHash-3-6-128. All tags are written as little-endian 64-bit words. An instance hashes one
 * message: call {@link #update} any number of times, then {@link #digest} once.
 */
final class SipHash {
  private long v0;
  private long v1;
  private long v2;
  private long v3;
  private final int cRounds;
  private final int dRounds;
  private final boolean is128;

  // pending bytes of an incomplete 8-byte block, packed little-endian
  private long tail;
  private int tailLength;
  private long total;

  SipHash(byte[] key, int cRounds, int dRounds, boolean is128) {
    if (key == null || key.length != 16) {
      throw new IllegalArgumentException("SipHash key must be exactly 16 bytes");
    }
    long k0 = readLong(key, 0);
    long k1 = readLong(key, 8);
    this.v0 = 0x736f6d6570736575L ^ k0;
    this.v1 = 0x646f72616e646f6dL ^ k1;
    this.v2 = 0x6c7967656e657261L ^ k0;
    this.v3 = 0x7465646279746573L ^ k1;
    if (is128) {
      this.v1 ^= 0xee;
    }
    this.cRounds = cRounds;
    this.dRounds = dRounds;
    this.is128 = is128;
  }

  /**
   * @param type checksum type from the envelope
   * @return the tag length in bytes for a known checksum type, 0 for an unknown one
   */
  static int tagLength(int type) {
    switch (type) {
      case 0:
        return 8;
      case 1:
      case 2:
        return 16;
      default:
        return 0;
    }
  }

  /**
   * @param type checksum type (0, 1 or 2)
   * @param key 16-byte key
   * @return a fresh hasher for the given checksum type
   */
  static SipHash forType(int type, byte[] key) {
    switch (type) {
      case 0:
        return new SipHash(key, 2, 4, false);
      case 1:
        return new SipHash(key, 2, 4, true);
      case 2:
        return new SipHash(key, 3, 6, true);
      default:
        throw new IllegalArgumentException("Unsupported Sereal checksum type " + type);
    }
  }

  static byte[] hash(int cRounds, int dRounds, boolean is128, byte[] key, byte[] data, int off, int len) {
    SipHash h = new SipHash(key, cRounds, dRounds, is128);
    h.update(data, off, len);
    return h.digest();
  }

  static byte[] hash24(byte[] key, byte[] data) {
    return hash(2, 4, false, key, data, 0, data.length);
  }

  static byte[] hash24_128(byte[] key, byte[] data) {
    return hash(2, 4, true, key, data, 0, data.length);
  }

  static byte[] hash36_128(byte[] key, byte[] data) {
    return hash(3, 6, true, key, data, 0, data.length);
  }

  void update(byte[] data, int off, int len) {
    int end = off + len;
    total += len;

    // complete a pending partial block first
    while (tailLength > 0 && off < end) {
      tail |= (data[off++] & 0xffL) << (8 * tailLength);
      if (++tailLength == 8) {
        compress(tail);
        tail = 0;
        tailLength = 0;
      }
    }
    while (end - off >= 8) {
      compress(readLong(data, off));
      off += 8;
    }
    while (off < end) {
      tail |= (data[off++] & 0xffL) << (8 * tailLength);
      tailLength++;
    }
  }

  byte[] digest() {
    long b = ((total & 0xff) << 56) | tail;
    compress(b);

    byte[] out = new byte[is128 ? 16 : 8];
    v2 ^= is128 ? 0xee : 0xff;
    rounds(dRounds);
    writeLong(out, 0, v0 ^ v1 ^ v2 ^ v3);
    if (is128) {
      v1 ^= 0xdd;
      rounds(dRounds);
      writeLong(out, 8, v0 ^ v1 ^ v2 ^ v3);
    }
    return out;
  }

  private void compress(long m) {
    v3 ^= m;
    rounds(cRounds);
    v0 ^= m;
  }

  private void rounds(int n) {
    for (int i = 0; i < n; i++) {
      v0 += v1;
      v1 = Long.rotateLeft(v1, 13);
      v1 ^= v0;
      v0 = Long.rotateLeft(v0, 32);
      v2 += v3;
      v3 = Long.rotateLeft(v3, 16);
      v3 ^= v2;
      v0 += v3;
      v3 = Long.rotateLeft(v3, 21);
      v3 ^= v0;
      v2 += v1;
      v1 = Long.rotateLeft(v1, 17);
      v1 ^= v2;
      v2 = Long.rotateLeft(v2, 32);
    }
  }

  private static long readLong(byte[] b, int off) {
    return (b[off] & 0xffL)
        | (b[off + 1] & 0xffL) << 8
        | (b[off + 2] & 0xffL) << 16
        | (b[off + 3] & 0xffL) << 24
        | (b[off + 4] & 0xffL) << 32
        | (b[off + 5] & 0xffL) << 40
        | (b[off + 6] & 0xffL) << 48
        | (b[off + 7] & 0xffL) << 56;
  }

  private static void writeLong(byte[] b, int off, long v) {
    for (int i = 0; i < 8; i++) {
      b[off + i] = (byte) (v >>> (8 * i));
    }
  }
}
