package com.booking.sereal;

import java.nio.charset.StandardCharsets;

/**
 * Protocol v5/v6 test vectors generated from the Perl Sereal::Encoder/Sereal::Decoder build of
 * {@code yves/v6_wip}. Every packet is a hex string; use {@link TestUtils#hex(String)}.
 *
 * <p>The body samples encode {@code {name=>"sereal", n=>-1000, list=>[1,-17,2**40,"x"x40],
 * v=>v1.2.3}} and all carry the same 85-byte payload, signed with {@link #KEY_HEX}.
 */
public final class V6Vectors {
  private V6Vectors() {}

  /** ASCII "0123456789abcdef". */
  public static final String KEY_HEX = "30313233343536373839616263646566";

  /** Key used by Decoder t/915 for the hand-built envelopes. */
  public static final String KEY_915 = "Out damned spot!";

  public static byte[] key() {
    return TestUtils.hex(KEY_HEX);
  }

  public static byte[] key915() {
    return KEY_915.getBytes(StandardCharsets.ISO_8859_1);
  }

  // body samples

  public static final String PLAIN_V6 =
      "3df3726c060054646c69737444013c112200008053262878787878787878787878787878787878787878787878787878787878787878787878787878787878616e3ce807646e616d656673657265616c61763e260676312e322e33";

  public static final String RAW_V0KEY =
      "3df3726c860000081c67b8b325a786ee5554646c69737444013c112200008053262878787878787878787878787878787878787878787878787878787878787878787878787878787878616e3ce807646e616d656673657265616c61763e260676312e322e33";
  public static final String RAW_V0KEY_TAG = "1c67b8b325a786ee";

  public static final String RAW_V1KEY =
      "3df3726c860001106008cd6c486fcbd2e0251c7a7735d0725554646c69737444013c112200008053262878787878787878787878787878787878787878787878787878787878787878787878787878787878616e3ce807646e616d656673657265616c61763e260676312e322e33";
  public static final String RAW_V1KEY_TAG = "6008cd6c486fcbd2e0251c7a7735d072";

  public static final String RAW_V2KEY =
      "3df3726c860002105b9794d91f2b16954839b7ea1fbc4a3d5554646c69737444013c112200008053262878787878787878787878787878787878787878787878787878787878787878787878787878787878616e3ce807646e616d656673657265616c61763e260676312e322e33";
  public static final String RAW_V2KEY_TAG = "5b9794d91f2b16954839b7ea1fbc4a3d";

  public static final String ZLIB_V2KEY =
      "3df3726cb6000210ef9d187819d8dac6edbbe02fd9da85934c55c900789c8dc0310ec0101400505ddab957e8209d24ad554c6ec0057ee24b241810313aae63b882676cf0a5aa43dc0f2143d3b76f8224e6651344740533428026e9d93ef633be0079c12094";
  public static final String ZLIB_V2KEY_TAG = "ef9d187819d8dac6edbbe02fd9da8593";

  public static final String ZSTD_V2KEY =
      "3df3726cc6000210fbfc6d7144dded95cf92911e216a412540be0028b52ffd2055ad0100e40254646c69737444013c112200008053262878616e3ce807646e616d656673657265616c61763e260676312e322e33010094200c";
  public static final String ZSTD_V2KEY_TAG = "fbfc6d7144dded95cf92911e216a4125";

  public static final String SNAPPY_INCR_V0KEY =
      "3df3726ca600000808b05c3bbb1817af36b400554454646c69737444013c1122000080532628789a01006c616e3ce807646e616d656673657265616c61763e260676312e322e33";
  public static final String SNAPPY_INCR_V0KEY_TAG = "08b05c3bbb1817af";

  /** Header user data {hdr=>"meta", n=>7}. */
  public static final String RAW_V1KEY_WITH_HEADER =
      "3df3726c860e0152616e0763686472646d65746101102c00434ab6b2e66aab0fd29d29f44cca5554646c69737444013c112200008053262878787878787878787878787878787878787878787878787878787878787878787878787878787878616e3ce807646e616d656673657265616c61763e260676312e322e33";
  public static final String RAW_V1KEY_WITH_HEADER_TAG = "2c00434ab6b2e66aab0fd29d29f44cca";

  // small signed packets of the integer 7

  public static final String RAW_V2KEY_7 = "3df3726c860002100f8ddc80c103af28700d68f7d168aa3b0107";

  /** Payload length 2 = "07 08", validly signed: decoding must fail the length check. */
  public static final String TRAILING_BYTE = "3df3726c860002105899f45c29a99d851b37be482e5bc6b8020708";

  /** Payload "07 3f" (trailing PAD), validly signed: still a length mismatch. */
  public static final String TRAILING_PAD = "3df3726c860002103e7b5f74f7cb5bce35d4c23d676d66cc02073f";

  /** Payload length 0, validly signed, with "07" after the payload. */
  public static final String PAYLOAD_LEN_0 = "3df3726c86000210a8c41b4aa3a7803d514df4e52163ef770007";

  /** Type 3, 16 'x' tag bytes, payload "07". */
  public static final String UNKNOWN_TYPE3 = "3df3726c86000310787878787878787878787878787878780107";

  // hand-built envelopes with KEY_915, each decoding to "k"

  public static final String LEGACY_SNAPPY_TYPE0 = "3df3726c960000081c5587ccce297b96040204616b";
  public static final String INCR_SNAPPY_TYPE2 = "3df3726ca6000210a695b2aba3e041dd909fefcde61adafe05040204616b";
  public static final String RAW_TYPE1_SHORT = "3df3726c86000110822ca3f73b4d1332bd5948516f55910302616b";

  /** LEGACY_SNAPPY_TYPE0 without the envelope: legacy snappy at v6 must be rejected. */
  public static final String LEGACY_SNAPPY_UNSIGNED = "3df3726c16000204616b";

  // tag samples

  public static final String NEG17 = "3df3726c06003c11";
  public static final String NEG1000 = "3df3726c06003ce807";
  public static final String LONG_MIN = "3df3726c06003c80808080808080808001";
  public static final String V5_ZIGZAG_NEG17 = "3df3726c05002121";
  public static final String VSTRING_1_2_3 = "3df3726c06003e260676312e322e33";
  public static final String VSTRING_65_66_67 = "3df3726c06003e26097636352e36362e3637";
  public static final String VSTRING_300_1 = "3df3726c06003e2706763330302e31";
  public static final String VSTRING_127 = "3df3726c06003e260476313237";
  public static final String VSTRING_128 = "3df3726c06003e270476313238";
  public static final String YES = "3df3726c06003b";
  public static final String NO = "3df3726c06003a";
  public static final String YES_TAG = "3df3726c060035";
  public static final String NO_TAG = "3df3726c060034";

  /** FLOAT_128 with the 16 bytes 00..0f. */
  public static final String FLOAT128_V5 = "3df3726c050038000102030405060708090a0b0c0d0e0f";

  // must fail

  public static final String DEADLY = "3df3726c06003e00";
  public static final String NEG_VARINT_ZERO = "3df3726c06003c00";
  public static final String NEG_VARINT_TOO_BIG = "3df3726c06003c81808080808080808001";
  public static final String NEG_VARINT_V5 = "3df3726c05003c11";
  public static final String EXTEND_UNKNOWN_5 = "3df3726c06003e05";
  public static final String VSTRING_EMPTY = "3df3726c06003e2600";
  public static final String VSTRING_BAD_SPELLING = "3df3726c06003e26026162";
  public static final String VSTRING_V128_AS_BINARY = "3df3726c06003e260476313238";
  public static final String VSTRING_V127_AS_UTF8 = "3df3726c06003e270476313237";
  public static final String EXTEND_V5 = "3df3726c05003e260676312e322e33";
  public static final String CHECKSUM_ON_V5 = "3df3726c85000210787878787878787878787878787878780107";
  public static final String VERSION_7 = "3df3726c0700";
  public static final String PLAIN_V6_7 = "3df3726c060007";
  public static final String PLAIN_V5_7 = "3df3726c050007";
}
