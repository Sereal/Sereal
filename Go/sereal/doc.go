/*
Package sereal implements the Sereal serialization format

It follows the standard Go Marshal/Unmarshal interface.

For more information on Sereal, please see
http://blog.booking.com/sereal-a-binary-data-serialization-format.html
and
http://github.com/Sereal/Sereal

Protocol version 6 adds the NEG_VARINT and EXTEND (v-string) tags and an
optional keyed SipHash checksum envelope. Use NewEncoderV6 with one of the
Encoder's ChecksumV0Key, ChecksumV1Key or ChecksumV2Key fields to sign a
document, and set the same key and a ChecksumPolicy on the Decoder to verify
it; GetChecksum and ValidateChecksum inspect a document without decoding it.
*/
package sereal
