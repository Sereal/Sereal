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

Decoder limits: the Decoder and the Merger have two limits, where 0 selects
the default and a negative value disables the limit. MaxRecursionDepth
(default DefaultMaxRecursionDepth, 10000, as in perl) bounds the nesting of
values; deeper documents are rejected with ErrMaxRecursionLimit.
MaxUncompressedSize (default DefaultMaxUncompressedSize, 1 GiB) bounds the
size a compressed body may decompress to; larger bodies, and declared
lengths the compressed data cannot hold, are rejected with
ErrMaxUncompressedSize or ErrCorrupt before the memory is allocated.
DecompressDocument always applies the default size limit.
*/
package sereal
