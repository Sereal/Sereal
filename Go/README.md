Sereal implementation in Go
===========================

Install
-------

Library:

    $ go get github.com/Sereal/Sereal/Go/sereal

To use C implementations of snappy and zlib instead of pure-Go ones:

    $ go get -tags clibs github.com/Sereal/Sereal/Go/sereal

Zstd frames include the uncompressed content size for compatibility with
Perl's decoder, including small bodies when `CompressionThreshold = 0`
forces compression. Both the pure-Go and `clibs` backends support this.

Hack
----

Run a test suite:

    $ cd $GOPATH/src/github.com/Sereal/Sereal/Go/sereal
    $ make test_all CORPUS_PROTO_VER=3 CORPUS_COMPRESS=SRL_ZLIB

Miscellaneous utils:

    $ cd $GOPATH/src/github.com/Sereal/Sereal/Go/sereal
    $ go run cmd/fuzzer/main.go | head -50

    $ cd $GOPATH/src/github.com/Sereal/Sereal/Go/sereal
    $ perl -Mblib=../../Perl/Encoder/blib -MSereal::Encoder -e'print encode_sereal({foo => [42, "bar"]})' | go run cmd/dsrl/main.go

Decoder limits
--------------

`Decoder` and `Merger` have two limits on untrusted input. For both, the zero
value selects the default and a negative value disables the limit.

- `MaxRecursionDepth` (default `DefaultMaxRecursionDepth`, 10000, as perl's
  `max_recursion_depth`) bounds how deeply values may nest; deeper documents
  fail with `ErrMaxRecursionLimit`.
- `MaxUncompressedSize` (default `DefaultMaxUncompressedSize`, 1 GiB) bounds
  the size a compressed body may decompress to; larger bodies fail with
  `ErrMaxUncompressedSize`. Declared lengths are checked before memory is
  allocated for them. `DecompressDocument` always uses the default limit.

A panic while decoding, merging or inspecting a document, including one raised
by a user `UnmarshalBinary` method, is returned as an error; runtime errors and
panic values that are neither errors nor strings wrap `ErrDecodePanic`.

Protocol v6 and checksums
-------------------------

The decoder reads protocol versions 1 to 6. `NewEncoderV6()` writes version 6,
which encodes negative integers below -16 as `NEG_VARINT` and perl v-strings
(`sereal.PerlVString`) as `EXTEND VSTRING`. A zero-value `Encoder{}` still
writes version 3.

`UNDEF_DEADLY` is the diagnostic extension `EXTEND 255` (bytes `3e ff`).
It is intended primarily for testing decoder behavior, not normal data.
`Decoder.UndefDeadlyPolicy` accepts `"undef"` (default, also `""`), `"warn"`
(decode as undef and warn per encounter), or `"die"` (`ErrUndefDeadlyTag`).
`UndefDeadlyWarningHandler` receives these warnings; nil prints to stderr.
No compliant encoder may emit this tag; the merger rejects it. Extension
ID 0 remains unassigned. The byte sequence inside string data is ordinary data.

Version 6 documents can carry a keyed SipHash checksum envelope. An encoder
signs with exactly one of `ChecksumV0Key` (SipHash-2-4, 64-bit),
`ChecksumV1Key` (SipHash-2-4-128) or `ChecksumV2Key` (SipHash-3-6-128); a
decoder verifies with the key of the document's checksum type:

```go
key := sereal.PrepareChecksumKey([]byte("my secret"), nil)

enc := sereal.NewEncoderV6()
enc.ChecksumV2Key = &key
packet, err := enc.Marshal(value)

dec := sereal.NewDecoder()
dec.ChecksumV2Key = &key
dec.ChecksumPolicy = sereal.ChecksumDie
err = dec.Unmarshal(packet, &decoded)
```

The decoder's `ChecksumPolicy` mirrors perl's `checksum_policy`:

| policy | unsigned document | signed, key for its type | signed, no key or unknown type |
|---|---|---|---|
| `ChecksumVerifyIfAvailable` (default) | accepted | verified | warning, decoded unauthenticated |
| `ChecksumDie` | accepted | verified | error |
| `ChecksumRequireKey` | error | verified | error |
| `ChecksumIgnore` | accepted | not verified | not verified |
| `ChecksumIgnoreWarn` | accepted | warning, not verified | warning, not verified |

Malformed envelope framing is an error under every policy, and a signed
payload that is not consumed exactly is `ErrChecksumPayloadLength`. The
checksum is verified before header user data is decoded and before
decompression. `GetChecksum` and `ValidateChecksum` inspect a packet without
decoding it.

Differences from the perl implementation:

- keys are exactly 16 bytes (`*[16]byte`); perl pads or truncates other
  lengths. Use `PrepareChecksumKey` (perl's `prepare_checksum_key`) to derive
  a key from arbitrary bytes.
- there are no environment or global defaults (`SEREAL_DEFAULT_KEY`,
  `SEREAL_CHECKSUM_POLICY`, `$Sereal::DefaultKey`): keys and policies are set
  per encoder and decoder.
- warnings go to `Decoder.ChecksumWarningHandler`; when it is nil they are
  printed to stderr as `sereal: <message>`, so unauthenticated decoding is
  never silent by default.
- `DecompressDocument` and `Merger.Append` refuse checksummed documents with
  `ErrChecksummedDocument`.
- `require` is not accepted as an alias of `require_key`; use
  `ChecksumRequireKey`.
- the encoder never writes the v5 `YES`/`NO` tags or legacy snappy at
  version 6; the decoder reads them.
- v-strings decode to `*sereal.PerlVString`, which keeps the original
  spelling; perl reconstructs the string value from it.
