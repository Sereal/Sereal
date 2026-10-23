Sereal implementation in Java
=============================

Java8 required.

Hack
----

Run the basic Java test suite

    $ mvn test

or

    $ make test

Run the full compatibility test suite (builds and uses the Perl Sereal
implementation)

    $ make compat CORPUS_PROTO_VER=[1|2|3|4|5|6] CORPUS_COMPRESS=[SRL_UNCOMPRESSED|SRL_SNAPPY|SRL_ZLIB|SRL_ZSTD]

The compatibility suite needs the Perl module Test::LongString (imported by
Sereal::TestSet).

Run the benchmarks:

    $ mvn -P benchmarks clean package
    # use java -jar benchmarks/target/benchmarks.jar -h for detailed usage
    $ java -jar benchmarks/target/benchmarks.jar

Protocol v6 and checksums
-------------------------

The decoders (`Decoder`, `TokenDecoder` and the jackson `SerealParser`)
accept protocol versions 1 to 6. The encoders default to protocol version 4;
set `EncoderOptions.protocolVersion(5)` or `(6)` to emit newer documents.

New values:

* `PerlVString` (EXTEND VSTRING, v6): a Perl v-string such as `v1.2.3`,
  carried as its spelling. `TokenDecoder` returns the `VSTRING` token and
  `TokenEncoder.appendVString()` writes one; jackson parses it as a string.
* `Float128` (FLOAT_128, v5): the 16 raw bytes of a quadmath float.
  `TokenDecoder` returns the `FLOAT128` token; jackson exposes it as an
  embedded object.
* YES/NO (v5) decode as `true`/`false`; NEG_VARINT (v6) decodes as a `Long`.
  At protocol version 6 the encoders write integers below -16 as NEG_VARINT.

### Keyed checksums

A v6 document can carry a keyed SipHash checksum ("envelope") that the
decoder verifies before decompressing the body or exposing the header. There
are three checksum types, each with its own 16-byte key option:

| option          | algorithm            | tag      |
|-----------------|----------------------|----------|
| `checksumV0Key` | SipHash-2-4          | 8 bytes  |
| `checksumV1Key` | SipHash-2-4-128      | 16 bytes |
| `checksumV2Key` | SipHash-3-6-128      | 16 bytes |

An encoder takes exactly one key, which selects the checksum type it
emits; keys require protocol version 6. A decoder can hold a key for each
type. `DecoderOptions.checksumPolicy()` decides what happens:

| policy                          | unsigned document | signed, key for its type | signed, no key / unknown type |
|---------------------------------|-------------------|--------------------------|-------------------------------|
| `VERIFY_IF_AVAILABLE` (default) | accepted          | verified                 | warning, decoded unauthenticated |
| `DIE`                           | accepted          | verified                 | error                         |
| `REQUIRE_KEY`                   | error             | verified                 | error                         |
| `IGNORE`                        | accepted          | not verified             | not verified                  |
| `IGNORE_WARN`                   | accepted          | warning, not verified    | warning, not verified         |

A checksum mismatch is always an error when the tag is verified. `REQUIRE_KEY`
needs at least one key at construction time (decoder and encoder). Warnings
go to `checksumWarningHandler()`, which by default prints `Sereal: <message>`
to standard error, as Perl's `warn` does.

```java
byte[] key = SerealChecksum.prepareKey(secret, null); // 16 bytes, same as Perl's prepare_checksum_key

Encoder encoder = new Encoder(new EncoderOptions()
    .protocolVersion(6)
    .checksumV2Key(key));
byte[] signed = encoder.write(value).getData();

Decoder decoder = new Decoder(new DecoderOptions()
    .checksumV2Key(key)
    .checksumPolicy(SerealChecksum.Policy.DIE));
decoder.setData(signed);
Object decoded = decoder.decode();
```

`SerealChecksum.getChecksum()` returns a document's tag without verifying it,
and `SerealChecksum.validateChecksum()` verifies a document without decoding
it.

Differences from the Perl implementation:

* keys must be exactly 16 bytes (Perl pads or truncates with a warning);
  derive one with `SerealChecksum.prepareKey()`;
* there are no environment defaults (`SEREAL_DEFAULT_KEY`,
  `SEREAL_CHECKSUM_POLICY`, `$Sereal::DefaultKey`): keys and policy are set
  per encoder/decoder, and Perl's `require` alias for `require_key` does not
  exist;
* warnings go through a `Consumer<String>` instead of `warn`;
* the encoders never emit YES/NO or legacy (non-incremental) Snappy, but the
  decoders read them;
* `SerealGenerator` (jackson) does not write `PerlVString` or `Float128`.
