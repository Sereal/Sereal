# Floating-point test corpus

This guide is for platform reporters collecting samples and maintainers
updating Decoder test inputs. Library users do not need to generate a corpus.
The stored corpus lets Decoder tests run without a matching Encoder.

## Collecting a platform sample

Use the branch linked in the issue or pull request. Build and test Encoder
and Decoder with the Perl you want to report on. From `Perl/Decoder`, run:

    perl -Mblib=../Encoder -Mblib author_tools/collect_float_corpus.pl --output /tmp/sereal-floats
    tar -C /tmp -czf sereal-floats.tar.gz sereal-floats

The output directory must not already exist. Attach the archive and test
log to the issue or pull request. It contains the packets (`corpus.pack`), producer details and local
results (`producer.json`), and Perl's build configuration (`perl-config.txt`).
The checked-in corpus is not changed. Collection records errors so samples
can still be gathered from a failing platform; it does not replace running
the test suite.

If a maintainer requests native long-double samples, make a separate capture
with `--include-native`:

    perl -Mblib=../Encoder -Mblib author_tools/collect_float_corpus.pl --include-native --output /tmp/sereal-native-floats

Native output on an unidentified layout may include unrelated process memory
in padding bytes. Include these samples only when appropriate to share them.
Known padding is cleared by the Encoder. Without this flag, the collector
skips native v7 output and skips legacy v6 output on long-double NV builds.
On ordinary double and quadmath builds, selecting native output has no effect.
Use `--label NAME` to distinguish builds if needed.

## Maintaining the stored groups

The corpus contains three kinds of input:

- `roundtrip-v1-*` through `roundtrip-v6-*`: captures from a little-endian,
  64-bit Perl with double NVs. Tests read them with
  `legacy_float_byteorder => 'little'` regardless of the test host. The
  `roundtrip-v7-*` groups cover portable v7 output.
- `float-wire-little` and `float-wire-big`: independently specified IEEE
  bytes, including rounding boundaries and signed zero. These test decoding
  without relying on Encoder output.
- `native-long-double-v7`: hand-built layout examples for binary64, x87,
  binary128, and IBM double-double. Each encodes exact `-1/8`. They test
  layout acceptance and rejection, not the output of every native platform.

Keep new platform captures separate from existing groups, with their producer
configuration and expected values. Review the recorded errors and precision
limits before importing them. A local round trip alone does not establish
that another build can read the packets. Unknown native layouts provide only
same-build value round-trip coverage.

## Fedora s390x capture

`capture-s390x-linux-thread-multi-double-8-legacy-v6` and
`capture-s390x-linux-thread-multi-double-8-portable-v7` were supplied by
pghmcfc in [issue #337](https://github.com/Sereal/Sereal/issues/337#issuecomment-6607065989).
They were collected on 2026-11-03 with Fedora s390x, Perl 5.44.0
(`s390x-linux-thread-multi`, 8-byte double NVs, big-endian byte order),
and Encoder/Decoder 7.000001 at commit `f2afe4ef516836d04f0b0f37b0d771a677cba1eb`.
All producer checkouts were reported clean. Native output was not collected.

Each group contains six values: 0.5, -1.5, 0.1, 1/3, negative zero, and the
smallest positive binary64 subnormal. Expectations for 0.1 and 1/3 use the
producer's binary64 precision even when tests run on wider NV builds.
`107_float_byteorder.t` checks values, signed zero, and wire bytes. Encoder
tests also compare local v7 output with the captured portable packets.

The original archive includes producer configuration, recipes, and local
results. Its SHA-256 is
`baaaafa85baa7cc8540046d9d22244f53e37f0030f9c8685bdaf6446857d3bd9`.
Preserve these groups as producer captures; the generators do not recreate them.

## Regenerating groups

Use a disposable checkout and build both modules with the Perl running the
generator. From `Perl/Decoder`, regenerate the independent wire examples with:

    perl -Mblib author_tools/generate_float_corpus.pl

Regenerate the portable v7 round-trip baseline with:

    perl -Mblib=../Encoder -Mblib author_tools/generate_roundtrip_corpus.pl 7

The round-trip generator replaces the selected protocol groups; without an
argument it selects all supported versions. Preserve the v1–v6 captures unless
you intend to replace those historical inputs. Their replacement must match
the documented producer configuration. The generator checks legacy byte
order, but that check alone does not establish matching NV format or size.
Inspect the changed groups before keeping the regenerated corpus.
