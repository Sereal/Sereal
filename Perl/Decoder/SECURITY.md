# Security Policy

## Decoding and trust

We work to make Sereal reject malformed packets safely, and we treat decoder
crashes, memory corruption and failures to enforce configured limits as defects
to investigate and fix. We cannot guarantee that every malformed packet is
handled correctly, and reports of inputs that expose these problems are welcome.

Successful decoding does not establish that the data is trustworthy. In
particular, decoding Perl objects can invoke application code through THAW
methods and destructors, even when the packet is well formed. Decoder size and
recursion limits constrain deserialization; they do not limit the runtime or
memory use of those callbacks.

## Packet authentication

The Perl implementation provides optional keyed checksums in protocol version 6
for packet authentication: [SipHash-2-4](https://github.com/veorq/SipHash#variants)
with a 64-bit tag (`checksum_v0_key`),
SipHash-2-4-128 with a 128-bit tag (`checksum_v1_key`), or SipHash-3-6-128 with a
128-bit tag (`checksum_v2_key`). These are message authentication codes (MACs),
and all use a 128-bit secret key. The MAC covers
the packet header and encoded payload, including compression framing, and
verification precedes decompression.

A valid tag establishes that the packet was produced by a holder of the key
and has not been altered. It provides neither confidentiality nor replay
protection, and cannot distinguish a trusted producer from a compromised key
holder.

Verification depends on the decoder's checksum policy. The default verifies
when a matching key is available and otherwise warns and continues without
authentication. The `die` policy rejects protected packets with an unsupported
checksum type, a missing key for that checksum type, or an invalid tag.
The `require_key` policy requires a configured key on both Encoder and Decoder.
Encoding always produces a checksummed packet; decoding rejects packets without
checksums and fails if verification cannot succeed. Both strict policies require
all configured keys to be exactly 16 bytes. `require` is an alias for `require_key`.

Encoder and Decoder keys are configured independently. `$Sereal::DefaultKey`
provides a shared default when explicit key options are absent. The policy can
be set explicitly or through `SEREAL_CHECKSUM_POLICY`; explicit options take
precedence. Policies other than `require_key` still accept packets without a
checksum.
The Encoder and Decoder documentation describes key configuration and the
remaining policy options.

Generating or verifying a tag makes an additional pass over the protected
bytes.

## Supported versions

Security fixes are made in current releases; older releases do not have a
guaranteed backport schedule. Decoder upgrades precede Encoder upgrades so
that readers understand the packets produced during a rollout. Protocol and
authentication support differ between language implementations.

## Reporting a vulnerability

Please report suspected vulnerabilities privately through
[GitHub Security Advisories](https://github.com/Sereal/Sereal/security/advisories/new)
so we can investigate and coordinate a fix before disclosure. A reproducer or
minimal packet, together with the affected implementation, version and platform,
helps us reproduce the failure. Expected and observed behavior, relevant decoder
settings and the suspected impact help establish its scope.

We work with reporters to validate issues, develop fixes and coordinate
disclosure through the private advisory. Patches and regression tests are welcome.
Ordinary bugs and support questions belong in the
[public issue tracker](https://github.com/Sereal/Sereal/issues).
