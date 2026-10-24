# Perl security policy

The Perl Encoder and Decoder are Sereal's reference implementation. Malformed
input should fail with a controlled exception. Known decoder crashes, memory
corruption, or bypasses of configured limits must be fixed before release. This
is a maintenance standard, not a guarantee that all malformed input is safe.

The installed module documentation contains the operational guidance:

- [Sereal::Decoder ROBUSTNESS](Decoder/lib/Sereal/Decoder.pm) covers untrusted
  packets, resource limits, authentication policy, and object callbacks.
- [Sereal::Encoder SECURITY](Encoder/lib/Sereal/Encoder.pm) covers callbacks,
  checksum emission, and what the receiving Decoder must verify.

Use `perldoc Sereal::Decoder` and `perldoc Sereal::Encoder` after installation.
In particular, protocol v6 checksums authenticate packets only when the
Decoder has a matching key and enforces verification. The `require_key` policy
rejects unsigned packets; the default policy does not.

The [project-wide policy](../SECURITY.md) explains supported versions and how
to report a vulnerability privately.
