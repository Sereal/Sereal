# Perl security policy

The Perl Encoder and Decoder are Sereal's reference implementation. Malformed
input should fail with a controlled exception. Known decoder crashes, memory
corruption, or bypasses of configured limits must be fixed before release. This
is a maintenance standard, not a guarantee that all malformed input is safe.

Security claims about handling untrusted data should specify `paranoid` mode
and any option overrides. We assess those claims against the mode and its
documented controls. Use without `paranoid` assumes some trust in the data
being encoded or decoded. Class allowlists narrow object handling but do not
make permitted classes safe.
Future releases may tighten the `paranoid` preset when needed for security;
applications that require a fixed policy should set the relevant options
explicitly.

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
