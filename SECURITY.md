# Security Policy

## Decoding and trust

We work to harden Sereal against malformed packets and malicious input, and
we treat vulnerabilities in the decoder as defects to investigate and fix.
This is a security goal, not a guarantee that decoding arbitrary input is safe.
Users are expected to take prudent measures to restrict decoding to trusted
sources and to protect packets against tampering in transit and storage.

Successful decoding does not establish that data is trustworthy. Validate the
decoded values for your application, set appropriate decoder size and recursion
limits, and apply process-level CPU and memory limits where needed. Object
decoding can invoke application code, including THAW methods and destructors.
Reject objects when they are not required; otherwise restrict the classes and
callbacks your application permits.

## Packet authentication

The Perl implementation provides optional keyed checksums in protocol version 6
for fast packet authentication: SipHash-2-4 with a 64-bit authentication tag
(`checksum_v0_key`) or SipHash-3-6-128 with a 128-bit authentication tag
(`checksum_v1_key`). Both use a 128-bit secret key. These are message
authentication codes (MACs), not public-key signatures. Tag size alone is not
a guarantee of an equivalent overall security level.

Use a random 16-byte secret key shared only with trusted producers and consumers.
Authentication does not provide encryption or replay protection, and it does
not protect against a malicious or compromised key holder.

Verification must be enforced by the receiving application. Configure the
matching decoder key and `checksum_policy => 'die'` to reject checksummed
packets that cannot be verified. `SEREAL_CHECKSUM_POLICY=die` sets this
default for newly created decoders; explicit decoder options take precedence.
Use `checksum_policy => 'require'`, or set `SEREAL_CHECKSUM_POLICY=require`,
to require a key for encoding and apply `die` verification rules when decoding.
Both strict policies require exactly 16 key bytes. The shared
`$Sereal::DefaultKey` can supply the key to both modules. On the first module
load, `SEREAL_DEFAULT_KEY` initializes it from 32 hexadecimal characters; other
input is normalized with `prepare_checksum_key()` and a warning. Code may set
or localize the package variable for new objects. Normalization does not add
entropy. Explicit key options take precedence over the default.
The built-in default policy warns and continues when
no matching key is available. **Even with `checksum_policy => 'die'`, ordinary
packets without a checksum are accepted.** Applications that require
authentication must also reject unprotected packet types at their input
boundary. See the Sereal::Decoder documentation for the complete option details.

If your application requires stronger assurances, confidentiality, replay
protection, or interoperability with implementations that lack these checksums,
protect Sereal payloads with an established external MAC or authenticated
encryption envelope appropriate to your requirements. Verify that protection
before passing the payload to Sereal. An unkeyed checksum alone does not
authenticate the source or prevent deliberate tampering.

Checksum generation and verification add CPU work proportional to the protected
packet size. The decoder's policy determines whether it verifies the MAC and
whether it emits warnings; warning handling and logging also have a cost.
Benchmark representative workloads with the authentication policy you intend
to deploy. Skipping verification removes authentication, so performance tuning
must preserve your application's trust requirements.

## Supported versions

Use the latest release of the Sereal implementation you deploy. Upgrade the
decoder before upgrading the encoder so that it can read the encoder's output.
Security fixes are made in current releases; do not assume older releases
receive backported fixes. Check which protocol versions and security options
your chosen implementation supports.

## Reporting a vulnerability

Please report suspected vulnerabilities privately using
[Report a vulnerability](https://github.com/Sereal/Sereal/security/advisories/new)
on the Sereal repository's Security page. Follow
[GitHub's private vulnerability reporting guidance](https://docs.github.com/en/code-security/how-tos/report-and-fix-vulnerabilities/report-privately).
Do not put exploit details or sensitive data in a public issue before a fix
and disclosure have been coordinated.

Include the affected Sereal implementation and version, Perl or other runtime
version, platform, reproduction steps or a minimal test packet, and the expected
and observed behavior. Explain the security impact and any relevant decoder
options. Patches and regression tests are welcome. Share sensitive reproduction
material only through the private report.

We aim to respond promptly and work with reporters to validate the issue,
develop a fix, and coordinate disclosure through GitHub Security Advisories.
We cannot guarantee a response or resolution deadline. Ordinary bugs and
support questions belong in the
[public issue tracker](https://github.com/Sereal/Sereal/issues).
