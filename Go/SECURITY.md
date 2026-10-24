# Go security policy

The Go Decoder and Merger should return errors for malformed input and enforce
their configured limits. Report inputs that cause a panic, excessive resource
use despite a configured limit, or incorrect checksum handling. The
[Go README](README.md) documents `MaxRecursionDepth`, `MaxUncompressedSize`,
and the error behavior of decoding and merging.

Protocol v6 keyed checksums are optional. The default
`ChecksumVerifyIfAvailable` policy accepts unsigned documents and warns when
a signed document cannot be verified. Set a matching key and
`ChecksumRequireKey` when every input must be authenticated. A checksum does
not encrypt a packet or prevent replay. Application-defined `UnmarshalBinary`
methods may execute during decoding; their behavior is outside the packet
parser's control.

Go's controls and defaults are not identical to Perl's. See the
[project-wide policy](../SECURITY.md) for reporting and supported-version
guidance.
