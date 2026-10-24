# JavaScript security policy

This implementation is experimental. Use it with caution, especially with
untrusted input; it may contain bugs, including security bugs.
We welcome expert help to maintain and improve this port.

This implementation supports Sereal protocol versions up to 3, as described
in the [JavaScript README](README.md). It does not provide protocol v6 keyed
checksum verification. A successful decode must not be treated as
authentication of the packet or validation of the resulting application data.

Malformed input should produce a controlled error. Report inputs that crash
the decoder, cause excessive resource use, or produce unexpected values.
This implementation has a different maturity and feature set from the Perl
reference implementation; its security controls should not be assumed to
match Perl's. Apply input size and provenance checks in the calling
application.

See the [project-wide policy](../SECURITY.md) for reporting and
supported-version guidance.
