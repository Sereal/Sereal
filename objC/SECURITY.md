# Objective-C security policy

This implementation is experimental. Use it with caution, especially with
untrusted input; it may contain bugs, including security bugs.
We welcome expert help to maintain and improve this port.

This implementation uses Objective-C and C code to decode packets. Malformed
input should be rejected without crashing or corrupting memory; report any
packet that violates this. The decoder has a recursion limit, but its format
coverage and validation are not identical to the Perl reference
implementation. The encoder writes protocol version 2 and this
implementation does not provide protocol v6 keyed checksum verification.

A successful decode does not authenticate a packet or validate the resulting
application values. Set application-level input size and provenance checks
where needed. macOS/Xcode validation is required for changes to this
implementation.

See the [project-wide policy](../SECURITY.md) for reporting and
supported-version guidance.
