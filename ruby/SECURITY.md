# Ruby security policy

This implementation is experimental. Use it with caution, especially with
untrusted input; it may contain bugs, including security bugs.
We welcome expert help to maintain and improve this port.

The Ruby implementation uses a native extension for decoding. Malformed
input should not crash the Ruby process or corrupt memory; report any packet
that does. Its feature set and validation differ from the Perl reference
implementation. The [Ruby README](README.md) describes supported formats,
streaming, and the optional `THAW` behavior.

When the `THAW` option is enabled, decoding can invoke application methods.
Do not treat a successful decode as authentication or as proof that the
resulting object is trustworthy. This implementation does not document
protocol v6 keyed checksum verification; applications requiring authenticated
input must provide it outside this decoder.

See the [project-wide policy](../SECURITY.md) for reporting and
supported-version guidance.
