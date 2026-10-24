# Python security policy

This implementation is experimental. Use it with caution, especially with
untrusted input; it may contain bugs, including security bugs.
We welcome expert help to maintain and improve this port.

This is a basic decoder for uncompressed protocol versions 3 and 4. The
[Python README](README.md) documents its supported input and the
`object_factory` callback. It does not provide protocol v6 keyed checksum
verification; a decoded packet has not thereby been authenticated.

Malformed input should raise a controlled error. Report inputs that crash
the decoder, cause excessive resource use, or bypass documented format
restrictions. A configured `object_factory` runs application code and must
be treated as part of the caller's trust boundary. This implementation's
controls and validation differ from the Perl reference implementation;
applications should bound and validate input according to their needs.

See the [project-wide policy](../SECURITY.md) for reporting and
supported-version guidance.
