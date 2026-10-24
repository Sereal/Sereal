# Erlang security policy

This implementation is experimental. Use it with caution, especially with
untrusted input; it may contain bugs, including security bugs.
We welcome expert help to maintain and improve this port.

The Erlang implementation uses native NIFs, including C decoder code. A
memory-safety error in a NIF can affect the whole Erlang VM. Report malformed
packets that crash the VM, corrupt memory, or cause excessive resource use.

This is an older implementation with different format coverage and validation
from the Perl reference implementation. It does not provide protocol v6 keyed
checksum verification. Do not treat a successful decode as authentication of
the packet; applications handling untrusted packets should apply their own
input bounds and provenance checks.

See the [project-wide policy](../SECURITY.md) for reporting and
supported-version guidance.
