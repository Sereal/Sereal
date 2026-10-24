# Security Policy

## Policy and scope

Sereal has implementations in several languages. The Perl implementation is
the reference implementation and follows a strict security policy: malformed
packets should fail with controlled errors, and known decoder crashes, memory
corruption, or bypasses of configured limits must be fixed before release.
This is a maintenance standard, not a guarantee that every malformed packet
is handled correctly.

The policy and available controls for other languages vary with each library's
maturity and the maintainers' available expertise. Their security features
and validation should not be assumed to match Perl's. Each language directory
has a `SECURITY.md` describing its own scope and controls. We investigate
reports of crashes, memory corruption, and limit failures in every
implementation; reproducing inputs help us establish the scope and fix them.

The JavaScript, Python, Ruby, Objective-C, and Erlang implementations are
experimental. Use them with caution; they may contain bugs, including security
bugs. Their language policies describe the controls currently available. We
welcome expert help to maintain and improve these ports.

Successful decoding establishes only that an implementation parsed a packet.
It does not make the resulting values trustworthy. Some implementations can
invoke application code while decoding objects or when decoded values are
destroyed.

Protocol version 6 can carry a keyed checksum in implementations that support
it. Authentication requires a matching secret key and a decoder policy that
rejects unsigned or unverifiable packets. Checksums do not encrypt data or
prevent replay. The available checksum types, resource limits, and object
controls differ by implementation and release.

## Language policies

- [Perl](Perl/SECURITY.md) — reference implementation; its Encoder and Decoder
  POD describes the controls shipped with the modules.
- [Go](Go/SECURITY.md) — decoder limits and protocol v6 checksum policies.
- [Java](Java/SECURITY.md) — decoder options and protocol v6 checksum policies.
- [JavaScript](JavaScript/SECURITY.md) — older protocol support.
- [Python](Python/SECURITY.md) — basic decoder with limited format support.
- [Ruby](ruby/SECURITY.md) — native extension and optional object callbacks.
- [Objective-C](objC/SECURITY.md) — native decoder with older protocol support.
- [Erlang](Erlang/SECURITY.md) — native NIF implementation.

Check the documentation for the implementation and version in use before
relying on a particular security control.

## Supported versions

Security fixes are made in current releases; older releases have no guaranteed
backport schedule. Upgrade decoders before encoders so readers understand the
packets produced during a rollout.

## Reporting a vulnerability

Please report suspected vulnerabilities privately through
[GitHub Security Advisories](https://github.com/Sereal/Sereal/security/advisories/new)
so we can investigate and coordinate a fix before disclosure. Include a
reproducer or minimal packet, the affected implementation, version and
platform, expected and observed behavior, relevant settings, and the suspected
impact when available.

We work with reporters to validate issues, develop fixes, and coordinate
disclosure through the private advisory. Patches and regression tests are
welcome. Ordinary bugs and support questions belong in the
[public issue tracker](https://github.com/Sereal/Sereal/issues).
