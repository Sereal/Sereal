# Java security policy

The Java decoders should reject malformed input with controlled errors and
enforce configured decoder limits. Report crashes, unbounded resource use
despite a configured limit, or incorrect checksum handling. The
[Java README](README.md) documents the supported protocol versions and keyed
checksum policies. `DecoderOptions` provides `maxBufferSize`,
`maxRecursionDepth`, `maxStringLength`, `maxNumArrayEntries`, and
`maxNumMapEntries`; check which limits the chosen decoder API applies.
The entry and string limits default to disabled. Choose a recursion limit
appropriate for the JVM stack, since the default can be too high for some
stacks.

Protocol v6 keyed checksums are optional. The default
`VERIFY_IF_AVAILABLE` policy accepts unsigned documents and warns when a
signed document cannot be verified. Configure the matching key and
`REQUIRE_KEY` when every packet must be authenticated. A checksum does not
encrypt data or prevent replay. Decoder, TokenDecoder, and the Jackson parser
have different interfaces; check the options for the API in use.

Java's controls and defaults are not identical to Perl's. See the
[project-wide policy](../SECURITY.md) for reporting and supported-version
guidance.
