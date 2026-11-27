# Sereal implementation in Python

Basic implementation of the Sereal protocol in Python.

### Usage

    $ from sereal.decoder import SrlDecoder
    $ decoder = SrlDecoder()
    $ decoder.decode(data)

### Decoder behaviour settings

    object_factory: Function to customize the construction of an object.
        The default function is:
        def _default_object_factory(classname, data):
            return {
                'class': classname,
                'object': data,
            }

    bin_mode_classic: Specify the binary mode to use when decoding BINARY tags.
        If bin_mode_classic is True the decoder will auto decode all BINARY tags as UTF-8 encoded
        If bin_mode_classic is False the decoder will read BINARY tags verbatim as bytes
        The default is True

    max_uncompressed_size: Maximum allowed decompressed body size in bytes for
        compressed documents (0 or None disables the limit).
        The default is 256 MiB (268435456 bytes).

### Miscellaneous

- Python 3.5+
- No Python encoder
- Supports Sereal protocol versions 3 and 4 (including documents encoded by Perl, Java, Go, etc.)
- Compression support:
  - Uncompressed (type 0) and Zlib (type 3) are supported via the Python standard library
  - Zstandard (type 4, protocol v4) is supported via Python 3.14+ `compression.zstd` or the `zstandard` package
  - Incremental Snappy (type 2) is supported if the `snappy` (`python-snappy`) package is installed; legacy Snappy (type 1, protocol v1 only) is rejected
- Protocol versions 1, 2, 5, 6, and 7 (including v6 keyed checksums and `LONG_DOUBLE`) are not supported
- Perl compatibility notes:
  - `BINARY` / `SHORT_BINARY` strings with non-UTF-8 bytes (such as raw bytes or Latin-1 Perl strings) require `bin_mode_classic=False`
  - `OBJECT` / `OBJECTV` / `OBJECT_FREEZE` / `OBJECTV_FREEZE` invoke `object_factory(classname, data)`
  - `REFN` and `WEAKEN` decode transparently to their referent values; `ALIAS` resolves like `REFP`