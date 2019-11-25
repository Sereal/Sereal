# Sereal implementation in Python

Basic implementation of the Sereal protocol in Python.

### Usage

    $ from sereal.decoder import SrlDecoder
    $ decoder = SrlDecoder()
    $ decoder.decode(data)

### Decoder behaviour settings

    bin_mode_classic: Specify the binary mode to use when decoding BINARY tags.
        If bin_mode_classic is True the decoder will auto decode all BINARY tags as UTF-8 encoded
        If bin_mode_classic is False the decoder will read BINARY tags verbatim
        The default is True

### Miscellaneous

- Python 3.5+
- No Python encoder
- Not Perl compatible