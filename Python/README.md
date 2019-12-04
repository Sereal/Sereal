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
        If bin_mode_classic is True, the decoder will auto decode all BINARY tags as UTF-8 encoded.
        If bin_mode_classic is False, the decoder will read BINARY tags verbatim.
        The default is True.

    re_bytes: Specify if the regular expressions will work with strings or with bytes.
        If re_bytes is True, the decoder will compile the regular expression Pattern as bytes.
        If re_bytes is False, the decoder will compile the regular expression Pattern as str.
        The default is False.

### Miscellaneous

- Python 3.5+
- No Python encoder
- Not Perl compatible
