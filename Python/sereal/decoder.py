import os
import re
import zlib

from sereal import constants as const
from sereal import reader
from sereal import exception

try:
    from compression import zstd as _stdlib_zstd
except ImportError:
    _stdlib_zstd = None

try:
    import zstandard as _zstandard
except ImportError:
    _zstandard = None

try:
    import snappy as _snappy
except ImportError:
    _snappy = None


class SrlDecoder(object):
    def __init__(self, object_factory=None, **settings):
        super(SrlDecoder, self).__init__()

        # default decode behaviour settings
        self.settings = {
            const.SETTING_BIN_MODE_CLASSIC: True,
            const.SETTING_MAX_UNCOMPRESSED_SIZE: const.SRL_MAX_UNCOMPRESSED_SIZE,
        }

        for k in self.settings:
            if k in settings:
                self.settings[k] = settings[k]

        max_uncompressed = self.settings[const.SETTING_MAX_UNCOMPRESSED_SIZE]
        if max_uncompressed is not None and max_uncompressed < 0:
            raise ValueError('max_uncompressed_size must be non-negative')

        self.header = {
            'version': None,
            'type': None
        }
        self.reader = None
        self.tracked_items = {}
        self.perl_compatible = False
        self.body_offset = 0
        self.copy_depth = 0
        self.depth = 0
        self.object_factory = object_factory if object_factory is not None else self._default_object_factory

    def decode(self, byte_str):
        self.reader = reader.SrlDocumentReader(byte_str)
        self.tracked_items = {}
        self.copy_depth = 0
        self.depth = 0

        self._decode_header()
        self.body_offset = self.reader.tell()
        return self._decode_body()

    def _decode_header(self):
        magic = self.reader.read_uint32()

        if (hex(magic) != const.SRL_MAGIC_STRING_HIGHBIT_UINT_LE):
            raise exception.SrlError('bad header: invalid magic string')

        doc_version_type = self.reader.read_uint8()
        doc_version = (doc_version_type & 15)
        doc_type = (doc_version_type >> 4) & 15

        if doc_version not in [3, 4]:
            raise exception.SrlError('bad header: unsupported protocol version {}'.format(doc_version))

        if doc_type == const.SRL_ENCODING_RAW or doc_type == const.SRL_ENCODING_ZLIB:
            pass
        elif doc_type == const.SRL_ENCODING_SNAPPY_INCREMENTAL:
            if _snappy is None:
                raise exception.SrlError('bad header: unsupported document type {} (snappy library not installed)'.format(doc_type))
        elif doc_type == const.SRL_ENCODING_ZSTD:
            if doc_version < 4:
                raise exception.SrlError('bad header: unsupported document type {} for protocol version {}'.format(doc_type, doc_version))
            if _stdlib_zstd is None and _zstandard is None:
                raise exception.SrlError('bad header: unsupported document type {} (zstd library not installed)'.format(doc_type))
        else:
            raise exception.SrlError('bad header: unsupported document type {}'.format(doc_type))

        header_suffix_size = self.reader.read_varint()
        if header_suffix_size:
            # Skipping the suffix.
            self.reader.read_bin(header_suffix_size)

        self.header['version'] = doc_version
        self.header['type'] = doc_type

    def _read_compressed_slice(self, compressed_len, codec_name):
        if compressed_len == 0:
            raise exception.SrlError('bad {} payload: empty compressed data'.format(codec_name))
        if compressed_len > self.reader.remaining():
            raise exception.SrlError('bad {} payload: compressed size exceeds remaining input'.format(codec_name))
        return self.reader.read_bin(compressed_len)

    def _check_uncompressed_limit(self, size, codec_name):
        max_size = self.settings[const.SETTING_MAX_UNCOMPRESSED_SIZE]
        if max_size and size > max_size:
            raise exception.SrlError(
                'bad {} payload: uncompressed size {} exceeds limit {}'.format(codec_name, size, max_size)
            )

    def _decompress_zlib(self):
        """Decompress a type-3 Sereal zlib payload (<uncompressed_len><compressed_len><blob>)."""
        uncompressed_len = self.reader.read_varint()
        self._check_uncompressed_limit(uncompressed_len, 'zlib')
        compressed_len = self.reader.read_varint()
        compressed_data = self._read_compressed_slice(compressed_len, 'zlib')
        if uncompressed_len == 0:
            raise exception.SrlError('bad zlib payload: empty uncompressed size')
        try:
            decomp = zlib.decompressobj()
            uncompressed = decomp.decompress(compressed_data, max_length=uncompressed_len)
            if not decomp.eof or decomp.unconsumed_tail or decomp.unused_data:
                raise exception.SrlError('bad zlib payload: stream length mismatch or trailing data')
            uncompressed += decomp.flush()
        except zlib.error as e:
            raise exception.SrlError('bad zlib payload: {}'.format(e))
        if len(uncompressed) != uncompressed_len:
            raise exception.SrlError(
                'bad zlib payload: declared {} bytes, got {} bytes'.format(uncompressed_len, len(uncompressed))
            )
        return uncompressed

    def _decompress_zstd(self):
        """Decompress a type-4 Sereal zstd payload (<compressed_len><blob>)."""
        compressed_len = self.reader.read_varint()
        compressed_data = self._read_compressed_slice(compressed_len, 'zstd')
        if _stdlib_zstd is not None:
            try:
                expected_size = _stdlib_zstd.get_frame_info(compressed_data).decompressed_size
            except _stdlib_zstd.ZstdError as e:
                raise exception.SrlError('bad zstd payload: {}'.format(e))
            if expected_size is None or expected_size == 0:
                raise exception.SrlError('bad zstd payload: missing or empty frame content size')
            self._check_uncompressed_limit(expected_size, 'zstd')
            try:
                decomp = _stdlib_zstd.ZstdDecompressor()
                uncompressed = decomp.decompress(compressed_data, max_length=expected_size)
            except _stdlib_zstd.ZstdError as e:
                raise exception.SrlError('bad zstd payload: {}'.format(e))
            if not decomp.eof or decomp.unused_data or len(uncompressed) != expected_size:
                raise exception.SrlError('bad zstd payload: uncompressed size mismatch')
            return uncompressed

        try:
            expected_size = _zstandard.frame_content_size(compressed_data)
        except _zstandard.ZstdError as e:
            raise exception.SrlError('bad zstd payload: {}'.format(e))
        if expected_size <= 0:
            raise exception.SrlError('bad zstd payload: missing or empty frame content size')
        self._check_uncompressed_limit(expected_size, 'zstd')
        try:
            uncompressed = _zstandard.ZstdDecompressor().decompress(compressed_data, max_output_size=expected_size)
        except _zstandard.ZstdError as e:
            raise exception.SrlError('bad zstd payload: {}'.format(e))
        if len(uncompressed) != expected_size:
            raise exception.SrlError('bad zstd payload: uncompressed size mismatch')
        return uncompressed

    def _decompress_snappy(self):
        """Decompress a type-2 Sereal incremental snappy payload (<compressed_len><blob>)."""
        compressed_len = self.reader.read_varint()
        compressed_data = self._read_compressed_slice(compressed_len, 'snappy')
        try:
            uncompressed = _snappy.uncompress(compressed_data)
        except Exception as e:
            raise exception.SrlError('bad snappy payload: {}'.format(e))
        self._check_uncompressed_limit(len(uncompressed), 'snappy')
        return uncompressed

    def _decode_body(self):
        doc_type = self.header['type']
        if doc_type != const.SRL_ENCODING_RAW:
            if doc_type == const.SRL_ENCODING_ZLIB:
                uncompressed = self._decompress_zlib()
            elif doc_type == const.SRL_ENCODING_ZSTD:
                uncompressed = self._decompress_zstd()
            elif doc_type == const.SRL_ENCODING_SNAPPY_INCREMENTAL:
                uncompressed = self._decompress_snappy()
            else:
                raise exception.SrlError('bad header: unsupported document type {}'.format(doc_type))
            # Offsets inside a decompressed body are 1-based from the start of the decompressed buffer.
            self.reader = reader.SrlDocumentReader(uncompressed)
            self.body_offset = 0
        return self._decode_bytes()

    def _decode_tag(self, tag):
        if tag >= const.SRL_TYPE_POS_0 and tag < const.SRL_TYPE_POS_0 + 16:
            return int(tag)

        elif tag >= const.SRL_NEG_16 and tag < const.SRL_NEG_16 + 16:
            return int(tag) - 32

        elif tag == const.SRL_TYPE_FLOAT:
            return self.reader.read_float()

        elif tag == const.SRL_TYPE_DOUBLE:
            return self.reader.read_double()

        elif tag == const.SRL_TYPE_VARINT:
            return self.reader.read_varint()

        elif tag == const.SRL_TYPE_ZIGZAG:
            return self._decode_zigzag()

        elif tag == const.SRL_TYPE_UNDEF:
            return None

        elif tag == const.SRL_TYPE_BINARY:
            return self._decode_binary()

        elif tag == const.SRL_TYPE_STR_UTF8:
            return self._decode_str_utf8()

        elif tag == const.SRL_TYPE_REFN:
            return self._decode_refn(tag)

        elif tag == const.SRL_TYPE_REFP:
            return self._decode_refp()

        elif tag == const.SRL_TYPE_HASH:
            ln = self.reader.read_varint()
            return self._decode_hash(ln)

        elif tag == const.SRL_TYPE_ARRAY:
            ln = self.reader.read_varint()
            return self._decode_array(ln)

        elif tag == const.SRL_TYPE_OBJECT:
            return self._decode_object()

        elif tag == const.SRL_TYPE_OBJECTV:
            return self._decode_objectv()

        elif tag == const.SRL_TYPE_ALIAS:
            return self._decode_refp()

        elif tag == const.SRL_TYPE_WEAKEN:
            return self._decode_bytes()

        elif tag == const.SRL_TYPE_OBJECT_FREEZE:
            return self._decode_object()

        elif tag == const.SRL_TYPE_OBJECTV_FREEZE:
            return self._decode_objectv()

        elif tag == const.SRL_TYPE_REGEXP:
            return self._decode_regexp()

        elif tag == const.SRL_TYPE_CANONICAL_UNDEF:
            return None

        elif tag == const.SRL_TYPE_FALSE:
            return False

        elif tag == const.SRL_TYPE_TRUE:
            return True

        elif tag == const.SRL_TYPE_COPY:
            return self._get_copy()

        elif tag >= const.SRL_TYPE_ARRAYREF_0 and tag < const.SRL_TYPE_ARRAYREF_0 + 16:
            ln = tag & 15
            return self._decode_array(ln)

        elif tag >= const.SRL_TYPE_HASHREF_0 and tag < const.SRL_TYPE_HASHREF_0 + 16:
            ln = tag & 15
            return self._decode_hash(ln)

        elif tag >= const.SRL_TYPE_SHORT_BINARY_0 and tag < const.SRL_TYPE_SHORT_BINARY_0 + 32:
            # Note: self.reader.read_str() automatically decodes as utf-8.
            # Thus, this returns a string, instead of returning bytes.
            return self._decode_short_binary(tag)

        else:
            raise exception.SrlError('bad tag: unsupported tag {}'.format(tag))

    def _decode_bytes(self, force_track_pos=False):
        track_pos = None
        tag = self.reader.read_uint8()
        while (tag & ~const.SRL_TRACK_BIT) == const.SRL_TYPE_PAD:
            tag = self.reader.read_uint8()

        if (tag & const.SRL_TRACK_BIT) != 0 or force_track_pos:
            tag = tag & ~const.SRL_TRACK_BIT
            track_pos = self.reader.tell() - self.body_offset

        self.depth += 1
        if self.depth > const.SRL_MAX_RECURSION_DEPTH:
            raise exception.SrlError('bad document: nesting deeper than {}'.format(const.SRL_MAX_RECURSION_DEPTH))
        item = self._decode_tag(tag)
        self.depth -= 1

        return self._track_item(track_pos, item)

    def _get_copy(self):
        if self.copy_depth > 0:
            raise exception.SrlError('bad nested copy tag: recursive copy tag found')

        tag_pos = self.reader.tell() - 1
        copy_pos = self.reader.read_varint()
        copy_pos += self.body_offset-1

        # offsets are 1-based from the body start and must point before the tag
        if copy_pos < self.body_offset or copy_pos >= tag_pos:
            raise exception.SrlError('bad copy tag: offset {} is not before the copy tag'.format(copy_pos - self.body_offset + 1))

        curr_pos = self.reader.tell()

        self.reader.seek(copy_pos, os.SEEK_SET)
        self.copy_depth += 1
        copy = self._decode_bytes()
        self.copy_depth -= 1
        self.reader.seek(curr_pos, os.SEEK_SET)

        return copy

    def _decode_array(self, ln):
        # every element takes at least one byte
        if ln > self.reader.remaining():
            raise exception.SrlError('bad array: count {} exceeds remaining input'.format(ln))
        a = []

        for i in range(0, ln):
            a.append(self._decode_bytes())

        return a

    def _decode_hash(self, ln):
        # every key and every value takes at least one byte
        if ln * 2 > self.reader.remaining():
            raise exception.SrlError('bad hash: count {} exceeds remaining input'.format(ln))
        h = {}

        for i in range(0, ln):
            key = self._decode_bytes()
            try:
                hash(key)
            except TypeError:
                raise exception.SrlError('bad hash key: unhashable {}'.format(type(key).__name__))
            h[key] = self._decode_bytes()

        return h

    def _decode_object(self):
        # Saving the classname in case an OBJECTV refers to it later.
        name = self._decode_bytes(force_track_pos=True)
        data = self._decode_bytes()
        return self.object_factory(classname=name, data=data)

    def _decode_objectv(self):
        key = self.reader.read_varint()
        name = self._get_tracked_item(key)
        data = self._decode_bytes()
        return self.object_factory(classname=name, data=data)

    @staticmethod
    def _default_object_factory(classname, data):
        return {
            'class': classname,
            'object': data,
        }

    def _decode_zigzag(self):
        uv = self.reader.read_varint()
        iv = (uv >> 1) ^ (-(uv & 1))
        return iv

    def _decode_str_utf8(self):
        ln = self.reader.read_varint()
        return self.reader.read_str(ln)

    def _decode_regexp(self):
        pattern = self._decode_bytes()
        flags = self._decode_bytes()
        if isinstance(flags, bytes):
            flags = flags.decode('latin-1')
        if not isinstance(flags, str):
            raise exception.SrlError('bad regexp: flags are not a string')
        python_flags = 0
        for f in flags:
            if f == 'p':
                continue
            upper_f = f.upper()
            if upper_f not in re.RegexFlag.__members__:
                raise exception.SrlError('bad regexp: unsupported flag {!r}'.format(f))
            python_flags |= re.RegexFlag[upper_f]
        try:
            return re.compile(pattern, python_flags)
        except (re.error, TypeError, ValueError) as e:
            raise exception.SrlError('bad regexp: {}'.format(e))

    def _decode_binary(self):
        return self._read_binary_payload(self.reader.read_varint())

    def _decode_short_binary(self, tag):
        return self._read_binary_payload(tag & const.SRL_SHORT_BINARY_LEN)

    def _read_binary_payload(self, ln):
        if self.settings[const.SETTING_BIN_MODE_CLASSIC]:
            return self.reader.read_str(ln)
        return self.reader.read_bin(ln)

    def _decode_refn(self, tag):
        return self._decode_bytes()

    def _track_item(self, track_pos, item):
        if track_pos is None or self.copy_depth > 0:
            return item
        self.tracked_items[track_pos] = item
        return item

    def _get_tracked_item(self, key):
        if key not in self.tracked_items:
            raise exception.SrlError('bad offset: {} is not a tracked item'.format(key))
        return self.tracked_items[key]

    def _decode_refp(self):
        key = self.reader.read_varint()
        return self._get_tracked_item(key)
