import os
import re

from functools import reduce

from sereal import constants as const
from sereal import reader
from sereal import exception


class SrlDecoder(object):
    def __init__(self, object_factory=None, bin_mode_classic=True, re_bytes=False):
        super(SrlDecoder, self).__init__()

        self.header = {
            'version': None,
            'type': None
        }
        self.reader = None
        self.tracked_items = {}
        self.perl_compatible = False
        self.body_offset = 0
        self.copy_depth = 0
        self.object_factory = object_factory if object_factory is not None else self._default_object_factory
        self.bin_mode_classic = bin_mode_classic
        self.re_bytes = re_bytes

    def decode(self, byte_str):
        self.reader = reader.SrlDocumentReader(byte_str)

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

        if doc_type < 0 or doc_type > 3:
            raise exception.SrlError('bad header: unsupported document type {}'.format(doc_type))

        header_suffix_size = self.reader.read_varint()
        if header_suffix_size:
            # Skipping the suffix.
            self.reader._read_unpack('{0}s'.format(header_suffix_size))

        self.header['version'] = doc_version
        self.header['type'] = doc_type

    def _decode_body(self):
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

        elif tag == const.SRL_TYPE_REGEXP:
            pattern = self._decode_bytes()
            flags = self._decode_bytes()
            if not self.bin_mode_classic:
                flags = flags.decode('utf-8')
            if not self.bin_mode_classic and not self.re_bytes:
                pattern = pattern.decode('utf-8')
            elif self.bin_mode_classic and self.re_bytes:
                pattern = pattern.encode('utf-8')
            python_flags = reduce(lambda x, y: x | re.RegexFlag[str(y).upper()], flags, 0)
            return re.compile(pattern, python_flags)

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

        if (tag & const.SRL_TRACK_BIT) != 0 or force_track_pos:
            tag = tag & ~const.SRL_TRACK_BIT
            track_pos = self.reader.tell() - self.body_offset

        return self._track_item(track_pos, self._decode_tag(tag))

    def _get_copy(self):
        if self.copy_depth > 0:
            raise exception.SrlError('bad nested copy tag: recursive copy tag found')

        copy_pos = self.reader.read_varint()
        copy_pos += self.body_offset-1

        curr_pos = self.reader.tell()

        self.reader.seek(copy_pos, os.SEEK_SET)
        self.copy_depth += 1
        copy = self._decode_bytes()
        self.copy_depth -= 1
        self.reader.seek(curr_pos, os.SEEK_SET)

        return copy

    def _decode_array(self, ln):
        a = []

        for i in range(0, ln):
            a.append(self._decode_bytes())

        return a

    def _decode_hash(self, ln):
        h = {}

        for i in range(0, ln):
            key = self._decode_bytes()
            h[key] = self._decode_bytes()

        return h

    def _decode_object(self):
        # Saving the classname in case an OBJECTV refers to it later.
        name = self._decode_bytes(force_track_pos=True)
        data = self._decode_bytes()
        return self.object_factory(classname=name, data=data)

    def _decode_objectv(self):
        key = self.reader.read_varint()
        name = self.tracked_items[key]
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

    def _decode_binary(self):
        ln = self.reader.read_varint()
        if self.bin_mode_classic:
            return self.reader.read_str(ln)
        else:
            return self.reader.read_bin(ln)

    def _decode_short_binary(self, tag):
        ln = tag & const.SRL_SHORT_BINARY_LEN
        if self.bin_mode_classic:
            return self.reader.read_str(ln)
        else:
            return self.reader.read_bin(ln)

    def _decode_refn(self, tag):
        return self._decode_bytes()

    def _track_item(self, track_pos, item):
        if track_pos is None:
            return item
        self.tracked_items[track_pos] = item
        return item

    def _decode_refp(self):
        key = self.reader.read_varint()
        return self.tracked_items[key]
