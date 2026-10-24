import os
from io import BytesIO
import struct

from sereal import exception


class SrlDocumentReader(object):
    def __init__(self, byte_str):
        super(SrlDocumentReader, self).__init__()
        self.stream = BytesIO(byte_str)
        self.stream.seek(0, os.SEEK_SET)
        self.length = len(byte_str)

    def _read_unpack(self, fmt):
        '''
        First get the number of bytes that struct will need to unpack. Then
        read those number of bytes from the io stream. The current io stream
        position will advance by the number of bytes read.
        '''
        plen = struct.calcsize(fmt)
        b = self._read_exact(plen)

        value = struct.unpack(fmt, b)[0]
        return value

    def _read_exact(self, n):
        if n > self.remaining():
            raise exception.SrlError('unexpected end of input')
        return self.stream.read(n)

    def remaining(self):
        return self.length - self.stream.tell()

    def tell(self):
        return self.stream.tell()

    def seek(self, offset, wench=os.SEEK_CUR):
        # seek returns the new absolute position
        if not self.stream.seekable():
            return 0
        return self.stream.seek(offset, wench)

    def read_varint(self):
        shift = 0
        result = 0
        while True:
            i = ord(self._read_exact(1))
            result |= (i & 0x7f) << shift
            shift += 7
            if not (i & 0x80):
                break

        return result

    def read_uint32(self):
        fmt = '<I'
        return self._read_unpack(fmt)

    def read_uint8(self):
        fmt = '<B'
        return self._read_unpack(fmt)

    def read_float(self):
        fmt = '<f'
        return self._read_unpack(fmt)

    def read_double(self):
        fmt = '<d'
        return self._read_unpack(fmt)

    def read_str(self, slen, str_encoding='utf-8'):
        val = self._read_exact(slen)
        return bytes.decode(val, encoding=str_encoding)

    def read_bin(self, slen):
        return self._read_exact(slen)
