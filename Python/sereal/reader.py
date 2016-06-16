import os
from io import BytesIO
import struct

class SrlDocumentReader(object):
    def __init__(self, byte_str):
        super(SrlDocumentReader, self).__init__()
        self.stream = BytesIO(byte_str)
        self.stream.seek(0, os.SEEK_SET)

    def _read_unpack(self, fmt):
        '''
        First get the number of bytes that struct will need to unpack. Then
        read those number of bytes from the io stream. The current io stream
        position will advance by the number of bytes read.
        '''
        plen = struct.calcsize(fmt)
        b = self.stream.read(plen)

        if (plen != len(b)):
            return 0

        value = struct.unpack(fmt, b)[0]
        return value

    def tell(self):
        return self.stream.tell()

    def seek(self, offset, wench = os.SEEK_CUR):
        # seek returns the new absolute position
        if not self.stream.seekable():
            return 0
        return self.stream.seek(offset, wench)

    def read_varint(self):
        shift = 0
        result = 0
        while True:
            i = ord(self.stream.read(1))
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

    def read_str(self, slen):
        fmt = '{0}s'.format(slen)
        val = self._read_unpack(fmt)
        return bytes.decode(val, encoding='utf-8')
