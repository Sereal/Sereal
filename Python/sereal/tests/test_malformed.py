import re
import unittest

from sereal.decoder import SrlDecoder
from sereal.exception import SrlError

# every packet starts with the v3 header: magic, version/type byte 0x03
# (protocol 3, raw body) and an empty header suffix (varint 0)
HEADER = '3df3726c0300'


def decode(hex_body, header=HEADER, **settings):
    return SrlDecoder(**settings).decode(bytes.fromhex(header + hex_body))


class TestMalformed(unittest.TestCase):
    def assertRejected(self, hex_body, message, header=HEADER):
        with self.assertRaises(SrlError) as cm:
            decode(hex_body, header=header)
        self.assertIn(message, str(cm.exception))

    def test_valid_document(self):
        # ARRAYREF_2 [1, SHORT_BINARY_3 "abc"]
        self.assertEqual(decode('420163616263'), [1, 'abc'])

    def test_array_count_past_end(self):
        # ARRAY with a count of 5000000 and no elements: used to decode as a
        # list of 5000000 zeros, one per byte read past the end of input
        self.assertRejected('2bc096b102', 'count')

    def test_hash_count_past_end(self):
        # HASH with a count of 1000 and only one byte left
        self.assertRejected('2ae80701', 'count')

    def test_arrayref_truncated(self):
        # ARRAYREF_2 whose second element, STR_UTF8 of length 1, is cut short
        self.assertRejected('42012701', 'unexpected end of input')

    def test_truncated_varint(self):
        # VARINT whose last byte still has the continuation bit set
        self.assertRejected('2080', 'unexpected end of input')

    def test_string_length_past_end(self):
        # STR_UTF8 claiming a huge length with no bytes following
        self.assertRejected('2780809aa6eaafe301', 'unexpected end of input')

    def test_truncated_double(self):
        # DOUBLE with only 3 of its 8 bytes present
        self.assertRejected('23000000', 'unexpected end of input')

    def test_truncated_header_suffix(self):
        # header suffix size 5 with only one suffix byte present
        self.assertRejected('', 'unexpected end of input', header='3df3726c030501')

    def test_empty_body(self):
        self.assertRejected('', 'unexpected end of input')

    def test_deep_nesting(self):
        # 10000 nested ARRAYREF_1 tags around the integer 1
        self.assertRejected('41' * 10000 + '01', 'nesting')

    def test_moderate_nesting(self):
        value = decode('41' * 100 + '01')
        for _ in range(100):
            value = value[0]
        self.assertEqual(value, 1)

    def test_refp_untracked(self):
        # REFP to offset 5, which was never tracked
        self.assertRejected('2905', 'not a tracked item')

    def test_objectv_untracked(self):
        # OBJECTV naming its class by offset 5, which was never tracked
        self.assertRejected('2d0501', 'not a tracked item')

    def test_unhashable_key(self):
        # HASHREF_1 whose key is an ARRAYREF_0
        self.assertRejected('514001', 'hash key')

    def test_bad_utf8(self):
        # STR_UTF8 of length 2 holding bytes that are not UTF-8
        self.assertRejected('2702fffe', 'utf-8')

    def test_regexp_flags_not_a_string(self):
        # REGEXP with pattern "a" and the integer 5 as its flags
        self.assertRejected('31616105', 'regexp')

    def test_regexp_unknown_flag(self):
        # REGEXP with pattern "a" and flags "q"
        self.assertRejected('3161616171', 'regexp')

    def test_regexp_bad_pattern(self):
        # REGEXP with pattern "(" and no flags
        self.assertRejected('31612860', 'regexp')

    def test_regexp_valid(self):
        # REGEXP with pattern "a" and flags "i"
        self.assertEqual(decode('3161616169'), re.compile('a', re.I))


if __name__ == '__main__':
    unittest.main()
