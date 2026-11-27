import re
import unittest
from unittest import mock
import zlib

from sereal import decoder as decoder_mod
from sereal.decoder import SrlDecoder
from sereal.exception import SrlError


def _encode_varint(n):
    out = bytearray()
    while True:
        byte = n & 0x7F
        n >>= 7
        if n:
            out.append(byte | 0x80)
        else:
            out.append(byte)
            break
    return bytes(out)


def _wrap_packet(doc_version, doc_type, body, header_suffix=b''):
    magic = b'=\xf3rl'
    vt = bytes([((doc_type & 0x0F) << 4) | (doc_version & 0x0F)])
    suffix = _encode_varint(len(header_suffix)) + header_suffix
    return magic + vt + suffix + body


def _wrap_zlib(doc_version, raw_body, header_suffix=b'', declared_len=None):
    compressed = zlib.compress(raw_body)
    if declared_len is None:
        declared_len = len(raw_body)
    payload = _encode_varint(declared_len) + _encode_varint(len(compressed)) + compressed
    return _wrap_packet(doc_version, 3, payload, header_suffix=header_suffix)


def _wrap_zstd(doc_version, raw_body, header_suffix=b'', write_content_size=True):
    if decoder_mod._stdlib_zstd is not None and write_content_size:
        compressed = decoder_mod._stdlib_zstd.compress(raw_body)
    elif decoder_mod._zstandard is not None:
        compressor = decoder_mod._zstandard.ZstdCompressor(write_content_size=write_content_size)
        compressed = compressor.compress(raw_body)
    else:
        raise unittest.SkipTest('no zstd backend installed')
    payload = _encode_varint(len(compressed)) + compressed
    return _wrap_packet(doc_version, 4, payload, header_suffix=header_suffix)


# Body from issue #299 sample: [{'projects': 'MYPROJ', 'name': 'DUMMY_FEATURE_NAME', 'id': 1, 'enabled': 'y'}]
ISSUE_299_BODY = bytes.fromhex(
    '282b01282a04'
    '270870726f6a6563747327064d5950524f4a'
    '27046e616d65271244554d4d595f464541545552455f4e414d45'
    '2702696401'
    '2707656e61626c6564270179'
)
ISSUE_299_EXPECTED = [{'projects': 'MYPROJ', 'name': 'DUMMY_FEATURE_NAME', 'id': 1, 'enabled': 'y'}]


class TestDecoderCompatibility(unittest.TestCase):
    def test_issue_299_uncompressed_v4(self):
        packet = _wrap_packet(4, 0, ISSUE_299_BODY)
        dec = SrlDecoder()
        self.assertEqual(dec.decode(packet), ISSUE_299_EXPECTED)
        self.assertEqual(dec.header, {'version': 4, 'type': 0})

    def test_zlib_v3_and_v4(self):
        for version in (3, 4):
            packet = _wrap_zlib(version, ISSUE_299_BODY)
            dec = SrlDecoder()
            self.assertEqual(dec.decode(packet), ISSUE_299_EXPECTED)
            self.assertEqual(dec.header, {'version': version, 'type': 3})

    def test_zlib_with_header_suffix_and_1_based_offsets(self):
        # ARRAYREF_3 [tracked SHORT_BINARY_1 "a", REFP 2, COPY 2]
        body = bytes.fromhex('43e16129022f02')
        packet = _wrap_zlib(4, body, header_suffix=b'\x00ignored-meta')
        self.assertEqual(SrlDecoder().decode(packet), ['a', 'a', 'a'])

    def test_zstd_v4_stdlib_and_zstandard_fallback(self):
        packet = _wrap_zstd(4, ISSUE_299_BODY)
        dec = SrlDecoder()
        self.assertEqual(dec.decode(packet), ISSUE_299_EXPECTED)
        self.assertEqual(dec.header, {'version': 4, 'type': 4})

        if decoder_mod._zstandard is not None:
            with mock.patch.object(decoder_mod, '_stdlib_zstd', None):
                dec2 = SrlDecoder()
                self.assertEqual(dec2.decode(packet), ISSUE_299_EXPECTED)
                self.assertEqual(dec2.header, {'version': 4, 'type': 4})

    def test_zstd_with_header_suffix_and_1_based_offsets(self):
        body = bytes.fromhex('43e16129022f02')
        packet = _wrap_zstd(4, body, header_suffix=b'\x00meta')
        self.assertEqual(SrlDecoder().decode(packet), ['a', 'a', 'a'])

    def test_zstd_missing_library_rejected(self):
        packet = _wrap_packet(4, 4, b'\x01\x00')
        with mock.patch.object(decoder_mod, '_stdlib_zstd', None), \
                mock.patch.object(decoder_mod, '_zstandard', None):
            with self.assertRaises(SrlError) as cm:
                SrlDecoder().decode(packet)
            self.assertIn('zstd', str(cm.exception))

    def test_zlib_malformed_cases(self):
        # Declared uncompressed size exceeds limit
        packet = _wrap_zlib(4, ISSUE_299_BODY)
        with self.assertRaises(SrlError) as cm:
            SrlDecoder(max_uncompressed_size=10).decode(packet)
        self.assertIn('exceeds limit', str(cm.exception))

        # Declared uncompressed size mismatches actual decompressed size
        bad_len_packet = _wrap_zlib(4, ISSUE_299_BODY, declared_len=len(ISSUE_299_BODY) + 5)
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(bad_len_packet)
        self.assertIn('zlib', str(cm.exception))

        # Declared uncompressed size smaller than actual stream
        short_len_packet = _wrap_zlib(4, ISSUE_299_BODY, declared_len=5)
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(short_len_packet)
        self.assertIn('zlib', str(cm.exception))

        # Declared uncompressed size 0
        zero_uncomp = _wrap_packet(4, 3, _encode_varint(0) + _encode_varint(2) + b'\x78\x9c')
        with self.assertRaises(SrlError):
            SrlDecoder().decode(zero_uncomp)

        # Declared compressed size 0
        zero_comp = _wrap_packet(4, 3, _encode_varint(5) + _encode_varint(0))
        with self.assertRaises(SrlError):
            SrlDecoder().decode(zero_comp)

        # Declared compressed size past remaining input
        truncated = _wrap_packet(4, 3, _encode_varint(10) + _encode_varint(50) + b'\x78\x9c')
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(truncated)
        self.assertIn('exceeds remaining input', str(cm.exception))

        # Trailing garbage inside declared compressed slice
        comp = zlib.compress(b'\x01') + b'\xff'
        trailing = _wrap_packet(4, 3, _encode_varint(1) + _encode_varint(len(comp)) + comp)
        with self.assertRaises(SrlError):
            SrlDecoder().decode(trailing)

        # Corrupt zlib bytes
        corrupt = _wrap_packet(4, 3, _encode_varint(4) + _encode_varint(4) + b'\x00\x01\x02\x03')
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(corrupt)
        self.assertIn('zlib', str(cm.exception))

    def test_zstd_malformed_cases(self):
        packet = _wrap_zstd(4, ISSUE_299_BODY)
        with self.assertRaises(SrlError) as cm:
            SrlDecoder(max_uncompressed_size=10).decode(packet)
        self.assertIn('exceeds limit', str(cm.exception))

        # Empty compressed length
        with self.assertRaises(SrlError):
            SrlDecoder().decode(_wrap_packet(4, 4, _encode_varint(0)))

        # Compressed length past end
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(_wrap_packet(4, 4, _encode_varint(50) + b'\x28\xb5\x2f\xfd'))
        self.assertIn('exceeds remaining input', str(cm.exception))

        # Corrupt zstd bytes
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(_wrap_packet(4, 4, _encode_varint(4) + b'\x00\x01\x02\x03'))
        self.assertIn('zstd', str(cm.exception))

        # Truncated zstd frame
        trunc = packet[:-2]
        with self.assertRaises(SrlError):
            SrlDecoder().decode(trunc)

        if decoder_mod._zstandard is not None:
            # Frame without Frame_Content_Size must be rejected
            no_size_pkt = _wrap_zstd(4, ISSUE_299_BODY, write_content_size=False)
            with self.assertRaises(SrlError) as cm:
                SrlDecoder().decode(no_size_pkt)
            self.assertIn('frame content size', str(cm.exception))

            with mock.patch.object(decoder_mod, '_stdlib_zstd', None):
                with self.assertRaises(SrlError) as cm:
                    SrlDecoder().decode(no_size_pkt)
                self.assertIn('frame content size', str(cm.exception))
                with self.assertRaises(SrlError) as cm:
                    SrlDecoder(max_uncompressed_size=10).decode(packet)
                self.assertIn('exceeds limit', str(cm.exception))

    def test_snappy_incremental_missing_and_mocked(self):
        packet = _wrap_packet(4, 2, _encode_varint(3) + b'xyz')
        with mock.patch.object(decoder_mod, '_snappy', None):
            with self.assertRaises(SrlError) as cm:
                SrlDecoder().decode(packet)
            self.assertIn('snappy', str(cm.exception))

        fake_snappy = mock.Mock()
        fake_snappy.uncompress.return_value = b'\x42\x01\x61a'
        with mock.patch.object(decoder_mod, '_snappy', fake_snappy):
            self.assertEqual(SrlDecoder().decode(packet), [1, 'a'])
            with self.assertRaises(SrlError) as cm:
                SrlDecoder(max_uncompressed_size=2).decode(packet)
            self.assertIn('exceeds limit', str(cm.exception))

    def test_max_uncompressed_size_validation(self):
        with self.assertRaises(ValueError):
            SrlDecoder(max_uncompressed_size=-1)
        packet = _wrap_zlib(4, ISSUE_299_BODY)
        self.assertEqual(SrlDecoder(max_uncompressed_size=0).decode(packet), ISSUE_299_EXPECTED)
        self.assertEqual(SrlDecoder(max_uncompressed_size=None).decode(packet), ISSUE_299_EXPECTED)

    def test_alias_weaken_pad_and_freeze_tags(self):
        # ARRAYREF_2 [tracked SHORT_BINARY_1 "x", ALIAS 2]
        self.assertEqual(SrlDecoder().decode(_wrap_packet(4, 0, bytes.fromhex('42e1782e02'))), ['x', 'x'])

        # WEAKEN wrapping REFN ARRAYREF_1 [1]
        self.assertEqual(SrlDecoder().decode(_wrap_packet(4, 0, bytes.fromhex('30284101'))), [1])

        # Long run of PAD tags before POS_7 must not hit recursion depth limit
        padded = b'\x3f' * 500 + b'\x07'
        self.assertEqual(SrlDecoder().decode(_wrap_packet(4, 0, padded)), 7)

        # OBJECT_FREEZE + OBJECTV_FREEZE
        # ARRAYREF_2 [OBJECT_FREEZE SHORT_BINARY_3 "Foo" POS_1, OBJECTV_FREEZE 2 POS_2]
        freeze_body = bytes.fromhex('423263466f6f01330302')
        self.assertEqual(
            SrlDecoder().decode(_wrap_packet(4, 0, freeze_body)),
            [{'class': 'Foo', 'object': 1}, {'class': 'Foo', 'object': 2}],
        )

    def test_copy_does_not_overwrite_tracked_items_at_source(self):
        # ARRAYREF_3 [
        #   ARRAYREF_1 [tracked SHORT_BINARY_1 "a"],  (outer at 1, inner at 2 -> "a")
        #   tracked COPY 1,                           (at offset 5, tracks its own result)
        #   REFP 3                                    (offset 3 is "a", must still be "a")
        # ]
        # Wait: byte offsets (1-based):
        # 1: 0x43 ARRAYREF_3
        # 2: 0x41 ARRAYREF_1
        # 3: 0xe1 (0x61 | 0x80) tracked SHORT_BINARY_1
        # 4: 0x61 "a"
        # 5: 0xaf (0x2f | 0x80) tracked COPY
        # 6: 0x02 (target offset 2: ARRAYREF_1)
        # 7: 0x29 REFP
        # 8: 0x05 (target offset 5: the tracked COPY)
        body = bytes.fromhex('4341e161af022905')
        res = SrlDecoder().decode(_wrap_packet(4, 0, body))
        self.assertEqual(res, [['a'], ['a'], ['a']])
        self.assertIs(res[1], res[2])
        self.assertIsNot(res[0], res[1])

    def test_regexp_perl_preserve_flag(self):
        # REGEXP pattern "abc", flags "ip"
        body = bytes.fromhex('3163616263626970')
        self.assertEqual(SrlDecoder().decode(_wrap_packet(4, 0, body)), re.compile('abc', re.I))

    def test_bin_mode_classic_vs_raw_bytes(self):
        # SHORT_BINARY_2 with non-UTF-8 bytes \x80\x93
        packet = _wrap_packet(4, 0, bytes.fromhex('628093'))
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(packet)
        self.assertIn('utf-8', str(cm.exception))

        self.assertEqual(SrlDecoder(bin_mode_classic=False).decode(packet), b'\x80\x93')

    def test_oversized_varint_rejected(self):
        # 11 continuation bytes -> > 64-bit varint
        packet = _wrap_packet(4, 0, b'\x20' + b'\x80' * 11 + b'\x01')
        with self.assertRaises(SrlError) as cm:
            SrlDecoder().decode(packet)
        self.assertIn('varint', str(cm.exception))


if __name__ == '__main__':
    unittest.main()
