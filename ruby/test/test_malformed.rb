# encoding: utf-8
require 'test/unit'
require 'tempfile'
require ENV['USE_CURRENT_DIRECTORY'] ? File.absolute_path(File.join(File.dirname(__FILE__),'..','lib','sereal')) : 'sereal'

# decoding of malformed or hostile documents: every case must raise, never crash
class TestSerealMalformed < Test::Unit::TestCase
  def srl(hex)
    [hex.delete(' ')].pack('H*')
  end

  def test_string_length_wrapping_u32
    # STR_UTF8 with varint length 0xfffffff5: pos + len wraps past 2**32
    assert_raise(RangeError) { Sereal.decode(srl('3df3726c0300 27 f5ffffff0f 61626364')) }
    # same for BINARY
    assert_raise(RangeError) { Sereal.decode(srl('3df3726c0300 26 f5ffffff0f 61626364')) }
    # length wider than 32 bits must not be truncated
    assert_raise(RangeError) { Sereal.decode(srl('3df3726c0300 26 8480808010 61626364')) }
    # SNAPPY_INCR with compressed length 0xfffffff6
    assert_raise(RangeError) { Sereal.decode(srl('3df3726c2300 f6ffffff0f c0843dfc3f420f0078')) }
  end

  def test_max_depth
    # 110 nested ARRAYREF(1): the depth error must not touch freed decoder state
    assert_raise(ArgumentError) { Sereal.decode(srl('3df3726c0300' + '41' * 110 + '01')) }
  end

  def test_array_count
    # ARRAY declaring 0xffffffff elements in a 13 byte document
    assert_raise(RangeError) { Sereal.decode(srl('3df3726c0300 2b ffffffff0f 01')) }
    # ARRAY with no elements
    assert_equal([], Sereal.decode(srl('3df3726c0300 2b 00')))
  end

  def test_large_array_in_thread
    # a valid 300000 element ARRAY must not need a stack frame sized by its count
    doc = srl('3df3726c0300 2b e0a712') + ("\x01" * 300000).b
    result = Thread.new { Sereal.decode(doc) }.value
    assert_equal(300000, result.size)
    assert_equal(1, result.last)
  end

  def test_snappy_short_output
    # SNAPPY body declares 4096 uncompressed bytes but its stream produces fewer
    assert_raise(TypeError) { Sereal.decode(srl('3df3726c1300 80200826fd1f')) }
  end

  def rss_kb
    if File.readable?('/proc/self/status')
      File.read('/proc/self/status')[/^VmRSS:\s+(\d+)/, 1].to_i
    else
      `ps -o rss= -p #{Process.pid}`.to_i
    end
  end

  def test_snappy_failure_frees_buffer
    # a valid 1 MiB SNAPPY document with a truncated copy op appended: the
    # body decompresses fully and then fails, which must not leak the buffer
    bad = Sereal.encode("\0".b * (1 << 20), Sereal::SNAPPY) + "\xff".b
    assert_raise(TypeError) { Sereal.decode(bad) }
    before = rss_kb
    omit('can not measure RSS') if before == 0
    omit('freed memory is held in quarantine under ASan') if ENV['ASAN_OPTIONS']
    200.times { assert_raise(TypeError) { Sereal.decode(bad) } }
    assert_operator(rss_kb - before, :<, 50 * 1024, 'rss grew by more than 50 MiB')
  end

  def test_snappy_huge_uncompressed_length
    # SNAPPY body declaring 0xffffffff uncompressed bytes in a 13 byte document
    assert_raise(TypeError, NoMemoryError) { Sereal.decode(srl('3df3726c1300 ffffffff0f 0061')) }
  end

  def test_ruby_exception_frees_state
    # REGEXP with pattern "(": RegexpError escapes from inside the decoder
    bad = srl('3df3726c0300 31 6128 60')
    assert_raise(RegexpError) { Sereal.decode(bad) }
    before = rss_kb
    omit('can not measure RSS') if before == 0
    omit('freed memory is held in quarantine under ASan') if ENV['ASAN_OPTIONS']
    50000.times { begin; Sereal.decode(bad); rescue RegexpError; end }
    assert_operator(rss_kb - before, :<, 20 * 1024, 'rss grew by more than 20 MiB')
  end

  def test_reference_to_untracked_offset
    # ARRAYREF(2): tracked POS 1, then REFP to offset 40, which holds nothing
    assert_raise(ArgumentError) { Sereal.decode(srl('3df3726c0300 42 81 2928')) }
    # REFP to offset 2, the tracked POS 1, still works
    assert_equal([1, 1], Sereal.decode(srl('3df3726c0300 42 81 2902')))
    # ALIAS to an offset past the end of the document
    assert_raise(ArgumentError) { Sereal.decode(srl('3df3726c0300 42 81 2eff7f')) }
  end

  def test_copy_offset_must_point_backwards
    # ARRAYREF(2): COPY(3) points forward at its own varint byte
    assert_raise(ArgumentError) { Sereal.decode(srl('3df3726c0300 42 2f03 01')) }
    # COPY(0) points into the header
    assert_raise(ArgumentError) { Sereal.decode(srl('3df3726c0300 42 2f00 01')) }
    # COPY(2) points at itself
    assert_raise(ArgumentError) { Sereal.decode(srl('3df3726c0300 42 2f02 01')) }
    # COPY(2) of the string at offset 2 is fine
    assert_equal(%w[abc abc], Sereal.decode(srl('3df3726c0300 42 63616263 2f02')))
  end

  def test_header_suffix
    # header suffix size 256 (two byte varint) in an 8 byte document
    assert_raise(RangeError) { Sereal.decode(srl('3df3726c03 8002 01')) }
    # a one byte suffix (empty bitfield) is skipped
    assert_equal(1, Sereal.decode(srl('3df3726c03 01 00 01')))
  end

  def test_protocol_version
    # protocol version 1 used absolute offsets
    assert_raise(TypeError) { Sereal.decode(srl('3df3726c01 00 01')) }
    # protocol version 0 and 7 are not defined
    assert_raise(TypeError) { Sereal.decode(srl('3df3726c00 00 01')) }
    assert_raise(TypeError) { Sereal.decode(srl('3df3726c07 00 01')) }
  end

  def test_unsupported_document_type
    # zlib (3) and zstd (4) bodies must not be parsed as raw
    assert_raise(TypeError) { Sereal.decode(srl('3df3726c33 00 01')) }
    assert_raise(TypeError) { Sereal.decode(srl('3df3726c44 00 01')) }
  end

  def test_unaligned_double
    # ARRAYREF(2): POS 1, DOUBLE 1.5 at an odd address (sanitizer builds flag misaligned loads)
    assert_equal([1, 1.5], Sereal.decode(srl('3df3726c0300 42 01 23 000000000000f83f')))
  end

  def test_stream_read_error
    # read() on a write-only file fails with -1, which must end the stream
    # rather than be taken as a 4 GiB read
    Tempfile.create('sereal') do |tmp|
      File.open(tmp.path, 'w') do |f|
        yielded = false
        assert_nil(Sereal.decode(f) { yielded = true })
        assert(!yielded)
      end
    end
  end
end
