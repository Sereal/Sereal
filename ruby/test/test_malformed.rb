# encoding: utf-8
require 'test/unit'
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
end
