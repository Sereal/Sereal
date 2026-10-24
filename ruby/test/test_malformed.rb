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
end
