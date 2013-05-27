# encoding: utf-8

require 'test/unit'
require ENV['USE_CURRENT_DIRECTORY'] ? File.absolute_path(File.join(File.dirname(__FILE__),'..','lib','sereal')) : 'sereal'

class ZXCNOSRL
end
class ZXC
  attr_accessor :z,:x,:c
  def initialize
    @z = 'z'
    @x = 'x'
    @c = 'c'
  end
  def to_srl
    { "z" => @z, "x" => @x, "c" => @c }
  end
end
class Test::Unit::TestCase
  def recode(obj,safe = false)
    Sereal.decode(Sereal.encode(obj,false),safe)
  end
  def test_compress
    obj = {"aaaaasdjkhaksjdhakjshdkjahsdkjhaskjhadkjshdkjashdkjhas" => "b"}
    assert_equal Sereal.decode(Sereal.encode(obj,true)), recode(obj)
    assert Sereal.encode(obj,true).length < Sereal.encode(obj,false).length
  end
  def test_numbers
    [0,1,2,3,4,16,17,2**31,2**32,2**60,2*64,0.1,Float(2**64),2.1**956].each do |x|
      assert_equal recode(x),x
      if x > 0
        neg = -x
        assert_equal recode(neg),neg
      end
    end
  end
  def test_nil
    assert_equal recode(nil),nil
  end

  def test_true
    assert_equal recode(true),true
  end

  def test_false
    assert_equal recode(false),false
  end

  def test_array
    a = ["a"]
    assert_equal recode(a),a
  end
  def test_zxc
    x = ZXC.new
    assert_equal recode(x),x.to_srl
    assert_raise(NoMethodError) do
        recode(ZXCNOSRL.new)
    end
  end
  def test_sym
    #symbols are converted
    assert_equal "x", recode(:x)
  end
  def test_hash
    a = {"a" => "b"}
    assert_equal recode(a),a
  end
  def test_regexp
    [/aaa./i,/aaa/m,/aaa/x,/aaa/imx,/aaa/].each do |x|
      assert_equal recode(x),x
    end
  end
  def test_utf
    a = "§§§§".encode("UTF-8")
    assert_equal recode(a),a
    assert_equal recode(a).encoding, a.encoding
    ascii = "a".encode("ASCII-8BIT")
    assert_equal recode(ascii).encoding,ascii.encoding
  end
  def test_recursion
    x = []
    x[0] = x
    assert_raise(ArgumentError) do 
      recode(x)
    end

    assert_nothing_raised do
      begin
        recode(x)
      rescue ArgumentError
      end
    end
  end
  def test_alias
    return 
    b = ["x"]
    a = [b,b,b,b]
    arr = Array.new(3,a)
    assert_equal arr.first.object_id,arr.last.object_id
    assert_equal arr.first.first.object_id,arr.last.last.object_id
    arr = recode(arr)
    assert_equal arr.first.object_id,arr.last.object_id
    assert_equal arr.first.first.object_id,arr.last.last.object_id
  end
end
