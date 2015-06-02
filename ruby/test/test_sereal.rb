# encoding: utf-8
require 'socket'
require 'thread'
require 'test/unit'
require ENV['USE_CURRENT_DIRECTORY'] ? File.absolute_path(File.join(File.dirname(__FILE__),'..','lib','sereal')) : 'sereal'

class ZXCNOSRL
end

class ZXCFile
  attr_accessor :path
  def initialize(path)
    @path = path
  end
  def FREEZE(serializer)
    @path
  end
  def self.THAW(serializer,path)
    self.new(path)
  end
end
class ZXCFREEZE
  class InnerClass
    attr_accessor :value
    def initialize(value)
      @value = value
    end
    def FREEZE(serializer)
      [value]
    end
    def self.THAW(serializer,*a)
      new(*a)
    end
  end

  attr_accessor :z,:x,:c,:serializer
  def initialize(z,x,c)
    @z = z
    @x = x
    @c = c
  end
  def FREEZE(serializer)
    [ @z, @x, @c ]
  end
  def self.THAW(serializer,*a)
    r = self.new(*a)
    r.serializer = serializer
    r
  end
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
class SerealPerlObject
  def value
    @value
  end
  def klass
    @class
  end
end
Time.class_eval do
  def FREEZE(serializer)
    to_a
  end
  def self.THAW(serializer,*a)
    mktime(*a)
  end
end

class Test::Unit::TestCase
  def recode(obj,safe = false)
    Sereal.decode(Sereal.encode(obj),safe)
  end

  def test_perl_object
    x = Sereal.decode(File.open(File.join(File.dirname(__FILE__),'example-perl-object.srl')).read,Sereal::THAW)
    assert_equal(x[0].class, SerealPerlObject)
    assert_equal(x[1].class, SerealPerlObject)
    assert_equal(x[1].klass, 'HTTP::Tiny')
    assert_equal(x[1].value["timeout"], 60)
    assert_equal(x[2].path,"/tmp/aaa.txt")
    assert_equal(x[3].path,"/tmp/aaa.txt")
  end

  def test_copy
    obj = "bazinga"
    a = [obj,"bazinga",obj]
    x = Sereal.decode(Sereal.encode(a,Sereal::COPY|Sereal::REF),Sereal::REF)
    assert_equal(x[0],x[1],x[2])
    assert_not_equal(x[0].object_id,x[1].object_id)
    assert_equal(x[0].object_id,x[2].object_id)
    assert Sereal.encode(a,Sereal::COPY|Sereal::REF).length < Sereal.encode(a,Sereal::REF).length
  end

  def test_nested_class_thaw
    original = ZXCFREEZE::InnerClass.new("hello")
    recoded = Sereal.decode(Sereal.encode(original,Sereal::THAW),Sereal::THAW)
    assert_equal(ZXCFREEZE::InnerClass, recoded.class)
    assert_equal(original.value,recoded.value)
  end

  def test_thaw
    [ Sereal::THAW,Sereal::THAW|Sereal::COPY].each do |flags|
      x = [ZXCFREEZE.new(6,7,8),ZXCFREEZE.new(6,7,9),ZXCFREEZE.new(6,7,10)]
      y = ZXCFREEZE.new(6,7,10)
      frozen = x[0].FREEZE(Sereal::FREEZER)
      recoded = Sereal.decode(Sereal.encode(x,flags),flags)
      assert_raise(TypeError) do
        recoded = Sereal.decode(Sereal.encode(x,flags))
      end
      assert_equal(frozen,recoded[0].FREEZE(Sereal::FREEZER))
    end
  end

  def test_thaw_tdata
    original = Time.now # Time is a T_DATA not a T_OBJECT
    recoded = Sereal.decode(Sereal.encode(original,Sereal::THAW),Sereal::THAW)
    assert_equal(original.to_a, recoded.to_a)
  end

  def test_references
    string = "aaa"
    a = [ string,string,1,2,3 ]
    aa = [a,a]
    aaa = [aa, aa]
    [a,aa,aaa].each do |x|
      [Sereal::RAW,Sereal::SNAPPY,Sereal::SNAPPY_INCR].each do |c|
        recoded = Sereal.decode(Sereal.encode(x,Sereal::REF|c))
        assert_equal(x,recoded)
        assert_equal(recoded[0].object_id,recoded[1].object_id)
      end
    end
  end
  def test_compress
    obj = {"aaaaasdjkhaksjdhakjshdkjahsdkjhaskjhadkjshdkjashdkjhas"*20 => "b" * 100000}
    [Sereal::SNAPPY_INCR].each do |c|
      concat = ""
      10.times do 
          concat << Sereal.encode(obj,c)
      end
      Sereal.decode(concat) do |x|
        assert_equal x,obj
      end
    end
    assert_equal Sereal.decode(Sereal.encode(obj,Sereal::SNAPPY)), recode(obj)
    assert_equal Sereal.decode(Sereal.encode(obj,Sereal::SNAPPY_INCR)), recode(obj)
    assert Sereal.encode(obj,true).length < Sereal.encode(obj,false).length
  end
  def test_invalid
    assert_raise(TypeError) do 
      Sereal.decode("=srl")
    end
  end
  def test_numbers
    [0,1,2,3,4,15,16,17,2**30,2**31,2**32,2**61,(2**64)-1,0.1,Float(2**64),2.1**956].each do |i|
      [i,-i].each do |x|
        if (i > (2**63))
          x = i
        end
        begin
          assert_equal x,recode(x),"testing: #{x.to_s}"
        rescue Exception => e
          raise "failed to recode #{x} - #{e.message}"
        end
      end
    end
  end
  def test_numbers_end
    [(2**64),-(2**64)].each do |x|
      assert_raise(RangeError) do
        recode(x);
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
  def test_stream
    bzbz = "bzbz"
    decoded = {"bbb"=>[0.123213, 1, 2, 3, "bzbz"], "aaa"=>[0.123213, 1, 2, 3, "bzbz"], "巴黎"=>{"123123"=>"巴黎"}}  
    Sereal.decode(File.open(File.join(File.dirname(__FILE__),"example.srl")).read) do |x|
      assert_equal(x,decoded)
    end
    decoded = {"bbb"=>[0.123213, 1, 2, 3, bzbz * 100], "aaa"=>[0.123213, 1, 2, 3, bzbz * 100], "巴黎"=>{"123123"=>"巴黎"}}  

    ['example-stream-snappy-i.srl','example-stream-no-snappy-i.srl'].each do |f|
      i = 1
      prev = nil
      Sereal.decode(File.open(File.join(File.dirname(__FILE__),f)).read) do |x|
        if (prev)
          assert_not_equal(prev,x["bbb"].object_id)
        end
        assert_equal(x["bbb"].object_id,x["aaa"].object_id)
        decoded["i"] = i
        assert_equal(decoded,x)
        i += 1
        prev = x["bbb"].object_id
      end
      i = 1
      assert_raise(TypeError) do
        x = Sereal.decode(File.open(File.join(File.dirname(__FILE__),f)))
      end
      prev = nil
      Sereal.decode(File.open(File.join(File.dirname(__FILE__),f))) do |x|
        if (prev)
          assert_not_equal(prev,x["bbb"].object_id)
        end
        assert_equal(x["bbb"].object_id,x["aaa"].object_id)
        decoded["i"] = i
        assert_equal(decoded,x)
        i += 1
        prev = x["bbb"].object_id
      end
    end
  end
end
