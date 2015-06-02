#begin
  require File.join(".",File.dirname(__FILE__),'..','lib','sereal')
#rescue LoadError
#  require 'sereal'
#end

require 'json'
require 'msgpack'
require 'benchmark/ips'
def rs
  rand(36**10).to_s(36)
end
a = Array.new(10) { |i|  rs }
aa = [a,a,a,a]
class ZXC
  @x = "a"
  @y = "b"
  def to_srl
    {x: @x, y: @y}
  end
  def to_json
    {x: @x, y: @y}.to_json
  end
  def to_msgpack
    {x: @x, y: @y}.to_msgpack
  end
end

string = "aaa"
a = [ string,string,1,2,3 ]
aa = [a,a,a,a,a,a]
aaa = [aa, aa]

[
  aaa,
  1,
  127,
  'a',
  [1,{'a' => 'b'},3,'aaa'],
  ZXC.new,
  Array.new(100,1),
  a,
  aa,
  "a",
  "bb",
  "ccc",
  "dddd",
  ["a"],
  {"a" => "b"},
  {"a" => ["a","b","c"]},
  Hash[* Array.new(100) { |i| rs }],
  ["a" * 1000],
  Array.new(100) { |i|  rs },
  Array.new(100) { |i|  { rs => rs} }
].each do |t|
  puts "\n\n#{t.to_s.scan(/(.{1,50})/).first}\n"
  Benchmark.ips do |x|
    v = nil
    x.report("srl-er") {v = Sereal.encode(t,Sereal::REF) }
    x.report("srl-dr") { Sereal.decode(v) }
    x.report("srl-erc") {v = Sereal.encode(t,Sereal::REF|Sereal::COPY) }
    x.report("srl-drc") { Sereal.decode(v) }

    x.report("srl-erS") {v = Sereal.encode(t,Sereal::REF|Sereal::SNAPPY) }
    x.report("srl-drS") { Sereal.decode(v) }

    x.report("srl-erSI") {v = Sereal.encode(t,Sereal::REF|Sereal::SNAPPY_INCR) }
    x.report("srl-drSI") { Sereal.decode(v) }

    x.report("srl-e ") {v = Sereal.encode(t,false)  }
    x.report("srl-d ") { Sereal.decode(v) }

    x.report("srl-eS ") { v = Sereal.encode(t,true)  }
    x.report("srl-dS") { Sereal.decode(v) }

    x.report("msg-e ") {  v = t.to_msgpack } 
    x.report("msg-d ") {  MessagePack.unpack(v) }
    x.report("jsn-e ") { v = t.to_json } unless t.kind_of?(String) || t.kind_of?(Fixnum)
    x.report("jsn-d ") {  JSON.parse(v) } unless t.kind_of?(String) || t.kind_of?(Fixnum)
  end
end
