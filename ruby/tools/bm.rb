require './lib/sereal'
require 'json'
require 'bson'
require 'msgpack'
require 'benchmark'
n = 50000
def rs
  rand(36**10).to_s(36)
end
a = Array.new(10) { |i|  rs }
aa = [a,a,a,a]
class ZXC
  @x = "a"
  @y = "b"
  def to_json
    {x: @x, y: @y}.to_json
  end
  def to_msgpack
    {x: @x, y: @y}.to_msgpack
  end
end
[
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
  Benchmark.bm do |x|
    v = nil
    x.report("srl-e ") { n.times { v = Sereal.encode(t,false) } }
    x.report("srl-d") { n.times { Sereal.decode(v)} }

    x.report("srl-eS ") { n.times { v = Sereal.encode(t,true) } }
    x.report("srl-dS") { n.times { Sereal.decode(v)} }

    x.report("msg-e ") { n.times { v = t.to_msgpack } } 
    x.report("msg-d") { n.times { MessagePack.unpack(v) } }
    x.report("jsn-e ") { n.times { v = t.to_json } } unless t.kind_of?(String)
    x.report("jsn-d") { n.times { JSON.parse(v) } } unless t.kind_of?(String)
    x.report("BSN-e ") { n.times { v = CBson.serialize(t,false,nil,10000) } } if t.kind_of?(Hash)
    x.report("BSN-d ") { n.times { CBson.deserialize(v) } } if t.kind_of?(Hash)
  end
end
