# -*- coding: utf-8 -*-
begin
  require File.join(".",File.dirname(__FILE__),'..','lib','sereal')
rescue LoadError
  require 'sereal'
end

require 'msgpack'
require 'benchmark'

obj = {
  "name" => "jack doe",
  "quantity" => 1_000_000,
  "some_unicode" => { "Chinese (汉语 or 漢語, Hànyǔ)" => [ 1,2,3,4,5 ] }
}

so = File.open("__stream_rb_bench_sereal.stream","w+")
mo = File.open("__stream_rb_bench_msgpack.stream","w+")
n = 1_000_000
n.times do
  so.write(Sereal.encode(obj,Sereal::COPY))
  mo.write(obj.to_msgpack)
end

so.seek(0,IO::SEEK_SET)
mo.seek(0,IO::SEEK_SET)

Benchmark.bm do |r|
  r.report("Sereal") do
    Sereal.decode(so) do |x|
    end
  end

  r.report("no-stram-Sereal-decode") do
    encoded = Sereal.encode(obj)
    n.times do
      decoded = Sereal.decode(encoded)
    end
  end

  r.report("MsgPack") do
    u = MessagePack::Unpacker.new(mo)
    u.each do |x|
    end
  end

  r.report("no-stram-MsgPack-decode") do
    encoded = obj.to_msgpack
    n.times do
      decoded = MessagePack.unpack(encoded)
    end
  end
end
