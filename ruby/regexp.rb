#encoding: UTF-8
require './lib/sereal'
x = /aaa\./i
p x
r = Sereal.decode(Sereal.encode(x,false))
p r

x = /§aaa./i
p x
r = Sereal.decode(Sereal.encode(x,false))
p r
