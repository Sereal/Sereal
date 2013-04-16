## implementation

This is incomplete implementation, it does not have support for REFP,REFN and ALIAS tags. 

#### requirements

* rake compiler - [gem install rake-compiler](https://github.com/luislavena/rake-compiler)
* ruby 1.9+ or rubinius supporting 1.9+

### install
```
$ git clone https://github.com/jackdoe/Sereal
$ cd Sereal/ruby
$ gem build sereal.gemspec 
$ gem install sereal-0.0.1.gem 
```

### examples

```ruby
require 'sereal'
object = { a: :b }
Sereal.encode(object)
Sereal.decode(Sereal.encode(object))
```
`Sereal.encode(object,true)` uses [snappy](http://code.google.com/p/snappy/) compression, disabled by default

### speed

currently it is on par with msgpack, you can run the benchmarks in tools/bm.rb

```
srl-e   0.080000   0.000000   0.080000 (  0.070764)
srl-d  0.110000   0.000000   0.110000 (  0.111373)
srl-eS   0.170000   0.110000   0.280000 (  0.320622) #compressed
srl-dS  0.130000   0.000000   0.130000 (  0.129671)  #compressed 
msg-e   0.080000   0.000000   0.080000 (  0.080496)
msg-d  0.100000   0.000000   0.100000 (  0.105858)
jsn-e   0.530000   0.000000   0.530000 (  0.526243)
jsn-d  0.600000   0.010000   0.610000 (  0.608191)
```
