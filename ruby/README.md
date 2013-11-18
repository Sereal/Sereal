## implementation

This is incomplete implementation, it does not have support for REFP,REFN and ALIAS tags. 

#### requirements

* rake compiler - [gem install rake-compiler](https://github.com/luislavena/rake-compiler)
* ruby 1.9+ or rubinius supporting 1.9+

### install
```
$ gem install sereal

# or you can build it from github

$ git clone https://github.com/Sereal/Sereal
$ cd Sereal/ruby
$ gem build sereal.gemspec 
$ gem install sereal-0.0.?.gem 
```

### examples

```ruby
require 'sereal'
object = { a: :b }
Sereal.encode(object)
Sereal.decode(Sereal.encode(object))

/*
 * Encode/Decode object using Sereal binary protocol:
 * https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod
 *
 *   Sereal.encode(object) -> serialized blob
 *   Sereal.encode(object,Sereal::LZ4) -> LZ4 compressed blob
 *   Sereal.encode(object,Sereal::LZ4HC) -> LZ4HC compressed blob
 *   Sereal.encode(object,Sereal::SNAPPY_INCR) -> snappy compressed blob
 *   Sereal.encode(object,Sereal::SNAPPY) -> snappy compressed blob
 *
 * LZ4 LZ4HC and SNAPPY_INCR can be appended into one output and then the
 * decoder will know what to do.
 *
 *   Sereal.decode(blob) - returns the decoded object
 *   
 * If the blob contains multiple compressed(with LZ4* or SNAPPY_INCR) 
 * sub-blobs you should call it with:
 *       
 *    Sereal.decode(blob) do |decoded|
 *       # do something with the decoded object 
 *    end
 * otherwise only the first decoded object will be returned
 *
 */
```

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
