## implementation

This is incomplete implementation, it does not have support for REFP,REFN and ALIAS tags 

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

# or just decode directly from a tcp socket stream
# (works also with any IO stream object (Socket, File, etc)
s = TCPSocket.new 'localhost', 2000
Sereal.decode(s) do |decoded|
  # do something with the decoded object
end
/*
 * Encode/Decode object using Sereal binary protocol:
 * https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod
 *
 *   Sereal.encode(object) -> serialized blob
 *   Sereal.encode(object,Sereal::SNAPPY_INCR) -> snappy compressed blob
 *   Sereal.encode(object,Sereal::SNAPPY) -> snappy compressed blob
 *
 * SNAPPY_INCR encoded objects can be appended into one output and then the
 * decoder will know what to do.
 *
 * Sereal.encode(object,Sereal::REF)
 * or Sereal::REF|Sereal::SNAPPY_INC, or Sereal::REF|Sereal::SNAPPY
 *
 * when encoding will try to use Sereal's REFP tag to transmit only the
 * the original object's offset in the packet.
 * So:
 *    one = [ 1,2,3,4,5 ]
 *    two = [ one, one ]
 *    Sereal.encode(two,Sereal::REF)
 * will send 'one' only once, and one REFP that points to the first one
 * it uses one.object_id as a hash key in a local tracker hash
 * and if it sees this object_id again it just sends the offset.
 *
 *
 *   Sereal.decode(blob) - returns the decoded object
 *   
 * If the blob contains multiple compressed
 * sub-blobs you should call it with:
 *
 *    Sereal.decode(blob) do |decoded|
 *       # do something with the decoded object
 *    end
 *
 * otherwise only the first decoded object will be returned
 * there is also streaming support which takes any kind of IO object
 * like socket, or just regular File, and it is really easy to use:
 *
 *    Sereal.decode(STDIN) do |decoded|
 *      # do something with the decoded object
 *    end
 *
 * it works both with `incremental snappy` and with just combined sereal packets.
 * another example but with TCPSocket:
 *
 *    s = TCPSocket.new 'localhost', 2000
 *    Sereal.decode(s) do |decoded|
 *      # do something with the decoded object
 *    end
 *
 */


```

it also creates the executable 'rsrl' which can be used like:

```
$ cat /tmp/file | Sereal_SNAPPY_INCR=1 rsrl > snappy_incr_compressed_incremental_sereal_binary # encode

$ rsrl snappy_incr_compressed_incremental_sereal_binary # decode

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

### LZ4

For brief period (version 0.0.5 to 0.0.6) there was a support for LZ4 and LZ4HC, which was pushed to the master branch by mistake. if you are depending on it please convert yout data using `bin/rsrl` or just use `0.0.5` version of the sereal gem.

```
gem 'sereal', '= 0.0.5'
#or
$ gem install sereal -v 0.0.5
```

### Stream support for the decoder (only in trunk)

```
Sereal.decode(STDIN) do |decoded|
  p decoded
end

```

As you can see it is really simple to use, it works both with `incremental snappy` and with just combined sereal packets, it also works with sockets

```
s = TCPSocket.new 'localhost', 2000
Sereal.decode(s) do |decoded|
  p decoded
end

# or any file
Sereal.decode(File.open('example-stream-snappy-i.srl')) do |decoded|
  p decoded
end

```

However it requires a block to be given when you are reading from a stream. And you can not put non-incremental Snappy packets in the stream.

### more then 1 packet in the buffer

It is easy to parse more then 1 packet a buffer:

```
buf = ""
buf << Sereal.encode([1,2,3],Sereal::SNAPPY_INCR)
buf << Sereal.encode([3,4,5])
buf << Sereal.encode([7,8,9],Sereal::SNAPPY_INCR)
Sereal.decode(buf) do |decoded|
  p decoded
end
```

However you can do that with non-incremental Snappy packets
