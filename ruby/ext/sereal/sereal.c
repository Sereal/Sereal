#include "sereal.h"
#include "encode.h"

VALUE Sereal = Qnil;
VALUE SerealPerlObject = Qnil;
ID FREEZE;
ID THAW;
ID TO_SRL;
ID SEREAL;
ID ID_CLASS;
ID ID_VALUE;
void Init_sereal();

/*
 * Encode/Decode object using Sereal binary protocol:
 * https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod
 *
 * ==install:
 *
 *  $ gem install sereal
 *
 * or you can build it from github which requires: 
 * 1. rake compiler - <code>gem install rake-compiler</code> (https://github.com/luislavena/rake-compiler)
 * 2. ruby 1.9+ or rubinius supporting 1.9+
 *
 *  $ git clone https://github.com/Sereal/Sereal
 *  $ cd Sereal/ruby  
 *  $ gem build sereal.gemspec
 *  $ gem install sereal-0.0.?.gem
 *
 * ==serialize:
 *   require 'sereal'
 *   Sereal.encode(object)
 *
 * ===serializing objects
 * if Sereal::THAW option is given the encoder will try to call FREEZE() instance method on
 * the object beeing serialized, and it will serialize the class name + the output of FREEZE (look the THAW constant for more information)
 * if the object does not respond to FREEZE it will call <code>to_srl</code> and serialize the result of that
 * ==deserialize:
 *   require 'sereal'
 *   Sereal.decode(blob) 
 * If the blob contains multiple compressed objects
 * sub-blobs you should call it with:
 *
 *    Sereal.decode(blob) do |decoded|
 *       # do something with the decoded object
 *    end
 *
 * otherwise only the first decoded object will be returned
 * ===stream decoding
 * there is also streaming support which takes any kind of IO object
 * like socket, or just regular File, and it is really easy to use:
 *
 *    Sereal.decode(STDIN) do |decoded|
 *      # do something with the decoded object
 *    end
 *
 * it works both with SNAPPY_INCR and with just combined sereal packets.
 * another example but with TCPSocket:
 *
 *    s = TCPSocket.new 'localhost', 2000
 *    Sereal.decode(s) do |decoded|
 *      # do something with the decoded object
 *    end
 *
 * ===multiple packets in one buffer
 * it also supports decoding of multiple packets in one buffer:
 *
 *    buf = ""
 *    buf << Sereal.encode([1,2,3],Sereal::SNAPPY_INCR)
 *    buf << Sereal.encode([3,4,5])
 *    buf << Sereal.encode([7,8,9],Sereal::SNAPPY_INCR)
 *    Sereal.decode(buf) do |decoded|
 *      p decoded
 *    end
 * ==Sereal.encode() and Sereal.decode() accept:
 * 1. compression types: RAW, SNAPPY_INCR, and SNAPPY
 * 2. flags: REF, COPY, THAW and DEBUG
 *
 * flags and compression types can be used in combinations like:
 *
 *    Sereal.encode([1,2,3],Sereal::REF|Sereal::COPY|Sereal::THAW|Sereal::SNAPPY_INCR)
 *
 * but you can not use 2 types of compression in the same time
 *
 * ==LZ4
 * For brief period (version 0.0.5 to 0.0.6) there was a support for LZ4 and LZ4HC, which was pushed to the master branch by mistake. if you are depending on it please convert yout data using <code>bin/rsrl</code> or just use <code>0.0.5</code> version of the sereal gem.
 *
 *    gem 'sereal', '= 0.0.5'
 *    #or
 *    $ gem install sereal -v 0.0.5
 *
 */
void Init_sereal() {
    TO_SRL = rb_intern("to_srl");
    THAW = rb_intern("THAW");
    FREEZE = rb_intern("FREEZE");
    SEREAL = rb_intern("Sereal");
    ID_CLASS = rb_intern("@class");
    ID_VALUE = rb_intern("@value");

    SerealPerlObject = rb_define_class("SerealPerlObject", rb_cObject);

    Sereal = rb_define_class("Sereal", rb_cObject);
    rb_define_singleton_method(Sereal, "encode", method_sereal_encode, -2);

    rb_define_singleton_method(Sereal, "decode", method_sereal_decode, -2);

    /*
     * instructs the encoder to use Snappy compression
     * nb: this is Sereal protocol version 1 only
     * do not use it if possible.
     * 
     *    Sereal.encode(object,Sereal::SNAPPY)
     */
    rb_define_const(Sereal, "SNAPPY",INT2NUM(__SNAPPY));


    /* 
     * instructs the encoder to use Snappy compression
     * but with support for incremental packet (meaning
     * you can combine packets into one big blob of data
     * and the encoder will be confident that there is no
     * corruption, because the SNAPPY_INCR packet contains the
     * uncompressed length)
     *
     *    Sereal.encode(object,Sereal::SNAPPY_INCR)
     */
    rb_define_const(Sereal, "SNAPPY_INCR",INT2NUM(__SNAPPY_INCR));

    /* 
     * intructs the encoder to use no compression (default)
     *
     *    Sereal.encode(object,Sereal::RAW)
     */
    rb_define_const(Sereal, "RAW",INT2NUM(__RAW));

    /*
     * (can also be used with any compression type/RAW)
     * instructs the encoder to keep track of object_ids
     * and when it sees that object with the same id has
     * already been encoded, it just creates REFP reference
     * with offset to the first item
     * so:
     *    name = "john doe"
     *    object = [ name, name ]
     *    Sereal.encode(name,Sereal::REF|Sereal::SNAPPY_INCR)
     *
     * will actually create something that looks like:
     *
     *    000006/000001: 42  066 ARRAYREF(2)
     *    000007/000002: 68* 232   SHORT_BINARY(8): 'john doe'
     *    000016/000011: 29  041   REFP(2)
     * 
     * instead of:
     * 
     *    000006/000001: 42  066 ARRAYREF(2)
     *    000007/000002: 68  104   SHORT_BINARY(8): 'john doe'
     *    000016/000011: 68  104   SHORT_BINARY(8): 'john doe'
     *
     * as you can see Sereal saved us 7 bytes
     *
     * this can hurts performance because the encoder must get the
     * object_id of every encoded object, and put it in a hash
     * with the current position, so it can be looked up later
     */
    rb_define_const(Sereal, "REF",INT2NUM(__REF));


    /* 
     * very similar to Sereal::REF, but it instructs the decoder
     * to create new item, by going back in time as if the OFFSET
     * of the COPY tag was beeing read right now
     * it puts every object as a key in a hash (instead of its 
     * object_id)
     * 
     *    object = [ { name => "john"} , { name => "john"} ]
     *    Sereal.encode(name,Sereal::COPY|Sereal::SNAPPY_INCR)
     *
     * will produce:
     *
     *    000006/000001: 42  066 ARRAYREF(2)
     *    000007/000002: 51  081   HASHREF(2)
     *        KEY:
     *    000008/000003: 64  100       SHORT_BINARY(4): 'name'
     *        VALUE:
     *    000013/000008: 64  100       SHORT_BINARY(4): 'john'
     *    000018/000013: 2f  047   COPY(2)
     *
     * as you can see the hash is sent only once
     * 
     * COPY can be used with REF like:
     * 
     *    object = "bazinga"
     *    
     *    Sereal.encode([object,"bazinga",object],Sereal::REF|Sereal::COPY)
     *
     * will produce:
     *    000006/000001: 43  067 ARRAYREF(3)
     *    000007/000002: 67* 231   SHORT_BINARY(7): 'bazinga'
     *    000015/000010: 2f  047   COPY(2)
     *    000017/000012: 29  041   REFP(2)
     *
     * using Sereal::COPY also hurts performance because every encoding step
     * the encoder must look into a hash if the object exists in it
     * so it can create a COPY tag instead of encoding the object again
     */
    rb_define_const(Sereal, "COPY",INT2NUM(__COPY));

    /*
     * add support for FREEZE/THAW
     * it calls <code>object.FREEZE(:Sereal)</code> and it serializes the result of that
     * with the object's class name, when deserializing it calls 
     * <code>class.THAW(:Sereal,the output of FREEZE)</code>
     * 
     *    class StorableFile
     *      attr_accessor :path
     *      def initialize(path,pos)
     *        @path = path
     *        @pos = pos
     *      end
     *      def read
     *        @pos += 1
     *      end
     *      def FREEZE(serializer)
     *        [@path,@pos]
     *      end
     *      def self.THAW(serializer,path,pos)
     *        self.new(path,pos)
     *      end
     *    end
     * 
     *    obj = StorableFile.new("/tmp/sereal.txt",0)
     *    obj.read # read some data
     *    encoded = Sereal.encode(obj,Sereal::THAW)
     *    # this actually encodes something like:
     *    #   'StorableFile' -> [ "/tmp/sereal.txt", 1 ]
     *    restored = Sereal.decode(encoded,Sereal::THAW)
     *    # this will call Sereal.THAW(:Sereal,"/tmp/sereal.txt",1)
     *
     * as you can see the array returned from FREEZE is exploded into
     * arguments for THAW, FREEZE *MUST* return array, otherwise the
     * encoder throws TypeException.
     * *BOTH* encoder *AND* decoder must be started with Sereal::THAW option
     * in order for it to work properly
     */
    rb_define_const(Sereal, "THAW",INT2NUM(__THAW));
    
    /*
     * the argument given to FREEZE() and THAW() as serializer
     * which at the moment is:
     *
     *    :Sereal
     */
    rb_define_const(Sereal, "FREEZER",ID2SYM(SEREAL));

    /*
     * enable debug output
     *
     *    name = "john"
     *    Sereal.decode([name,name],Sereal::REF|Sereal::DEBUG)
     *
     * procudes:
     *
     *    initialized (s) with compression type: 0 { p: 6, s: 14, l: 0, h: 0 }  method_sereal_decode()
     *    header end at 6 { p: 6, s: 14, l: 0, h: 6 }  method_sereal_decode()
     *      tracking object of class: String(id: 7678920) at position: 8 { p: 12, s: 14, l: 2, h: 6 }  sereal_to_rb_object()
     *      object: String: john { p: 12, s: 14, l: 2, h: 6 }  sereal_to_rb_object()
     *      reading reference from offset: 8, id: 7678920 { p: 14, s: 14, l: 2, h: 6 }  s_read_ref()
     *      object: String: john { p: 14, s: 14, l: 2, h: 6 }  sereal_to_rb_object()
     *     object: Array: ["john", "john"] { p: 14, s: 14, l: 1, h: 6 }  sereal_to_rb_object()
     *
     */
    rb_define_const(Sereal, "DEBUG",INT2NUM(__DEBUG));

    s_init_writers();
}

