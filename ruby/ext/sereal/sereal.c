#include "sereal.h"
#include "encode.h"

VALUE Sereal = Qnil;
void Init_sereal();

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
void Init_sereal() {
        Sereal = rb_define_class("Sereal", rb_cObject);
        rb_define_singleton_method(Sereal, "encode", method_sereal_encode, -2);
        rb_define_singleton_method(Sereal, "decode", method_sereal_decode, -2);
        rb_define_const(Sereal, "SNAPPY",INT2NUM(__SNAPPY));
        rb_define_const(Sereal, "SNAPPY_INCR",INT2NUM(__SNAPPY_INCR));
        rb_define_const(Sereal, "RAW",INT2NUM(__RAW));
        rb_define_const(Sereal, "REF",INT2NUM(__REF));
        s_init_writers();
}

