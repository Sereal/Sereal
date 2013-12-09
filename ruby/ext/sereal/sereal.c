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
 *   Sereal.decode(blob) - returns the decoded object
 *   
 * If the blob contains multiple compressed
 * sub-blobs you should call it with:
 *       
 *    Sereal.decode(blob) do |decoded|
 *       # do something with the decoded object 
 *    end
 * otherwise only the first decoded object will be returned
 *
 */
void Init_sereal() {
        Sereal = rb_define_class("Sereal", rb_cObject);
        rb_define_singleton_method(Sereal, "encode", method_sereal_encode, -2);
        rb_define_singleton_method(Sereal, "decode", method_sereal_decode, -2);
        rb_define_const(Sereal, "SNAPPY",INT2NUM(__SNAPPY));
        rb_define_const(Sereal, "SNAPPY_INCR",INT2NUM(__SNAPPY_INCR));
        rb_define_const(Sereal, "RAW",INT2NUM(__RAW));
        s_init_writers();
}

