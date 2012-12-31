#include <sereal.h>

VALUE Sereal = Qnil;
void Init_sereal();
void Init_sereal() {
        Sereal = rb_define_class("Sereal", rb_cObject);
        rb_define_singleton_method(Sereal, "encode", method_sereal_encode, -2);
        rb_define_singleton_method(Sereal, "decode", method_sereal_decode, 1);
}

