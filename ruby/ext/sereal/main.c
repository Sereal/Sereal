#include "main.h"

VALUE Sereal = Qnil;
void Init_sereal();
VALUE method_sereal_encode(VALUE self, VALUE payload,VALUE compress);
VALUE method_sereal_decode(VALUE self, VALUE payload);
void Init_sereal() {
        Sereal = rb_define_class("Sereal", rb_cObject);
        rb_define_singleton_method(Sereal, "encode", method_sereal_encode, -2);
        rb_define_singleton_method(Sereal, "decode", method_sereal_decode, 1);
}

