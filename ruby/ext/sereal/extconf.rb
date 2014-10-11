require 'mkmf'

have_func("rb_intern_str", ["ruby.h"])
have_func("rb_sym_to_s", ["ruby.h"])
$CFLAGS << '-Wall -Werror -Wno-unknown-warning-option -Wno-error=unknown-warning -Wno-variadic-macros -Wno-gnu-zero-variadic-macro-arguments -Wno-c99-extensions -O3 -pedantic '
extension_name = 'sereal'
dir_config(extension_name)
create_makefile(extension_name)
