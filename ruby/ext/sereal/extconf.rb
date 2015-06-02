require 'mkmf'

have_func("rb_intern_str", ["ruby.h"])
have_func("rb_sym_to_s", ["ruby.h"])
have_func("rb_hash_clear", ["ruby.h"])
$CFLAGS << ' -Wall -Wextra -Wno-unknown-warning-option -Wno-variadic-macros -Wno-gnu-zero-variadic-macro-arguments -Wno-declaration-after-statement -Wno-unused-parameter -Wno-c99-extensions -O3 '

extension_name = 'sereal'
dir_config(extension_name)
create_makefile(extension_name)
