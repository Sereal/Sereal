require 'mkmf'

have_func("rb_intern_str", ["ruby.h"])
have_func("rb_sym_to_s", ["ruby.h"])

extension_name = 'sereal'
dir_config(extension_name)
create_makefile(extension_name)
