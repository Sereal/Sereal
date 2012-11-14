#!/usr/bin/python

from distutils.core import setup, Extension
import os
srl_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

_sereal = Extension("_sereal", 
    sources = ['_serealmodule.c', 'srl_decoder.c'],
    depends = ['srl_common.h', 'srl_protocol.h', 'srl_inline.h', 'srl_decoder.h', 'snappy/snappy_decompress.c'],
    include_dirs = [os.path.join(srl_root, 'Perl', 'shared')],
)

setup(name = "sereal",
      version = "0.1",
      author = "Dennis Kaarsemaker",
      author_email = "dennis@kaarsemaker.net",
      url = "http://github.com/Sereal/Sereal",
      description = "Python implemetation of the Sereal protocol",
      py_modules = ["sereal"],
      ext_modules = [_sereal],
      classifiers = [
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
      ]
)
