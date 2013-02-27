from distutils.core import setup, Extension
import os

srl_py_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

modules = [
    Extension('srlencoder',
              sources=['srl_encoder_module.c','srl_encoder.c'],
              depends=['srl_encoder.h','srl_inline.h','srl_buffer.h','util.h'],
              include_dirs=[os.path.join(srl_py_root, 'shared')],
              ),
    ]

setup(name = 'sereal',
      version = '0.01',
      description = 'Encode data structures in Sereal format.',
      ext_modules = modules
      )
