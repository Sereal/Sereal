#!/bin/bash

set -e

sudo apt-get install libtest-longstring-perl libtest-warn-perl

pushd Perl/Encoder && perl Makefile.PL && make && popd
pushd Perl/Decoder && perl Makefile.PL && make && popd

pushd Perl/Decoder && make test ; popd
pushd Perl/Encoder && make test ; popd
