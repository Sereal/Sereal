#!/bin/bash

set -e

cpanm Test::LongString
cpanm Test::Warn

pushd Perl/Decoder; perl Makefile.PL && make ; popd
pushd Perl/Encoder; perl Makefile.PL && make ; popd
pushd Perl/Splitter; perl Makefile.PL && make ; popd

pushd Perl/Decoder ; make test ; popd
pushd Perl/Encoder ; make test ; popd
pushd Perl/Splitter ; make test ; popd
