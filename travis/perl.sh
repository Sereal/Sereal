#!/bin/bash

set -e

cpanm Test::Deep
cpanm Test::Differences
cpanm Test::Exception
cpanm Test::LongString
cpanm Test::Warn

pushd Perl/Decoder; perl Makefile.PL && make ; popd
pushd Perl/Encoder; perl Makefile.PL && make ; popd

pushd Perl/Decoder ; make test ; popd
pushd Perl/Encoder ; make test ; popd

cpanm Sereal::Decoder
cpanm Sereal::Encoder

pushd Perl/Splitter; perl Makefile.PL && make ; popd
pushd Perl/Splitter; make test ; popd

pushd Perl/Path; perl Makefile.PL && make ; popd
pushd Perl/Path; make test ; popd
