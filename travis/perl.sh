#!/usr/bin/env bash

set -Eeuo pipefail

perl -V

cpanm Test::Deep
cpanm Test::Differences
cpanm Test::Exception
cpanm Test::LongString
cpanm Test::Warn
cpanm Test::MemoryGrowth

pushd Perl/Decoder
perl Makefile.PL
make
popd

pushd Perl/Encoder
perl Makefile.PL
make
popd

pushd Perl/Decoder
make test
popd

pushd Perl/Encoder
make test
popd
