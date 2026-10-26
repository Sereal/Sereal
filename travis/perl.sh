#!/usr/bin/env bash

set -Eeuo pipefail

perl -V

cpanm Test::Deep
cpanm Test::Differences
cpanm Test::Exception
cpanm Test::LongString
cpanm Test::Warn
# Exclude Test::MemoryGrowth on Perl 5.30, including dependency self-tests.
if perl -e 'exit($] >= 5.030 && $] < 5.031 ? 1 : 0)'; then
    cpanm Test::MemoryGrowth
fi

pushd Perl/Decoder
perl Makefile.PL
make
popd

pushd Perl/Encoder
perl Makefile.PL
make
popd

export PERL5LIB="$PWD/Perl/Decoder/blib/lib:$PWD/Perl/Decoder/blib/arch:$PWD/Perl/Encoder/blib/lib:$PWD/Perl/Encoder/blib/arch${PERL5LIB:+:$PERL5LIB}"

pushd Perl/Decoder
make test
popd

pushd Perl/Encoder
make test
popd

pushd Perl/Sereal
perl Makefile.PL
make test
popd
