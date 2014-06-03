#!/bin/bash

set -e

pushd Perl/Decoder; cpanm --quiet --installdeps --notest . ; perl Makefile.PL && make ; popd
pushd Perl/Encoder; cpanm --quiet --installdeps --notest . ; perl Makefile.PL && make ; popd

pushd Perl/Decoder ; make test ; popd
pushd Perl/Encoder ; make test ; popd
