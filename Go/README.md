Sereal implementation in Go
===========================

Install
-------

Library:

    $ go get github.com/Sereal/Sereal/Go/sereal

To use C implementations of snappy and zlib instead of pure-Go ones:

    $ go get -tags clibs github.com/Sereal/Sereal/Go/sereal

Hack
----

Run a test suite:

    $ cd $GOPATH/src/github.com/Sereal/Sereal/Go/sereal
    $ make test_all CORPUS_PROTO_VER=3 CORPUS_COMPRESS=SRL_ZLIB

Miscellaneous utils:

    $ cd $GOPATH/src/github.com/Sereal/Sereal/Go/sereal
    $ go run cmd/fuzzer/main.go | head -50

    $ cd $GOPATH/src/github.com/Sereal/Sereal/Go/sereal
    $ perl -Mblib=../../Perl/Encoder/blib -MSereal::Encoder -e'print encode_sereal({foo => [42, "bar"]})' | go run cmd/dsrl/main.go
