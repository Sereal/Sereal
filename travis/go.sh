#!/bin/sh

set -e

export GOPATH=$HOME/gopath
go version
cd Go/sereal
go get -d -v ./... && go build -v ./...
make test_files
go test -v ./...
make compat
