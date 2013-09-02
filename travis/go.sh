#!/bin/sh

set -e

export GOPATH=$HOME/gopath
go version
cd Go/sereal
go get -d -v ./... && go build -v ./...
go test -v ./...
make compat
