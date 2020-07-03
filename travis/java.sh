#!/bin/sh

set -e

mvn --version
cd Java
mvn clean package
make test_files
make compat