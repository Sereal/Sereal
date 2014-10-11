#!/bin/bash

set -e
for ver in "1.9.3" "2.0.0" "2.1.0"; do
    rvm use $ver
    gem install rake-compiler
    cd ruby
    rake clean && rake && rake test
done
