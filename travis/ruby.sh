#!/bin/bash

set -e
cd ruby
for ver in "1.9.3" "2.0.0" "2.1.0"; do
    rvm alias create default $ver
    gem install rake-compiler
    rake clean && rake && rake test
done
