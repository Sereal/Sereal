#!/bin/bash

set -e

sudo gem install rake-compiler
cd ruby
rake && rake && rake test 
