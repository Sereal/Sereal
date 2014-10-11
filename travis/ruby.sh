#!/bin/bash

set -e

gem install rake-compiler
cd ruby
rake && rake && rake test 
