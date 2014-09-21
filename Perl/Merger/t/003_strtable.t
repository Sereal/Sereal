#!perl
use strict;
use warnings;
use Sereal::Merger;
$| = 1;
print "1..30\n";
Sereal::Merger::_strtabletest::test();

