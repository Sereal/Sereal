#!perl
use strict;
use warnings;
use Sereal::Merger;
$| = 1;
print "1..35\n";
Sereal::Merger::_strtabletest::test();
Sereal::Merger::_strtabletest::test_purge();

