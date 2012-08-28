#!perl
use strict;
use warnings;
use Sereal::Encoder;
$| = 1;
print "1..40\n";
Sereal::Encoder::_ptabletest::test();

