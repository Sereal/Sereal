#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet;
use Sereal::Encoder;
$| = 1;
print "1..40\n";
Sereal::Encoder::_ptabletest::test();

