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
use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);

# Test a couple of the basic constants

use Test::More tests => 2;

is(SRL_MAGIC_STRING, "=srl", "check magic string");
is(SRL_HDR_UNDEF, 37, "check arbitrary header constant");

