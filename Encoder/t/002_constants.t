#!perl
use strict;
use warnings;
use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);

# Test a couple of the basic constants

use Test::More tests => 2;

is(SRL_MAGIC_STRING, "srl", "check magic string");
is(SRL_HDR_ASCII_LOW, 0b0100_0000, "check arbitrary header constant");

