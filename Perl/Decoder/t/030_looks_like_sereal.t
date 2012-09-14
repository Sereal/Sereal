#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal looks_like_sereal);
use Sereal::Decoder::Constants qw(:all);
use Data::Dumper;
use File::Spec;
use Devel::Peek;

use Test::More;

# Simple tests for looks_like_sereal.

my @tests = (
    # input, bool outcome, name
    ["", 0, "empty string is not Sereal"],
    [undef, 0, "undef string is not Sereal"],
    [SRL_MAGIC_STRING, 0, "SRL_MAGIC_STRING alone is not Sereal"],
    [SRL_MAGIC_STRING . chr(1) . chr(0), 0, "SRL_MAGIC_STRING with header is not Sereal"],
    [SRL_MAGIC_STRING . chr(1) . chr(0) . chr(SRL_HDR_UNDEF), 1, "SRL_MAGIC_STRING with header and small payload is Sereal"],
    [SRL_MAGIC_STRING . chr(0) . chr(0) . chr(SRL_HDR_UNDEF), 0, "SRL_MAGIC_STRING with bad header is not Sereal"],
    ["=Srl". chr(1) . chr(0) . chr(SRL_HDR_UNDEF), 0, "wrong magic string is not Sereal"],
);

plan tests => @tests * 3;

my $decoder = Sereal::Decoder->new;
foreach my $t (@tests) {
    my ($input, $outcome, $name) = @$t;
    ok(looks_like_sereal($input) == $outcome, $name . " (function)");
    ok($decoder->looks_like_sereal($input) == $outcome, $name . " (object method)");
    ok(Sereal::Decoder->looks_like_sereal($input) == $outcome, $name . " (class method)");
}
