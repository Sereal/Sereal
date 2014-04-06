#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal looks_like_sereal scalar_looks_like_sereal);
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

plan tests => 2 + @tests * 5;

is prototype(\&looks_like_sereal), undef;
is prototype(\&scalar_looks_like_sereal), "\$";

my $decoder = Sereal::Decoder->new;
foreach my $t (@tests) {
    my ($input, $outcome, $name) = @$t;
    is scalar_looks_like_sereal($input), !!$outcome, "$name (new function oppable)";
    is &scalar_looks_like_sereal($input), !!$outcome, "$name (new function non-oppable)";
    is looks_like_sereal($input), !!$outcome, "$name (old function)";
    is $decoder->looks_like_sereal($input), !!$outcome, "$name (object method)";
    is +Sereal::Decoder->looks_like_sereal($input), !!$outcome, "$name (class method)";
}
