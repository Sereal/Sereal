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
use Test::More;
use Data::Dumper;
use File::Spec;
use Devel::Peek;

use Sereal::Decoder qw(decode_sereal looks_like_sereal scalar_looks_like_sereal);
use Sereal::Decoder::Constants qw(:all);


sub doc {
    my ($high, $version, $good)= @_;

    return(
        ($high eq "utf8" ? SRL_MAGIC_STRING_HIGHBIT_UTF8 :
         $high ? SRL_MAGIC_STRING_HIGHBIT : SRL_MAGIC_STRING) .
        chr($version) .
        chr(0) .
        ($good ? chr(SRL_HDR_UNDEF) : "")
    );
}


# Simple tests for looks_like_sereal.

my @tests = (
    # input, bool outcome, name
    [ "",                       "", "empty string is not Sereal"],
    [ undef,                    "", "undef string is not Sereal"],
    [ {},                       "", "{} is not Sereal"],
    [ [],                       "", "[] is not Sereal"],

    [ SRL_MAGIC_STRING,         "", "SRL_MAGIC_STRING alone is not Sereal"],
    [ doc(0, 0, 1),             "", "SRL_MAGIC_STRING with bad protocol is not Sereal"],
    [ doc(0, 1, 0),             "", "SRL_MAGIC_STRING protocol 1 with short body is not Sereal"],
    [ doc(0, 1, 1),              1, "SRL_MAGIC_STRING protocol 1 with small payload is Sereal"],
    [ doc(0, 2, 0),             "", "SRL_MAGIC_STRING protocol 2 with short body is not Sereal"],
    [ doc(0, 2, 1),              2, "SRL_MAGIC_STRING protocol 2 with small payload is Sereal"],
    [ doc(0, 3, 0),             "", "SRL_MAGIC_STRING protocol 3 with short body is not Sereal"],
    [ doc(0, 3, 1),             "", "SRL_MAGIC_STRING protocol 3 with small payload is Sereal"],

    [SRL_MAGIC_STRING_HIGHBIT,  "", "SRL_MAGIC_STRING_HIGHBIT alone is not Sereal"],
    [ doc(     1, 0, 1),        "", "SRL_MAGIC_STRING_HIGHBIT with bad protocol is not Sereal"],
    [ doc(     1, 1, 0),        "", "SRL_MAGIC_STRING_HIGHBIT protocol 1 with short body is not Sereal"],
    [ doc(     1, 1, 1),        "", "SRL_MAGIC_STRING_HIGHBIT protocol 1 with small payload is not Sereal"],
    [ doc(     1, 2, 0),        "", "SRL_MAGIC_STRING_HIGHBIT protocol 2 with short body is not Sereal"],
    [ doc(     1, 2, 1),        "", "SRL_MAGIC_STRING_HIGHBIT protocol 2 with small payload is not Sereal"],
    [ doc(     1, 3, 0),        "", "SRL_MAGIC_STRING_HIGHBIT protocol 3 with short body is not Sereal"],
    [ doc(     1, 3, 1),         3, "SRL_MAGIC_STRING_HIGHBIT protocol 3 with small payload is Sereal"],
    [ doc("utf8", 3, 1),         0, "SRL_MAGIC_STRING_HIGHBIT_UTF8 protocol 3 with small payload is identified as utf8"],

    ["=Srl". chr(1) . chr(0) . chr(SRL_HDR_UNDEF), "", "wrong magic string is not Sereal"],
);

plan tests => 2 + @tests * 5;

is(prototype(\&looks_like_sereal), undef);
is(prototype(\&scalar_looks_like_sereal), "\$");

my $decoder = Sereal::Decoder->new;
foreach my $t (@tests) {
    my ($input, $outcome, $name) = @$t;
    is( scalar_looks_like_sereal($input), $outcome, "$name (new function oppable)" );
    is( &scalar_looks_like_sereal($input), $outcome, "$name (new function non-oppable)" );
    is( looks_like_sereal($input), $outcome, "$name (old function)" );
    is( $decoder->looks_like_sereal($input), $outcome, "$name (object method)" );
    is( Sereal::Decoder->looks_like_sereal($input), $outcome, "$name (class method)" );
}
