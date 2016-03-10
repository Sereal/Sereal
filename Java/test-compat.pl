#!/usr/bin/env perl

use strict;
use warnings;

use blib "../Perl/Decoder/blib/";
use blib "../Perl/Encoder/blib/";
use lib "../Perl/shared/t/lib/";

use Sereal::Decoder qw(decode_sereal);
use Test::More;
use Data::Dumper;

$Data::Dumper::Indent = 0;
$Data::Dumper::Sortkeys = 1;

sub slurp {
    my $n = shift;
    open (my $fh, "<", $n) or die "can't open $n: $!\n";
    local $/ = undef;
    my $d = <$fh>;
    return $d;
}

# Some parts of the Sereal specification (like unsigned integers) are deliberately not
# implemented in Java. As a result a set of tests checking omitted functionality
# will fail. To reduce a level of false negatives here we list names of all
# tests that are supposed to fail and skip them later.
#
# Multiple original tests share the same name making the following list not
# 100% reliable and accurate. To mitigate it we also maintain a counter holding
# a total number of tests to be skipped.
#
# those overflow a signed long
my @big_integers = qw(
    -9223372036854775808
    -9223372036854775807
    9223372036854775808
    18446744073709551614
    18446744073709551615
    11285066962739960988
);
# those could be fixed by mangling the output the correct way, but I
# don't think it's worth the trouble
my @big_integer_tests = (
    'integer: %s',
    'scalar ref to integer: %s',
    'nested scalar ref to integer: %s',
    'array ref to integer: %s',
    'hash ref to integer: %s',
    'array ref to duplicate integer: %s',
    'AoA of duplicates integer: %s',
    'array ref to aliases integer: %s',
    'array ref to scalar refs to same integer: %s',
);
my $skip_total =
    (@big_integer_tests * @big_integers) +
    5;
my %skip = map { $_ => 1 } (
    (map {
        my $pattern = $_;
        map sprintf($pattern, $_), @big_integers
     } @big_integer_tests),
    'BlessedArrayCheck 1',
    'BlessedArrayCheck 2',
    'Scalar Cross Blessed Array',
    'TODO BlessedArrayCheck 1',
    'TODO BlessedArrayCheck 2',
    'TODO Scalar Cross Blessed Array',
    'scalar cross',
    'weak scalar cross',
);

my $skipped = 0;

for my $n (glob("test_dir/test_data_?????")) {

    (my $test_number = $n) =~ s/.*test_data_0*//;

    chomp(my $name = slurp(sprintf("test_dir/test_name_%05d", $test_number)));

    if ($skip{$name}) {
        SKIP: { skip "$name ($n) not implemented", 1; };
        $skipped++;
        next;
    }

    if (not -f "$n-java.out") {
        fail($name);
        diag("No Java test output for $n");
#        die;
        next;
    }

    my $testdata = slurp($n);
    my $p;
    eval {
        $p = decode_sereal($testdata);
        1;
    } or do {
        my $err = $@;
        fail($name);
        diag("Failed unpacking perl $n: $err");
        next;
    };
    
    $testdata = slurp("$n-java.out");
    my $g;

    eval {
        $g = decode_sereal($testdata);
        1;
    } or do {
        my $err = $@;
        fail($name);
        diag("Failed unpacking java $n: $err");
        next;
    };

    my $dg = Dumper($g);
    my $dp = Dumper($p);

    if (!ok($dg eq $dp, $name)) {
        diag("$n\nGot: $dg\nExp: $dp");
#        die;
        next;
    }
}

is($skipped, $skip_total, "skipped expected number of tests");

done_testing();

