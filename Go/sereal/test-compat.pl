#!/usr/bin/env perl

use strict;
use warnings;

use blib "../../Perl/Decoder/blib/";
use blib "../../Perl/Encoder/blib/";
use lib "../../Perl/shared/t/lib/";

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

my %skip = map { $_ => 1 } qw(); # 513..591; # alias tests

for my $n (glob("test_dir/test_data_?????")) {

    (my $test_number = $n) =~ s/.*test_data_0*//;

    chomp(my $name = slurp(sprintf("test_dir/test_name_%05d", $test_number)));

    print("test number $test_number\n");

    do { fail($n) ; next } if $skip{$test_number};

    if (not -f "$n-go.out") {
        print "No Go test output for $n ($name) -- skipping\n";
        fail($n);
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
        print "Failed unpacking perl $n: $err\n";
        fail($n);
        next;
    };
    
    $testdata = slurp("$n-go.out");
    my $g;

    eval {
        $g = decode_sereal($testdata);
        1;
    } or do {
        my $err = $@;
        print "Failed unpacking go $n: $err\n";
        fail($n);
        next;
    };

    my $dg = Dumper($g);
    my $dp = Dumper($p);

    if (!ok($dg eq $dp, $name)) {
        print "Got: $dg\nExp: $dp\n";
#        die;
    }
}

done_testing();

