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

    do { fail($name); diag("No Go support for $n"); next } if $skip{$test_number};

    if (not -f "$n-go.out") {
        fail($name);
        diag("No Go test output for $n");
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
    
    $testdata = slurp("$n-go.out");
    my $g;

    eval {
        $g = decode_sereal($testdata);
        1;
    } or do {
        my $err = $@;
        fail($name);
        diag("Failed unpacking go $n: $err");
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

done_testing();

