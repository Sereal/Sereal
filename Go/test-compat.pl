#!/usr/bin/env perl

use blib "../Perl/Decoder/blib/";
use blib "../Perl/Encoder/blib/";
use lib "../Perl/shared/t/lib/";

use Sereal::Decoder qw(decode_sereal);
use Test::More;
use Data::Dumper;

sub slurp {
    my $n = shift;
    open (my $fh, "<", $n) or die "can't open $n: $!\n";
    local $/ = undef;
    my $d = <$fh>;
    return $d;
}

for my $n (glob("test_dir/test_data_?????")) {

    if (not -f "$n-go.out") {
        print "No Go test output for $n -- skipping\n";
        fail($n);
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
    
    printf ("Got:%s\nWanted:%s\n", Dumper($g), Dumper($p)) unless is_deeply($g, $p, $n);
}

done_testing();

