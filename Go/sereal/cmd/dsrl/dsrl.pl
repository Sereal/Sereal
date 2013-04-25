#!/usr/bin/perl

use blib "../../Perl/Decoder/blib/";
use blib "../../Perl/Encoder/blib/";
use lib "../../Perl/shared/t/lib/";

use Sereal::Decoder qw(decode_sereal);
use Data::Dumper;

$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

sub slurp {
    my $n = shift;
    open (my $fh, "<", $n) or die "can't open $n: $!\n";
    local $/ = undef;
    my $d = <$fh>;
    return $d;
}

my $fname = shift;

my $s = slurp($fname);

my $o = decode_sereal($s);

print Dumper($o), "\n";
