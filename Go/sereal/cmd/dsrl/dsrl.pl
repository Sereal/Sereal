#!/usr/bin/perl

use blib "../../Perl/Decoder/blib/";
use blib "../../Perl/Encoder/blib/";
use lib "../../Perl/shared/t/lib/";

use Sereal::Decoder qw(decode_sereal);
use Data::Dumper;

$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

$/ = undef;

while(<>) {
    my $o = decode_sereal($_);
    print Dumper($o), "\n";
}
