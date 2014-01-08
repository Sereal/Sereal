#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Test::More tests => 4;

for my $ref (\"", [], {}, \*STDERR) {
    eval {
        decode_sereal($ref);
        1;
    } or do {
        like($@, qr/We can't decode a reference as Sereal!/, "We'll die on " . ref($ref) . " references");
    };
}
