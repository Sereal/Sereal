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
use Test::More tests => 4;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);

for my $ref (\"", [], {}, \*STDERR) {
    eval {
        decode_sereal($ref);
        1;
    } or do {
        like($@, qr/We can't decode a reference as Sereal!/, "We'll die on " . ref($ref) . " references");
    };
}
