#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
use Sereal;

use Test::More tests => 3;
my $s = Sereal::encode_sereal("foo");
ok(defined $s);
ok(Sereal::looks_like_sereal($s)) or diag $s;
is(Sereal::decode_sereal($s), "foo");

