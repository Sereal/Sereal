#!perl
use strict;
use warnings;
use Sereal;

use Test::More tests => 3;
my $s = Sereal::encode_sereal("foo");
ok(defined $s);
ok(Sereal::looks_like_sereal($s));
is(Sereal::decode_sereal($s), "foo");

