#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $sp = Sereal::Path->new(encode_sereal([ 'test' ]));
ok($sp->value('$[0]') eq 'test', '->value() upon match');
ok(!defined $sp->value('$[1]'),  '->value() upon mismatch');

ok(1 == $sp->values('$[0]'), '->values() in scalar context upon match');
ok(0 == $sp->values('$[1]'), '->values() in scalar context upon mismatch');

is_deeply([ $sp->values('$[0]') ], [ 'test' ], '->values() in list context upon match');
is_deeply([ $sp->values('$[1]') ], [],         '->values() in list context upon match');

my $ok = eval { $sp->set(encode_sereal([ 'set' ])); 1 };
ok($ok, "set Sereal::Path");

ok($sp->value('$[0]') eq 'set', '->value() upon match after seting');
ok(!defined $sp->value('$[1]'),   '->value() upon mismatch after seting');

done_testing();
