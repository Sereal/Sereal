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

my $ok = eval { $sp->reset(encode_sereal([ 'reset' ])); 1 };
ok($ok, "reset Sereal::Path");

ok($sp->value('$[0]') eq 'reset', '->value() upon match after reseting');
ok(!defined $sp->value('$[1]'),   '->value() upon mismatch after reseting');

done_testing();
