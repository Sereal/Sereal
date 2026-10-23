#!perl
use strict;
use warnings;
use Config;
use Test::More tests => 4;
use Sereal::Path::Iterator;

my $header = "=\xF3rl" . chr(4) . chr(0);
my $overflow = $Config{uvsize} == 8
    ? (chr(0x80) x 9) . chr(0x02) # 2^64
    : (chr(0x80) x 4) . chr(0x10); # 2^32

# ARRAYREF_2 contains two values. Moving to the second requires the iterator
# to skip the first value's varint, which must reject the overflowing value.
my $packet = $header . chr(0x42) . chr(0x20) . $overflow . chr(1);
my $iter = Sereal::Path::Iterator->new($packet);
$iter->step_in();
my $ok = eval { $iter->next(); 1 };
my $err = $@;
ok(!$ok, 'iterator rejects an overflowing varint while skipping a value');
like($err, qr/varint too big/i, 'iterator reports varint overflow');

my $uv_max = $Config{uvsize} == 8
    ? (chr(0xFF) x 9) . chr(0x01)
    : (chr(0xFF) x 4) . chr(0x0F);
my $valid_packet = $header . chr(0x42) . chr(0x20) . $uv_max . chr(1);
my $valid_iter = Sereal::Path::Iterator->new($valid_packet);
$valid_iter->step_in();
my $valid_ok = eval { $valid_iter->next(); 1 };
ok($valid_ok, 'iterator accepts a maximum UV varint while skipping a value')
    or diag $@;
is($valid_iter->decode(), 1, 'iterator remains correctly positioned after skip');
