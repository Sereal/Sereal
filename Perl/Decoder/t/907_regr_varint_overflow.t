#!perl
use strict;
use warnings;
use Config;
use Test::More;
use Sereal::Decoder qw(decode_sereal);

plan skip_all => 'Requires 32-bit or 64-bit UVs'
    unless $Config{uvsize} == 4 || $Config{uvsize} == 8;

my $header = "=\xF3rl" . chr(4) . chr(0);
my $overflow = $Config{uvsize} == 8
    ? (chr(0x80) x 9) . chr(2)
    : (chr(0x80) x 4) . chr(0x10);
my $maximum = $Config{uvsize} == 8
    ? (chr(0xff) x 9) . chr(1)
    : (chr(0xff) x 4) . chr(0x0f);
my $maximum_value = $Config{uvsize} == 8
    ? '18446744073709551615'
    : '4294967295';

for my $case (
    [ 'scalar safe reader', chr(0x20) . $overflow ],
    [ 'scalar fast reader', chr(0x20) . $overflow . (chr(0) x 16) ],
    [ 'array count', chr(0x28) . chr(0x2b) . $overflow . chr(1) ],
    [ 'hash count', chr(0x28) . chr(0x2a) . $overflow . chr(0x61) . 'k' . chr(1) ],
    [ 'binary length', chr(0x26) . $overflow . 'SECRET' ],
) {
    my ($name, $body) = @$case;
    my $ok = eval { decode_sereal($header . $body); 1 };
    ok(!$ok, "$name rejects an overflowing varint");
    like($@, qr/varint (?:too big|overflows U32)/i,
        "$name reports the varint overflow");
}

for my $case (
    [ 'safe reader', chr(0x20) . $maximum ],
    [ 'fast reader', chr(0x20) . $maximum . (chr(0) x 16) ],
) {
    my ($name, $body) = @$case;
    my $value = eval { decode_sereal($header . $body) };
    is($@, '', "$name accepts the maximum UV");
    is("$value", $maximum_value, "$name preserves the maximum UV");
}

done_testing;
