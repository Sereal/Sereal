#!perl
use strict;
use warnings;

use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $a = { foo => 'bar' };
my $enc = encode_sereal(
    [ 70, 71, ['a', 'b', 'c', 'd'], 82, 83, $a, $a, 84 ],
    { sort_keys => 1 },
);

my $spi = Sereal::Path::Iterator->new($enc);
$spi->step_in();

is($spi->decode(), 70, 'decode item 70 in array');
$spi->step_in();
is($spi->decode(), 71, 'decode item 71 in array');
$spi->step_in();

$spi->step_in();
is($spi->decode(), 'a', 'decode item a in array');
$spi->step_in();
is($spi->decode(), 'b', 'decode item b in array');
$spi->step_in();
is($spi->decode(), 'c', 'decode item c in array');
$spi->step_in();
is($spi->decode(), 'd', 'decode item d in array');
$spi->step_out();

is($spi->decode(), 82, 'decode item 82 in array');
$spi->step_in();
is($spi->decode(), 83, 'decode item 83 in array');
$spi->step_in();

$spi->step_in();
is($spi->decode(), 'foo', 'decode hash key');
$spi->step_in();
is($spi->decode(), 'bar', 'decode hash value');
$spi->step_out();

$spi->step_in(); # step inside REFP
is($spi->hash_key(), 'foo', 'decode key foo in hash');
$spi->step_in();
is($spi->decode(), 'bar', 'decode item foo in hash');
$spi->step_out();

is($spi->decode(), 84, 'decode item 84 in array');

done_testing();
