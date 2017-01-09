#!perl
use strict;
use warnings;

use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $a = { foo => 'bar' };
my $spi = Sereal::Path::Iterator->new(encode_sereal(
    [ 70, 71, ['a', 'b', 'c', 'd'], 82, 83, $a, $a, 84 ]
));

$spi->step_in();

ok($spi->decode() == 70, 'decode item 70 in array');
$spi->next();
ok($spi->decode() == 71, 'decode item 71 in array');
$spi->next();

$spi->step_in();
ok($spi->decode() eq 'a', 'decode item a in array');
$spi->next();
ok($spi->decode() eq 'b', 'decode item b in array');
$spi->next();
ok($spi->decode() eq 'c', 'decode item c in array');
$spi->next();
ok($spi->decode() eq 'd', 'decode item d in array');
$spi->step_out();

ok($spi->decode() == 82, 'decode item 82 in array');
$spi->next();
ok($spi->decode() == 83, 'decode item 83 in array');
$spi->next();

$spi->next(); # skip first $a

$spi->step_in(); # step inside REFP
ok($spi->hash_key() eq 'foo', 'decode key foo in hash');
$spi->next();
ok($spi->decode() eq 'bar', 'decode item foo in hash');
$spi->step_out();

ok($spi->decode() == 84, 'decode item 84 in array');

done_testing();
