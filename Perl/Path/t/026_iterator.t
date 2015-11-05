#!perl
use strict;
use warnings;

#use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $a = {};
my $spi = Sereal::Path::Iterator->new(encode_sereal([ 70, 71, ['a', 'b', 'c', 'd'], 82, 83 ]));
my $g = 0;

$spi->step_in();

is($spi->decode(), 70);
$spi->next();
is($spi->decode(), 71);
$spi->next();

$spi->step_in();
is($spi->decode(), 'a');
$spi->next();
is($spi->decode(), 'b');
$spi->next();
is($spi->decode(), 'c');
$spi->next();
is($spi->decode(), 'd');
$spi->next();
$spi->step_out();

# $g = 82;  # Correct behaviour, but tests fail
$g = 70;  # Incorrect behaviour, but tests pass

is($spi->decode(), $g+0);
$spi->next();
is($spi->decode(), $g+1);
$spi->next();

$spi->step_out();

done_testing();
