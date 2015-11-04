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

ok($spi->decode() == 70, 'decode item 70 in array');
$spi->next();
ok($spi->decode() == 71, 'decode item 71 in array');
$spi->next();

$spi->step_in();
ok($spi->decode() eq 'a', 'decode item a in array');
$spi->next();
ok($spi->decode() eq 'b', 'decode item b in array');
$spi->next();
ok($spi->decode() eq 'c', 'decode item b in array');
$spi->next();
ok($spi->decode() eq 'd', 'decode item b in array');
$spi->next();
$spi->step_out();

TODO: {
	local $TODO = "Not working now, possible bug";

	$g = 82;
	#$g = 70;
	ok($spi->decode() == ($g + 0), 'decode item 82 in array');
	$spi->next();
	ok($spi->decode() == ($g + 1), 'decode item 83 in array');
	$spi->next();
};

$spi->step_out();

done_testing();
