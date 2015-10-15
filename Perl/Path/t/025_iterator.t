#!perl
use strict;
use warnings;

#use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $a = {};
my $spi = Sereal::Path::Iterator->new(encode_sereal([ 0, 1, 2, $a, $a ]));
is_deeply([$spi->info()], ['ARRAY', 5], 'current element is ARRAY');

$spi->next();
ok($spi->eof() == 1, 'reached EOF');

$spi->reset();
ok($spi->eof() == 0, 'returned to begging');

$spi->step_in();
is_deeply([$spi->stack_info()], ['ARRAY', 5], 'element on stack is ARRAY');

ok($spi->decode() == 0, 'decode first item in array');

$spi->array_goto(2);
ok($spi->decode() == 2, 'decode third item in array');

is_deeply([$spi->info()], ['SCALAR', 0], 'second element in array is SCALAR');

$spi->array_goto(4);
is_deeply([$spi->info()], ['HASH', 0], 'fifth element in array is HASH');

done_testing();
