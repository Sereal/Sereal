#!perl
use strict;
use warnings;

#use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $spi = Sereal::Path::Iterator->new(encode_sereal([ 0, 1, 2 ]));
is_deeply([$spi->info()], ['ARRAY', 3], 'current element is ARRAY');

$spi->next();
ok($spi->eof() == 1, 'reached EOF');

$spi->reset();
ok($spi->eof() == 0, 'returned to begging');

$spi->step_in();
is_deeply([$spi->stack_info()], ['ARRAY', 3], 'current element on stack is ARRAY');

ok($spi->decode() == 0, 'decode first item in array');

$spi->array_goto(2);
ok($spi->decode() == 2, 'decode third item in array');

is_deeply([$spi->info()], ['SCALAR', 0], 'current element on stack is SCALAR');

done_testing();
