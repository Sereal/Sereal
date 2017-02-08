#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

my $enc = encode_sereal(
    [
        70,
        71,
        [
            'a',
            'b',
            'c',
            'd',
            {
                foo => 'barbar',
                foobar => bless [
                    1,
                    2,
                    3,
                    bless [
                        \"scalar"
                    ], "Foo",
                ], "Foo",
            }
        ]
    ],
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
$spi->step_in();

is_deeply(
    [ $spi->info() ],
    [ SRL_INFO_REF_TO | SRL_INFO_HASH, 2 ],
    'get information about hashref'
);

$spi->step_in();
is($spi->decode(), 'foo', 'decode hash key #1');
$spi->step_in();
is($spi->decode(), 'barbar', 'decode hash value #1');
$spi->step_in();
is($spi->decode(), 'foobar', 'decode hash key #2');
$spi->step_in();

is_deeply(
    [ $spi->info() ],
    [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_ARRAY, 4, 'Foo' ],
    'get information about blessed arrayhref'
);

$spi->step_in();
is($spi->decode(), 1, 'decode item 1 in blessed array');
$spi->step_in();
is($spi->decode(), 2, 'decode item 2 in blessed array');
$spi->step_in();
is($spi->decode(), 3, 'decode item 3 in blessed array');
$spi->step_in();

is_deeply(
    [ $spi->info() ],
    [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_ARRAY, 1, 'Foo' ],
    'get information about blessed arrayref'
);

$spi->step_in();
is_deeply(
    [ $spi->info() ],
    [ SRL_INFO_REF_TO | SRL_INFO_SCALAR, 1 ],
    'get information about scalarref'
);

$spi->step_in();
is($spi->decode(), "scalar", 'decode scalar');
$spi->step_in();

dies_ok(sub { $spi->step_in() }, "expecting last step_in to die");

done_testing();
