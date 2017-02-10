#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Test::Exception;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

my $a = [ 'a', 'b', 'c', 'd' ];
my $h = { foo => 'bar' };
my $enc = encode_sereal(
    [
        71,
        $a,
        $a,
        80,
        bless({}, "Foo"),
        bless({ a => { b => { c => $a } } }, "Foo"),
        90,
        $h,
        $h,
        100,
    ],
    { sort_keys => 1 },
);

my $spi = Sereal::Path::Iterator->new($enc);

$spi->step_in();
$spi->next(3);
is($spi->decode(), 80, 'decode 80 after 3 steps');
$spi->next(3);
is($spi->decode(), 90, 'decode 90 after 3 steps');
$spi->next(3);
is($spi->decode(), 100, 'decode 100 after 3 steps');
$spi->next();
dies_ok(sub { $spi->next() }, "expecting last step to die");

done_testing();
