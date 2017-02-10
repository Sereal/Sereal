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
        bless([[[ $a ]]], "Foo"),
        90,
    ],
    { sort_keys => 1 },
);

my $spi = Sereal::Path::Iterator->new($enc);

$spi->step_in(5);
is($spi->decode(), 'a', 'decode 90 after 3 steps');
$spi->next(4);
lives_ok(sub { $spi->step_out() }, 'expect step_out() to live');
dies_ok(sub { $spi->step_in() }, 'expect step_in() to die because to more records');
lives_ok(sub { $spi->step_out(3) }, 'expect step_out() to live');
is($spi->decode(), '90', 'decode 90 after steps out');

done_testing();
