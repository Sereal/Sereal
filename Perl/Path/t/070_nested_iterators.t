#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

my $data = [
    { a => 1 },
    { a => 2 },
];

my $sp = Sereal::Path->new;
my $spi = Sereal::Path::Iterator->new(encode_sereal($data));
$spi->step_in();

{
    $spi->disjoin();
    $sp->set($spi);
    ok(1 == $sp->value('$.a'), 'match first element in array');
    $spi->unite();
}

$spi->next();

{
    $spi->disjoin();
    $sp->set($spi);
    ok(2 == $sp->value('$.a'), 'match second element in array');
    $spi->unite();
}

$spi->next();
ok(1 == $spi->eof(), "reached EOF");

done_testing();
