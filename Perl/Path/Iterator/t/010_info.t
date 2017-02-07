#!perl
use strict;
use warnings;

use Test::More;
use Data::Dumper;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;

my $spi;

$spi = Sereal::Path::Iterator->new(encode_sereal(1));
is_deeply([ $spi->info() ], [ SRL_INFO_SCALAR, 1 ], "info of SCALAR");

$spi = Sereal::Path::Iterator->new(encode_sereal([ 1, 2, 3 ]));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_ARRAY, 3 ], "info of ARRAYREF_3");

$spi = Sereal::Path::Iterator->new(encode_sereal({ foo => 'bar', foofoo => 'barbar' }));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_HASH, 2 ], "info of HASHREF_2");

$spi = Sereal::Path::Iterator->new(encode_sereal(\{}));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_REF, 1 ], "info of REFN HASHREF_0");

$spi = Sereal::Path::Iterator->new(encode_sereal(\[]));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_REF, 1 ], "info of REFN ARRAYREF_0");

$spi = Sereal::Path::Iterator->new(encode_sereal(\"scalar"));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_SCALAR, 1 ], "info of REFN SCALAR");

$spi = Sereal::Path::Iterator->new(encode_sereal(\\"scalar"));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_REF, 1 ], "info of REFN REFN SCALAR");

$spi = Sereal::Path::Iterator->new(encode_sereal(bless {}, "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_HASH, 0, 'Foo' ], "info of OBJECT REFN HASHREF_0");

$spi = Sereal::Path::Iterator->new(encode_sereal(bless { foo => 'bar', foofoo => 'barbar' }, "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_HASH, 2, 'Foo' ], "info of OBJECT REFN HASHREF_2");

$spi = Sereal::Path::Iterator->new(encode_sereal(bless [], "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_ARRAY, 0, 'Foo' ], "info of OBJECT REFN ARRAYREF_0");

$spi = Sereal::Path::Iterator->new(encode_sereal(bless [ foo => 'bar', foofoo => 'barbar' ], "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_ARRAY, 4, 'Foo' ], "info of OBJECT REFN ARRAYREF_4");

my $s = "scalar";
$spi = Sereal::Path::Iterator->new(encode_sereal(bless \$s, "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_SCALAR, 1, 'Foo' ], "info of OBJECT REFN SCALAR");

my $s1 = "scalar";
$spi = Sereal::Path::Iterator->new(encode_sereal(bless \\$s1, "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_REF, 1, 'Foo' ], "info of OBJECT REFN REFN SCALAR");

$spi = Sereal::Path::Iterator->new(encode_sereal(bless \(bless {}, "Foo"), "Foo"));
is_deeply([ $spi->info() ], [ SRL_INFO_BLESSED | SRL_INFO_REF_TO | SRL_INFO_REF, 1, 'Foo' ], "info of OBJECT REFN REFN SCALAR");

$spi = Sereal::Path::Iterator->new(encode_sereal(\(bless {}, "Foo")));
is_deeply([ $spi->info() ], [ SRL_INFO_REF_TO | SRL_INFO_REF, 1 ], "info of OBJECT REFN REFN SCALAR");

done_testing();
