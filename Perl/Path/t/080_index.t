#!perl
use strict;
use warnings;

#use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $data =
    [
     21,
     22,
     [
      'a',
      'b',
     ],
     23,
     24,
    ];

my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

$spi->create_index();

done_testing();
