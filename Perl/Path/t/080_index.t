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
     23,
     [
      'a',
      'b',
     ],
     24,
#     {
#         gonzo => 1966,
#         ale => 1971,
#         ages => [ 13, 10 ],
#     },
    ];

my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

$spi->create_index();

done_testing();
