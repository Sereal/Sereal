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
        # [
        #   'a',
        #   'b',
        #   'c',
        # ],
        {
            # 'Thorin' => 'Dwarf',
            # 'Frodo' => 'Hobbit',
            # 'Gandalf' => [ 'Wizard', 'Ainu' ],
            'Iluvatar' => [ 'Maia', 'God' ],
            # 'Oakenshield' => [ 'Dwarf' ],
        }
        # 23,
        # 24,
    ];

my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

$spi->create_index();

done_testing();
