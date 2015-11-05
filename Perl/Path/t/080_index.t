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
            'Thorin'      => 'Dwarf',
            'Frodo'       => 'Hobbit',
            'Mithrandir'  => 'Wizard',
            'Morgoth'     => 'Maia',
            'Iluvatar'    => 'God',
            'Aragorn'     => 'Man',
            'Mandos'      => 'Maia',
            'Saruman'     => 'Wizard',
            'Boromir'     => 'Man',
            'Meriadoc'    => 'Hobbit',
            'Peregrin'    => 'Hobbit',
            'Balin'       => 'Dwarf',
        }
        # 23,
        # 24,
    ];

my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

$spi->create_index(1_000_000, 2);

done_testing();
