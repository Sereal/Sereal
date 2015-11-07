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
            'c',
        ],
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
        },
        23,
        24,
    ];

my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

my $memory_size = 1_000_000;  # limit index memory to 1M bytes
# my $memory_size = 40;         # limit index memory to 40 bytes

my $index_depth = 0;     # no limits on index depth
# my $index_depth = 2;     # will not index the Middle Earth hash

my $hash_factor = 1.5;   # make hash tables use this many times the required space
# my $hash_factor = 1.0;   # make hash tables use exactly the required space

$spi->create_index(
    {
        memory_size => $memory_size,
        index_depth => $index_depth,
        hash_factor => $hash_factor,
    });

done_testing();
