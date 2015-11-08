#!perl
use strict;
use warnings;

#use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $all_data = [

    # Some single scalars
    11,

    3.1415,

    "Gonzo",

    # A simple array
    [ 4, 5, 6, ],

    # A simple hash
    { 'nl' => 31, 'cl' => 56, },

    # A complex nested data collection
    [
        21,
        22,
        [
            'a',
            'b',
            [
                1,
                2,
                3,
                4,
                5,
            ],
            'c',
        ],
        [
            'x',
            {
                'Thorin'      => 'Dwarf',
                'Frodo'       => 'Hobbit',
                'Mithrandir'  => 'Wizard',
                'Morgoth'     => 'Maia',
                'Iluvatar'    => 'God',
                'Aragorn'     => 'Man',
                'Elrond'      => ['Man', 'Elf'],
                'Mandos'      => 'Maia',
                'Saruman'     => 'Wizard',
                'Boromir'     => 'Man',
                'Meriadoc'    => 'Hobbit',
                'Peregrin'    => 'Hobbit',
                'Balin'       => 'Dwarf',
            },
            'y',
        ],
        23,
        24,
    ],
];

my $memory_size = 1_000_000;  # limit index memory to 1M bytes
# my $memory_size = 40;         # limit index memory to 40 bytes

# my $index_depth = 0;     # no limits on index depth
my $index_depth = 2;     # will not index the Middle Earth hash

my $hash_factor = 1.5;   # make hash tables use this many times the required space
# my $hash_factor = 1.0;   # make hash tables use exactly the required space

for my $data (@$all_data) {
    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

    $spi->create_index(
        {
            memory_size => $memory_size,
            index_depth => $index_depth,
            hash_factor => $hash_factor,
        });
}

done_testing();
