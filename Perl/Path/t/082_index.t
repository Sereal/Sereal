#!perl
use strict;
use warnings;

#use Sereal::Path;
use Sereal::Path::Iterator;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Data::Dumper;

my $all_data = [

    # Array with alternating small and large elements
    [
        21,
        { 'Mario' => 1, 'Lore'    => 0, 'Gonzo'      => 1, },
        23,
        { 'Ale'   => 0, 'Rodrigo' => 1, 'Maria Jose' => 0, },
        25,
    ],
];

my $memory_size = 110;    # limit index memory

my $index_depth = 0;     # no limits on index depth

for my $data (@$all_data) {
    my $spi = Sereal::Path::Iterator->new(encode_sereal($data));

    $spi->create_index(
        {
            memory_size => $memory_size,
            index_depth => $index_depth,
        });
}

done_testing();
