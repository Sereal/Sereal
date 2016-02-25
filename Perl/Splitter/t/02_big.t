#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Splitter;

use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal);

my $data = do{local(@ARGV,$/)='big.srl';<>};
my $struct = decode_sereal($data);

my $size = 50 * 1024;
my $o = Sereal::Splitter->new({chunk_size => $size, input => $data, compress => 2});

my @acc;

my $st = 0;
my $total_time = 0;
my $validate = 1;
my $nb_chunks = 0;
while (1) {
    my $chunk;
    my $error;

    $chunk = $o->next_chunk();
    defined $chunk
      or last;

    $nb_chunks++;

    if ($validate) {
        my $chunk_struct = decode_sereal($chunk);
        my $elts = @$chunk_struct;
        my $new_struct = [ @{$struct}[$st .. $st + $elts-1] ];
        push @acc, @$new_struct;
        my $new_data = encode_sereal($new_struct, { dedupe_strings => 1 });

        is_deeply($chunk_struct, $new_struct, "validating $st -> " . ($st + $elts - 1));

        $st += $elts;
    }
}

if ($validate) {
    is_deeply([@acc], $struct);
}

is($nb_chunks, 18);

done_testing;



