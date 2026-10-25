#!perl 
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet::Corpus qw(read_packet);
use Test::More tests => 3;
use Sereal::Decoder;

# Regression test for issue 15 on github:
# https://github.com/Sereal/Sereal/issues/15

# Snappy compression was broken by assuming remaining document length
# is the same as compressed-data-length.

my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
my $decoder = Sereal::Decoder->new();
my $single = read_packet( $corpus, 'decoder-features', 'issue15/single' );
my $joined = read_packet( $corpus, 'decoder-features', 'issue15/concatenated' );

# Establish base behaviour from an Encoder-generated corpus packet.
is( $decoder->decode($single), 'foo', 'normal decode' );

# Decode concatenated packets and verify decode_with_offset advances correctly.
is_deeply( _decode_with_offset( $joined, $decoder ), [ 'foo', 'foo' ], 'decode with offset' );

sub _decode_with_offset {
    my ( $value, $decoder ) = @_;

    my @decoded_values;
    my $pos = 0;

    #my $first = index($value, "=srl", 1);
    #$value = substr($value, 0, $first);
    my $ok = eval {
        while ( $pos < length($value) ) {
            push @decoded_values, $decoder->decode_with_offset( $value, $pos );
            last if $decoder->bytes_consumed == 0;
            $pos += $decoder->bytes_consumed;
        }
        1;
    };
    my $err = $@;
    ok( $ok, "decoding did not die" )
        or diag("Exception: $err"), return;

    return \@decoded_values;
}

