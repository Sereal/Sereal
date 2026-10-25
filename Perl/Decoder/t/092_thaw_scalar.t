#!/usr/bin/env perl
use strict;
use warnings;

use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Test::More;
use Sereal::TestSet qw(:all);
use Sereal::Decoder qw(decode_sereal);
use Sereal::TestSet::Corpus qw(read_packet);
use Data::Dumper;

{

    package Rot13;

    sub rot13 {
        my ($d) = @_;
        $d =~ tr/a-zA-Z/nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM/;
        return $d;
    }

    sub new {
        my ( $class, $secret ) = @_;
        bless \$secret, $class;
    }

    sub FREEZE {
        my ( $self, $serializer ) = @_;
        return rot13($$self);
    }

    sub THAW {
        my ( $class, $serializer, $data ) = @_;
        return rot13($data);
    }
}

my $corpus = File::Spec->catdir(qw(t data packet_corpus));
my $rot13_packet = read_packet( $corpus, 'decoder-features', 'thaw_scalar/rot13' );
my $return_packet = read_packet( $corpus, 'decoder-features', 'thaw_scalar/returns' );

my $secret = "SECRET";

my $rot13_values = decode_sereal($rot13_packet);
is( $rot13_values->[0], $secret, 'THAW returns the decoded scalar' );

is_deeply(
    $rot13_values->[1], { secret => $secret },
    'THAW scalar return is installed in its container',
);

{

    package ScalarReturn;
    sub FREEZE { return $_[0]->[0] }
    sub THAW   { return $_[2] }
}

my $return_values = decode_sereal($return_packet);
for my $i ( 0 .. $#$return_values ) {
    my $value = ( undef, 0, '', 42, 'text' )[$i];
    is( $return_values->[$i], $value,
        'THAW preserves ' . ( defined($value) ? "scalar '$value'" : 'undef' ) );
}

done_testing;
