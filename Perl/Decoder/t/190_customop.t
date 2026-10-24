#!perl

use strict;
use warnings;

use File::Spec;
use lib File::Spec->catdir(qw(t lib));
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Decoder qw(sereal_decode_with_object scalar_looks_like_sereal);
use Test::More;
use Data::Dumper;
use B ();
use Config;

my $srl_decoder           = Sereal::Decoder->new();
my $empty_array_as_sereal = "=\363rl\4\0\@";

my $dec = sereal_decode_with_object( $srl_decoder, $empty_array_as_sereal );
is( Dumper($dec), Dumper( [] ), "check that sereal_decode_with_object works as expected" );
is(
    scalar_looks_like_sereal($empty_array_as_sereal), 4,    #expect version 4
    "check that scalar_looks_like_sereal works as expected"
);
pass("did not segfault!");

sub custom_op_metadata {
    my ($code) = @_;
    my @pending = ( B::svref_2object($code)->ROOT );
    my %metadata;
    while ( my $op = pop @pending ) {
        next unless $$op;
        my $name = $op->name;
        $metadata{$name} = $op->desc
            if $name =~ /\A(?:sereal_|scalar_looks_like_sereal|get_sereal_checksum|validate_sereal_checksum)/;
        push @pending, $op->sibling;
        push @pending, $op->first if $op->can('first');
    }
    return \%metadata;
}

my $decoder_probe = eval q{sub {
    Sereal::Decoder::sereal_decode_with_object($_[0], $_[1]);
    Sereal::Decoder::scalar_looks_like_sereal($_[1]);
    Sereal::Decoder::get_sereal_checksum($_[1]);
    Sereal::Decoder::validate_sereal_checksum($_[1]);
}};
die $@ unless $decoder_probe;
my %expected_decoder = map { $_ => $_ } qw(
    sereal_decode_with_object scalar_looks_like_sereal
    get_sereal_checksum validate_sereal_checksum
);
my $have_custom_ops = $] >= 5.014;

SKIP: {
    skip 'custom ops are unavailable on this Perl', 1 unless $have_custom_ops;
    is_deeply( custom_op_metadata($decoder_probe), \%expected_decoder,
        'all four decoder custom ops keep distinct names and descriptions' );
}

my $encoder_probe;
if ( have_encoder_and_decoder() ) {
    $encoder_probe = eval q{sub { Sereal::Encoder::sereal_encode_with_object($_[0], $_[1]) }};
    die $@ unless $encoder_probe;
}
my $expected_encoder = { sereal_encode_with_object => 'sereal_encode_with_object' };
SKIP: {
    skip 'encoder custom op is unavailable', 1 unless $encoder_probe && $have_custom_ops;
    is_deeply( custom_op_metadata($encoder_probe), $expected_encoder,
        'encoder custom op keeps its name and description' );
}

SKIP: {
    skip 'threaded custom ops are unavailable', 5 unless $Config{useithreads} && $have_custom_ops;
    require threads;
    my $result = threads->create(sub {
        my $decoder = Sereal::Decoder->new;
        return [
            custom_op_metadata($decoder_probe),
            $encoder_probe ? custom_op_metadata($encoder_probe) : undef,
            sereal_decode_with_object($decoder, $empty_array_as_sereal),
            scalar_looks_like_sereal($empty_array_as_sereal),
        ];
    })->join;
    is_deeply( $result->[0], \%expected_decoder, 'cloned decoder custom-op metadata works' );
    SKIP: {
        skip 'encoder is unavailable', 1 unless $encoder_probe;
        is_deeply( $result->[1], $expected_encoder, 'cloned encoder custom-op metadata works' );
    }
    is_deeply( $result->[2], [], 'custom-op decoding works in the child interpreter' );
    is( $result->[3], 4, 'custom-op inspection works in the child interpreter' );
    is_deeply( custom_op_metadata($decoder_probe), \%expected_decoder,
        'parent metadata survives child interpreter destruction' );
}

done_testing();
