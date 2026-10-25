#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder;
use Sereal::Decoder::Constants qw(SRL_PROTOCOL_VERSION_MASK);
use Sereal::TestSet::Corpus qw(read_packet);

my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
my $stored = read_packet( $corpus, 'decoder-features', 'snappy_and_warn_unknown/v6' );
my $decoder = Sereal::Decoder->new;
my $decoded = $decoder->decode($stored);
is( scalar(@$decoded), 2, 'stored v6 Snappy packet has both values' );
like( $decoded->[0], qr/\ACODE\(0x[0-9a-f]+\)\z/i, 'stored CODE value is stringified' );
is( $decoded->[1], 'abcd' x 1024, 'stored Snappy payload decodes' );

SKIP: {
    skip 'Sereal::Encoder is not installed', 7
        unless eval { require Sereal::Encoder; require Sereal::Encoder::Constants; 1 };

    my $version = Sereal::Encoder::Constants::SRL_PROTOCOL_VERSION();
    my $encoder = Sereal::Encoder->new( {
        protocol_version  => $version,
        snappy_incr       => 1,
        stringify_unknown => 1,
        warn_unknown      => 1,
    } );
    my $payload = [ sub { 1 }, 'abcd' x 1024 ];
    my ( $packet, $warning );
    my $ok = eval {
        local $SIG{__WARN__} = sub { $warning .= join '', @_ };
        $packet = $encoder->encode($payload);
        1;
    };
    ok( $ok, 'snappy_incr and warn_unknown allow CODE stringification' ) or diag $@;
    like( $warning, qr/Found type .*CODE/s, 'warn_unknown reports the CODE reference' );
    is(
        ord( substr( $packet // '', 4, 1 ) ) & SRL_PROTOCOL_VERSION_MASK,
        $version,
        'encoder emits its selected protocol version',
    );

    my $live = eval { $decoder->decode($packet) };
    ok( defined $live, 'Encoder output decodes' ) or diag $@;
    is( scalar( @{ $live || [] } ), 2, 'decoded live payload has both elements' );
    like(
        $live ? $live->[0] : '', qr/\ACODE\(0x[0-9a-f]+\)\z/i,
        'live CODE reference is stringified'
    );
    is( $live && $live->[1], $payload->[1], 'live long string survives Snappy compression' );
}

done_testing();
