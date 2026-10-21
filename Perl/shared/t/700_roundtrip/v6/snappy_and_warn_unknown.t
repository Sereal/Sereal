#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;
use Sereal::Encoder::Constants qw(SRL_PROTOCOL_VERSION_MASK);

my $ok = have_encoder_and_decoder();
if ( not $ok ) {
    plan skip_all => 'Did not find right version of encoder';
}
else {
    my ($version) = $0 =~ m![\\/]v(\d+)[\\/]!;
    die "Failed to detect protocol version from test path\n" unless $version;

    my $e = Sereal::Encoder->new( {
        protocol_version  => $version,
        snappy_incr       => 1,
        stringify_unknown => 1,
        warn_unknown      => 1,
    } );
    my $d = Sereal::Decoder->new();

    my $out;
    my $payload   = [ sub { 1 }, 'abcd' x 1024 ];
    my $warning   = '';
    my $encode_ok = eval {
        local $SIG{__WARN__} = sub { $warning .= join '', @_ };
        $out = $e->encode($payload);
        1;
    };
    my $err = $@ || 'Zombie error';
    ok( $encode_ok, 'snappy_incr and warn_unknown allow CODE stringification' )
        or diag($err);
    like( $warning, qr/Found type .*CODE/s, 'warn_unknown warns about the CODE reference' );
    is(
        ord( substr( $out, 4, 1 ) ) & SRL_PROTOCOL_VERSION_MASK,
        $version,
        'encoder emits the protocol version selected by the test path',
    ) if defined $out;

    my $decoded;
    my $decode_ok = eval { $decoded = $d->decode($out); 1 };
    $err = $@ || 'Zombie error';
    ok( $decode_ok, 'snappy_incr output decodes' )
        or do {
        diag($err);
        hobodecode($out) if $ENV{DEBUG_SEREAL};
        };

    is( scalar(@$decoded), 2, 'decoded payload has both elements' ) if $decode_ok;
    like( $decoded->[0], qr/\ACODE\(0x[0-9a-f]+\)\z/i, 'CODE reference was stringified' )
        if $decode_ok;
    is( $decoded->[1], $payload->[1], 'long string survives incremental compression' )
        if $decode_ok;
}

pass();
done_testing();
