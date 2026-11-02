#!perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use Config;
use Sereal::TestSet qw(Header floating_point_wire_cases);
use Sereal::Decoder;
use Sereal::TestSet::Corpus    qw(read_packet);
use Sereal::Decoder::Constants qw(:all);

# Fixed IEEE payload bytes provide an oracle independent of the Encoder and
# the host byte order. The expected DOUBLE value is rounded through native
# double so an extended-NV Perl compares against the precision on the wire.
my @cases = (
    [ 'FLOAT 0.5',  SRL_HDR_FLOAT,  '0000003f',         0.5,                             'f', 1 ],
    [ 'FLOAT -1.5', SRL_HDR_FLOAT,  '0000c0bf',         -1.5,                            'f', 1 ],
    [ 'DOUBLE 0.1', SRL_HDR_DOUBLE, '9a9999999999b93f', unpack( 'd', pack( 'd', 0.1 ) ), 'd', 1 ],
);
my $quadmath = ( $Config{usequadmath} || '' ) eq 'define';
if ($quadmath) {
    push @cases, [
        'FLOAT_128 precision',              SRL_HDR_FLOAT_128,
        '0010000000000000000000000000ff3f', 1 + 2**-100,
        'F',                                5
        ];
}
my $dec     = Sereal::Decoder->new( { legacy_float_byteorder => 'little' } );
my $default = Sereal::Decoder->new;
eval { Sereal::Decoder->new( { legacy_float_byteorder => 'middle' } ) };
like( $@, qr/Invalid legacy_float_byteorder/, 'invalid legacy byte order is rejected' );
is(
    Sereal::Decoder->new( { legacy_float_byteorder => 'big' } )
        ->decode( Header(7) . chr(SRL_HDR_DOUBLE) . pack( 'H*', '9a9999999999b93f' ) ),
    unpack( 'd<', pack( 'H*', '9a9999999999b93f' ) ),
    'v7 ignores legacy source-order overrides'
);
my $legacy =
    Sereal::Decoder->new( { legacy_float_byteorder => 'native', unknown_option_policy => 'die' } );
for my $case (@cases) {
    my ( $name, $tag, $hex, $expected, $native_format, $min_version ) = @$case;
    for my $version ( $min_version .. 7 ) {
        my $prefix = Header($version) . chr($tag);
        is( $dec->decode( $prefix . pack( 'H*', $hex ) ),
            $expected, "v$version $name decodes little-endian wire bytes" );
        if ( $version < 7 ) {
            is( $legacy->decode( $prefix . pack( $native_format, $expected ) ),
                $expected, "v$version $name explicit native-order decoding" );
            is( $default->decode( $prefix . pack( $native_format, $expected ) ),
                $expected, "v$version $name defaults to legacy native order" );
        }
        else {
            is( $default->decode( $prefix . pack( 'H*', $hex ) ),
                $expected, "v7 $name defaults to little endian" );
        }
        my $bad = $prefix . substr( pack( 'H*', $hex ), 0, -1 );
        eval { $dec->decode($bad) };
        like(
            $@,
            qr/(?:Unexpected end|Premature end|not enough|while reading)/i,
            "v$version $name rejects truncated payload"
        );
    }
}

# Stored inputs exercise both legacy source orders even when no Encoder is
# installed. Generating them with the local Encoder would cover only the
# host's order and could conceal a shared Encoder/Decoder error.
for my $order ( 'little', 'big' ) {
    my $reader = Sereal::Decoder->new( { legacy_float_byteorder => $order } );
    for my $version ( 1 .. ( $order eq 'little' ? 7 : 6 ) ) {
        for my $case ( floating_point_wire_cases() ) {
            my ( $name, undef, undef, $expected, $min_version ) = @$case;
            next if $version < $min_version;
            my $packet =
                read_packet( 't/data/packet_corpus', "float-wire-$order", "v$version/$name" );
            my $value = $reader->decode($packet);
            is( $value, $expected, "stored $order v$version $name" );
            is( unpack( 'H*', pack( 'd<', $value ) ),
                '0000000000000080', 'negative zero sign preserved' )
                if $name eq 'binary128/negative-zero';
        }
    }
}

# Non-finite values and overflow are supported without a quadmath build.
for my $case (
    [ 'positive infinity', '0000000000000000000000000000ff7f', '000000000000f07f' ],
    [ 'negative infinity', '0000000000000000000000000000ffff', '000000000000f0ff' ],
    )
{
    my $value = $default->decode( Header(7) . chr(SRL_HDR_FLOAT_128) . pack( 'H*', $case->[1] ) );
    is( unpack( 'H*', pack( 'd<', $value ) ), $case->[2], "binary128 $case->[0]" );
}
my $nan = $default->decode(
    Header(7) . chr(SRL_HDR_FLOAT_128) . pack( 'H*', '0000000000000000000000000080ff7f' ) );
ok( $nan != $nan, 'binary128 quiet NaN' );

# These legacy packets retain the host's LONG_DOUBLE representation and
# storage size. Selecting little endian changes only their byte order; it
# does not claim that another ABI with a different format can read them.
SKIP: {
    skip 'long-double packing unavailable', 12 unless eval { pack( 'D<', 0.1 ); 1 };
    for my $version ( 1 .. 6 ) {
        my $prefix = Header($version) . chr(SRL_HDR_LONG_DOUBLE);
        is(
            $dec->decode( $prefix . pack( 'D<', 0.1 ) ),
            unpack( 'D<', pack( 'D<', 0.1 ) ),
            "v$version LONG_DOUBLE little-endian payload"
        );
        is(
            $legacy->decode( $prefix . pack( 'D', 0.1 ) ),
            unpack( 'D', pack( 'D', 0.1 ) ),
            "v$version LONG_DOUBLE legacy native-order migration"
        );
    }
}

# Only the Encoder distribution tests its own live Encoder. Decoder coverage
# above always runs, including with no Encoder or a previous stable Encoder.
if ( $Sereal::TestSet::Class eq 'Sereal::Encoder' ) {
    require Sereal::Encoder;
    for my $case (@cases) {
        my ( $name, $tag, $hex, $value, undef, $min_version ) = @$case;
        for my $version ( $min_version .. 7 ) {
            my $encoder = Sereal::Encoder->new( {
                protocol_version    => $version,
                use_standard_double => $tag == SRL_HDR_DOUBLE ? 1 : 0
            } );

            # Force an NV; literal or dual-valued integers can use integer tags.
            my $nv = unpack( $case->[4], pack( $case->[4], $value ) );
            is(
                unpack( 'H*', $encoder->encode($nv) ),
                unpack( 'H*', Header($version) . chr($tag) )
                    . ( $version >= 7 ? $hex : unpack( 'H*', pack( $case->[4], $value ) ) ),
                "v$version $name Encoder emits little-endian bytes"
            );
        }
    }
}
done_testing;
