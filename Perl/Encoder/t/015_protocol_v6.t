#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Scalar::Util ();
use Config;
use Sereal::TestSet qw(Header varint);
use Sereal::Encoder;
use Sereal::Encoder::Constants qw(:all);

sub packet {
    my ( $version, $payload ) = @_;
    return Header($version) . $payload;
}

my $enc6 = Sereal::Encoder->new( { protocol_version => 6 } );
my $enc5 = Sereal::Encoder->new( { protocol_version => 5 } );

for my $n ( -16, -17, -33 ) {
    my $expected6 =
        $n >= -16
        ? chr( 32 + $n )
        : chr(SRL_HDR_NEG_VARINT) . varint( -$n );
    is( $enc6->encode($n), packet( 6, $expected6 ), "v6 encoding of $n" );

    my $expected5 =
        $n >= -16
        ? chr( 32 + $n )
        : chr(SRL_HDR_ZIGZAG) . varint( ( -$n * 2 ) - 1 );
    is( $enc5->encode($n), packet( 5, $expected5 ), "v5 compatible encoding of $n" );
}

my $iv_bits = $Config{ivsize} * 8;
my $iv_min  = $iv_bits == 64 ? ( -9223372036854775807 - 1 ) : ( -2147483647 - 1 );
my $min_magnitude_varint =
    ( chr(0x80) x int( ( $iv_bits - 1 ) / 7 ) ) . chr( 1 << ( ( $iv_bits - 1 ) % 7 ) );
is(
    $enc6->encode($iv_min),
    packet( 6, chr(SRL_HDR_NEG_VARINT) . $min_magnitude_varint ),
    'v6 encoding of the minimum signed integer',
);

SKIP: {
    my $binary_vstring = v01.002.003;
    my $utf8_vstring   = v01.002.0300;
    skip 'this perl does not retain v-string magic', 4
        unless ( ( Scalar::Util::reftype( \$binary_vstring ) || '' ) eq 'VSTRING'
        && ( Scalar::Util::reftype( \$utf8_vstring ) || '' ) eq 'VSTRING' );

    my $binary_spelling = 'v01.002.003';
    my $utf8_spelling   = 'v01.002.0300';
    my $utf8_pv         = "$utf8_vstring";
    utf8::encode($utf8_pv);
    is(
        $enc6->encode($binary_vstring),
        packet(
            6,
            chr(SRL_HDR_EXTEND)
                . chr(SRL_EXT_VSTRING_BINARY)
                . varint( length($binary_spelling) )
                . $binary_spelling
        ),
        'binary v-string uses EXTEND and the BINARY subtype',
    );
    is(
        $enc6->encode($utf8_vstring),
        packet(
            6,
            chr(SRL_HDR_EXTEND)
                . chr(SRL_EXT_VSTRING_UTF8)
                . varint( length($utf8_spelling) )
                . $utf8_spelling
        ),
        'UTF-8 v-string uses EXTEND and the UTF8 subtype',
    );
    is(
        $enc5->encode($binary_vstring),
        packet(
            5, chr( SRL_HDR_SHORT_BINARY_LOW | length("$binary_vstring") ) . "$binary_vstring"
        ),
        'v5 emits a regular binary string for a v-string',
    );
    is(
        $enc5->encode($utf8_vstring),
        packet( 5, chr(SRL_HDR_STR_UTF8) . varint( length($utf8_pv) ) . $utf8_pv ),
        'v5 emits a regular UTF-8 string for a v-string',
    );
}

SKIP: {
    my ( $false, $true );
    my $has_core_boolean = eval q{
        no warnings 'experimental::builtin';
        require builtin;
        $false = builtin::false();
        $true  = builtin::true();
        1;
    };
    skip 'this perl does not provide core boolean values', 2
        unless $has_core_boolean;

    is( $enc5->encode($false), packet( 5, chr(SRL_HDR_NO) ),  'v5 emits NO for core false' );
    is( $enc5->encode($true),  packet( 5, chr(SRL_HDR_YES) ), 'v5 emits YES for core true' );
}

done_testing();
