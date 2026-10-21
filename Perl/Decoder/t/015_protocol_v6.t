#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Config;
use Scalar::Util               qw(reftype);
use Sereal::TestSet            qw(Header varint);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);

sub packet {
    my ( $version, $payload ) = @_;
    return Header($version) . $payload;
}

sub decode_ok {
    my ( $data, $expected, $name ) = @_;
    my $decoded = eval { decode_sereal($data) };
    ok( !$@, "$name decodes" ) or diag $@;
    is( $decoded, $expected, "$name value matches" );
}

sub decode_fails {
    my ( $data, $error, $name ) = @_;
    my $decoded = eval { decode_sereal($data) };
    ok( $@, "$name is rejected" )
        or diag "unexpected value: " . ( defined($decoded) ? $decoded : '<undef>' );
    like( $@, $error, "$name has a useful error" );
}

decode_ok( packet( 6, chr(SRL_HDR_NEG_VARINT) . varint(17) ), -17, 'v6 NEG_VARINT' );
decode_ok( packet( 6, chr(SRL_HDR_NEG_VARINT) . varint(33) ), -33, 'v6 NEG_VARINT' );
decode_ok( packet( 5, chr(SRL_HDR_ZIGZAG) . varint(33) ),     -17, 'v5 ZIGZAG' );
decode_ok( packet( 6, chr(SRL_HDR_ZIGZAG) . varint(33) ),     -17, 'legacy ZIGZAG in v6' );

decode_fails(
    packet( 5, chr(SRL_HDR_NEG_VARINT) . varint(17) ),
    qr/NEG_VARINT tag requires protocol version 6/,
    'NEG_VARINT in v5',
);
decode_fails(
    packet( 6, chr(SRL_HDR_NEG_VARINT) . varint(0) ),
    qr/Invalid NEG_VARINT magnitude/,
    'zero NEG_VARINT magnitude',
);
my $iv_bits = $Config{ivsize} * 8;
my $iv_min  = $iv_bits == 64 ? ( -9223372036854775807 - 1 ) : ( -2147483647 - 1 );
my $iv_min_magnitude =
    ( chr(0x80) x int( ( $iv_bits - 1 ) / 7 ) ) . chr( 1 << ( ( $iv_bits - 1 ) % 7 ) );
is(
    decode_sereal( packet( 6, chr(SRL_HDR_NEG_VARINT) . $iv_min_magnitude ) ),
    $iv_min,
    'NEG_VARINT decodes the minimum signed integer',
);
decode_fails(
    packet( 6, chr(SRL_HDR_NEG_VARINT) . ( "\xff" x 9 ) . "\x01" ),
    qr/Invalid NEG_VARINT magnitude|too big/i,
    'NEG_VARINT magnitude outside signed integer range',
);

my $binary_spelling = 'v01.002.003';
my $utf8_spelling   = 'v01.002.0300';
my $binary_packet   = packet(
    6,
    chr(SRL_HDR_EXTEND)
        . chr(SRL_EXT_VSTRING_BINARY)
        . varint( length($binary_spelling) )
        . $binary_spelling
);
my $utf8_packet = packet(
    6,
    chr(SRL_HDR_EXTEND)
        . chr(SRL_EXT_VSTRING_UTF8)
        . varint( length($utf8_spelling) )
        . $utf8_spelling
);
for my $case (
    [ binary => $binary_packet, $binary_spelling ],
    [ utf8   => $utf8_packet,   $utf8_spelling ],
    )
{
    my ( $name, $data, $spelling ) = @$case;
    my $decoded = eval { decode_sereal($data) };
    ok( !$@, "$name v-string decodes" ) or diag $@;
    is( reftype( \$decoded ), 'VSTRING', "$name result has v-string magic" );
    my $magic_spelling = '';
    require B;
    for my $magic ( B::svref_2object( \$decoded )->MAGIC ) {
        $magic_spelling = $magic->PTR if $magic->TYPE eq 'V';
    }
    is( $magic_spelling, $spelling, "$name spelling is preserved" );
}

decode_fails(
    packet(
        5,
        chr(SRL_HDR_EXTEND)
            . chr(SRL_EXT_VSTRING_BINARY)
            . varint( length($binary_spelling) )
            . $binary_spelling
    ),
    qr/EXTEND tag requires protocol version 6/,
    'EXTEND in v5',
);
decode_fails(
    packet(
        6, chr(SRL_HDR_EXTEND) . chr(0x25) . varint( length($binary_spelling) ) . $binary_spelling
    ),
    qr/Unknown extended tag ID/,
    'unknown extended tag ID',
);
decode_fails(
    packet(
        6,
        chr(SRL_HDR_EXTEND)
            . chr(SRL_EXT_VSTRING_UTF8)
            . varint( length($binary_spelling) )
            . $binary_spelling
    ),
    qr/VSTRING extension ID does not match/,
    'v-string subtype encoding mismatch',
);

for my $reserved ( SRL_HDR_RESERVED, SRL_HDR_RESERVED_1 ) {
    decode_fails( packet( 6, chr($reserved) ), qr/Unexpected/, "reserved tag $reserved" );
}

for my $doc_type ( 5, 6 ) {
    my $data = packet( 6, chr(0) );
    substr( $data, 4, 1, chr( ( $doc_type << SRL_PROTOCOL_VERSION_BITS ) | 6 ) );
    decode_fails( $data, qr/unknown format/, "reserved document type $doc_type" );
}

my $no = eval { decode_sereal( packet( 5, chr(SRL_HDR_NO) ) ) };
ok( !$@ && defined($no) && !$no, 'v5 NO boolean tag decodes false' );
my $yes = eval { decode_sereal( packet( 5, chr(SRL_HDR_YES) ) ) };
ok( !$@ && defined($yes) && $yes, 'v5 YES boolean tag decodes true' );
decode_fails( packet( 4, chr(SRL_HDR_NO) ),  qr/NO tag requires protocol version 5/,  'NO in v4' );
decode_fails( packet( 4, chr(SRL_HDR_YES) ), qr/YES tag requires protocol version 5/, 'YES in v4' );
decode_fails(
    packet( 4, chr(SRL_HDR_FLOAT_128) ),
    qr/FLOAT_128 tag requires protocol version 5/,
    'FLOAT_128 in v4',
);
SKIP: {
    skip 'this perl was not built with quadmath', 1
        unless ( $Config{usequadmath} || '' ) eq 'define';
    is(
        decode_sereal( packet( 5, chr(SRL_HDR_FLOAT_128) . ( "\0" x 16 ) ) ),
        0,
        'v5 FLOAT_128 decodes a quadmath zero',
    );
}

done_testing();
