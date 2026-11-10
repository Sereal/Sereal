#!perl
use strict;
use warnings;
use lib 't/lib';
use Test::More;
use Config;
use Sereal::TestSet            qw(Header varint read_varint_at native_long_double_wire_cases);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::TestSet::Corpus    qw(read_packet);
use Sereal::Decoder::Constants qw(:all);

# Exact -1/8 avoids NV rounding differences when testing native layouts.
# Perl's layout identifiers and pack are independent of the C detector.
my $native = eval { pack( 'D', -0.125 ) };
my $format;
my $perl_kind = defined( $Config{longdblkind} ) ? $Config{longdblkind} : 'UNKNOWN';
# On Perls whose NV is double, pack D can return eight bytes even when
# C long double (used by the decoder) occupies 12 or 16 bytes.
$native = undef if defined($native) && length($native) != $Config{longdblsize};
# Older Perls omit pack D and longdblkind. When both Perl's NV and C long
# double are eight-byte doubles, pack d supplies the native bytes instead.
$native = pack( 'd', -0.125 )
    if !defined($native) && $Config{longdblsize} == 8 && $Config{doublesize} == 8
    && ( $perl_kind eq '0' || ( $Config{nvtype} || '' ) eq 'double' );
if ( defined($native) ) {
    for my $case ( native_long_double_wire_cases() ) {
        my $bytes = $native;
        substr( $bytes, 10 ) = "\0" x ( length($bytes) - 10 )
            if $case->[1] == 2 && length($bytes) >= 10;
        # An exact double-double value may have either sign of zero in
        # its low component. That sign does not change the storage layout.
        if ( length($bytes) == 16 && ( $case->[1] & 127 ) == 4 ) {
            my $low_offset = $case->[1] & 128 ? 8 : 0;
            my $sign_offset = $case->[1] & 128 ? 0 : 7;
            my $low = substr( $bytes, $low_offset, 8 );
            substr( $low, $sign_offset, 1 ) = chr( ord( substr( $low, $sign_offset, 1 ) ) & 127 );
            substr( $bytes, $low_offset, 8 ) = $low if $low eq "\0" x 8;
        }
        $format = $case->[1] if $bytes eq $case->[2];
    }
}
# With no pack D, use Perl's declared IEEE/x87 layout. Double-double
# requires inspecting the component order, not just its Perl kind number.
if ( !defined($native) ) {
    my %perl_kind_to_format = ( 1 => 3, 2 => 0x83, 3 => 2, 4 => 0x82 );
    $format = $perl_kind_to_format{$perl_kind};
    for my $case ( native_long_double_wire_cases() ) {
        $native = $case->[2]
            if defined($format) && $format == $case->[1]
            && $Config{longdblsize} == length( $case->[2] );
    }
}
# Older Perls may lack pack D and longdblkind on a known x86 ABI.
if ( !defined($native) && $Config{archname} =~ /^(?:amd64|x86_64|i[3-6]86)-/
    && ( $Config{longdblsize} == 12 || $Config{longdblsize} == 16 ) )
{
    $format = 2;
    $native = pack( 'H*', '0000000000000080fcbf' ) . ( "\0" x ( $Config{longdblsize} - 10 ) );
}

sub declaration {
    my ( $id, $size ) = @_;
    return
          chr(SRL_HDR_EXTEND)
        . chr(SRL_EXT_DECLARE)
        . varint(3)
        . varint(SRL_DECL_LONG_DOUBLE)
        . chr($id)
        . chr($size);
}
my $header = Header(7);
my $dec    = Sereal::Decoder->new;
for my $case (
    [ chr(SRL_HDR_LONG_DOUBLE) . ( 'x' x 32 ), qr/requires a declaration/, 'missing declaration' ],
    [
        chr(SRL_HDR_EXTEND) . chr(SRL_EXT_DECLARE) . varint(1) . varint(99) . "\0",
        qr/Unknown declaration kind/,
        'unknown kind'
    ], [
        chr(SRL_HDR_EXTEND) . chr(SRL_EXT_DECLARE) . varint(2) . varint(1) . "\0\0",
        qr/declaration length/,
        'wrong descriptor length'
    ],
    [ declaration( 0, 16 ) . "\0", qr/format mismatch/, 'unknown native format' ],
    [
        chr( SRL_HDR_EXTEND | SRL_HDR_TRACK_FLAG )
            . chr(SRL_EXT_DECLARE)
            . varint(3)
            . varint(1)
            . chr(0)
            . chr(16) . "\0",
        qr/outside document prefix/,
        'tracked declaration'
    ], [
        chr( SRL_HDR_ARRAYREF_LOW + 1 ) . declaration( 0, 16 ) . "\0",
        qr/outside document prefix/,
        'nested declaration'
    ], [
        chr(SRL_HDR_PAD) . declaration( 0, 16 ) . "\0",
        qr/outside document prefix/,
        'declaration after PAD'
    ],
    [ chr(7) . declaration( 0, 16 ) . "\0", qr/outside document prefix/, 'declaration after root' ],
    [
        chr(SRL_HDR_EXTEND) . chr(SRL_EXT_DECLARE) . varint(3) . varint(1) . "\0",
        qr/(?:end|space|reading declaration)/i,
        'truncated declaration'
    ],
    )
{
    eval { $dec->decode( $header . $case->[0] ) };
    like( $@, $case->[1], $case->[2] );
    is( $dec->decode( $header . "\x07" ), 7, 'decoder recovers after declaration error' );
}
SKIP: {
    skip 'native long-double layout unavailable', 12 unless defined($format) && defined($native);
    my $prefix = declaration( $format, length($native) );
    my $value  = chr(SRL_HDR_LONG_DOUBLE) . $native;
    is( $dec->decode( $header . $prefix . $value ), -0.125, 'matching native descriptor decodes' );
    is( $dec->decode( $header . $prefix . "\x07" ), 7,    'declaration does not produce a value' );
    for my $case (
        [ $prefix . $prefix . "\0", qr/Duplicate/, 'duplicate kind' ],
        [
            declaration( $format ^ 0x80, length($native) ) . "\0",
            qr/format mismatch/,
            'wrong endianness'
        ], [
            declaration( $format, length($native) + 1 ) . "\0",
            qr/format mismatch/,
            'wrong native size'
        ], [
            $prefix . chr(SRL_HDR_COPY) . varint(1),
            qr/offset|backref|COPY/i,
            'COPY into declaration'
        ],
        )
    {
        eval { $dec->decode( $header . $case->[0] ) };
        like( $@, $case->[1], $case->[2] );
        is( $dec->decode( $header . "\x07" ), 7, 'decoder state does not leak' );
    }
    my $trusted = Sereal::Decoder->new( { allow_unknown_long_double => 1 } );
    eval { $trusted->decode( $header . declaration($format ^ 0x80, length($native)) . $value ) };
    like( $@, qr/format mismatch/, 'trust does not override a known byte-order mismatch' );
    eval { $dec->decode( $header . $value ) };
    like( $@, qr/requires a declaration/, 'a declaration does not survive object reuse' );
}
# An opaque payload is a valid local C long-double object, but its format
# declaration intentionally does not identify that layout. Default readers
# reject it; explicitly trusted readers can recover the value on this ABI.
SKIP: {
    skip 'native long-double bytes unavailable', 6 unless defined($native);
    my $opaque = declaration( SRL_LD_OPAQUE, length($native) );
    my $body = chr(SRL_HDR_LONG_DOUBLE) . $native;
    my $trusted = Sereal::Decoder->new( { allow_unknown_long_double => 1 } );
    eval { $dec->decode( $header . $opaque . $body ) };
    like( $@, qr/format mismatch/, 'opaque storage requires explicit reader trust' );
    is( $trusted->decode( $header . $opaque . $body ), -0.125,
        'explicit trust reads opaque storage on the local ABI' );
    is( $trusted->decode( $header . declaration(5, length($native)) . $body ), -0.125,
        'explicit trust also permits an unrecognized future representation ID' );
    eval { $trusted->decode( $header . declaration(SRL_LD_OPAQUE, length($native)+1) . $body ) };
    like( $@, qr/format mismatch/, 'trust does not override a native storage-size mismatch' );
    eval { $trusted->decode( $header . $opaque . $opaque . $body ) };
    like( $@, qr/Duplicate/, 'trust does not permit duplicate declarations' );
    eval { $trusted->decode( $header . $body ) };
    like( $@, qr/requires a declaration/, 'trust does not permit undeclared native storage' );
}
for my $version ( 1 .. 6 ) {
    eval { $dec->decode( Header($version) . declaration( 0, 0 ) . "\0" ) };
    like(
        $@,
        qr/outside document prefix|Unexpected|requires protocol/,
        "v$version does not accept declarations"
    );
}
for my $case ( native_long_double_wire_cases() ) {
    my ( $name, $source_format, $payload ) = @$case;
    my $packet = read_packet( 't/data/packet_corpus', 'native-long-double-v7', $name );
    if ( defined($format) && $format == $source_format && length($native) == length($payload) ) {
        is( $dec->decode($packet), -0.125, "stored native $name matches this reader" );
    }
    else {
        eval { $dec->decode($packet) };
        like( $@, qr/format mismatch/, "stored native $name is rejected safely" );
    }
}

if ( $Sereal::TestSet::Class eq 'Sereal::Encoder' ) {
    require Sereal::Encoder;
    for my $type ( 'ieee', 'unknown', '' ) {
        eval { Sereal::Encoder->new( { float_type => $type, unknown_option_policy => 'die' } ) };
        like( $@, qr/Invalid float_type/, 'invalid float_type is rejected' );
    }
    my $strict_native = eval { Sereal::Decoder->new( { allow_unknown_long_double => 1,
        unknown_option_policy => 'die' } ) };
    ok( $strict_native, 'native-reader trust is a recognized constructor option' );
    my $probe = 1 + 2**-60;
    is(
        Sereal::Encoder->new( { float_type => 'std' } )->encode($probe),
        Sereal::Encoder->new->encode($probe),
        'explicit standard representation matches the default'
    );

    if (   ( $Config{nvtype} || '' ) eq 'long double'
        && ( $Config{nvsize} || 8 ) > 8
        && defined($format) )
    {
        eval { Sereal::Encoder->new( { float_type => 'native', use_standard_double => 1 } ) };
        like( $@, qr/mutually exclusive/, 'native output conflicts with forced DOUBLE output' );
        my $value = 1 + 2**-60;
        for my $compression ( 0, 1, 2, 3 ) {
            for my $protected ( 0, 1 ) {
                my $key = '0123456789abcdef';
                my %opt = (
                    protocol_version       => 7,
                    float_type => 'native',
                    compress               => $compression,
                    compress_threshold     => 0
                );
                $opt{checksum_key} = $key if $protected;
                my $encoder = Sereal::Encoder->new( \%opt );
                my $reader  = Sereal::Decoder->new( $protected ? { checksum_key => $key } : {} );
                my $packet  = $encoder->encode( [ $value, $value ], { metadata => $value } );
                is_deeply(
                    $reader->decode($packet),
                    [ $value, $value ],
                    "native round trip compression=$compression checksum=$protected"
                );
                is_deeply(
                    $reader->decode_only_header($packet),
                    { metadata => $value },
                    'header metadata stays independently readable'
                );
                my $shared = [$value];
                my $graph  = $reader->decode( $encoder->encode( [ $shared, $shared ] ) );
                is( $graph->[0],    $graph->[1], 'declaration prefix preserves reference offsets' );
                is( $graph->[0][0], $value,      'shared native float retains precision' );
                is_deeply(
                    $reader->decode( $encoder->encode( [ $value, $value ] ) ),
                    [ $value, $value ],
                    'native declaration survives encoder/decoder reuse'
                );
            }
        }
        my $packet = Sereal::Encoder->new( { protocol_version => 7 } )->encode($value);
        is( substr( $packet, 6, 1 ), chr(SRL_HDR_FLOAT_128), 'extended NV defaults to FLOAT_128' );
        is( $dec->decode($packet),   $value, 'portable binary128 retains x87 precision' );
        if ( ( $format & 127 ) == 2 ) {
            my $raw = Sereal::Encoder->new( { float_type => 'native' } )->encode($value);
            my ( $suffix_len, $body ) = read_varint_at( $raw, 5 );
            $body += $suffix_len;
            my ( $decl_len, $descriptor ) = read_varint_at( $raw, $body + 2 );
            my $value_offset = $descriptor + $decl_len + 1;
            is(
                substr( $raw, $value_offset + 10 ),
                "\0" x ( length($native) - 10 ),
                'unused native x87 storage is zeroed'
            );
        }
        for my $version ( 4 .. 6 ) {
            my $old = Sereal::Encoder->new( { protocol_version => $version } )->encode($value);
            is(
                substr( $old, 6, 1 ),
                chr(SRL_HDR_LONG_DOUBLE),
                "v$version retains native LONG_DOUBLE tag"
            );
            my $expected_bytes = pack( 'D', $value );
            substr( $expected_bytes, 10 ) = "\0" x ( length($expected_bytes) - 10 )
                if defined($format) && ( $format & 127 ) == 2;
            is( substr( $old, 7 ),
                $expected_bytes, "v$version retains native value bytes with zero padding" );
            is( $dec->decode($old), $value, "v$version native round trip" );
        }
    }
    elsif (($Config{nvtype} || '') eq 'long double' && ($Config{nvsize} || 8) == 8 && defined($format)) {
        ok(Sereal::Encoder->new({float_type => 'native'}), 'native alias of binary64 is supported');
    }
    elsif ( ( $Config{nvtype} || '' ) eq 'long double' ) {
        my $encoder = Sereal::Encoder->new( { float_type => 'native' } );
        my $reader = Sereal::Decoder->new( { allow_unknown_long_double => 1 } );
        is( $reader->decode($encoder->encode($probe)), $probe,
            'explicit native selection supports an unidentified local layout' );
    }
    else {
        is(
            Sereal::Encoder->new( { float_type => 'native' } )->encode($probe),
            Sereal::Encoder->new( { float_type => 'std' } )->encode($probe),
            'native selection has no effect on a double or quadmath NV'
        );
    }
}
done_testing;
