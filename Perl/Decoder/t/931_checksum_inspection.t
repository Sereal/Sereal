#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder qw(get_sereal_checksum validate_sereal_checksum);
use lib 't/lib';
use Sereal::TestSet qw(Header varint);
use B::Concise;

local $ENV{SEREAL_CHECKSUM_POLICY} = 'ignore';
local $Sereal::DefaultKey;
my $key       = pack( 'C*', 0 .. 15 );
my $getter    = \&get_sereal_checksum;
my $validator = \&validate_sereal_checksum;
for my $type ( 0, 1 ) {
    for my $suffix ( undef, "\x07" ) {
        for my $payload ( "\x07", "\x3e\0", "\x26" . varint(130) . ( 'x' x 130 ) ) {
            my $header = Header( 6, $suffix );
            substr( $header, 4, 1 ) = chr(0x86);
            my $prefix = $header . chr($type) . varint( $type ? 16 : 8 );
            my $tail   = varint( length($payload) ) . $payload;
            my $tag =
                $type
                ? Sereal::Decoder::siphash36( $prefix . $tail, $key )
                : Sereal::Decoder::siphash24( $prefix . $tail, $key );
            my $packet = $prefix . $tag . $tail;
            my $label =
                "type=$type suffix=" . ( defined($suffix) ? 1 : 0 ) . ' bytes=' . length($payload);
            is( get_sereal_checksum($packet), $tag, "$label direct extraction" );
            is( $getter->($packet),           $tag, "$label XSUB extraction" );
            ok( validate_sereal_checksum( $packet, $key ), "$label direct validation" );
            ok( $validator->( $packet, $key ),             "$label XSUB validation" );
            is(
                get_sereal_checksum( 'prefix' . $packet . 'trailer', 6 ), $tag,
                "$label extraction with offset"
            );
            ok(
                validate_sereal_checksum( 'prefix' . $packet . 'trailer', $key, 6 ),
                "$label validation with offset"
            );
            is( $getter->( 'prefix' . $packet, 6 ), $tag, "$label XSUB extraction with offset" );
            ok( $validator->( 'prefix' . $packet, $key, 6 ), "$label XSUB validation with offset" );
            {
                local $Sereal::DefaultKey = $key;
                ok( validate_sereal_checksum($packet), "$label direct validation with shared key" );
                ok( $validator->($packet),             "$label XSUB validation with shared key" );
            }
            my $bad = $packet;
            substr( $bad, length($prefix), 1 ) = chr( ord( substr( $tag, 0, 1 ) ) ^ 1 );
            ok( !validate_sereal_checksum( $bad, $key ), "$label checksum mismatch returns false" );
            ok( !$validator->( $bad, $key ),             "$label XSUB mismatch returns false" );
            is(
                get_sereal_checksum($bad), substr( $bad, length($prefix), length($tag) ),
                "$label inspection returns the stored tag without authenticating"
            );
            $bad = $packet;
            substr( $bad, -1, 1 ) = chr( ord( substr( $bad, -1 ) ) ^ 1 );
            ok( !validate_sereal_checksum( $bad, $key ), "$label payload tampering returns false" );
            for my $length ( 0 .. length($packet) - 1 ) {
                my $truncated = substr( $packet, 0, $length );
                eval { get_sereal_checksum($truncated); 1 };
                ok( $@ ne '', "$label extraction rejects truncation at $length" );
                eval { $validator->( $truncated, $key ); 1 };
                ok( $@ ne '', "$label validation rejects truncation at $length" );
            }
            eval { validate_sereal_checksum($packet); 1 };
            like( $@, qr/requires a key/, "$label missing default key is rejected" );
            eval { $validator->( $packet, 'short' ); 1 };
            like( $@, qr/exactly 16 bytes/, "$label short validation key is rejected" );
            my $unknown = $packet;
            substr( $unknown, length($header), 1 ) = chr(2);
            is(
                get_sereal_checksum($unknown), $tag,
                "$label unknown checksum type can be inspected"
            );
            eval { validate_sereal_checksum( $unknown, $key ); 1 };
            like(
                $@, qr/Unsupported Sereal checksum type/,
                "$label unknown checksum cannot be validated"
            );
            my @tag = get_sereal_checksum($packet);
            is_deeply( \@tag, [$tag], "$label extraction returns one value in list context" );
            my @valid = validate_sereal_checksum( $packet, $key );
            is_deeply( \@valid, [1], "$label validation returns one value in list context" );
            get_sereal_checksum($packet);
            validate_sereal_checksum( $packet, $key );
        }
    }
}
for my $version ( 1 .. 6 ) {
    my $plain = Header($version) . "\x07";
    is( get_sereal_checksum($plain), undef, "unsigned v$version has no checksum" );
    ok( !validate_sereal_checksum($plain), "unsigned v$version is not authenticated" );
}
for my $input ( [], {}, \7 ) {
    eval { get_sereal_checksum($input); 1 };
    like( $@, qr/reference/, 'reference input is rejected' );
}
for my $offset ( -1, 100 ) {
    eval { get_sereal_checksum( Header(6) . "\x07", $offset ); 1 };
    like( $@, qr/offset is outside/, 'out-of-range offset is rejected' );
}
{
    my $bad = Header(6) . "\x07";
    substr( $bad, 4, 1 ) = chr(0x86);
    substr( $bad, 5 ) = "\x80" x 12;
    eval { get_sereal_checksum($bad); 1 };
    like( $@, qr/varint/, 'oversized header-suffix varint is rejected' );
}

# Assert actual opcode substitution rather than merely calling the same names.
for my $case (
    [ 'get_sereal_checksum',      sub { get_sereal_checksum( $_[0] ) } ],
    [ 'validate_sereal_checksum', sub { validate_sereal_checksum( $_[0], $_[1] ) } ] )
{
    my $tree = '';
    B::Concise::walk_output( \$tree );
    B::Concise::compile( '-basic', $case->[1] )->();
    like( $tree, qr/\b\Q$case->[0]\E\b/, "$case->[0] uses its custom opcode" );
    unlike( $tree, qr/\bentersub\b/, "$case->[0] direct call has no entersub" );
}
B::Concise::walk_output( \*STDOUT );
done_testing;
