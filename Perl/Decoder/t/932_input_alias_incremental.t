#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;
use lib 't/lib';
use Sereal::TestSet qw(Header varint);

local $Sereal::DefaultKey;
local $ENV{SEREAL_CHECKSUM_POLICY} = 'verify-if-available';
for my $version ( 1 .. 6 ) {
    for my $utf8 ( 0, 1 ) {
        my $dec    = Sereal::Decoder->new;
        my $packet = Header($version) . "\x42\x07\x08";
        utf8::upgrade($packet) if $utf8;
        $dec->decode( $packet, $packet );
        is_deeply( $packet, [ 7, 8 ], "v$version utf8=$utf8 output may alias input" );
        if ( $version >= 2 ) {
            $packet = Header( $version, "\x07" ) . "\x42\x07\x08";
            utf8::upgrade($packet) if $utf8;
            my $body;
            $dec->decode_with_header( $packet, $body, $packet );
            is( $packet, 7, "v$version utf8=$utf8 header output may alias input" );
            is_deeply(
                $body, [ 7, 8 ],
                "v$version utf8=$utf8 body survives aliased header output"
            );
        }
        my $first  = Header($version) . "\x07";
        my $second = Header($version) . "\x08";
        for my $offset ( 0, 6 ) {
            $packet = ( 'prefix' x ( $offset ? 1 : 0 ) ) . $first . $second;
            utf8::upgrade($packet) if $utf8;
            my $incremental = Sereal::Decoder->new( { incremental => 1 } );
            is(
                $incremental->decode_with_offset( $packet, $offset ), 7,
                "v$version utf8=$utf8 offset=$offset incremental first"
            );
            is(
                $incremental->bytes_consumed, length($first),
                "v$version offset=$offset bytes_consumed excludes prefix"
            );
            is( $packet, $second, "v$version offset=$offset retains second packet" );
            is( $incremental->decode($packet), 8,  "v$version incremental second" );
            is( $packet,                       '', "v$version consumes input" );
        }
        $packet = $first;
        my $inc = Sereal::Decoder->new( { incremental => 1 } );
        eval { $inc->decode( $packet, $packet ) };
        like(
            $@, qr/Incremental decode input and output must be different scalars/,
            "v$version incremental output alias rejected"
        );
        is( $packet,               $first, "v$version rejected alias leaves input intact" );
        is( $inc->decode($packet), 7,      "v$version decoder reusable after alias rejection" );
        is( $packet,               '',     "v$version reuse consumes packet" );
        is(
            Sereal::Decoder->new( { incremental => 1 } )->decode( Header($version) . "\x07" ), 7,
            "v$version temporary incremental input is not stolen"
        );
    }
}

# Copying an aliased input must preserve authentication and start offsets too.
my $key    = 'k' x 16;
my $prefix = Header(6);
substr( $prefix, 4, 1 ) = chr(0x86);
$prefix .= "\x01" . varint(16);
my $tail   = varint(2) . "\x41\x07";
my $signed = $prefix . Sereal::Decoder::siphash36( $prefix . $tail, $key ) . $tail;
my $packet = 'prefix' . $signed;
my $dec    = Sereal::Decoder->new( { checksum_v1_key => $key, checksum_policy => 'die' } );
$dec->decode_with_offset( $packet, 6, $packet );
is_deeply( $packet, [7], 'authenticated offset input may alias output' );
my $inc = Sereal::Decoder->new( { incremental => 1, checksum_v1_key => $key } );
$packet = 'prefix' . $signed . Header(6) . "\x08";
is_deeply( $inc->decode_with_offset( $packet, 6 ), [7], 'authenticated incremental offset decode' );
is( $packet, Header(6) . "\x08", 'authenticated incremental removal includes prefix' );
done_testing;
