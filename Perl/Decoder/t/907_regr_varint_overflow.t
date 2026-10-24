#!perl
use strict;
use warnings;
use Config;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder qw(decode_sereal);

my $header   = "=\xF3rl" . chr(4) . chr(0);
my $overflow = $Config{uvsize} == 8
    ? ( chr(0x80) x 9 ) . chr(0x02)     # 2^64
    : ( chr(0x80) x 4 ) . chr(0x10);    # 2^32

my $uv_max =
    $Config{uvsize} == 8
    ? ( chr(0xFF) x 9 ) . chr(0x01)
    : ( chr(0xFF) x 4 ) . chr(0x0F);
my $uv_max_as_string =
    $Config{uvsize} == 8
    ? '18446744073709551615'
    : '4294967295';

my @cases = (
    [ scalar_safe_reader => $header . chr(0x20) . $overflow ],
    [ scalar_fast_reader => $header . chr(0x20) . $overflow . ( chr(0) x 16 ) ],
    [ array_count        => $header . chr(0x28) . chr(0x2B) . $overflow . chr(1) ],
    [ hash_count         => $header . chr(0x28) . chr(0x2A) . $overflow . chr(0x61) . 'k' . chr(1) ],
    [ binary_length      => $header . chr(0x26) . $overflow . 'SECRET' ],
);

for my $case (@cases) {
    my ( $name, $packet ) = @$case;
    my $ok  = eval { decode_sereal($packet); 1 };
    my $err = $@;
    ok( !$ok, "$name rejects a varint outside UV range" );
    like( $err, qr/varint (?:too big|overflows U32)/i, "$name reports varint overflow" );
}

for my $tail ( '', chr(0) x 16 ) {
    my $value = eval { decode_sereal( $header . chr(0x20) . $uv_max . $tail ) };
    ok( !$@, 'maximum UV varint is accepted' );
    is( "$value", $uv_max_as_string, 'maximum UV varint retains its value' );
}

# Exercise every overflowing terminal byte, not only the first value outside
# UV. Cover both reader paths and each kind of length/offset consumer.
my $continuations = $Config{uvsize} == 8 ? 9 : 4;
my $terminal_max  = $Config{uvsize} == 8 ? 1 : 15;
my @prefixes      = (
    [ 'VARINT',               $header . "\x20" ],
    [ 'ZIGZAG',               $header . "\x21" ],
    [ 'BINARY length',        $header . "\x26" ],
    [ 'UTF8 length',          $header . "\x27" ],
    [ 'ARRAY count',          $header . "\x28\x2b" ],
    [ 'HASH count',           $header . "\x28\x2a" ],
    [ 'COPY offset',          $header . "\x2f" ],
    [ 'REFP offset',          $header . "\x29" ],
    [ 'header suffix length', substr( $header, 0, 5 ) ],
);
for my $terminal ( $terminal_max + 1 .. 127 ) {
    my $encoded = ( "\x80" x $continuations ) . chr($terminal);
    for my $prefix (@prefixes) {
        for my $padding ( '', "\0" x 16 ) {
            eval { decode_sereal( $prefix->[1] . $encoded . $padding ) };
            like(
                $@, qr/varint (?:too big|overflows U32)/i,
                "$prefix->[0] rejects terminal $terminal (padding " . length($padding) . ')'
            );
        }
    }
}
done_testing;
