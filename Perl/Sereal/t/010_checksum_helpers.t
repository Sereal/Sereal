#!perl
use strict;
use warnings;
use Test::More;
use lib 't/lib';
use Sereal::TestSet ();
use Sereal          qw(encode_sereal decode_sereal get_sereal_checksum validate_sereal_checksum);

local $Sereal::DefaultKey = pack( 'C*', 0 .. 15 );
local $ENV{SEREAL_CHECKSUM_POLICY} = 'require';
my $packet = encode_sereal( [ 7, 'wrapper' ] );
is( length( get_sereal_checksum($packet) ), 16, 'wrapper exports checksum inspection' );
ok( validate_sereal_checksum($packet), 'wrapper exports checksum validation with shared default' );
is_deeply(
    decode_sereal($packet), [ 7, 'wrapper' ],
    'wrapper encoding and decoding share the key'
);
my $get      = \&get_sereal_checksum;
my $validate = \&validate_sereal_checksum;
is( $get->($packet), get_sereal_checksum($packet), 'wrapper XSUB and opcode extraction agree' );
ok( $validate->($packet), 'wrapper XSUB validation succeeds' );
substr( $packet, -1, 1 ) = chr( ord( substr( $packet, -1 ) ) ^ 1 );
ok( !validate_sereal_checksum($packet), 'wrapper validator rejects tampering' );
done_testing;
