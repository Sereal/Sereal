#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More tests => 4;
use Sereal::TestSet qw(Header);
use Sereal::Decoder;

my $decoder = Sereal::Decoder->new;

# The suffix contains only its bitfield, but claims to contain user data.
# The following bytes belong to the main body and must not be consumed as
# metadata when decode_with_header() is used.
my $truncated_header = Header( 6, '' ) . chr(1) . chr(2);
my $ok               = eval { $decoder->decode_with_header($truncated_header); 1 };
ok( !$ok, 'header metadata cannot read into the main body' );
like( $@, qr/unexpected end of input stream/i, 'truncated metadata fails at the header boundary' );

my $valid_packet = Header( 6, chr(1) ) . chr(2);
my $decoded      = $decoder->decode_with_header($valid_packet);
is( $decoded->[0], 1, 'valid header metadata is decoded' );
is( $decoded->[1], 2, 'main body begins after the declared header suffix' );
