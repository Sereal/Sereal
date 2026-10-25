use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet::Corpus qw(read_packet);
use Test::More tests => 2;
use Sereal::Decoder;

# Regression test for RT #93563

# Decoder is (was) not re-entrant.

my $dec;

package Foo;
sub THAW   { bless( { a => $dec->decode( $_[2] ) }, $_[0] ) }

package main;

$dec = Sereal::Decoder->new;
my $expected = [ bless( { a => 42 }, 'Foo' ) ];
push @$expected, $expected;
my $packet = read_packet(
    File::Spec->catdir( 't', 'data', 'packet_corpus' ),
    'decoder-features', 'reentrancy/freeze_thaw'
);
my ( $decoded, $error );
eval { $decoded = $dec->decode($packet); 1 }
    or $error = $@ || 'Zombie error';
ok( !$error, 'Decoding did not barf' ) or diag("Decoding barfed with '$error'");
is_deeply( $decoded, $expected, 'Output from decoding is correct' );
