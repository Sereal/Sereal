#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::Decoder;

# Header user data is decoded and finalized (THAW methods, thaw_allow_classes
# callbacks, DESTROY) before the main body is parsed, and the body is then
# read from the same input buffer. A callback that modifies the input scalar
# used to leave the decoder reading freed memory; it must die instead.

BEGIN {
    if ( !eval { require Sereal::Encoder; Sereal::Encoder->VERSION(3.002); 1 } ) {
        plan skip_all => 'Sereal::Encoder not available';
    }
}

our $PKT;
our $UNRELATED;
our $MODE = '';

package Mutator;
sub FREEZE { "f" }

sub THAW {
    my $class = shift;
    if    ( $main::MODE eq 'assign' )    { $main::PKT = "x" x 4096 }
    elsif ( $main::MODE eq 'append' )    { $main::PKT .= "x" x 100 }
    elsif ( $main::MODE eq 'read' )      { my $l = length $main::PKT }
    elsif ( $main::MODE eq 'unrelated' ) { $main::UNRELATED = "x" x 4096 }
    return bless {}, $class;
}

package main;

my $err_re = qr/input scalar was modified by a callback during decoding/;

my $enc    = Sereal::Encoder->new( { freeze_callbacks => 1 } );
my $body   = { map { ( "key$_" => "value$_" x 20 ) } 1 .. 200 };
my $packet = $enc->encode( $body, [ bless( {}, 'Mutator' ) ] );

sub check_ok {
    my ( $dec, $name ) = @_;
    my ( $b, $h );
    my $ok = eval { $dec->decode_with_header( $PKT, $b, $h ); 1 };
    ok( $ok, "$name: decode succeeds" ) or diag $@;
    is( ref($b),                      'HASH',        "$name: body is a hash" );
    is( scalar( keys %{ $b || {} } ), 200,           "$name: body has 200 keys" );
    is( $b && $b->{key7},             "value7" x 20, "$name: body value intact" );
    is( ref($h),                      'ARRAY',       "$name: header is an array" );
    is( scalar( @{ $h || [] } ),      1,             "$name: header has one element" );
    is( ref( $h && $h->[0] ),         'Mutator',     "$name: header object thawed" );
}

sub check_dies {
    my ( $dec, $name ) = @_;
    my ( $b, $h );
    my $ok = eval { $dec->decode_with_header( $PKT, $b, $h ); 1 };
    ok( !$ok, "$name: decode dies" );
    like( $@, $err_re, "$name: error mentions the modified input" );
}

for my $mode (qw(assign append)) {
    local $MODE = $mode;
    $PKT = $packet;
    check_dies( Sereal::Decoder->new, "THAW $mode to input" );
}

for my $mode (qw(read unrelated)) {
    local $MODE = $mode;
    $PKT = $packet;
    check_ok( Sereal::Decoder->new, "THAW $mode" );
}

{
    local $MODE = '';
    $PKT = $packet;
    my $dec = Sereal::Decoder->new( { thaw_allow_classes => sub { $main::PKT = ''; 1 } } );
    check_dies( $dec, "thaw_allow_classes callback clears input" );
}

# decode_only_header reads nothing after the header, so a harmless append
# to the input by THAW must not make it die; decode_with_header on the same
# packet still has a body to read and must.
{
    local $MODE = 'append';
    $PKT = $packet;
    my $h = eval { Sereal::Decoder->new->decode_only_header($PKT) };
    is( $@,                   '',        "decode_only_header: THAW append to input does not die" );
    is( ref($h),              'ARRAY',   "decode_only_header: header is an array" );
    is( ref( $h && $h->[0] ), 'Mutator', "decode_only_header: header object thawed" );

    $PKT = $packet;
    check_dies( Sereal::Decoder->new, "decode_with_header after header-only case" );
}

# the decoder object stays usable after a rejected decode
{
    local $MODE = 'assign';
    $PKT = $packet;
    my $dec = Sereal::Decoder->new;
    check_dies( $dec, "reuse: first decode" );

    my $fresh = Header(6) . "\x51\x61k\x01";     # { k => 1 }
    my $out   = eval { $dec->decode($fresh) };
    is( $@, '', "reuse: fresh packet decodes without error" );
    is_deeply( $out, { k => 1 }, "reuse: fresh packet decoded correctly" );

    local $MODE = '';
    $PKT = $packet;
    check_ok( $dec, "reuse: header packet without mutation" );
}

done_testing;
