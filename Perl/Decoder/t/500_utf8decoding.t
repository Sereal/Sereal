#!perl

use strict;
use warnings;
use Test::More;
use File::Spec;
use Encode;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet::Corpus qw(read_packet);
use Sereal::Decoder qw(decode_sereal);

my $corpus = File::Spec->catdir(qw(t data packet_corpus));
my @tests = (
    {
        packet => read_packet( $corpus, 'decoder-features', 'utf8decoding/snappy' ),
        input  => {
            foo => 'bar', f111 => 'bar', f1111 => 'bar', f11111 => 'bar',
            f111111 => 'bar', f1111111 => 'bar', f11111111 => 'bar',
            f111111111 => 'bar', f1111111111 => 'bar', f11111111111 => 'bar',
            f111111111111 => 'bar', f1111111111111 => 'bar',
            f11111111111111 => 'bar', f111111111111111 => 'bar',
            f1111111111111111 => 'bar', f11111111111111111 => 'bar',
        },
    },
    {
        packet => read_packet( $corpus, 'decoder-features', 'utf8decoding/plain' ),
        input  => { therefore => "\x{2234}" },
    },
);
plan tests => 9 * @tests;

# The testing routine
sub decode_with_utf8_flag {
    my ( $s1, $input ) = @_;
    my $s2 = $s1;
    ok( !utf8::is_utf8($s1), "encoder returns a string without the utf8 flag" );
    $s2 = encode( "utf8", $s2 );
    Encode::_utf8_on($s2);
    ok( utf8::is_utf8($s2), "the copy of the string has the utf8 flag turned on" );
    is( $s1, $s2, "the strings are still the same for perl" );

    my $output;
    my $ok  = eval { decode_sereal( $s1, { validate_utf8 => 1 }, $output ); 1 };
    my $err = $@ || 'Zombie error';
    ok( $ok, "did not die while decoding the first string" ) or diag $err;
    is( ref $output, 'HASH', "correctly decoded to a hashref" );
    is_deeply( $output, $input, "correctly decoded" );

    undef $output;
    $ok  = eval { decode_sereal( $s2, { validate_utf8 => 1 }, $output ); 1 };
    $err = $@ || 'Zombie error';
    ok( $ok, "did not die while decoding the utf8 string" ) or diag $err;
    is( ref $output, 'HASH', "correctly decoded to a hashref" );
    is_deeply( $output, $input, "correctly decoded" );
}

for my $t (@tests) {
    decode_with_utf8_flag( $t->{packet}, $t->{input} );
}
