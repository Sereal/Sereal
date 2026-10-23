#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;

# test our test framework

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;
use Config;

sub exception (&);

# needs more tests
ok( _test_deep_cmp( ["x"],         {} ) );
ok( _test_deep_cmp( { "x" => 1 },  { "y" => 1 } ) );
ok( _test_deep_cmp( { "x" => 1 },  { "x" => 2 } ) );
ok( _test_deep_cmp( { "x" => 1 },  { "x" => 2, "y" => 1 } ) );
ok( !_test_deep_cmp( { "x" => 1 }, { "x" => 1 } ) );
ok( !_test_deep_cmp( ["x"],        ["x"] ) );
ok( _test_deep_cmp( ["x"],         [ "y", "p" ] ) );
ok( _test_deep_cmp( [ "a", "x" ],  ["y"] ) );
ok( _cmp_str( "foo",       "bar" ) );
ok( !_cmp_str( "aaa",      "aaa" ) );
ok( _cmp_str( "aaacowbbb", "aaadogbb" ) );

my ( $varint_value, $varint_end ) = read_varint_at( "xx" . varint(16_384) . "tail", 2 );
is( $varint_value, 16_384, 'read_varint_at decodes from a supplied offset' );
is( $varint_end,   5,      'read_varint_at returns the offset after the varint' );
like(
    exception { read_varint_at( "\x80", 0 ) }, qr/truncated varint/,
    'read_varint_at rejects a truncated varint'
);
like(
    exception { read_varint_at( "", 1 ) }, qr/offset is outside input/,
    'read_varint_at rejects an invalid offset'
);
my $too_long_varint = "\x80" x ( 8 * $Config{uvsize} / 7 + 1 );
like(
    exception { read_varint_at( $too_long_varint, 0 ) }, qr/(?:too long|too big)/,
    'read_varint_at rejects a varint beyond the native UV width'
);

my $l = "ba\xDF";
my $u = $l;
utf8::upgrade($u);
ok( _cmp_str( $l, $u ) );
my $vstring                               = v01.002.003;
my $same_value_different_vstring_spelling = v1.2.3;
ok( !_test_deep_cmp( $vstring, $vstring ), 'identical v-strings compare equal' );
ok(
    _test_deep_cmp( $vstring, $same_value_different_vstring_spelling ),
    'v-string spelling is part of roundtrip equality',
);
ok(
    !_test_deep_cmp( \$vstring, \$vstring ),
    'references to identical v-strings compare equal',
);
pass();
done_testing();

sub exception (&) {
    my ($code) = @_;
    eval { $code->(); 1 } ? '' : $@;
}

