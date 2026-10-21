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

