use strict;
use warnings;

use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);
my $v1 = [
    0,
    1,
    "foo",
    1,
    2,
    3,
    3,
    4,
    3,
    3,
    2,
    1,
    27,
    1,
];
if (have_encoder_and_decoder()) {
    plan tests => 10 + @$v1 * 3;
} else {
    plan skip_all => 'Did not find right version of encoder';
}


my $enc= Sereal::Encoder->new();
my $dec= Sereal::Decoder->new( { alias_varint_under => 100000 } );

my $sereal_v1 = $enc->encode( $v1 );
my $v2 = $dec->decode( $sereal_v1 );

my $sereal_v2 = $enc->encode( $v2 );
my $v3 = $dec->decode( $sereal_v2 );

if ( 0 ) {
    diag "arrays: ";
    diag "v1: @$v1";
    diag "v2: @$v2";
    diag "v3: @$v3";
    hobodecode($sereal_v2);
}

for my $i ( 0 .. $#$v1) {
    is( $v2->[$i], $v1->[$i], "first copy idx $i is same as source" );
    is( $v3->[$i], $v1->[$i], "second copy idx $i is same as source" );
    is( $v3->[$i], $v2->[$i], "first and second copy idx $i is same" );
}

ok( \$v2->[1] == \$v2->[3], "first copy has expected aliases" );
ok( \$v3->[1] == \$v3->[3], "second copy has expected aliases" );

ok( \$v2->[0] != \$v2->[1], "first copy idx 0 is not an alias of idx 1" );
ok( \$v2->[0] != \$v2->[3], "first copy idx 0 is not an alias of idx 3" );
ok( \$v2->[2] != \$v2->[1], "first copy idx 2 is not an alias of idx 1" );
ok( \$v2->[2] != \$v2->[3], "first copy idx 2 is not an alias of idx 3" );

ok( \$v3->[0] != \$v3->[1], "second copy idx 0 is not an alias of idx 1" );
ok( \$v3->[0] != \$v3->[3], "second copy idx 0 is not an alias of idx 3" );
ok( \$v3->[2] != \$v3->[1], "second copy idx 2 is not an alias of idx 1" );
ok( \$v3->[2] != \$v3->[3], "second copy idx 2 is not an alias of idx 3" );

