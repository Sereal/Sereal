use strict;
use warnings;

use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::Decoder;
use Sereal::TestSet::Corpus qw(read_packet);

my @tests = (
    [ 15,  alias_smallint     => 1 ],
    [ 127, alias_varint_under => 128 ],
);

my $num_tests = 0;
$num_tests += ( ( 16 + $_->[0] + 2 ) * 2 ) for @tests;
$num_tests += 2;
plan tests => $num_tests;
my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
foreach my $test (@tests) {
    my ( $up_to, $opt, $opt_val ) = @$test;

    #diag "$up_to: $opt $opt_val";

    my $dec = Sereal::Decoder->new( { $opt => $opt_val } );

    my $packet = read_packet( $corpus, 'decoder-features', "alias_options/$up_to" );
    my $got = $dec->decode($packet);

    # undef the decoder to make sure if it blows up on DESTROY it does it before we test.
    undef $dec;

    # Make sure we get the expected aliases
    for ( -16 .. $up_to ) {
        ok(
            \$got->{array}[ $_ + 16 ] == \$got->{array2}[ -1 - ( $_ + 16 ) ],
            "$opt: array alias: $_"
        );
        ok( \$got->{$_} == \$got->{array}[ $_ + 16 ], "$opt: array alias: $_" );
    }

    # Make sure the aliases are readonly.
    my $eval_ok = eval { $got->{$up_to} = 123; };
    my $error   = $eval_ok ? "" : ( "$@" || "Zombie error" );
    ok( !$eval_ok, "$opt: expect modification of \$got->{$up_to} to die" );
    like( $error, qr/read-only/, "$opt: expect an error about read-only values" );
}

my $large_alias_threshold = eval {
    Sereal::Decoder->new( { alias_varint_under => '18446744073709551615' } );
    1;
};
ok( !$large_alias_threshold, 'reject alias threshold too large for cache indexing' );
like( $@, qr/alias_varint_under/, 'oversized alias threshold reports the option' );

