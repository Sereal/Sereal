#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

{

    package OuterArgs;
    sub THAW { return [ @_[ 2 .. $#_ ] ] }

    package MutateArgs;
    our $mode;

    sub THAW {
        my $args = $_[2];
        if    ( $mode eq 'hole' )     { delete $args->[1] }
        elsif ( $mode eq 'truncate' ) { $#$args = 0 }
        elsif ( $mode eq 'extend' )   { $args->[4] = 9 }
        return 7;
    }
}

for my $version ( 2 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    my $outer  = 'OuterArgs';
    my $inner  = 'MutateArgs';
    my $prefix = "\x32" . chr( 0x60 + length($outer) ) . $outer . "\x28";
    my $offset = length($prefix) + 1;
    my $packet =
          $header
        . $prefix
        . "\xab\3\x32"
        . chr( 0x60 + length($inner) )
        . $inner
        . "\x41\x29"
        . chr($offset) . "\1\2";
    my $dec = Sereal::Decoder->new;
    for my $case (
        [ 'hole',   [ 7, undef, 2 ] ], [ 'truncate', [7] ],
        [ 'extend', [ 7, 1,     2, undef, 9 ] ] )
    {
        $MutateArgs::mode = $case->[0];
        is_deeply(
            $dec->decode($packet), $case->[1],
            "v$version earlier THAW may $case->[0] the outer argument array"
        );
    }
}
done_testing;
