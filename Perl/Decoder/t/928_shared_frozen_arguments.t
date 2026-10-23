#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

{

    package FrozenA;
    our @calls;
    sub THAW { push @calls, 'A'; return 'A' }

    package FrozenB;
    sub THAW { push @FrozenA::calls, 'B'; return 'B' }
}

for my $version ( 2 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    for my $options (
        {}, { no_thaw_objects => 1 }, { set_readonly => 1 },
        { thaw_allow_classes => [], thaw_deny_action => 'raw' } )
    {
        my $dec    = Sereal::Decoder->new($options);
        my $label  = "v$version " . join( ',', sort keys %$options );
        my $first  = "\x42\x32\x67FrozenA\x28";
        my $offset = length($first) + 1;
        for my $second ( "\x32\x67FrozenA", "\x32\x67FrozenB", "\x33\x03" ) {
            @FrozenA::calls = ();
            my $packet = $header . $first . "\xab\0" . $second . "\x29" . chr($offset);
            eval { $dec->decode($packet) };
            like(
                $@, qr/Frozen argument array is already assigned/,
                "$label repeated frozen declaration is rejected"
            );
            is_deeply( \@FrozenA::calls, [], "$label declaration conflict invokes no callbacks" );
            is( $dec->decode( $header . "\x07" ), 7, "$label decoder reusable after conflict" );
        }

        # Nesting declarations around the same destination used to enqueue
        # it twice and reinterpret already-thawed data on the second visit.
        eval { $dec->decode( $header . "\x32\x67FrozenA\x32\x67FrozenB\x40" ) };
        like(
            $@, qr/Frozen argument array is already assigned/,
            "$label nested declarations cannot share one destination"
        );
    }
    my $out = Sereal::Decoder->new->decode( $header . "\x42\x32\x67FrozenA\x28\xab\0\x29\x0c" );
    is_deeply( $out, [ 'A', 'A' ], "v$version ordinary REFP reuse remains supported" );
}
done_testing;
