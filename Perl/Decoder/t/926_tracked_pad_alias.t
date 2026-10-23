#!perl
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(refaddr);
use Sereal::Decoder;
use lib 't/lib';
use Sereal::TestSet qw(Header varint);

for my $version ( 1 .. 6 ) {
    my $header = Header($version);
    my $base   = $version == 1 ? length($header) : 1;
    for my $options (
        {}, { use_undef => 1 }, { alias_smallint => 1 },
        { alias_varint_under => 100 }, { set_readonly => 1 },
        { use_undef          => 1, set_readonly         => 1 },
        { alias_smallint     => 1, set_readonly_scalars => 1 } )
    {
        my $dec   = Sereal::Decoder->new($options);
        my $label = "v$version " . join( ',', sort keys %$options );
        for my $case (
            [ 'alias',  "\x2e" . chr( $base + 1 ), 7 ],
            [ 'undef',  "\x25",     undef ], [ 'smallint', "\x07", 7 ],
            [ 'varint', "\x20\x2a", 42 ] )
        {
            # Both tracked PADs must resolve to the final scalar even when
            # ALIAS, use_undef, or integer caching substitutes its SV.
            my $body =
                  "\x45\x87\xbf\x3f\xbf"
                . $case->[1] . "\x2e"
                . chr( $base + 2 ) . "\x2e"
                . chr( $base + 4 ) . "\x29"
                . chr( $base + 2 );
            my $out = $dec->decode( $header . $body );
            is_deeply( [ @$out[ 0 .. 3 ] ], [ 7, ( $case->[2] ) x 3 ], "$label $case->[0] values" );
            is(
                refaddr( \$out->[1] ), refaddr( \$out->[2] ),
                "$label first PAD aliases final scalar"
            );
            is(
                refaddr( \$out->[1] ), refaddr( \$out->[3] ),
                "$label second PAD aliases final scalar"
            );
            is( refaddr( $out->[4] ), refaddr( \$out->[1] ), "$label REFP sees final scalar" );
        }
    }
}

# Exercise the whole run, including offsets needing multi-byte varints. The
# final tag may itself be tracked, and untracked PADs must stay untracked.
my @layouts = (
    [ 'single',         "\xbf",                             0 ],
    [ 'adjacent',       "\xbf\xbf\xbf",                     1 ],
    [ 'mixed',          "\x3f\xbf\x3f\xbf\x3f",             1 ],
    [ 'long',           "\xbf" . ( "\x3f" x 130 ) . "\xbf", 0 ],
    [ 'final tag only', "\x3f\x3f",                         1 ],
);
for my $version ( 1 .. 6 ) {
    my $header = Header($version);
    my $base   = $version == 1 ? length($header) : 1;
    my @cases  = (
        [ 'alias',             "\x2e" . varint( $base + 1 ), 7 ],
        [ 'undef',             "\x25",                       undef ],
        [ 'smallint',          "\x07",                       7 ],
        [ 'negative smallint', "\x19",                       -7 ],
        [ 'varint',            "\x20\x2a",                   42 ],
        [ 'zigzag',            "\x21\x0d",                   -7 ],

        # Payload bytes that resemble tracked tags must not be retargeted.
        [ 'string',    "\x62\xbf\x87", "\xbf\x87" ],
        [ 'reference', "\x41\x09",     [9] ],
    );
    push @cases, [ 'canonical undef', "\x39",     undef ] if $version >= 3;
    push @cases, [ 'negative varint', "\x3c\x07", -7 ]    if $version >= 6;
    my $plain_dec = Sereal::Decoder->new;
    for my $layout (@layouts) {
        my @untracked =
            grep { ord( substr( $layout->[1], $_, 1 ) ) == 0x3f } 0 .. length( $layout->[1] ) - 1;
        next unless @untracked;

        # Check both ends of an untracked span without repeating every byte
        # of the long PAD run.
        for my $index ( $untracked[0], $untracked[-1] ) {
            for my $tag ( 0x2e, 0x29 ) {
                eval {
                    $plain_dec->decode( $header . "\x42"
                            . $layout->[1] . "\x07"
                            . chr($tag)
                            . varint( $base + 1 + $index ) );
                    1;
                };
                like(
                    $@, qr/references an unknown item/,
                    "v$version $layout->[0] untracked PAD is not registered for tag $tag"
                );
            }
        }
    }
    for my $options (
        {}, { use_undef => 1 }, { alias_smallint => 1 },
        { alias_varint_under   => 100 }, { set_readonly => 1 },
        { set_readonly_scalars => 1 },
        { use_undef            => 1,   set_readonly         => 1 },
        { alias_varint_under   => 100, set_readonly_scalars => 1 } )
    {
        my $dec = Sereal::Decoder->new($options);
        for my $layout (@layouts) {
            for my $kind ( 'array', 'hash' ) {
                for my $case (@cases) {
                    my ( $name, $value, $expected ) = @$case;
                    my $label =
                          "v$version "
                        . join( ',', sort keys %$options )
                        . " $layout->[0] $kind $name";
                    my $body = "\x43\x87" . ( $kind eq 'array' ? "\x41" : "\x51\x61v" );
                    my @offsets;
                    for my $tag ( unpack( 'C*', $layout->[1] ) ) {
                        push @offsets, $base + length($body) if $tag & 128;
                        $body .= chr($tag);
                    }
                    if ( $layout->[2] ) {
                        push @offsets, $base + length($body);
                        substr( $value, 0, 1 ) = chr( ord($value) | 128 );
                    }
                    $body .= $value;
                    $body .= "\x28\x2b" . varint( 2 * @offsets + 2 );
                    my $last_alias;
                    for my $offset (@offsets) {
                        $last_alias = $base + length($body);
                        $body .= "\xae" . varint($offset);
                    }
                    my $chain = $base + length($body);
                    $body .= "\xae" . varint($last_alias) . "\x2e" . varint($chain);
                    $body .= join( '', map { "\x29" . varint($_) } @offsets );
                    my $packet  = $header . $body;
                    my $out     = $dec->decode($packet);
                    my $aliases = $out->[2];
                    is_deeply(
                        $out->[1], $kind eq 'array' ? [$expected] : { v => $expected },
                        "$label container value"
                    );

                    # Perl treats PL_sv_undef in a hash as a placeholder;
                    # taking an lvalue reference to that slot attempts creation.
                    my $slot =
                          $kind eq 'array'                             ? \$out->[1][0]
                        : !defined($expected) && $options->{use_undef} ? \$aliases->[0]
                        :                                                \$out->[1]{v};
                    my $count = @offsets + 2;
                    is_deeply(
                        [ @$aliases[ 0 .. $count - 1 ] ], [ ($expected) x $count ],
                        "$label values including chained aliases"
                    );
                    is_deeply(
                        [ map { refaddr( \$aliases->[$_] ) } 0 .. $count - 1 ],
                        [ ( refaddr($slot) ) x $count ], "$label all ALIAS offsets share the slot"
                    );
                    is_deeply(
                        [ map { refaddr( $aliases->[$_] ) } $count .. $#$aliases ],
                        [ ( refaddr($slot) ) x @offsets ],
                        "$label all REFP offsets reference the slot"
                    );
                    is(
                        $dec->bytes_consumed, length($packet),
                        "$label consumes exactly the packet"
                    );
                    if ( $name eq 'alias' ) {
                        is(
                            refaddr($slot), refaddr( \$out->[0] ),
                            "$label retains the original alias target"
                        );
                    }

                    # Changing a writable value must propagate through every
                    # tracked offset, rather than merely compare equal initially.
                    if ( !ref($expected) && !Internals::SvREADONLY($$slot) ) {
                        $$slot = 'changed';
                        is_deeply(
                            [ @$aliases[ 0 .. $count - 1 ] ], [ ('changed') x $count ],
                            "$label mutations propagate through aliases"
                        );
                        is_deeply(
                            [ map { ${ $aliases->[$_] } } $count .. $#$aliases ],
                            [ ('changed') x @offsets ],
                            "$label mutations propagate through references"
                        );
                    }
                }
            }
        }

        # Pending tracked offsets from a failed parse must not survive reuse.
        for my $tail ( "\xbf\x3f\xbf", "\xbf\x2e", "\xbf\xa0\x80" ) {
            eval { $dec->decode( $header . "\x41" . $tail ); 1 };
            like(
                $@, qr/unexpected end of input|Unexpected termination|end of packet reached/,
                "v$version truncated tracked PAD value fails cleanly"
            );
            eval { $dec->decode( $header . "\x41\x2e" . varint( $base + 1 ) ); 1 };
            like(
                $@, qr/references an unknown item/,
                "v$version failed decode does not leave a tracked offset behind"
            );
            is_deeply(
                $dec->decode( $header . "\x41\x07" ), [7],
                "v$version decoder remains reusable after a tracked PAD error"
            );
        }
    }
}
done_testing;
