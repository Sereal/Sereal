#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;

for my $version ( 1 .. 6 ) {
    my $header = ( $version < 3 ? '=srl' : "=\xf3rl" ) . chr($version) . "\0";
    my $offset = $version == 1 ? 7 : 2;
    for my $options ( {}, { set_readonly => 1 }, { set_readonly_scalars => 1 } ) {
        my $dec   = Sereal::Decoder->new($options);
        my $label = "v$version " . join( ',', sort keys %$options );
        for my $case (
            [ 'array',            "\x41\x07", 1 ], [ 'hash',    "\x51\x61x\x07", 1 ],
            [ 'scalar reference', "\x28\x07", 1 ], [ 'integer', "\x07",          0 ],
            [ 'string',           "\x61x",    0 ], [ 'object',  "\x2c\x61X\x40", 1 ] )
        {
            my $out      = $dec->decode( $header . "\x42" . $case->[1] . "\x2f" . chr($offset) );
            my $readonly = $options->{set_readonly}
                || ( $options->{set_readonly_scalars} && !$case->[2] );
            is(
                !!Internals::SvREADONLY( $out->[0] ), !!$readonly,
                "$label direct $case->[0] has expected readonly flag"
            );
            is(
                !!Internals::SvREADONLY( $out->[1] ), !!$readonly,
                "$label COPY $case->[0] has the same readonly flag"
            );
            is_deeply( $out->[1], $out->[0], "$label COPY preserves $case->[0] value" );
        }
    }
}
done_testing;
