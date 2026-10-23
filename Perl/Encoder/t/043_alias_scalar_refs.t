#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(refaddr);
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;

plan skip_all => 'Did not find right version of decoder' if !have_encoder_and_decoder();

for my $version ( 1 .. 6 ) {
    for my $option (qw(dedupe_strings aliased_dedupe_strings)) {
        for my $text ( 'duplicate', "\x{100}duplicate" ) {
            my ( $a, $b ) = ( $text, $text );
            my $encoder = Sereal::Encoder->new( { protocol_version => $version, $option => 1 } );
            for my $pass ( 1 .. 2 ) {
                my $label   = "v$version $option pass=$pass wide=" . utf8::is_utf8($text);
                my $decoded = eval {
                    Sereal::Decoder->new->decode(
                        $encoder->encode( [
                                $text, \$a, \$b, \$b, $text,
                            ] ) );
                };
                my $error = $@;
                ok( !$error, "$label: duplicate scalar referents decode" ) or diag($error);
                SKIP: {
                    skip 'encode/decode failed', 5 if $error;
                    is_deeply(
                        $decoded, [ $text, \$a, \$b, \$b, $text ],
                        "$label: values survive"
                    );
                    isnt(
                        refaddr( $decoded->[1] ), refaddr( $decoded->[2] ),
                        "$label: distinct referents remain distinct"
                    );
                    is(
                        refaddr( $decoded->[2] ), refaddr( $decoded->[3] ),
                        "$label: repeated reference still shares its referent"
                    );
                    isnt(
                        refaddr( $decoded->[1] ), refaddr( \$decoded->[0] ),
                        "$label: REFN creates its own scalar"
                    );
                    if ( $option eq 'aliased_dedupe_strings' ) {
                        is(
                            refaddr( \$decoded->[0] ), refaddr( \$decoded->[4] ),
                            "$label: container values still alias"
                        );
                    }
                    else {
                        isnt(
                            refaddr( \$decoded->[0] ), refaddr( \$decoded->[4] ),
                            "$label: COPY creates independent values"
                        );
                    }
                }
            }
            my $decoded =
                eval { Sereal::Decoder->new->decode( $encoder->encode( [ \$a, $text, \$b ] ) ) };
            my $error = $@;
            ok( !$error, "v$version $option: a scalar referent can be the first string target" )
                or diag($error);
            SKIP: {
                skip 'encode/decode failed', 2 if $error;
                is_deeply(
                    $decoded, [ \$a, $text, \$b ],
                    'later values and scalar references round-trip'
                );
                if ( $option eq 'aliased_dedupe_strings' ) {
                    is(
                        refaddr( $decoded->[0] ), refaddr( \$decoded->[1] ),
                        'a later container value aliases the first scalar referent'
                    );
                }
                else {
                    isnt(
                        refaddr( $decoded->[0] ), refaddr( \$decoded->[1] ),
                        'ordinary deduplication copies the scalar referent'
                    );
                }
            }
        }
    }
}

# COPY must not merge scalar objects or change either object's class.
my ( $left, $right ) = ( 'duplicate', 'duplicate' );
my $objects = [ bless( \$left, 'LeftScalar' ), bless( \$right, 'RightScalar' ) ];
my $decoded = eval {
    Sereal::Decoder->new->decode(
        Sereal::Encoder->new( { aliased_dedupe_strings => 1 } )->encode($objects) );
};
my $error = $@;
ok( !$error, 'duplicate blessed scalar referents decode' ) or diag($error);
SKIP: {
    skip 'encode/decode failed', 1 if $error;
    is_deeply( $decoded, $objects, 'each scalar object keeps its class' );
}
done_testing();
