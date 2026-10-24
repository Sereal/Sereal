#!perl
use strict;
use warnings;
use File::Spec;
use B ();
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder qw(encode_sereal encode_sereal_with_header_data);

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

{

    package ChangingString;
    sub TIESCALAR { bless { fetches => 0 }, shift }
    sub FETCH     { ++$_[0]{fetches} <= 3 ? "\xff" x 8 : "\x{100}" x 8 }
}
{

    package StringifiedCode;
    use overload '""' => sub { $_[0]->() }, fallback => 1;
}

for my $version ( 1 .. 6 ) {
    for my $option (qw(dedupe_strings aliased_dedupe_strings)) {
        tie my $value, 'ChangingString';
        my $packet = Sereal::Encoder->new( { protocol_version => $version, $option => 1 } )
            ->encode( \$value );
        my $decoded = eval { Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($packet) };
        my $error   = $@;
        ok( !$error, "v$version $option: tied FETCH cannot mix bytes and UTF-8 flags" )
            or diag($error);
        SKIP: {
            skip 'decode failed', 1 if $error;
            is( $$decoded, "\xff" x 8, "v$version $option: preserve the captured string" );
        }
    }

    for my $text ( "\x{100}", "\xff" ) {
        my $code = bless sub { $text }, 'StringifiedCode';
        my $packet =
            Sereal::Encoder->new( { protocol_version => $version, stringify_unknown => 1 } )
            ->encode($code);
        my $decoded = Sereal::Decoder->new( { validate_utf8 => 1 } )->decode($packet);
        is( $decoded, $text, "v$version: preserve overloaded string contents" );
        is(
            !!utf8::is_utf8($decoded), !!utf8::is_utf8($text),
            "v$version: preserve overloaded string UTF-8 flag"
        );
    }

    # The raw byte inspected below is inside the allocated PV, just after CUR.
    # MALLOC_PERTURB_=165 makes the pre-fix failure deterministic on glibc.
    for my $api ( 'functional', 'object', ( $version >= 2 ? 'header' : () ) ) {
        my $packet;
        if ( $api eq 'functional' ) {
            $packet = encode_sereal( 'Q' x 100, { protocol_version => $version } );
        }
        elsif ( $api eq 'header' ) {
            $packet = encode_sereal_with_header_data(
                'Q' x 100, { a => 1 },
                { protocol_version => $version } );
        }
        else {
            $packet = Sereal::Encoder->new( { protocol_version => $version } )->encode( 'Q' x 100 );
        }
        my $length   = length($packet);
        my $capacity = B::svref_2object( \$packet )->LEN;
        ok( $capacity > $length, "v$version $api: capacity includes a terminator" );
        SKIP: {
            skip 'no room for terminator', 1 if $capacity <= $length;
            my $allocated = unpack( 'P' . ( $length + 1 ), pack( 'P', $packet ) );
            is(
                ord( substr( $allocated, $length, 1 ) ), 0,
                "v$version $api: PV is NUL terminated"
            );
        }
        is(
            Sereal::Decoder->new->decode($packet), 'Q' x 100,
            "v$version $api: terminator is outside the encoded packet"
        );
    }
}
done_testing();
