#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Scalar::Util    qw(weaken);
use Sereal::TestSet qw(Header);
use Sereal::Decoder qw(decode_sereal);

local $ENV{SEREAL_CHECKSUM_POLICY};
delete $ENV{SEREAL_CHECKSUM_POLICY};
local $Sereal::DefaultKey;

sub allow_with_marker {
    my ($marker) = @_;
    return sub { return !!$marker };
}

for my $case (
    [ 'deny action',     { thaw_deny_action   => 'invalid' },              qr/thaw_deny_action/ ],
    [ 'alias threshold', { alias_varint_under => '18446744073709551615' }, qr/alias_varint_under/ ],
    )
{
    my ( $name, $extra, $error ) = @$case;
    for my $functional ( 0, 1 ) {
        my $marker = {};
        my $weak   = $marker;
        weaken($weak);
        my $options = { thaw_allow_classes => allow_with_marker($marker), %$extra };
        undef $marker;
        my $ok = eval {
            $functional
                ? decode_sereal( Header(6) . chr(7), $options )
                : Sereal::Decoder->new($options);
            1;
        };
        my $caught = $@;
        undef $options;
        my $label = ( $functional ? 'functional' : 'object' ) . " invalid $name";
        ok( !$ok, "$label is rejected" );
        like( $caught, $error, "$label preserves its exception" );
        ok( !defined $weak, "$label releases the retained callback" );
    }
}

# These fail before the allow-list is stored, but after allocating the
# decoder and its tables. Valgrind must find no leaked constructor state.
for my $version ( 0 .. 2 ) {
    my $ok = eval {
        Sereal::Decoder->new( { "checksum_v${version}_key" => "\x{100}" } );
        1;
    };
    ok( !$ok, "wide checksum v$version key is rejected" );
    like( $@, qr/Wide character/, "wide checksum v$version key preserves its exception" );

    local $SIG{__WARN__} = sub { die "key warning became an exception\n" };
    $ok = eval {
        Sereal::Decoder->new( { "checksum_v${version}_key" => 'short' } );
        1;
    };
    ok( !$ok, "fatal checksum v$version warning is propagated" );
    like(
        $@, qr/key warning became an exception/,
        "fatal checksum v$version warning is preserved"
    );
}

{
    my $marker = {};
    my $weak   = $marker;
    weaken($weak);
    my $decoder = Sereal::Decoder->new( { thaw_allow_classes => allow_with_marker($marker) } );
    undef $marker;
    ok( defined $weak, 'successful construction keeps its callback' );
    is( $decoder->decode( Header(6) . chr(7) ), 7, 'constructed decoder works' );
    is( $decoder->decode( Header(6) . chr(8) ), 8, 'constructed decoder can be reused' );
    undef $decoder;
    ok( !defined $weak, 'destroying a successful decoder releases its callback' );
}

{
    my $marker = {};
    my $weak   = $marker;
    weaken($weak);
    my $options = { thaw_allow_classes => allow_with_marker($marker) };
    undef $marker;
    is( decode_sereal( Header(6) . chr(7), $options ), 7, 'functional decoding still works' );
    undef $options;
    ok( !defined $weak, 'successful functional decoding releases its temporary decoder' );
}

done_testing();
