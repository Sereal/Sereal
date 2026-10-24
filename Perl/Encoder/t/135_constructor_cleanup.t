#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Scalar::Util    qw(weaken);
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder qw(encode_sereal);

local $ENV{SEREAL_CHECKSUM_POLICY};
delete $ENV{SEREAL_CHECKSUM_POLICY};
local $Sereal::DefaultKey;

sub allow_with_marker {
    my ($marker) = @_;
    return sub { return !!$marker };
}

my $key = '0123456789abcdef';
for my $case (
    [ 'deny action',       { freeze_deny_action => 'invalid' }, qr/freeze_deny_action/ ],
    [ 'compression',       { compress           => 99 },        qr/compression format/ ],
    [ 'compression level', { compress           => 2, compress_level => 99 }, qr/compress_level/ ],
    [ 'freeze protocol',   { protocol_version   => 1 },                    qr/protocol version 2/ ],
    [ 'no blessing',       { no_bless_objects   => 1 },                    qr/mutually exclusive/ ],
    [ 'unknown options', { undef_unknown   => 1, stringify_unknown => 1 }, qr/mutually exclusive/ ],
    [ 'wide key',        { checksum_v0_key => "\x{100}" },                 qr/Wide character/ ],
    [
        'checksum protocol', { protocol_version => 5, checksum_v0_key => $key },
        qr/protocol version 6/
    ],
    [ 'multiple keys', { checksum_v0_key => $key, checksum_v1_key => $key }, qr/Specify only one/ ],
    )
{
    my ( $name, $extra, $error ) = @$case;
    for my $functional ( 0, 1 ) {
        my $marker = {};
        my $weak   = $marker;
        weaken($weak);
        my $options = { freeze_allow_classes => allow_with_marker($marker), %$extra };
        undef $marker;
        my $ok = eval {
            $functional ? encode_sereal( 7, $options ) : Sereal::Encoder->new($options);
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

for my $version ( 0 .. 2 ) {
    local $SIG{__WARN__} = sub { die "key warning became an exception\n" };
    my $ok = eval { Sereal::Encoder->new( { "checksum_v${version}_key" => 'short' } ); 1 };
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
    my $encoder = Sereal::Encoder->new( { freeze_allow_classes => allow_with_marker($marker) } );
    undef $marker;
    ok( defined $weak, 'successful construction keeps its callback' );
    my $first  = $encoder->encode(7);
    my $second = $encoder->encode(7);
    is( $second, $first, 'a successfully constructed encoder can be reused' );
    undef $encoder;
    ok( !defined $weak, 'destroying an encoder releases its callback' );
}

{
    my $marker = {};
    my $weak   = $marker;
    weaken($weak);
    my $options = { freeze_allow_classes => allow_with_marker($marker) };
    undef $marker;
    my $packet = encode_sereal( 7, $options );
    undef $options;
    ok( length($packet), 'functional encoding still returns a packet' );
    ok( !defined $weak,  'functional encoding releases its temporary encoder' );
    SKIP: {
        skip 'decoder is unavailable', 1 unless have_encoder_and_decoder();
        is( Sereal::Decoder->new->decode($packet), 7, 'functional packet decodes correctly' );
    }
}

done_testing();
