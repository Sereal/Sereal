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
    [ 'wide key',        { checksum_key_v1 => "\x{100}" },                 qr/Wide character/ ],
    [
        'checksum protocol', { protocol_version => 5, checksum_key_v1 => $key },
        qr/protocol version 6/
    ],
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

{
    my $ok = eval { Sereal::Encoder->new( {
        checksum_key_v1 => $key,
        checksum_key_v2 => 'fedcba9876543210',
    } ); 1 };
    ok( !$ok, 'different versioned checksum keys are rejected' );
    like( $@, qr/Different keys were supplied for checksum_key/, 'different versioned checksum keys explain the conflict' );
}

for my $version ( 0 .. 2 ) {
    local $SIG{__WARN__} = sub { die "key warning became an exception\n" };
    my $ok = eval { Sereal::Encoder->new( { ( "checksum_key_v" . ( $version + 1 ) ) => 'short' } ); 1 };
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

subtest 'constructor option dependencies do not depend on argument order' => sub {
    local $Sereal::DefaultKey;
    local $Sereal::Encoder::DefaultKey;
    my $data = { z => [('repeat' x 50) x 4], a => [1, 2, 3] };
    for my $case (
        [ 'explicit protocol overrides old alias',
          { protocol_version => 2, use_protocol_v1 => 1 }, { protocol_version => 2 } ],
        [ 'undefined protocol falls back to old alias',
          { protocol_version => undef, use_protocol_v1 => 1 }, { protocol_version => 1 } ],
        [ 'explicit compression disables old aliases',
          { compress => 0, snappy => 1, snappy_incr => 1 }, { compress => 0 } ],
        [ 'explicit compression format overrides old aliases',
          { compress => 2, snappy => 1, snappy_incr => 1, protocol_version => 3,
            compress_level => 5, compress_threshold => 0 },
          { compress => 2, protocol_version => 3, compress_level => 5, compress_threshold => 0 } ],
        [ 'Snappy framing uses final protocol',
          { snappy => 1, protocol_version => 2, use_protocol_v1 => 1, compress_threshold => 0 },
          { snappy => 1, protocol_version => 2, compress_threshold => 0 } ],
        [ 'general threshold overrides Snappy threshold',
          { compress => 1, compress_threshold => 1000000, snappy_threshold => 0 }, { compress => 0 } ],
        [ 'undefined general threshold falls back to Snappy threshold',
          { compress => 1, compress_threshold => undef, snappy_threshold => 0 },
          { compress => 1, compress_threshold => 0 } ],
        [ 'explicit false canonical settings override alias',
          { canonical => 3, sort_keys => 0, canonical_refs => 0 }, { canonical => 0 } ],
        [ 'sort mode overrides canonical sort mode',
          { canonical => 3, sort_keys => 1 }, { sort_keys => 1, canonical_refs => 1 } ],
        [ 'undefined sort setting overrides alias',
          { canonical => 1, sort_keys => undef }, { sort_keys => 0, canonical_refs => 1 } ],
        [ 'undefined reference setting overrides alias',
          { canonical => 1, canonical_refs => undef }, { sort_keys => 1, canonical_refs => 0 } ],
        [ 'aliased dedupe implies dedupe regardless of explicit false',
          { aliased_dedupe_strings => 1, dedupe_strings => 0 }, { aliased_dedupe_strings => 1 } ],
        [ 'false alias permits ordinary dedupe',
          { aliased_dedupe_strings => 0, dedupe_strings => 1 }, { dedupe_strings => 1 } ],
    ) {
        my ($name, $options, $expected) = @$case;
        my $actual = Sereal::Encoder->new({ checksum_key => undef, %$options })->encode($data);
        my $reference = Sereal::Encoder->new({ checksum_key => undef, %$expected })->encode($data);
        is($actual, $reference, $name);
    }
};

{
    my $options = { protocol_version => 6, freeze_deny_action => 'serialize' };
    $options->{freeze_allow_classes} = $options;
    my $encoder = Sereal::Encoder->new($options);
    delete $options->{freeze_allow_classes}; # Break the test's reference cycle.
    ok($encoder, 'allow-list can reuse the options hash without resetting its iterator');
    ok(length($encoder->encode(7)), 'encoder constructed with a self-referential allow-list works');

    $options = { protocol_version => 1 };
    $options->{freeze_allow_classes} = $options;
    eval { Sereal::Encoder->new($options); 1 };
    my $error = $@;
    delete $options->{freeze_allow_classes};
    like($error, qr/protocol version 2 or later/,
        'self-referential allow-list does not skip the protocol setting');
}

done_testing();
