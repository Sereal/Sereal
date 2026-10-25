#!perl
use strict;
use warnings;
no warnings 'once';
use File::Spec;
use lib 't/lib';
use Sereal::TestSet qw(Header varint read_varint_at checksum_tag have_encoder_and_decoder);
use Sereal::TestSet::Corpus qw(read_packet);
use Test::More;
use Sereal::Decoder;

subtest 'checksum constructor assignments' => sub {
    local $Sereal::ChecksumMode;
    local $Sereal::DefaultKey;
    local $Sereal::Encoder::ChecksumMode;
    local $Sereal::Encoder::DefaultKey;
    local $Sereal::Decoder::DefaultKey;

    my $root = File::Spec->catdir('t', 'data', 'packet_corpus');
    my @fixtures = (
        [ 'checksum/v0', 'Out damned spot!' ],
        [ 'checksum/v1', 'Call me Ishmael.' ],
        [ 'checksum/v2', 'Call me Ishmael.' ],
    );
    my @packets = map { read_packet($root, 'decoder-features', $_->[0]) } @fixtures;
    my $plain = read_packet($root, 'decoder-features', 'checksum/plain');
    my $expected = 'checksummed Sereal payload';
    my @classes = ('Sereal::Decoder');
    push @classes, 'Sereal::Encoder'
        if eval { require Sereal::Encoder; Sereal::Encoder->VERSION($Sereal::Decoder::VERSION); 1 };

    my $check_options = sub {
        my ($class, $name, $options, $mode, $key_warnings, $mode_warnings) = @_;
        $mode_warnings = 0 if $class eq 'Sereal::Decoder';
        my (@warnings, $object);
        {
            local $SIG{__WARN__} = sub { push @warnings, @_ };
            $object = $class->new({ checksum_policy => 'die', %$options });
        }
        is(scalar(grep { /Attempt to set checksum_key more than once/ } @warnings),
            $key_warnings, "$name: duplicate key warnings");
        is(scalar(grep { /Attempt to set checksum_mode more than once/ } @warnings),
            $mode_warnings, "$name: duplicate mode warnings");
        is(scalar(@warnings), $key_warnings + $mode_warnings, "$name: no other warnings");
        for my $warning (grep { /Attempt to set checksum_mode/ } @warnings) {
            my $chosen = $mode ? "v$mode" : 'none';
            like($warning, $mode_warnings == 1 ? qr/using \Q$chosen\E\b/ : qr/using (?:none|v[123])\b/,
                "$name: mode warning reports the selection");
        }
        if ($class eq 'Sereal::Encoder') {
            my $packet = $object->encode($expected);
            my $actual_mode = (ord(substr($packet, 4, 1)) & 128)
                ? ord(substr($packet, 6, 1)) + 1 : 0;
            is($actual_mode, $mode, "$name: emits the selected checksum mode");
            my $key = $fixtures[($mode || 1) - 1][1];
            is(Sereal::Decoder->new({ checksum_key => $key, checksum_mode => $mode, checksum_policy => 'die' })->decode($packet),
                $expected, "$name: Decoder verifies the selected key");
        }
        else {
            my $key = $fixtures[($mode || 1) - 1][1];
            is($object->decode($plain), $expected, "$name: die policy accepts unsigned input");
            for my $type (0 .. 2) {
                if ($fixtures[$type][1] eq $key) {
                    is($object->decode($packets[$type]), $expected,
                        "$name: document selects checksum algorithm " . ($type + 1));
                }
                else {
                    eval { $object->decode($packets[$type]); 1 };
                    like($@, qr/checksum verification failed/,
                        "$name: configured key still rejects a different key");
                }
            }
        }
    };

    for my $class (@classes) {
        subtest $class => sub {
            for my $alias (1 .. 3) {
                my $option = "checksum_key_v$alias";
                my $key = $fixtures[$alias - 1][1];
                $check_options->($class, "$option alone", { $option => $key }, $alias, 0, 0);
                $check_options->($class, "$option plus generic key",
                    { $option => $key, checksum_key => $key }, $alias, 1, 0);
                for my $mode (0 .. 3) {
                    my $selected = $alias > $mode ? $alias : $mode;
                    my $selected_key = $fixtures[$selected - 1][1];
                    for my $value ($mode, $mode ? "v$mode" : 'none') {
                        $check_options->($class, "$option plus mode $value",
                            { $option => $selected_key, checksum_mode => $value }, $selected, 0, 1);
                        $check_options->($class, "$option plus generic key and mode $value",
                            { $option => $selected_key, checksum_key => $selected_key, checksum_mode => $value },
                            $selected, 1, 1);
                    }
                }
            }
            for my $mode (0 .. 3) {
                my $key = $fixtures[($mode || 1) - 1][1];
                $check_options->($class, "generic mode $mode",
                    { checksum_key => $key, checksum_mode => $mode }, $mode, 0, 0);
            }
            for my $left (1 .. 2) {
                for my $right ($left + 1 .. 3) {
                    my $key = $fixtures[$right - 1][1];
                    $check_options->($class, "aliases v$left and v$right",
                        { "checksum_key_v$left" => $key, "checksum_key_v$right" => $key },
                        $right, 1, 1);
                }
            }
            $check_options->($class, 'all aliases plus explicit mode',
                { checksum_key_v1 => $fixtures[2][1], checksum_key_v2 => $fixtures[2][1],
                  checksum_key_v3 => $fixtures[2][1], checksum_mode => 1 }, 3, 2, 3);

            my @key_options = qw(checksum_key checksum_key_v1 checksum_key_v2 checksum_key_v3);
            for my $left (0 .. $#key_options - 1) {
                for my $right ($left + 1 .. $#key_options) {
                    my @warnings;
                    my $ok;
                    {
                        local $SIG{__WARN__} = sub { push @warnings, @_ };
                        $ok = eval { $class->new({
                            $key_options[$left] => 'secret-left-key!',
                            $key_options[$right] => 'secret-right-key',
                            checksum_mode => 0, checksum_policy => 'ignore',
                        }); 1 };
                    }
                    ok(!$ok, "different $key_options[$left] and $key_options[$right] fail even when disabled");
                    like($@, qr/Different keys were supplied for checksum_key/, 'key conflict explains the error');
                    unlike($@ . join('', @warnings), qr/secret-(?:left|right)-key/, 'diagnostics do not reveal keys');
                }
            }

            {
                no strict 'refs';
                local $Sereal::ChecksumMode = 'v3';
                local $Sereal::DefaultKey = 'shared-default!!';
                local $Sereal::Encoder::ChecksumMode = 'v2';
                local ${"${class}::DefaultKey"} = 'module-default!!';
                $check_options->($class, 'alias overrides package defaults',
                    { checksum_key_v1 => $fixtures[0][1] }, 1, 0, 0);
                $check_options->($class, 'generic options override package defaults',
                    { checksum_key => $fixtures[0][1], checksum_mode => 1 }, 1, 0, 0);
            }
        };
    }
};

subtest 'minimum checksum policies use the document algorithm' => sub {
    local $Sereal::ChecksumPolicy;
    local $Sereal::DefaultKey;
    local $Sereal::Decoder::ChecksumPolicy;
    local $Sereal::Decoder::DefaultKey;
    local $Sereal::Encoder::ChecksumPolicy;
    local $Sereal::Encoder::DefaultKey;
    local $Sereal::ChecksumMode;
    local $Sereal::Encoder::ChecksumMode;
    my $root = File::Spec->catdir('t', 'data', 'packet_corpus');
    my @keys = ('Out damned spot!', 'Call me Ishmael.', 'Call me Ishmael.');
    my @packets = map { read_packet($root, 'decoder-features', "checksum/v$_") } (0 .. 2);
    my $plain = read_packet($root, 'decoder-features', 'checksum/plain');
    my $have_encoder = eval { require Sereal::Encoder; Sereal::Encoder->VERSION($Sereal::Decoder::VERSION); 1 };
    for my $policy ('require_key', 'require', 'require_v1', 'require_v2', 'require_v3') {
        my $minimum = $policy eq 'require_v3' ? 3 : $policy eq 'require_v2' ? 2 : 1;
        eval { Sereal::Decoder->new({ checksum_policy => $policy }); 1 };
        like($@, qr/requires a checksum key/, "$policy requires a Decoder key");
        eval { Sereal::Decoder->new({ checksum_policy => $policy, checksum_key => 'short' }); 1 };
        like($@, qr/exactly 16 bytes/, "$policy requires a full length key");
        for my $mode (1 .. 3) {
            my $dec = Sereal::Decoder->new({
                checksum_policy => $policy, checksum_key => $keys[$mode - 1],
                checksum_mode => 0, # An Encoder setting must not disable authentication.
            });
            my $value = eval { $dec->decode($packets[$mode - 1]) };
            if ($mode < $minimum) {
                like($@, qr/requires checksum mode v$minimum or higher/,
                    "$policy rejects document mode v$mode");
            }
            else {
                is($@, '', "$policy accepts document mode v$mode");
                is($value, 'checksummed Sereal payload', "$policy authenticates the corpus");
                my $bad = $packets[$mode - 1];
                substr($bad, -1, 1) ^= "\x01";
                eval { $dec->decode($bad); 1 };
                like($@, qr/checksum verification failed/, "$policy rejects tampered v$mode input");
            }
            eval { $dec->decode($plain); 1 };
            like($@, qr/requires a checksummed packet/, "$policy rejects unsigned input");
            my $unknown = $packets[1];
            substr($unknown, 6, 1) = chr(255);
            eval { $dec->decode($unknown); 1 };
            like($@, qr/Unsupported Sereal checksum type/, "$policy rejects unknown algorithms");
        }
        if ($have_encoder) {
            for my $mode (0 .. 3) {
                my $enc = eval { Sereal::Encoder->new({
                    checksum_policy => $policy, checksum_key => $keys[0], checksum_mode => $mode,
                }) };
                if ($mode < $minimum) {
                    like($@, qr/(?:requires a checksum key|requires checksum mode v$minimum or higher)/,
                        "$policy rejects Encoder mode $mode");
                }
                else {
                    is($@, '', "$policy accepts Encoder mode v$mode");
                    my $packet = $enc->encode('minimum policy');
                    is(ord(substr($packet, 6, 1)) + 1, $mode, "$policy emits the requested mode");
                    is(Sereal::Decoder->new({ checksum_policy => $policy, checksum_key => $keys[0] })
                        ->decode($packet), 'minimum policy', "$policy interoperates between Encoder and Decoder");
                }
            }
        }
    }
    {
        local $Sereal::ChecksumPolicy = 'require_v3';
        local $Sereal::Decoder::ChecksumPolicy = 'require_v2';
        my $dec = Sereal::Decoder->new({ checksum_key => $keys[1] });
        is($dec->decode($packets[1]), 'checksummed Sereal payload', 'module policy overrides shared minimum');
        is(Sereal::Decoder->new({ checksum_key => $keys[0], checksum_policy => 'require_v1' })
            ->decode($packets[0]), 'checksummed Sereal payload', 'constructor policy overrides module minimum');
        local $Sereal::Decoder::ChecksumPolicy;
        eval { Sereal::Decoder->new({ checksum_key => $keys[1] })->decode($packets[1]); 1 };
        like($@, qr/requires checksum mode v3 or higher/, 'shared minimum applies when module policy is unset');
    }
};

unless ( have_encoder_and_decoder(6.000_003) ) {
    my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
    my @fixtures = (
        [ 0, 'Out damned spot!', 'checksum/v0',            'checksummed Sereal payload' ],
        [ 1, 'Call me Ishmael.', 'checksum/v1',            'checksummed Sereal payload' ],
        [ 2, 'Call me Ishmael.', 'checksum/v2',            'checksummed Sereal payload' ],
    );
    for my $fixture (@fixtures) {
        my ( $type, $key, $name, $expected ) = @$fixture;
        my $packet = read_packet( $corpus, 'decoder-features', $name );
        my $decoder = Sereal::Decoder->new( {
            checksum_policy => 'die',
            ( "checksum_key_v" . ( $type + 1 ) ) => $key,
        } );
        is( $decoder->decode($packet), $expected, "corpus checksum version $type decodes" );
        ok(
            Sereal::Decoder::validate_sereal_checksum( $packet, $key ),
            "corpus checksum version $type validates"
        );
        eval { Sereal::Decoder->new( { checksum_policy => 'die' } )->decode($packet); 1 };
        like( $@, qr/requires a 'checksum_key'/, "checksum version $type requires its key" );
        my $tampered = $packet;
        substr( $tampered, -1, 1 ) = chr( ord( substr( $tampered, -1, 1 ) ) ^ 1 );
        eval { $decoder->decode($tampered); 1 };
        like( $@, qr/checksum verification failed/i, "checksum version $type rejects tampering" );
    }
    my $sip_key = pack( 'C*', 0 .. 15 );
    is(
        unpack( 'H*', Sereal::Decoder::siphash24( '', $sip_key ) ),
        '310e0edd47db6f72',
        'Decoder exposes SipHash-2-4'
    );
    done_testing();
    exit 0;
}
local $Sereal::DefaultKey;
local $Sereal::ChecksumPolicy = 'die';
my $key = pack( 'C*', 0 .. 15 );

# SipHash-2-4/128 reference vectors from
# https://github.com/veorq/SipHash/blob/master/vectors.h (vectors_sip128).
for my $vector (
    [ 0,  'a3817f04ba25a8e66df67214c7550293' ],
    [ 1,  'da87c1d86b99af44347659119b22fc45' ],
    [ 15, '5493e99933b0a8117e08ec0f97cfc3d9' ],
    [ 16, '6ee2a4ca67b054bbfd3315bf85230577' ],
    [ 63, '5150d1772f50834a503e069a973fbd7c' ],
    )
{
    my ( $length, $expected ) = @$vector;
    my $data = pack( 'C*', 0 .. 63 );
    $data = substr( $data, 0, $length );
    for my $module ( 'Sereal::Encoder', 'Sereal::Decoder' ) {
        my $function = $module->can('siphash24_128');
        is(
            unpack( 'H*', $function->( $data, $key ) ), $expected,
            "$module SipHash-2-4/128 reference vector length=$length"
        );
        eval { $function->( $data, 'short' ); 1 };
        like( $@, qr/exactly 16 bytes/, "$module rejects a short primitive key" );
    }
}

my %keys =
    map { ( "checksum_key_v" . ( $_ + 1 ) => pack( 'C*', ( $_ * 16 ) .. ( $_ * 16 + 15 ) ) ) } ( 0, 1, 2 );
for my $version ( 0, 1, 2 ) {
    my $mode = "v" . ( $version + 1 );
    my $packet = Sereal::Encoder->new(
        { checksum_key => $key, checksum_mode => $mode } )->encode('generic checksum options');
    is(
        ord( substr( $packet, 6, 1 ) ), $version,
        "checksum_key plus checksum_mode emits $mode"
    );
    is(
        Sereal::Decoder->new( {
            checksum_key => $key,
            checksum_mode => $mode,
            checksum_policy => 'die',
        } )->decode($packet),
        'generic checksum options',
        "checksum_key plus checksum_mode verifies $mode"
    );
    is(
        Sereal::Decoder::decode_sereal( $packet, {
            checksum_key => $key,
            checksum_mode => $mode,
            checksum_policy => 'die',
        } ),
        'generic checksum options',
        "functional Decoder verifies $mode with generic options"
    );
    is(
        Sereal::Decoder->new( { checksum_key => $key, checksum_policy => 'die' } )->decode($packet),
        'generic checksum options',
        "generic Decoder key verifies $mode without a mode restriction"
    );
    my $numeric_packet = Sereal::Encoder->new( {
        checksum_key => $key,
        checksum_mode => $version + 1,
    } )->encode('numeric checksum mode');
    is(
        ord( substr( $numeric_packet, 6, 1 ) ), $version,
        "numeric checksum mode " . ( $version + 1 ) . " selects wire type $version"
    );
}
{
    local $SIG{__WARN__} = sub {}; # A mode duplicate may warn before the key conflict.
    my $ok = eval { Sereal::Encoder->new( {
        checksum_key => $key,
        checksum_mode => 'v1',
        checksum_key_v3 => 'fedcba9876543210',
    } ); 1 };
    ok( !$ok, 'different generic and versioned keys are rejected' );
    like( $@, qr/Different keys were supplied/, 'different checksum keys explain the conflict' );
}
{
    my $ok = eval { Sereal::Decoder->new( {
        checksum_key_v1 => $key,
        checksum_key_v3 => 'fedcba9876543210',
    } ); 1 };
    ok( !$ok, 'different Decoder versioned keys are rejected' );
    like( $@, qr/Different keys were supplied for checksum_key/,
        'different Decoder keys explain the conflict' );
}
{
    my $packet = Sereal::Encoder->new( {
        checksum_key => $key,
        checksum_mode => 'none',
    } )->encode('no checksum');
    is( ord( substr( $packet, 4, 1 ) ) & 128, 0, "checksum_mode none disables checksums" );
    my $numeric_packet = Sereal::Encoder->new( { checksum_mode => 0 } )->encode('no checksum');
    is( ord( substr( $numeric_packet, 4, 1 ) ) & 128, 0, "numeric checksum_mode 0 disables checksums" );
}
{
    local $Sereal::ChecksumMode = 'v1';
    local $Sereal::Encoder::ChecksumMode = undef;
    my $shared_mode_packet = Sereal::Encoder->new( { checksum_key => $key } )->encode(7);
    is( ord( substr( $shared_mode_packet, 6, 1 ) ), 0, 'shared package mode is used when Encoder mode is unset' );

    local $Sereal::Encoder::ChecksumMode = 'v2';
    my $module_mode_packet = Sereal::Encoder->new( { checksum_key => $key } )->encode(7);
    is( ord( substr( $module_mode_packet, 6, 1 ) ), 1, 'Encoder package mode overrides shared mode' );
    my $constructor_mode_packet = Sereal::Encoder->new( {
        checksum_key => $key,
        checksum_mode => 'v3',
    } )->encode(7);
    is( ord( substr( $constructor_mode_packet, 6, 1 ) ), 2, 'constructor mode overrides package modes' );
}
{
    my $packet = Sereal::Encoder->new( { checksum_key => $key, checksum_mode => 'v1' } )->encode(7);
    local $Sereal::ChecksumMode = 'v3';
    is(
        Sereal::Decoder->new( { checksum_key => $key, checksum_policy => 'die' } )->decode($packet),
        7,
        'Encoder mode defaults do not restrict Decoder authentication'
    );
}
{
    my $module_key = 'fedcba9876543210';
    local $Sereal::DefaultKey = $key;
    local $Sereal::Encoder::DefaultKey = $module_key;
    my $packet = Sereal::Encoder->new->encode('package default key');
    local $Sereal::Decoder::DefaultKey = $module_key;
    is(
        Sereal::Decoder->new( { checksum_policy => 'die' } )->decode($packet),
        'package default key',
        'module-specific default keys override the shared default key'
    );
    my $explicit = Sereal::Encoder->new( { checksum_key => $key } )->encode('constructor key');
    is(
        Sereal::Decoder->new( { checksum_key => $key, checksum_policy => 'die' } )->decode($explicit),
        'constructor key',
        'constructor keys override package default keys'
    );
}
for my $version ( 0, 1, 2 ) {
    for my $compress ( 0, 2 ) {
        my $option      = "checksum_key_v" . ( $version + 1 );
        my $version_key = $keys{$option};
        my $encoder     = Sereal::Encoder->new(
            { $option => $version_key, compress => $compress, compress_threshold => 1 } );
        my $body   = 'checksummed payload' x 100;
        my $packet = $encoder->encode( $body, { header => 'authenticated' } );
        my ( $suffix, $offset ) = read_varint_at( $packet, 5 );
        $offset += $suffix;
        is(
            ord( substr( $packet, $offset++, 1 ) ), $version,
            "wire checksum version=$version compression=$compress"
        );
        my ( $tag_len, $tag_start ) = read_varint_at( $packet, $offset );
        is( $tag_len, $version == 0 ? 8 : 16, "version=$version has the specified tag length" );
        my $covered = substr( $packet, 0, $tag_start ) . substr( $packet, $tag_start + $tag_len );
        is(
            substr( $packet, $tag_start, $tag_len ),
            checksum_tag( $version, $covered, $version_key ),
            "version=$version uses its specified SipHash variant"
        );
        my $decoder = Sereal::Decoder->new( { $option => $version_key, checksum_policy => 'die' } );
        is( $decoder->decode($packet), $body, "version=$version decodes with its selected key and mode" );
        ok(
            Sereal::Decoder::validate_sereal_checksum( $packet, $version_key ),
            "version=$version inspection validates"
        );
        is_deeply(
            $decoder->decode_only_header($packet), { header => 'authenticated' },
            "version=$version verifies the user header"
        );

        for my $bad_len ( 0, 7, 8, 15, 16, 17 ) {
            next if $bad_len == $tag_len;
            my $bad = $packet;
            substr( $bad, $tag_start - 1, 1 ) = varint($bad_len);
            for my $policy ( 'die', 'ignore' ) {
                eval {
                    Sereal::Decoder->new( { checksum_policy => $policy, $option => $version_key } )->decode($bad); 1;
                };
                like(
                    $@, qr/Invalid checksum length/,
                    "version=$version $policy rejects length=$bad_len"
                );
            }
        }
        if ( $version != 0 ) {

            # Same tag size does not permit changing 2-4/128 into 3-6/128.
            my $bad = $packet;
            substr( $bad, $offset - 1, 1 ) = chr( $version == 1 ? 2 : 1 );
            eval {
                Sereal::Decoder->new( { checksum_key => $version_key, checksum_policy => 'die' } )
                    ->decode($bad);
                1;
            };
            like(
                $@, qr/checksum verification failed/,
                "version=$version cannot be relabeled as the other 128-bit variant"
            );
        }
    }
}

for my $version ( 0, 1, 2 ) {
    eval {
        Sereal::Encoder->new( { protocol_version => 5, ( "checksum_key_v" . ( $version + 1 ) ) => $key } ); 1;
    };
    like(
        $@, qr/requires Sereal protocol version 6/,
        "version=$version key cannot emit an older protocol"
    );
}
for my $left ( 0, 1, 2 ) {
    for my $right ( 0, 1, 2 ) {
        next if $left >= $right;
        my @warnings;
        local $SIG{__WARN__} = sub { push @warnings, @_ };
        my $encoder = Sereal::Encoder->new(
            { ( "checksum_key_v" . ( $left + 1 ) ) => $key,
              ( "checksum_key_v" . ( $right + 1 ) ) => $key } );
        my $packet = $encoder->encode(7);
        is(
            ord( substr( $packet, 6, 1 ) ), $right,
            "Encoder uses the highest simultaneous mode $right"
        );
        like( join( '', @warnings ), qr/Attempt to set checksum_key more than once/,
            "Encoder warns for simultaneous versions $left and $right" );
    }
}
{
    local $Sereal::DefaultKey = $key;
    my $packet = Sereal::Encoder->new->encode(7);
    is(
        ord( substr( $packet, 6, 1 ) ), 2,
        'shared default retains SipHash-3-6/128 under version 2'
    );
    is( Sereal::Decoder->new->decode($packet), 7, 'shared default verifies version 2' );
    my $plain = Sereal::Encoder->new( { checksum_key_v3 => undef } )->encode(7);
    is(
        ord( substr( $plain, 4, 1 ) ) & 128, 0,
        'explicit undef version-2 key suppresses the default'
    );
}
for my $unknown ( 3, 4, 255 ) {
    my $header = Header(6);
    substr( $header, 4, 1 ) = chr(0x86);
    my $packet = $header . chr($unknown) . varint(16) . ( 'x' x 16 ) . varint(1) . chr(7);
    eval { Sereal::Decoder->new->decode($packet); 1 };
    like(
        $@, qr/Unsupported Sereal checksum type/,
        "unknown/reserved version=$unknown rejected strictly"
    );
    eval { Sereal::Decoder::validate_sereal_checksum( $packet, $key ); 1 };
    like(
        $@, qr/Unsupported Sereal checksum type/,
        "unknown/reserved version=$unknown cannot be validated"
    );
    is(
        Sereal::Decoder->new( { checksum_policy => 'ignore' } )->decode($packet), 7,
        "unknown/reserved version=$unknown can be skipped by length"
    );
}
done_testing();
