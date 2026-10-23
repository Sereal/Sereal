#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;
use lib 't/lib';
use Sereal::TestSet qw(Header short_string varint);

local $ENV{SEREAL_CHECKSUM_POLICY};
delete $ENV{SEREAL_CHECKSUM_POLICY};
local $Sereal::DefaultKey;
my $key = 'Out damned spot!';
my @packets;
for my $type ( 0, 1 ) {
    my $header = Header(6);
    substr( $header, 4, 1 ) = chr(0x86);
    my $prefix = $header . chr($type) . varint( $type ? 16 : 8 );
    my $tail   = varint(1) . "\x07";
    my $tag =
        $type
        ? Sereal::Decoder::siphash36( $prefix . $tail, $key )
        : Sereal::Decoder::siphash24( $prefix . $tail, $key );
    my $good = $prefix . $tag . $tail;
    my $bad  = $good;
    substr( $bad, length($prefix), 1 ) = chr( ord( substr( $tag, 0, 1 ) ) ^ 1 );
    push @packets, [ $good, $bad ];
}

for my $policy ( 'verify-if-available', 'die', 'ignore', 'ignore-warn' ) {
    $ENV{SEREAL_CHECKSUM_POLICY} = $policy;
    for my $type ( 0, 1 ) {
        for my $has_key ( 0, 1 ) {
            my %options = $has_key ? ( "checksum_v${type}_key" => $key ) : ();
            my $dec     = Sereal::Decoder->new( \%options );
            for my $tampered ( 0, 1 ) {
                my ( @warnings, $out, $error );
                {
                    local $SIG{__WARN__} = sub { push @warnings, @_ };
                    eval { $out = $dec->decode( $packets[$type][$tampered] ); 1 };
                    $error = $@;
                }
                my $label  = "$policy type=$type key=$has_key tampered=$tampered";
                my $verify = $policy eq 'die' || $policy eq 'verify-if-available';
                if ( $policy eq 'die' && !$has_key ) {
                    like(
                        $error, qr/requires a 'checksum_v${type}_key'/,
                        "$label rejects missing key"
                    );
                }
                elsif ( $verify && $has_key && $tampered ) {
                    like(
                        $error, qr/checksum verification failed/,
                        "$label rejects invalid authentication"
                    );
                }
                else {
                    is( $error, '', "$label decodes successfully" );
                    is( $out,   7,  "$label decodes the value" );
                }
                my $warn = $policy eq 'ignore-warn'
                    || ( $policy eq 'verify-if-available' && !$has_key );
                is( scalar @warnings, $warn ? 1 : 0, "$label expected warning count" );
            }
            is(
                $dec->decode( Header(6) . "\x07" ), 7,
                "$policy retains acceptance of packets without checksums"
            );
        }
    }

    # Cover constructors without options and the functional entry point too.
    for my $options ( undef, {}, { checksum_policy => undef } ) {
        my $dec = defined($options) ? Sereal::Decoder->new($options) : Sereal::Decoder->new;
        local $SIG{__WARN__} = sub { };
        eval { $dec->decode( $packets[0][0] ); 1 };
        if ( $policy eq 'die' ) {
            like(
                $@, qr/requires a 'checksum_v0_key'/,
                "$policy applies without an explicit policy"
            );
        }
        else {
            is( $@, '', "$policy applies without an explicit policy" );
        }
    }
    local $SIG{__WARN__} = sub { };
    eval { Sereal::Decoder::decode_sereal( $packets[0][0] ); 1 };
    if ( $policy eq 'die' ) {
        like( $@, qr/requires a 'checksum_v0_key'/, "$policy applies to functional decoding" );
    }
    else {
        is( $@, '', "$policy applies to functional decoding" );
    }
}

for my $environment ( 'die', 'invalid' ) {
    $ENV{SEREAL_CHECKSUM_POLICY} = $environment;
    for my $policy ( 'verify-if-available', 'die', 'ignore', 'ignore-warn' ) {
        my ( @warnings, $out );
        {
            local $SIG{__WARN__} = sub { push @warnings, @_ };
            my $dec =
                Sereal::Decoder->new( { checksum_policy => $policy, checksum_v0_key => $key } );
            $out = $dec->decode( $packets[0][0] );
        }
        is( $out, 7, "explicit $policy overrides $environment environment default" );
        is(
            scalar @warnings, $policy eq 'ignore-warn' ? 1 : 0,
            "explicit $policy retains its warning behavior"
        );
    }
}
for my $invalid ( 'invalid', 'DIE', ' die', 'die ', '0' ) {
    $ENV{SEREAL_CHECKSUM_POLICY} = $invalid;
    for ( 1 .. 3 ) {
        eval { Sereal::Decoder->new; 1 };
        like(
            $@, qr/Invalid SEREAL_CHECKSUM_POLICY/,
            "invalid environment policy '$invalid' fails at construction"
        );
    }
}
for my $environment ( undef, '' ) {
    $ENV{SEREAL_CHECKSUM_POLICY} = $environment;
    delete $ENV{SEREAL_CHECKSUM_POLICY} unless defined $environment;
    my @warnings;
    {
        local $SIG{__WARN__} = sub { push @warnings, @_ };
        is(
            Sereal::Decoder->new->decode( $packets[0][0] ), 7,
            'unset or empty environment retains the original default'
        );
    }
    like(
        join( '', @warnings ), qr/without authentication/,
        'original default warns about missing key'
    );
}
$ENV{SEREAL_CHECKSUM_POLICY} = 'die';
my $existing = Sereal::Decoder->new;
$ENV{SEREAL_CHECKSUM_POLICY} = 'ignore';
eval { $existing->decode( $packets[0][0] ); 1 };
like( $@, qr/requires a 'checksum_v0_key'/, 'environment changes do not alter existing decoders' );
is(
    Sereal::Decoder->new->decode( $packets[0][0] ), 7,
    'new decoders read the changed environment'
);

for my $source ( 'option', 'environment' ) {
    $ENV{SEREAL_CHECKSUM_POLICY} = $source eq 'environment' ? 'require' : 'ignore';
    my %policy = $source eq 'option' ? ( checksum_policy => 'require' ) : ();
    for my $keys (
        { checksum_v0_key => $key }, { checksum_v1_key => $key },
        { checksum_v0_key => $key, checksum_v1_key => $key } )
    {
        my %options = ( %policy, %$keys );
        my $dec     = Sereal::Decoder->new( \%options );
        for my $type ( 0, 1 ) {
            for my $tampered ( 0, 1 ) {
                my ( $out, @warnings );
                {
                    local $SIG{__WARN__} = sub { push @warnings, @_ };
                    eval { $out = $dec->decode( $packets[$type][$tampered] ); 1 };
                }
                my $error = $@;
                if ( !exists $keys->{"checksum_v${type}_key"} ) {
                    like(
                        $error, qr/requires a 'checksum_v${type}_key'/,
                        "$source require rejects packets needing the other key"
                    );
                }
                elsif ($tampered) {
                    like(
                        $error, qr/checksum verification failed/,
                        "$source require rejects tampering"
                    );
                }
                else {
                    is( $error, '', "$source require verifies a packet with a matching key" );
                    is( $out,   7,  "$source require returns the verified value" );
                }
                is_deeply(
                    \@warnings, [],
                    "$source require never silently falls back to a warning"
                );
            }
        }
        for my $version ( 1 .. 6 ) {
            is(
                $dec->decode( Header($version) . "\x07" ), 7,
                "$source require retains die semantics for unsigned v$version packets"
            );
        }
        my $unknown = $packets[0][0];
        substr( $unknown, length( Header(6) ), 1 ) = chr(2);
        eval { $dec->decode($unknown); 1 };
        like(
            $@, qr/Unsupported Sereal checksum type/,
            "$source require rejects unknown checksum types"
        );
        is(
            Sereal::Decoder::decode_sereal( $packets[0][0], { %options, checksum_v0_key => $key } ),
            7,
            "$source require works through the functional interface"
        );
    }
}
$ENV{SEREAL_CHECKSUM_POLICY} = 'require';
is(
    Sereal::Decoder->new->decode( Header(6) . "\x07" ), 7,
    'environment require keeps die semantics for unsigned packets without a key'
);
eval { Sereal::Decoder::decode_sereal( $packets[0][0] ); 1 };
like(
    $@, qr/requires a 'checksum_v0_key'/,
    'environment require rejects authenticated packets without a matching key'
);
is(
    Sereal::Decoder->new( { checksum_policy => 'die' } )->decode( Header(6) . "\x07" ), 7,
    'explicit die overrides environment require without a key'
);
$ENV{SEREAL_CHECKSUM_POLICY} = 'invalid';
is(
    Sereal::Decoder->new( { checksum_policy => 'require', checksum_v0_key => $key } )
        ->decode( $packets[0][0] ), 7,
    'explicit require with a key overrides an invalid environment default'
);

{

    package ReenterChecksumEnv;
    our ( $decoder, $packet, $error );

    sub THAW {
        local $ENV{SEREAL_CHECKSUM_POLICY} = 'ignore';
        eval { $decoder->decode($packet); 1 };
        $error = $@;
        return 7;
    }
}

# A THAW callback can change the environment and reenter the same decoder.
# Its internal clone must inherit the original policy, not the new default.
$ENV{SEREAL_CHECKSUM_POLICY} = 'die';
local $ReenterChecksumEnv::decoder = Sereal::Decoder->new( { checksum_v0_key => $key } );
local $ReenterChecksumEnv::packet  = $packets[1][0];
my $payload = "\x32" . short_string('ReenterChecksumEnv') . "\x40";
my $prefix  = Header(6);
substr( $prefix, 4, 1 ) = chr(0x86);
$prefix .= "\0" . varint(8);
my $tail  = varint( length($payload) ) . $payload;
my $outer = $prefix . Sereal::Decoder::siphash24( $prefix . $tail, $key ) . $tail;
is(
    $ReenterChecksumEnv::decoder->decode($outer), 7,
    'outer THAW completes after nested authentication error'
);
like(
    $ReenterChecksumEnv::error, qr/requires a 'checksum_v1_key'/,
    'recursive decoder clone retains the original policy despite environment changes'
);

done_testing;
