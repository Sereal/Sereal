#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;
use lib 't/lib';
use Sereal::TestSet qw(Header short_string varint checksum_tag);

local $Sereal::ChecksumPolicy;
$Sereal::ChecksumPolicy = undef;
local $Sereal::DefaultKey;
my $key = 'Out damned spot!';
my @packets;
for my $type ( 0, 1, 2 ) {
    my $header = Header(6);
    substr( $header, 4, 1 ) = chr(0x86);
    my $prefix = $header . chr($type) . varint( $type ? 16 : 8 );
    my $tail   = varint(1) . "\x07";
    my $tag    = checksum_tag( $type, $prefix . $tail, $key );
    my $good   = $prefix . $tag . $tail;
    my $bad    = $good;
    substr( $bad, length($prefix), 1 ) = chr( ord( substr( $tag, 0, 1 ) ) ^ 1 );
    $packets[$type] = [ $good, $bad ];
}

for my $policy ( 'verify-if-available', 'die', 'ignore', 'ignore-warn' ) {
    $Sereal::ChecksumPolicy = $policy;
    for my $type ( 0, 1, 2 ) {
        for my $has_key ( 0, 1 ) {
            my $key_option = "checksum_key_v" . ( $type + 1 );
            my %options = $has_key ? ( $key_option => $key ) : ();
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
                        $error, qr/requires a 'checksum_key'/,
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
                $@, qr/requires a 'checksum_key'/,
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
        like( $@, qr/requires a 'checksum_key'/, "$policy applies to functional decoding" );
    }
    else {
        is( $@, '', "$policy applies to functional decoding" );
    }
}

for my $environment ( 'die', 'invalid' ) {
    $Sereal::ChecksumPolicy = $environment;
    for my $policy ( 'verify-if-available', 'die', 'ignore', 'ignore-warn' ) {
        my ( @warnings, $out );
        {
            local $SIG{__WARN__} = sub { push @warnings, @_ };
            my $dec =
                Sereal::Decoder->new( { checksum_policy => $policy, checksum_key_v1 => $key } );
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
    $Sereal::ChecksumPolicy = $invalid;
    for ( 1 .. 3 ) {
        eval { Sereal::Decoder->new; 1 };
        like(
            $@, qr/Invalid \$Sereal::ChecksumPolicy/,
            "invalid environment policy '$invalid' fails at construction"
        );
    }
}
for my $environment ( undef, '' ) {
    $Sereal::ChecksumPolicy = $environment;
    $Sereal::ChecksumPolicy = undef unless defined($environment) && length($environment);
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
$Sereal::ChecksumPolicy = 'die';
my $existing = Sereal::Decoder->new;
$Sereal::ChecksumPolicy = 'ignore';
eval { $existing->decode( $packets[0][0] ); 1 };
like( $@, qr/requires a 'checksum_key'/, 'environment changes do not alter existing decoders' );
is(
    Sereal::Decoder->new->decode( $packets[0][0] ), 7,
    'new decoders read the changed environment'
);

for my $source ( 'option', 'environment' ) {
    $Sereal::ChecksumPolicy = $source eq 'environment' ? 'require_key' : 'ignore';
    my %policy = $source eq 'option' ? ( checksum_policy => 'require_key' ) : ();
    for my $keys (
        { checksum_key_v1 => $key }, { checksum_key_v2 => $key }, { checksum_key_v3 => $key },
        { checksum_key_v1 => $key, checksum_key_v2 => $key },
        { checksum_key_v1 => $key, checksum_key_v3 => $key },
        { checksum_key_v2 => $key, checksum_key_v3 => $key },
        { checksum_key_v1 => $key, checksum_key_v2 => $key, checksum_key_v3 => $key } )
    {
        my %options = ( %policy, %$keys );
        my $selected_type = exists $keys->{checksum_key_v3} ? 2
                          : exists $keys->{checksum_key_v2} ? 1 : 0;
        my $key_count = scalar grep { exists $keys->{$_} }
            qw(checksum_key_v1 checksum_key_v2 checksum_key_v3);
        my @constructor_warnings;
        my $dec;
        {
            local $SIG{__WARN__} = sub { push @constructor_warnings, @_ };
            $dec = Sereal::Decoder->new( \%options );
        }
        if ($key_count > 1) {
            like(join('', @constructor_warnings), qr/Attempt to set checksum_key more than once/,
                "$source warns when versioned aliases assign the key more than once");
        }
        else {
            is_deeply(\@constructor_warnings, [], "$source accepts one versioned key without warning");
        }
        for my $type ( 0, 1, 2 ) {
            for my $tampered ( 0, 1 ) {
                my ( $out, @warnings );
                {
                    local $SIG{__WARN__} = sub { push @warnings, @_ };
                    eval { $out = $dec->decode( $packets[$type][$tampered] ); 1 };
                }
                my $error = $@;
                if ($tampered) {
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
            eval { $dec->decode( Header($version) . "\x07" ); 1 };
            like(
                $@, qr/require_key.*checksummed packet/,
                "$source require_key rejects unsigned v$version packets"
            );
        }
        my $unknown = $packets[0][0];
        substr( $unknown, length( Header(6) ), 1 ) = chr(3);
        eval { $dec->decode($unknown); 1 };
        like(
            $@, qr/Unsupported Sereal checksum type/,
            "$source require rejects unknown checksum types"
        );
        my %functional_options = (
            %policy,
            checksum_key  => $key,
            checksum_mode => 'v' . ( $selected_type + 1 ),
        );
        is(
            Sereal::Decoder::decode_sereal( $packets[$selected_type][0], \%functional_options ),
            7,
            "$source require works through the functional interface"
        );
    }
}
$Sereal::ChecksumPolicy = 'require_key';
eval { Sereal::Decoder->new; 1 };
like(
    $@, qr/checksum_policy 'require_key'.*checksum key/,
    'environment require_key rejects keyless decoder construction'
);
eval { Sereal::Decoder::decode_sereal( Header(6) . "\x07" ); 1 };
like(
    $@, qr/checksum_policy 'require_key'.*checksum key/,
    'environment require_key rejects unsigned input without a configured key'
);
eval { Sereal::Decoder::decode_sereal( $packets[0][0] ); 1 };
like(
    $@, qr/checksum_policy 'require_key'.*checksum key/,
    'environment require_key rejects authenticated packets without a configured key'
);
is(
    Sereal::Decoder->new( { checksum_policy => 'die' } )->decode( Header(6) . "\x07" ), 7,
    'explicit die overrides environment require without a key'
);
$Sereal::ChecksumPolicy = 'invalid';
is(
    Sereal::Decoder->new( { checksum_policy => 'require_key', checksum_key_v1 => $key } )
        ->decode( $packets[0][0] ), 7,
    'explicit require with a key overrides an invalid environment default'
);

{

    package ReenterChecksumEnv;
    our ( $decoder, $packet, $error );

    sub THAW {
        local $Sereal::ChecksumPolicy = 'ignore';
        eval { $decoder->decode($packet); 1 };
        $error = $@;
        return 7;
    }
}

# A THAW callback can change the environment and reenter the same decoder.
# Its internal clone must inherit the original policy, not the new default.
$Sereal::ChecksumPolicy = 'die';
local $ReenterChecksumEnv::decoder = Sereal::Decoder->new( { checksum_key_v1 => $key } );
local $ReenterChecksumEnv::packet  = $packets[1][1];
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
    $ReenterChecksumEnv::error, qr/checksum verification failed/,
    'recursive decoder clone retains the original policy despite environment changes'
);

$ReenterChecksumEnv::decoder = Sereal::Decoder->new( {
    checksum_policy => 'require_key', checksum_key_v1 => $key,
} );
$ReenterChecksumEnv::packet = Header(6) . "\x07";
is(
    $ReenterChecksumEnv::decoder->decode($outer), 7,
    'require_key authenticates the outer packet before invoking THAW'
);
like(
    $ReenterChecksumEnv::error, qr/require_key.*checksummed packet/,
    'recursive decoder clone still rejects unsigned input after the environment changes'
);

{

    package UnsignedHeaderCallback;
    our $called = 0;
    sub THAW { $called++; return 7 }
}
my $unsigned_header = Header(
    6,
    "\x32" . short_string('UnsignedHeaderCallback') . "\x40"
) . "\x07";
eval { $ReenterChecksumEnv::decoder->decode_only_header($unsigned_header); 1 };
like(
    $@, qr/require_key.*checksummed packet/,
    'require_key rejects unsigned metadata before decoding it'
);
is( $UnsignedHeaderCallback::called, 0, 'unsigned metadata never invokes THAW' );

done_testing;
