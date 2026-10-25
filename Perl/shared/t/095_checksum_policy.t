#!perl
use strict;
use warnings;
no warnings 'once';
use lib 't/lib';
use File::Spec;
use Sereal::TestSet qw(Header have_encoder_and_decoder);
use Sereal::TestSet::Corpus qw(read_packet);
use Test::More;
use IPC::Open3 qw(open3);
use Symbol     qw(gensym);
use Sereal::Decoder;

my $misspelled_key = pack( 'C*', 0 .. 15 );
sub capture_warnings {
    my ($code) = @_;
    my @warnings;
    my $result;
    {
        local $SIG{__WARN__} = sub { push @warnings, @_ };
        $result = $code->();
    }
    return ( $result, \@warnings );
}

{
    local $ENV{SEREAL_UNKNOWN_OPTION_POLICY};
    local $Sereal::UnknownOptionPolicy;
    local $Sereal::Decoder::UnknownOptionPolicy;
    delete $ENV{SEREAL_UNKNOWN_OPTION_POLICY};
    $Sereal::UnknownOptionPolicy = undef;
    $Sereal::Decoder::UnknownOptionPolicy = undef;

    my ( $decoder, $warnings ) = capture_warnings(
        sub { Sereal::Decoder->new( { checksum_key_v2_ky => $misspelled_key } ) } );
    ok( $decoder, 'default unknown option policy keeps constructing the Decoder' );
    like( join( '', @$warnings ), qr/Unknown option keys passed to Sereal::Decoder: .*checksum_key_v2_ky/,
        'default policy warns about a misspelled checksum key' );
    is( scalar(@$warnings), 1, 'one warning summarizes the unknown keys' );

    ( $decoder, $warnings ) = capture_warnings(
        sub {
            Sereal::Decoder->new( {
                checksum_key_v2_ky => $misspelled_key,
                checksum_key_v3_typo => $misspelled_key,
            } );
        } );
    ok( $decoder, 'multiple unknown options still permit construction under warn policy' );
    is( scalar(@$warnings), 1, 'multiple unknown options produce one warning' );
    like( join( '', @$warnings ), qr/checksum_key_v2_ky/, 'combined warning names the first unknown option' );
    like( join( '', @$warnings ), qr/checksum_key_v3_typo/, 'combined warning names the second unknown option' );

    eval {
        Sereal::Decoder->new( {
            checksum_key_v2_ky => $misspelled_key,
            checksum_key_v3_typo => $misspelled_key,
            unknown_option_policy => 'die',
        } );
        1;
    };
    like( $@, qr/Unknown option keys passed to Sereal::Decoder: (?=.*checksum_key_v2_ky)(?=.*checksum_key_v3_typo)/s,
        "constructor 'die' policy reports all unknown keys together" );

    ( my $decoded, $warnings ) = capture_warnings(
        sub {
            Sereal::Decoder::decode_sereal(
                Header(6) . "\x07", { checksum_key_v2_ky => $misspelled_key } );
        } );
    is( $decoded, 7, 'functional Decoder continues with an unknown key under the default policy' );
    like( join( '', @$warnings ), qr/Unknown option keys passed to Sereal::Decoder: .*checksum_key_v2_ky/,
        'functional Decoder warns about a misspelled checksum key' );

    eval { Sereal::Decoder->new( { checksum_key_v2_ky => $misspelled_key, unknown_option_policy => 'die' } ); 1 };
    like( $@, qr/Unknown option keys passed to Sereal::Decoder: .*checksum_key_v2_ky/,
        "constructor 'die' policy rejects an unknown key" );

    ( $decoder, $warnings ) = capture_warnings(
        sub {
            Sereal::Decoder->new( {
                checksum_key_v2_ky => $misspelled_key,
                unknown_option_policy => 'ignore',
            } );
        } );
    ok( $decoder && !@$warnings, "constructor 'ignore' policy silently accepts an unknown key" );

    local $Sereal::UnknownOptionPolicy = 'ignore';
    ( $decoder, $warnings ) = capture_warnings(
        sub { Sereal::Decoder->new( { checksum_key_v2_ky => $misspelled_key } ) } );
    ok( $decoder && !@$warnings, 'shared package policy is applied' );

    local $Sereal::UnknownOptionPolicy = 'die';
    local $Sereal::Decoder::UnknownOptionPolicy = 'ignore';
    ( $decoder, $warnings ) = capture_warnings(
        sub { Sereal::Decoder->new( { checksum_key_v2_ky => $misspelled_key } ) } );
    ok( $decoder && !@$warnings, 'module policy takes precedence over the shared package policy' );

    local $Sereal::UnknownOptionPolicy = 'ignore';
    local $Sereal::Decoder::UnknownOptionPolicy = 'die';
    ( $decoder, $warnings ) = capture_warnings(
        sub {
            Sereal::Decoder->new( {
                checksum_key_v2_ky => $misspelled_key,
                unknown_option_policy => 'warn',
            } );
        } );
    ok( $decoder, 'constructor policy overrides the package policy' );
    like( join( '', @$warnings ), qr/Unknown option keys passed to Sereal::Decoder: .*checksum_key_v2_ky/,
        'constructor policy takes precedence over module, shared package, and environment policies' );

    local $Sereal::UnknownOptionPolicy = undef;
    local $Sereal::Decoder::UnknownOptionPolicy = undef;
    ( $decoder, $warnings ) = capture_warnings(
        sub { Sereal::Decoder->new( { checksum_key_v2_ky => $misspelled_key } ) } );
    ok( $decoder, 'default warn policy applies when no constructor or package policy is set' );
    like( join( '', @$warnings ), qr/Unknown option keys passed to Sereal::Decoder: .*checksum_key_v2_ky/,
        'default policy warns when no constructor or package policy is set' );

    local $Sereal::UnknownOptionPolicy = 'invalid';
    eval { Sereal::Decoder->new; 1 };
    like( $@, qr/Invalid \$Sereal::UnknownOptionPolicy/, 'invalid shared package policy is rejected' );
}

unless ( have_encoder_and_decoder(6.000_003) ) {
    my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
    my @fixtures = (
        [ 0, 'Out damned spot!', 'checksum/v0',              'checksummed Sereal payload' ],
        [ 1, 'Call me Ishmael.', 'checksum/v1-compressed',   'checksummed Sereal payload' x 100 ],
        [ 1, 'Call me Ishmael.', 'checksum/v1',              'checksummed Sereal payload' ],
        [ 2, 'Call me Ishmael.', 'checksum/v2',              'checksummed Sereal payload' ],
    );
    for my $fixture (@fixtures) {
        my ( $type, $key, $name, $expected ) = @$fixture;
        my $packet = read_packet( $corpus, 'decoder-features', $name );
        my $decoder = Sereal::Decoder->new( {
            checksum_policy => 'require_key',
            ( "checksum_key_v" . ( $type + 1 ) ) => $key,
        } );
        is( $decoder->decode($packet), $expected, "corpus checksum type $type verifies and decodes" );
        ok(
            Sereal::Decoder::validate_sereal_checksum( $packet, $key ),
            "corpus checksum type $type validates"
        );
        substr( $packet, -1, 1 ) = chr( ord( substr( $packet, -1, 1 ) ) ^ 1 );
        eval { $decoder->decode($packet); 1 };
        like( $@, qr/checksum verification failed/i, "corpus checksum type $type rejects tampering" );
    }
    eval {
        Sereal::Decoder->new( { checksum_policy => 'require_key' } )->decode( Header(6) . "\x07" );
        1;
    };
    like( $@, qr/requires a checksum key/, 'require_key rejects keyless construction' );
    done_testing();
    exit 0;
}

for my $case (
    [
        'constructor',
        sub { Sereal::Encoder->new( { checksum_key_v2_ky => $misspelled_key } ) },
    ],
    [
        'functional encoder',
        sub { Sereal::Encoder::encode_sereal( 7, { checksum_key_v2_ky => $misspelled_key } ) },
    ],
    )
{
    my ( $name, $build ) = @$case;
    my ( $encoder, $warnings ) = capture_warnings($build);
    ok( $encoder, "$name continues with a misspelled checksum key by default" );
    like( join( '', @$warnings ), qr/Unknown option keys passed to Sereal::Encoder: .*checksum_key_v2_ky/,
        "$name warns about a misspelled checksum key" );
}
{
    local $ENV{SEREAL_UNKNOWN_OPTION_POLICY} = 'ignore';
    local $Sereal::UnknownOptionPolicy = 'warn';
    local $Sereal::Encoder::UnknownOptionPolicy = 'die';
    eval {
        Sereal::Encoder->new( {
            checksum_key_v2_ky => $misspelled_key,
            checksum_key_v3_typo => $misspelled_key,
        } );
        1;
    };
    like( $@, qr/Unknown option keys passed to Sereal::Encoder: (?=.*checksum_key_v2_ky)(?=.*checksum_key_v3_typo)/s,
        'Encoder package policy overrides shared package and environment policies' );
    my ( $encoder, $warnings ) = capture_warnings(
        sub {
            Sereal::Encoder->new( {
                checksum_key_v2_ky => $misspelled_key,
                unknown_option_policy => 'ignore',
            } );
        } );
    ok( $encoder && !@$warnings, 'Encoder constructor policy overrides its package policy' );
    my ( $many_encoder, $many_warnings ) = capture_warnings(
        sub {
            Sereal::Encoder->new( {
                checksum_key_v2_ky => $misspelled_key,
                checksum_key_v3_typo => $misspelled_key,
                unknown_option_policy => 'warn',
            } );
        } );
    ok( $many_encoder, 'Encoder constructs with multiple unknown options under warn policy' );
    is( scalar(@$many_warnings), 1, 'Encoder combines multiple unknown options into one warning' );
    like( join( '', @$many_warnings ), qr/(?=.*checksum_key_v2_ky)(?=.*checksum_key_v3_typo)/s,
        'Encoder warning includes every unknown key' );
}
eval { Sereal::Encoder->new( { unknown_option_policy => 'bad' } ); 1 };
like( $@, qr/Invalid unknown_option_policy/, 'Encoder rejects an invalid constructor policy' );
ok( Sereal::Encoder->new( { compress_threshold => 1 } ), 'recognized conditionally used Encoder option is accepted' );
ok( Sereal::Decoder->new( { thaw_allow_classes => [] } ), 'recognized conditionally used Decoder option is accepted' );

local $Sereal::ChecksumPolicy;
local $Sereal::Encoder::ChecksumPolicy;
local $Sereal::Decoder::ChecksumPolicy;
$Sereal::ChecksumPolicy = undef;
$Sereal::Encoder::ChecksumPolicy = undef;
$Sereal::Decoder::ChecksumPolicy = undef;
local $Sereal::DefaultKey;
my $key       = pack( 'C*', 0 .. 15 );
my $other     = pack( 'C*', 16 .. 31 );
my $encode_op = eval 'sub { Sereal::Encoder::sereal_encode_with_object($_[0], 7) }';
die $@ unless $encode_op;
my $decode_op = eval 'sub { Sereal::Decoder::sereal_decode_with_object($_[0], $_[1]) }';
die $@ unless $decode_op;

for my $source ( 'option', 'package default' ) {
    $Sereal::ChecksumPolicy = $source eq 'package default' ? 'require_key' : 'ignore';
    my %policy = $source eq 'option' ? ( checksum_policy => 'require_key' ) : ();
    $Sereal::ChecksumPolicy = 'ignore' if $source eq 'option';
    for my $class ( 'Sereal::Encoder', 'Sereal::Decoder' ) {
        eval { $class->new( \%policy ); 1 };
        like(
            $@, qr/checksum_policy 'require_key'.*checksum key/,
            "$source require_key rejects keyless $class construction"
        );
    }
    eval { Sereal::Encoder::encode_sereal( 7, \%policy ); 1 };
    like(
        $@, qr/checksum_policy 'require_key'.*checksum key/,
        "$source require_key rejects keyless functional encoding"
    );
    eval { Sereal::Decoder::decode_sereal( Header(6) . "\x07", \%policy ); 1 };
    like(
        $@, qr/checksum_policy 'require_key'.*checksum key/,
        "$source require_key rejects keyless functional decoding of unsigned input"
    );
    for my $type ( 0, 1, 2 ) {
        my %options = ( %policy, ( "checksum_key_v" . ( $type + 1 ) ) => $key );
        my $packet  = Sereal::Encoder->new( \%options )->encode( [ 7, 'authenticated' ] );
        my $dec     = Sereal::Decoder->new( \%options );
        is(
            $decode_op->( $dec, $encode_op->( Sereal::Encoder->new( \%options ) ) ), 7,
            "$source shared authentication works through encode/decode opcodes"
        );
        is_deeply(
            $dec->decode($packet), [ 7, 'authenticated' ],
            "$source require authenticates type $type"
        );
        substr( $packet, -1, 1 ) = chr( ord( substr( $packet, -1 ) ) ^ 1 );
        eval { $dec->decode($packet); 1 };
        like( $@, qr/checksum verification failed/, "$source require rejects tampering" );
        eval { $dec->decode( Header(6) . "\x07" ); 1 };
        like(
            $@, qr/require_key.*checksummed packet/,
            "$source require_key rejects unsigned input with a key configured"
        );
    }
}
for my $source ( 'option', 'package default' ) {
    local $Sereal::ChecksumPolicy = $source eq 'package default' ? 'require' : 'ignore';
    my %policy = $source eq 'option' ? ( checksum_policy => 'require' ) : ();
    eval { Sereal::Encoder->new( \%policy )->encode(7); 1 };
    like(
        $@, qr/checksum_policy 'require_key'.*checksum key/,
        "$source legacy require retains require_key semantics"
    );
    my %options = ( %policy, checksum_key_v2 => $key );
    my $decoder = Sereal::Decoder->new( \%options );
    eval { $decode_op->( $decoder, Header(6) . "\x07" ); 1 };
    like(
        $@, qr/require_key.*checksummed packet/,
        "$source require alias rejects unsigned input through the decoder opcode"
    );
    my $packet = Sereal::Encoder->new( \%options )->encode(7);
    is(
        Sereal::Decoder->new( \%options )->decode($packet), 7,
        "$source legacy require authenticates like require_key"
    );
}
{
    local $Sereal::ChecksumPolicy = 'ignore';
    local $Sereal::Encoder::ChecksumPolicy = 'require_key';
    local $Sereal::Decoder::ChecksumPolicy = 'require_key';
    for my $class ( 'Sereal::Encoder', 'Sereal::Decoder' ) {
        eval { $class->new; 1 };
        like( $@, qr/checksum_policy 'require_key'.*checksum key/,
            "$class package policy overrides the shared checksum policy" );
        ok( $class->new( { checksum_policy => 'ignore' } ),
            "$class constructor checksum policy overrides package defaults" );
    }
}
for my $class ( 'Sereal::Encoder', 'Sereal::Decoder' ) {
    for my $policy ( 'require_key', 'require' ) {
        local $Sereal::ChecksumPolicy = $policy;
        local $Sereal::Encoder::ChecksumPolicy = undef;
        local $Sereal::Decoder::ChecksumPolicy = undef;
        local $Sereal::DefaultKey;
        for my $options ( undef, {}, { checksum_policy => undef } ) {
            eval { defined($options) ? $class->new($options) : $class->new; 1 };
            like(
                $@, qr/checksum_policy 'require_key'.*checksum key/,
                "$class $policy rejects a missing key through the package default"
            );
        }
        ok(
            $class->new( { checksum_policy => 'die' } ),
            "$class explicit die permits a keyless configuration"
        );
        {
            local $Sereal::DefaultKey = $key;
            ok( $class->new, "$class $policy accepts the shared default key" );
            for my $type ( 0, 1, 2 ) {
                eval { $class->new( { ( "checksum_key_v" . ( $type + 1 ) ) => undef } ); 1 };
                like(
                    $@, qr/checksum_policy 'require_key'.*checksum key/,
                    "$class $policy explicit undef type $type suppresses the default and fails"
                );
            }
        }
    }
}
for my $class ( 'Sereal::Encoder', 'Sereal::Decoder' ) {
    for my $policy ( 'die', 'require_key', 'require' ) {
        for my $type ( 0, 1, 2 ) {
            for my $length ( 0, 1, 15, 17, 32 ) {
                eval {
                    $class->new(
                        { checksum_policy => $policy, ( "checksum_key_v" . ( $type + 1 ) ) => 'x' x $length } );
                    1;
                };
                like(
                    $@, qr/exactly 16 bytes/,
                    "$class $policy rejects $length-byte type $type key"
                );
            }
        }
        local $Sereal::DefaultKey = 'short';
        eval { $class->new( { checksum_policy => $policy } ); 1 };
        like( $@, qr/exactly 16 bytes/, "$class $policy validates the shared default key too" );
    }
    for my $policy ( 'verify-if-available', 'ignore', 'ignore-warn' ) {
        my @warnings;
        local $SIG{__WARN__} = sub { push @warnings, @_ };
        ok(
            $class->new( { checksum_policy => $policy, checksum_key_v1 => 'short' } ),
            "$class $policy retains explicit-key normalization"
        );
        like(
            join( '', @warnings ), qr/padding with nulls/,
            "$class $policy warns about normalization"
        );
    }
}
{
    local $Sereal::DefaultKey = $key;
    my $enc    = Sereal::Encoder->new( { checksum_policy => 'require_key' } );
    my $dec    = Sereal::Decoder->new( { checksum_policy => 'require_key' } );
    my $packet = $enc->encode('default');
    is( ord( substr( $packet, 6, 1 ) ), 2, 'shared default selects SipHash-3-6-128 for encoding' );
    is( $dec->decode($packet),          'default', 'both modules use the shared default' );
    for my $type ( 0, 1, 2 ) {
        my $explicit = Sereal::Encoder->new( { ( "checksum_key_v" . ( $type + 1 ) ) => $other } )->encode(7);
        is(
            Sereal::Decoder->new( { ( "checksum_key_v" . ( $type + 1 ) ) => $other } )->decode($explicit), 7,
            "explicit type $type key overrides the shared default"
        );
        eval { $dec->decode($explicit); 1 };
        like(
            $@, qr/checksum verification failed/,
            'default key does not replace an explicit key'
        );
        my $same = Sereal::Encoder->new( { ( "checksum_key_v" . ( $type + 1 ) ) => $key } )->encode(7);
        is( $dec->decode($same), 7, "default decoder key verifies type $type" );
    }
    {
        local $Sereal::DefaultKey = $other;
        my $inner = Sereal::Encoder->new( { checksum_policy => 'require_key' } )->encode('inner');
        is(
            Sereal::Decoder->new( { checksum_policy => 'require_key' } )->decode($inner), 'inner',
            'localized default applies to new objects'
        );
        eval { $dec->decode($inner); 1 };
        like( $@, qr/checksum verification failed/, 'existing decoder retains its original key' );
        is(
            $dec->decode( $enc->encode('old') ), 'old',
            'existing encoder retains its original key'
        );
    }
    is(
        Sereal::Decoder->new->decode($packet), 'default',
        'leaving localization restores the default'
    );
    my $disabled = Sereal::Encoder->new( { checksum_policy => 'die', checksum_key_v2 => undef } );
    is(
        ord( substr( $disabled->encode(7), 4, 1 ) ) & 128, 0,
        'explicit undef suppresses the shared default'
    );
    eval { Sereal::Encoder->new( { protocol_version => 5 } ); 1 };
    like(
        $@, qr/requires Sereal protocol version 6/,
        'default key cannot silently emit an older protocol'
    );
}

my $child_code = <<'CHILD';
use strict;
use warnings;
my ($first, $second) = @ARGV;
my $input = $ENV{SEREAL_DEFAULT_KEY};
my @warnings;
{
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    (my $file = $first) =~ s{::}{/}g;
    require "$file.pm";
}
if (defined $input) {
    my $expected;
    if ($input =~ /\A[0-9a-fA-F]{32}\z/) {
        $expected = pack('H*', $input);
        die "unexpected warning" if @warnings;
    } else {
        no strict 'refs';
        $expected = &{"${first}::prepare_checksum_key"}($input);
        die "missing warning" unless @warnings == 1 && $warnings[0] =~ /prepare_checksum_key/;
        die "warning exposes key" if length($input) && index($warnings[0], $input) >= 0;
    }
    die "incorrect default" unless defined($Sereal::DefaultKey) && $Sereal::DefaultKey eq $expected;
} else {
    die "unexpected default" if defined $Sereal::DefaultKey || @warnings;
}
$Sereal::DefaultKey = 'replacement key!';
$ENV{SEREAL_DEFAULT_KEY} = 'ff' x 16;
(my $file = $second) =~ s{::}{/}g;
require "$file.pm";
die "second module overwrote default" unless $Sereal::DefaultKey eq 'replacement key!';
print "ok\n";
CHILD
local $ENV{SEREAL_DEFAULT_KEY};
for my $input (
    undef, '0123456789abcdef' x 2, '0123456789ABCDEF' x 2,
    '', '1234', 'ab' x 17, 'z' x 32, 'a' x 32 . "\n", 'a passphrase'
    )
{
    if ( defined $input ) { $ENV{SEREAL_DEFAULT_KEY} = $input }
    else                  { delete $ENV{SEREAL_DEFAULT_KEY} }
    for my $order (
        [ 'Sereal::Encoder', 'Sereal::Decoder' ],
        [ 'Sereal::Decoder', 'Sereal::Encoder' ] )
    {
        my $stderr = gensym;
        my $pid    = open3(
            my $in, my $out, $stderr,
            $^X, ( map { ( '-I', $_ ) } grep { !ref } @INC ), '-e', $child_code, @$order
        );
        close $in;
        my $stdout = do { local $/; <$out> };
        my $errors = do { local $/; <$stderr> };
        waitpid( $pid, 0 );
        is( $?,      0, "$order->[0] initializes the environment key correctly" ) or diag($errors);
        is( $stdout, "ok\n", 'module load order preserves the application override' );
    }
}
done_testing;
