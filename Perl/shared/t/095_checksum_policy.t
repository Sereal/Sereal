#!perl
use strict;
use warnings;
use lib 't/lib';
use Sereal::TestSet qw(Header have_encoder_and_decoder);
use Test::More;
use IPC::Open3 qw(open3);
use Symbol     qw(gensym);

plan skip_all => 'matching Encoder and Decoder required' unless have_encoder_and_decoder();
local $ENV{SEREAL_CHECKSUM_POLICY};
delete $ENV{SEREAL_CHECKSUM_POLICY};
local $Sereal::DefaultKey;
my $key       = pack( 'C*', 0 .. 15 );
my $other     = pack( 'C*', 16 .. 31 );
my $encode_op = eval 'sub { Sereal::Encoder::sereal_encode_with_object($_[0], 7) }';
die $@ unless $encode_op;
my $decode_op = eval 'sub { Sereal::Decoder::sereal_decode_with_object($_[0], $_[1]) }';
die $@ unless $decode_op;

for my $source ( 'option', 'environment' ) {
    $ENV{SEREAL_CHECKSUM_POLICY} = $source eq 'environment' ? 'require' : 'ignore';
    my %policy = $source eq 'option' ? ( checksum_policy => 'require' ) : ();
    my $enc    = Sereal::Encoder->new( \%policy );
    eval { $encode_op->($enc); 1 };
    like(
        $@, qr/checksum_policy 'require'.*key.*encode/,
        "$source require applies through the encoding opcode"
    );
    for ( 1 .. 3 ) {
        eval { $enc->encode(7); 1 };
        like(
            $@, qr/checksum_policy 'require'.*key.*encode/,
            "$source require rejects keyless encoding"
        );
    }
    eval { Sereal::Encoder::encode_sereal( 7, \%policy ); 1 };
    like(
        $@, qr/checksum_policy 'require'.*key.*encode/,
        "$source require applies to functional encoding"
    );
    for my $type ( 0, 1 ) {
        my %options = ( %policy, "checksum_v${type}_key" => $key );
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
        is(
            $dec->decode( Header(6) . "\x07" ), 7,
            "$source require preserves die semantics for unsigned input"
        );
    }
}
delete $ENV{SEREAL_CHECKSUM_POLICY};
for my $class ( 'Sereal::Encoder', 'Sereal::Decoder' ) {
    for my $policy ( 'die', 'require' ) {
        for my $type ( 0, 1 ) {
            for my $length ( 0, 1, 15, 17, 32 ) {
                eval {
                    $class->new(
                        { checksum_policy => $policy, "checksum_v${type}_key" => 'x' x $length } );
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
            $class->new( { checksum_policy => $policy, checksum_v0_key => 'short' } ),
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
    my $enc    = Sereal::Encoder->new( { checksum_policy => 'require' } );
    my $dec    = Sereal::Decoder->new( { checksum_policy => 'require' } );
    my $packet = $enc->encode('default');
    is( ord( substr( $packet, 6, 1 ) ), 1, 'shared default selects a 128-bit tag for encoding' );
    is( $dec->decode($packet),          'default', 'both modules use the shared default' );
    for my $type ( 0, 1 ) {
        my $explicit = Sereal::Encoder->new( { "checksum_v${type}_key" => $other } )->encode(7);
        is(
            Sereal::Decoder->new( { "checksum_v${type}_key" => $other } )->decode($explicit), 7,
            "explicit type $type key overrides the shared default"
        );
        eval { $dec->decode($explicit); 1 };
        like(
            $@, qr/checksum verification failed/,
            'default key does not replace an explicit key'
        );
        my $same = Sereal::Encoder->new( { "checksum_v${type}_key" => $key } )->encode(7);
        is( $dec->decode($same), 7, "default decoder key verifies type $type" );
    }
    {
        local $Sereal::DefaultKey = $other;
        my $inner = Sereal::Encoder->new( { checksum_policy => 'require' } )->encode('inner');
        is(
            Sereal::Decoder->new( { checksum_policy => 'require' } )->decode($inner), 'inner',
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
    my $disabled = Sereal::Encoder->new( { checksum_policy => 'die', checksum_v1_key => undef } );
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
