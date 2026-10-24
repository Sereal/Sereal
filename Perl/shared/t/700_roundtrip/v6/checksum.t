#!perl
use strict;
use warnings;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use IPC::Open3      qw(open3);
use Symbol          qw(gensym);
use Sereal::TestSet qw(Header read_varint_at);
use Sereal::Encoder;
use Sereal::Decoder;
use Sereal::Encoder::Constants qw(SRL_PROTOCOL_ENCODING_CHECKSUM);

my $sip_key = pack( 'C*', 0 .. 15 );
is(
    unpack( 'H*', Sereal::Encoder::siphash24( '', $sip_key ) ),
    '310e0edd47db6f72', 'Encoder exposes the SipHash-2-4 primitive'
);
is(
    unpack( 'H*', Sereal::Decoder::siphash24( '', $sip_key ) ),
    '310e0edd47db6f72', 'Decoder exposes the SipHash-2-4 primitive'
);
is(
    unpack( 'H*', Sereal::Encoder::siphash36( '', $sip_key ) ),
    '37228a2b7a1e505b60ae19c48695eb27', 'Encoder exposes SipHash-3-6-128'
);
is(
    unpack( 'H*', Sereal::Decoder::siphash36( '', $sip_key ) ),
    '37228a2b7a1e505b60ae19c48695eb27', 'Decoder exposes SipHash-3-6-128'
);

my $key0     = 'Out damned spot!';
my $key1     = 'Call me Ishmael.';
my $prepared = Sereal::Encoder::prepare_checksum_key( 'some key', 'optional' );
is( length($prepared), 16, 'prepare_checksum_key returns 16 bytes' );
is(
    unpack( 'H*', $prepared ), '0c5fa2723161734cd1d1080d782a1db5',
    'prepare_checksum_key output is stable'
);
is(
    Sereal::Decoder::prepare_checksum_key( 'some key', 'optional' ), $prepared,
    'Encoder and Decoder key preparation agree'
);
is(
    Sereal::Encoder::prepare_checksum_key(), Sereal::Encoder::prepare_checksum_key( '', '' ),
    'omitted key arguments default to empty strings'
);

my $plain_value = 'checksummed Sereal payload';
my $enc0        = Sereal::Encoder->new( { protocol_version => 6, checksum_v0_key => $key0 } );
my $enc1        = Sereal::Encoder->new( {
    protocol_version   => 6,
    checksum_v1_key    => $key1,
    compress           => 2,
    compress_threshold => 1,
} );
my $packet0 = $enc0->encode($plain_value);
my $packet1 = $enc1->encode( $plain_value x 100 );

ok(
    ord( substr( $packet0, 4, 1 ) ) & SRL_PROTOCOL_ENCODING_CHECKSUM,
    'v0 packet sets the checksum flag'
);
ok(
    ord( substr( $packet1, 4, 1 ) ) & SRL_PROTOCOL_ENCODING_CHECKSUM,
    'v1 compressed packet sets the checksum flag'
);

my $dec0 = Sereal::Decoder->new( { checksum_v0_key => $key0, checksum_policy => 'die' } );
my $dec1 = Sereal::Decoder->new( { checksum_v1_key => $key1, checksum_policy => 'die' } );
is( $dec0->decode($packet0), $plain_value, 'v0 packet verifies and decodes' );
is(
    $dec1->decode($packet1), $plain_value x 100,
    'v1 packet verifies before decompressing and decodes'
);

sub run_hobodecoder {
    my ( $packet, @args ) = @_;
    my ( $stdin, $stdout );
    my $stderr = gensym;
    my $pid    = open3(
        $stdin, $stdout,             $stderr,
        $^X,    '-Mblib=../Encoder', '-Mblib=../Decoder',
        'author_tools/hobodecoder.pl', @args
    );
    binmode $stdin;
    binmode $stdout;
    binmode $stderr;
    print {$stdin} $packet;
    close $stdin;
    my $out = do { local $/; <$stdout> // '' };
    my $err = do { local $/; <$stderr> // '' };
    waitpid( $pid, 0 );
    return ( $? >> 8, $out, $err );
}

SKIP: {
    skip 'hobodecoder is not included in this distribution', 6
        unless -f 'author_tools/hobodecoder.pl';
    my ( $hobo_status, $hobo_out, $hobo_err ) =
        run_hobodecoder( $packet0, '--checksum-v0-key-hex=' . unpack( 'H*', $key0 ) );
    is( $hobo_status, 0, 'hobodecoder accepts a checksummed raw packet with its key' );
    like(
        $hobo_out, qr/SipHash-2-4 checksum verified/,
        'hobodecoder verifies the SipHash-2-4 checksum'
    );
    ( $hobo_status, $hobo_out, $hobo_err ) =
        run_hobodecoder( $packet1, '--checksum-v1-key-hex=' . unpack( 'H*', $key1 ) );
    is( $hobo_status, 0, 'hobodecoder accepts a checksummed compressed packet with its key' );
    like(
        $hobo_out, qr/SipHash-2-4-128 checksum verified/,
        'hobodecoder verifies the checksum before decompressing'
    );
    ( $hobo_status, $hobo_out, $hobo_err ) =
        run_hobodecoder( $packet1, '--checksum-v1-key-hex=' . unpack( 'H*', 'wrong key wrong!' ) );
    isnt( $hobo_status, 0, 'hobodecoder rejects a compressed packet with the wrong key' );
    like(
        $hobo_err, qr/checksum verification failed/i,
        'hobodecoder reports checksum verification failure'
    );
}

for my $version ( 1, 2 ) {
    SKIP: {
        skip 'hobodecoder is not included in this distribution', 2
            unless -f 'author_tools/hobodecoder.pl';
        my $packet =
            Sereal::Encoder->new( { "checksum_v${version}_key" => $key1 } )->encode($plain_value);
        my ( $status, $out, $err ) = run_hobodecoder(
            $packet,
            "--checksum-v${version}-key-hex=" . unpack( 'H*', $key1 ) );
        is( $status, 0, "hobodecoder accepts checksum version $version" ) or diag($err);
        my $variant = $version == 1 ? '2-4-128' : '3-6-128';
        like(
            $out, qr/SipHash-\Q$variant\E checksum verified/,
            "hobodecoder identifies version $version correctly"
        );
    }
}

sub checksum_tag_offset {
    my ($packet) = @_;
    my ( $header_len, $offset ) = read_varint_at( $packet, 5 );
    $offset += $header_len;
    my $type_offset   = $offset;
    my $checksum_type = ord( substr( $packet, $offset++, 1 ) );
    my ( $tag_len, $tag_start ) = read_varint_at( $packet, $offset );
    my $payload_len_offset = $tag_start + $tag_len;
    my ( $payload_len, $payload_start ) = read_varint_at( $packet, $payload_len_offset );
    return (
        $checksum_type,      $tag_len,     $tag_start, $type_offset,
        $payload_len_offset, $payload_len, $payload_start
    );
}

my ( $type0, $tag_len0, $tag_start0 ) = checksum_tag_offset($packet0);
is( $type0,    0, 'v0 checksum type is encoded' );
is( $tag_len0, 8, 'v0 uses an eight-byte tag' );
my ( $type1, $tag_len1 ) = checksum_tag_offset($packet1);
is( $type1,    1,  'v1 checksum type is encoded' );
is( $tag_len1, 16, 'v1 uses a sixteen-byte tag' );
my $bad_tag = $packet0;
substr( $bad_tag, $tag_start0, 1 ) = chr( ord( substr( $bad_tag, $tag_start0, 1 ) ) ^ 1 );

my $ok = eval { $dec0->decode($bad_tag); 1 };
ok( !$ok, 'wrong tag is rejected when a key is configured' );
like( $@, qr/checksum verification failed/i, 'wrong-tag error identifies checksum verification' );

my $bad_payload = $packet0;
substr( $bad_payload, -1, 1 ) = chr( ord( substr( $bad_payload, -1, 1 ) ) ^ 1 );
$ok = eval { $dec0->decode($bad_payload); 1 };
ok( !$ok, 'payload modification is rejected by checksum verification' );
like( $@, qr/checksum verification failed/i, 'payload tampering reports a checksum error' );

my ( undef, undef, undef, $type_offset0, $payload_len_offset0 ) = checksum_tag_offset($packet0);
my $unknown_type = $packet0;
substr( $unknown_type, $type_offset0, 1 ) = chr(99);
is(
    Sereal::Decoder->new( { checksum_policy => 'ignore' } )->decode($unknown_type), $plain_value,
    'ignore policy can skip an unknown checksum type using its explicit lengths'
);

my $oversized_payload = $packet0;
substr( $oversized_payload, $payload_len_offset0, 1 ) = chr(0x7f);
$ok = eval { $dec0->decode($oversized_payload); 1 };
ok( !$ok, 'payload length beyond the packet is rejected' );
like(
    $@, qr/payload length exceeds the available input/i,
    'oversized envelope length has a clear error'
);

my @warnings;
my $unkeyed_value;
{
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    $unkeyed_value = Sereal::Decoder->new->decode($packet0);
}
is( $unkeyed_value, $plain_value, 'default policy decodes without a key' );
like(
    join( '', @warnings ), qr/without authentication/i,
    'default policy warns when it cannot authenticate'
);

my $ignore_value = Sereal::Decoder->new( { checksum_policy => 'ignore' } )->decode($bad_tag);
is( $ignore_value, $plain_value, 'ignore policy skips verification and decodes' );

@warnings = ();
{
    local $SIG{__WARN__} = sub { push @warnings, @_ };
    $ignore_value = Sereal::Decoder->new( { checksum_policy => 'ignore-warn' } )->decode($bad_tag);
}
is( $ignore_value, $plain_value, 'ignore-warn policy decodes without verifying' );
like(
    join( '', @warnings ), qr/ignoring.*checksum/i,
    'ignore-warn policy reports that authentication was skipped'
);

$ok = eval { Sereal::Decoder->new( { checksum_policy => 'die' } )->decode($packet0); 1 };
ok( !$ok, 'die policy rejects a checksummed packet without a key' );
like( $@, qr/requires a 'checksum_v0_key'/, 'missing-key error names the required option' );

$ok = eval { Sereal::Decoder->new( { checksum_policy => 'invalid' } ); 1 };
ok( !$ok, 'invalid checksum policy is rejected' );
like( $@, qr/Invalid checksum_policy/, 'invalid-policy error is descriptive' );

my $plain_packet = Sereal::Encoder->new( { protocol_version => 6 } )->encode($plain_value);
is(
    Sereal::Decoder->new( { checksum_policy => 'die' } )->decode($plain_packet), $plain_value,
    'die policy still accepts packets without a checksum'
);

done_testing();
