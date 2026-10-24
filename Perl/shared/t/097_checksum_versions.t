#!perl
use strict;
use warnings;
use lib 't/lib';
use Sereal::TestSet qw(Header varint read_varint_at checksum_tag have_encoder_and_decoder);
use Test::More;

plan skip_all => 'matching Encoder and Decoder required' unless have_encoder_and_decoder();
local $Sereal::DefaultKey;
local $ENV{SEREAL_CHECKSUM_POLICY} = 'die';
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
    map { ( "checksum_v${_}_key" => pack( 'C*', ( $_ * 16 ) .. ( $_ * 16 + 15 ) ) ) } ( 0, 1, 2 );
for my $version ( 0, 1, 2 ) {
    for my $compress ( 0, 2 ) {
        my $option      = "checksum_v${version}_key";
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
        my $decoder = Sereal::Decoder->new( \%keys );
        is( $decoder->decode($packet), $body, "version=$version decodes with a multi-key decoder" );
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
                    Sereal::Decoder->new( { checksum_policy => $policy, %keys } )->decode($bad); 1;
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
            eval { $decoder->decode($bad); 1 };
            like(
                $@, qr/checksum verification failed/,
                "version=$version cannot be relabeled as the other 128-bit variant"
            );
        }
    }
}

for my $version ( 0, 1, 2 ) {
    eval {
        Sereal::Encoder->new( { protocol_version => 5, "checksum_v${version}_key" => $key } ); 1;
    };
    like(
        $@, qr/requires Sereal protocol version 6/,
        "version=$version key cannot emit an older protocol"
    );
}
for my $left ( 0, 1, 2 ) {
    for my $right ( 0, 1, 2 ) {
        next if $left >= $right;
        eval {
            Sereal::Encoder->new(
                { "checksum_v${left}_key" => $key, "checksum_v${right}_key" => $key } ); 1;
        };
        like( $@, qr/Specify only one/, "Encoder rejects simultaneous versions $left and $right" );
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
    my $plain = Sereal::Encoder->new( { checksum_v2_key => undef } )->encode(7);
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
