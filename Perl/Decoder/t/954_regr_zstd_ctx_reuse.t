#!perl
use strict;
use warnings;
use Config;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Test::More;
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(SRL_HDR_OBJECT_FREEZE SRL_HDR_BINARY);
use Sereal::TestSet            qw(Header varint read_varint_at short_string array);
use Sereal::TestSet::Corpus    qw(packet_names read_packet);

# Exercise decoder context reuse even with an old or absent Encoder.
# These checked-in v6 packets contain real Zstd frames from a capable Encoder.
{
    my $root  = File::Spec->catdir(qw(t data packet_corpus));
    my @names = grep { m{^roundtrip/\d+/AoA of duplicates long ascii string 'a' x 102[345]$} }
        packet_names( $root, 'roundtrip-v6-zstd' );
    is( scalar(@names), 3, 'stored Zstd context fixtures are available' );
    die 'Missing required Zstd context fixtures' unless @names == 3;

    my $decoder       = Sereal::Decoder->new;
    my $plain_decoder = Sereal::Decoder->new;
    my @stored;
    for my $name (@names) {
        my $packet = read_packet( $root, 'roundtrip-v6-zstd',  $name );
        my $plain  = read_packet( $root, 'roundtrip-v6-plain', $name );
        is( ord( substr( $packet, 4, 1 ) ) >> 4, 4, "$name uses Zstd encoding" );
        push @stored, [ $packet, $plain, $plain_decoder->decode($plain) ];
    }
    for my $pass ( 1 .. 2 ) {
        for my $case (@stored) {
            is_deeply(
                $decoder->decode( $case->[0] ), $case->[2],
                "stored Zstd packet decodes on reuse pass $pass"
            );
            is_deeply(
                $decoder->decode( $case->[1] ), $case->[2],
                "raw packet interleaves with Zstd on reuse pass $pass"
            );
        }
    }

    my ( $valid, undef, $expected ) = @{ $stored[0] };
    my ( $length, $offset ) = read_varint_at( $valid, 6 );
    my $frame  = substr( $valid, $offset, $length );
    my $header = substr( $valid, 0,       6 );
    for my $bad ( substr( $frame, 0, int( length($frame) / 2 ) ), 'bad!' . substr( $frame, 4 ) ) {
        ok(
            !eval { $decoder->decode( $header . varint( length($bad) ) . $bad ); 1 },
            'stored malformed Zstd frame is rejected'
        );
        is_deeply(
            $decoder->decode($valid), $expected,
            'stored Zstd packet decodes after an error'
        );
    }
    is_deeply( decode_sereal($valid), $expected, 'one-shot decode of stored Zstd packet' );

    {

        package ZstdCtxCorpusObj;

        sub THAW {
            my ( $class, $serializer, $packet ) = @_;
            return bless { inner => $decoder->decode($packet) }, $class;
        }
    }
    my $outer =
          Header(6)
        . chr(SRL_HDR_OBJECT_FREEZE)
        . short_string('ZstdCtxCorpusObj')
        . array( chr(SRL_HDR_BINARY) . varint( length($valid) ) . $valid );
    my $object = $decoder->decode($outer);
    is_deeply( $object->{inner},         $expected, 'THAW clone decodes a stored Zstd packet' );
    is_deeply( $decoder->decode($valid), $expected, 'parent context survives THAW clone cleanup' );

    SKIP: {
        skip 'Perl not compiled with useithreads', 2 unless $Config{useithreads};
        require threads;
        my @threads = map {
            threads->create(
                sub {
                    my $child = Sereal::Decoder->new;
                    $child->decode($valid) for 1 .. 3;
                    return $child->decode($valid);
                } )
        } 1 .. 2;
        is_deeply( $_->join, $expected, 'thread decodes stored Zstd packets with its own context' )
            for @threads;
    }
}

# Zstd was introduced in Encoder 4.002. Loading an older Encoder does not
# imply that it exports the compression constants or can make these packets.
SKIP: {
    skip 'live Zstd round trips require Sereal::Encoder 4.002 or newer', 1
        unless eval { require Sereal::Encoder; Sereal::Encoder->VERSION('4.002'); 1 };

# Reusable Zstd compression/decompression contexts across varying sizes
# (including payloads above the 512 KiB CCtx retention cap and back down),
# multiple compression levels, interleaved algorithms, error recovery,
# FREEZE/THAW re-entrancy (including cloned compress_level preservation),
# functional one-shot calls, and ithreads.

    srand(1337);
    my $rand_pool = join '', map { chr( int( rand(256) ) ) } 1 .. ( 600 * 1024 );

    sub make_payload {
        my ( $kind, $size ) = @_;
        if ( $kind eq 'compressible' ) {
            return {
                tag  => "comp_$size",
                data => ( 'ABCD1234' x int( $size / 8 + 1 ) ),
            };
        }
        return {
            tag  => "rand_$size",
            data => substr( $rand_pool, 0, $size ),
        };
    }

    my $enc = Sereal::Encoder->new(
        { compress => Sereal::Encoder::SRL_ZSTD(), compress_threshold => 0 } );
    my $dec = Sereal::Decoder->new();

# 1. Sequential encodes/decodes across sizes (50B .. 600KB > 512KB cap .. 1KB)
    my @cases = (
        [ compressible => 50 ],
        [ random       => 50 ],
        [ compressible => 1024 ],
        [ random       => 1024 ],
        [ compressible => 16 * 1024 ],
        [ random       => 16 * 1024 ],
        [ compressible => 128 * 1024 ],
        [ random       => 128 * 1024 ],
        [ compressible => 600 * 1024 ],
        [ random       => 600 * 1024 ],
        [ compressible => 1024 ],
        [ random       => 1024 ],
    );

    for my $pass ( 1 .. 2 ) {
        for my $c (@cases) {
            my ( $kind, $sz ) = @$c;
            my $orig = make_payload( $kind, $sz );
            my $blob = $enc->encode($orig);
            ok( defined($blob) && length($blob) > 0, "pass $pass encode zstd $kind $sz" );
            my $got = $dec->decode($blob);
            is_deeply( $got, $orig, "pass $pass decode zstd $kind $sz roundtrips" );
        }
    }

# 2. Multiple compress_level instances and interleaved compression formats
    my $enc_lvl1 =
        Sereal::Encoder->new(
        { compress => Sereal::Encoder::SRL_ZSTD(), compress_level => 1, compress_threshold => 0 } );
    my $enc_lvl9 =
        Sereal::Encoder->new(
        { compress => Sereal::Encoder::SRL_ZSTD(), compress_level => 9, compress_threshold => 0 } );
    my $enc_zlib = Sereal::Encoder->new(
        { compress => Sereal::Encoder::SRL_ZLIB(), compress_threshold => 0 } );
    my $enc_snpy = Sereal::Encoder->new(
        { compress => Sereal::Encoder::SRL_SNAPPY(), compress_threshold => 0 } );
    my $enc_raw = Sereal::Encoder->new( { compress => 0 } );

    for my $i ( 1 .. 3 ) {
        my $orig = make_payload( 'compressible', 4096 + $i );
        for my $pair (
            [ zstd_lvl1 => $enc_lvl1 ],
            [ zstd_lvl9 => $enc_lvl9 ],
            [ zlib      => $enc_zlib ],
            [ snappy    => $enc_snpy ],
            [ raw       => $enc_raw ],
            )
        {
            my ( $label, $e ) = @$pair;
            my $blob = $e->encode($orig);
            is_deeply( $dec->decode($blob), $orig, "interleaved decode $label iter $i" );
        }
    }

# 3. Error recovery on reusable decoder (no dirty ZSTD_DCtx carry-over)
    my $valid_sample = make_payload( 'compressible', 2048 );
    my $valid_zstd   = $enc->encode($valid_sample);

# Build malformed Zstd packets from a real Zstd packet header
    my $hdr_zstd = Header(5);
    substr( $hdr_zstd, 4, 1 ) = chr( ( 0x04 << 4 ) | 5 );    # SRL_PROTOCOL_ENCODING_ZSTD = 4

# Extract compressed Zstd frame from valid_zstd (after 6-byte v5 header + compressed_len varint)
    {
        my ( undef, $pos ) = read_varint_at( $valid_zstd, 6 );
        my $zstd_frame = substr( $valid_zstd, $pos );

        # 3a: Truncated Zstd frame
        my $trunc_frame = substr( $zstd_frame, 0, int( length($zstd_frame) / 2 ) );
        ok(
            !eval {
                $dec->decode( $hdr_zstd . varint( length($trunc_frame) ) . $trunc_frame ); 1;
            },
            'truncated zstd frame fails'
        );
        is_deeply(
            $dec->decode($valid_zstd), $valid_sample,
            'decoder recovers cleanly after truncated zstd frame'
        );

        # 3b: Invalid Zstd magic. Flipping a payload byte can leave a valid
        # frame with different decoded data when no checksum is present.
        my $bad_frame = $zstd_frame;
        substr( $bad_frame, 0, 4 ) = 'bad!';
        ok(
            !eval { $dec->decode( $hdr_zstd . varint( length($bad_frame) ) . $bad_frame ); 1 },
            'corrupted zstd magic fails'
        );
        is_deeply(
            $dec->decode($valid_zstd), $valid_sample,
            'decoder recovers cleanly after corrupted zstd frame'
        );

        # 3c: Unknown uncompressed content size (clear ContentSize flag bit 5 in Zstd FHD byte after 4-byte magic)
        my $no_size_frame = $zstd_frame;
        if ( length($no_size_frame) >= 6 ) {
            my $fhd = ord( substr( $no_size_frame, 4, 1 ) );
            substr( $no_size_frame, 4, 1 ) = chr( $fhd & ~0x20 );
        }
        ok(
            !eval {
                $dec->decode( $hdr_zstd . varint( length($no_size_frame) ) . $no_size_frame ); 1;
            },
            'zstd frame without content size fails'
        );
        is_deeply(
            $dec->decode($valid_zstd), $valid_sample,
            'decoder recovers cleanly after unknown content size error'
        );
    }

# 4. FREEZE / THAW re-entrancy using the SAME encoder and decoder (plus compress_level check)
    {
        my $inner_enc_blob_len;
        my $re_enc = Sereal::Encoder->new( {
            compress           => Sereal::Encoder::SRL_ZSTD(),
            compress_level     => 9,
            compress_threshold => 0,
            freeze_callbacks   => 1,
        } );
        my $re_dec = Sereal::Decoder->new();

        {

            package ZstdCtxReentrantObj;

            sub FREEZE {
                my ( $self, $serializer ) = @_;

                # Re-enter the same Zstd encoder inside FREEZE; verify it uses cloned compress_level
                my $inner_data = { nested => ( 'ZSTD_NESTED_' x 80 ), id => $self->{id} };
                my $inner_blob = $re_enc->encode($inner_data);
                $inner_enc_blob_len = length($inner_blob);
                return ( $self->{id}, $inner_blob );
            }

            sub THAW {
                my ( $class, $serializer, $id, $inner_blob ) = @_;

                # Re-enter the same Zstd decoder inside THAW
                my $inner_got = $re_dec->decode($inner_blob);
                return bless { id => $id, inner => $inner_got }, $class;
            }
        }

        my $standalone_inner = $enc_lvl9->encode( { nested => ( 'ZSTD_NESTED_' x 80 ), id => 7 } );
        my $outer_obj        = bless { id => 7 }, 'ZstdCtxReentrantObj';
        my $outer_data       = { outer => $outer_obj, pad => ( 'OUTER_' x 80 ) };
        my $outer_blob       = $re_enc->encode($outer_data);
        SKIP: {
            skip 'FREEZE clone compression levels require Encoder 7.008 or newer', 1
                unless eval { Sereal::Encoder->VERSION('7.008'); 1 };
            is(
                $inner_enc_blob_len, length($standalone_inner),
                're-entrant FREEZE clone preserves compress_level=9'
            );
        }

        my $outer_got = $re_dec->decode($outer_blob);
        is(
            ref( $outer_got->{outer} ), 'ZstdCtxReentrantObj',
            'outer Zstd THAW object class restored'
        );
        is( $outer_got->{outer}{id},        7, 'outer Zstd THAW id preserved' );
        is( $outer_got->{outer}{inner}{id}, 7, 're-entrant Zstd THAW inner id preserved' );
        is(
            $outer_got->{outer}{inner}{nested}, ( 'ZSTD_NESTED_' x 80 ),
            're-entrant Zstd THAW payload matches'
        );
    }

# 5. Functional one-shot encode_sereal / decode_sereal (non-reusable path)
    {
        my $orig = make_payload( 'compressible', 2048 );
        my $blob = Sereal::Encoder::encode_sereal(
            $orig,
            { compress => Sereal::Encoder::SRL_ZSTD(), compress_threshold => 0 } );
        is_deeply(
            decode_sereal($blob), $orig,
            'one-shot encode_sereal/decode_sereal zstd roundtrips'
        );
    }

# 6. ithreads safety
    SKIP: {
        skip 'Perl not compiled with useithreads', 2 unless $Config{useithreads};
        require threads;
        my @thrs = map {
            my $tid = $_;
            threads->create(
                sub {
                    my $e = Sereal::Encoder->new(
                        { compress => Sereal::Encoder::SRL_ZSTD(), compress_threshold => 0 } );
                    my $d = Sereal::Decoder->new();
                    my $p = { tid => $tid, payload => ( "thr_$tid:" x 150 ) };
                    my $g = $d->decode( $e->encode($p) );
                    return ( $g->{tid} == $tid && $g->{payload} eq $p->{payload} ) ? 1 : 0;
                } );
        } 1 .. 2;
        for my $i ( 0 .. $#thrs ) {
            is(
                $thrs[$i]->join(), 1,
                "ithreads zstd encode/decode thread " . ( $i + 1 ) . " succeeded"
            );
        }
    }

}
done_testing();
