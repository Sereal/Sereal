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
use Sereal::Decoder qw(decode_sereal);
use Sereal::TestSet qw(Header varint);

BEGIN {
    if ( !eval { require Sereal::Encoder; 1 } ) {
        plan skip_all => 'Sereal::Encoder is required for Zstd context reuse roundtrip tests';
    }
    Sereal::Encoder->import(qw(encode_sereal SRL_SNAPPY SRL_ZLIB SRL_ZSTD));
}

# Reusable Zstd compression/decompression contexts across varying sizes
# (including payloads above the 512 KiB CCtx retention cap and back down),
# multiple compression levels, interleaved algorithms, error recovery,
# FREEZE/THAW re-entrancy (including cloned compress_level preservation),
# functional one-shot calls, and ithreads.

srand(1337);
my $rand_pool = join '', map { chr(int(rand(256))) } 1 .. ( 600 * 1024 );

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

my $enc = Sereal::Encoder->new( { compress => SRL_ZSTD, compress_threshold => 0 } );
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
my $enc_lvl1 = Sereal::Encoder->new( { compress => SRL_ZSTD, compress_level => 1,  compress_threshold => 0 } );
my $enc_lvl9 = Sereal::Encoder->new( { compress => SRL_ZSTD, compress_level => 9,  compress_threshold => 0 } );
my $enc_zlib = Sereal::Encoder->new( { compress => SRL_ZLIB, compress_threshold => 0 } );
my $enc_snpy = Sereal::Encoder->new( { compress => SRL_SNAPPY, compress_threshold => 0 } );
my $enc_raw  = Sereal::Encoder->new( { compress => 0 } );

for my $i ( 1 .. 3 ) {
    my $orig = make_payload( 'compressible', 4096 + $i );
    for my $pair (
        [ zstd_lvl1 => $enc_lvl1 ],
        [ zstd_lvl9 => $enc_lvl9 ],
        [ zlib      => $enc_zlib ],
        [ snappy    => $enc_snpy ],
        [ raw       => $enc_raw ],
    ) {
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
substr( $hdr_zstd, 4, 1 ) = chr( ( 0x04 << 4 ) | 5 ); # SRL_PROTOCOL_ENCODING_ZSTD = 4

# Extract compressed Zstd frame from valid_zstd (after 6-byte v5 header + compressed_len varint)
{
    my $pos = 6;
    $pos++ while ord( substr( $valid_zstd, $pos++, 1 ) ) & 0x80;
    my $zstd_frame = substr( $valid_zstd, $pos );

    # 3a: Truncated Zstd frame
    my $trunc_frame = substr( $zstd_frame, 0, int( length($zstd_frame) / 2 ) );
    ok( !eval { $dec->decode( $hdr_zstd . varint( length($trunc_frame) ) . $trunc_frame ); 1 },
        'truncated zstd frame fails' );
    is_deeply( $dec->decode($valid_zstd), $valid_sample, 'decoder recovers cleanly after truncated zstd frame' );

    # 3b: Corrupted payload bytes inside Zstd frame
    my $bad_frame = $zstd_frame;
    my $mid = int( length($bad_frame) * 3 / 4 );
    substr( $bad_frame, $mid, 1 ) = chr( ord( substr( $bad_frame, $mid, 1 ) ) ^ 0xff );
    ok( !eval { $dec->decode( $hdr_zstd . varint( length($bad_frame) ) . $bad_frame ); 1 },
        'corrupted zstd frame fails' );
    is_deeply( $dec->decode($valid_zstd), $valid_sample, 'decoder recovers cleanly after corrupted zstd frame' );

    # 3c: Unknown uncompressed content size (clear ContentSize flag bit 5 in Zstd FHD byte after 4-byte magic)
    my $no_size_frame = $zstd_frame;
    if ( length($no_size_frame) >= 6 ) {
        my $fhd = ord( substr( $no_size_frame, 4, 1 ) );
        substr( $no_size_frame, 4, 1 ) = chr( $fhd & ~0x20 );
    }
    ok( !eval { $dec->decode( $hdr_zstd . varint( length($no_size_frame) ) . $no_size_frame ); 1 },
        'zstd frame without content size fails' );
    is_deeply( $dec->decode($valid_zstd), $valid_sample, 'decoder recovers cleanly after unknown content size error' );
}

# 4. FREEZE / THAW re-entrancy using the SAME encoder and decoder (plus compress_level check)
{
    my $inner_enc_blob_len;
    my $re_enc = Sereal::Encoder->new( {
        compress           => SRL_ZSTD,
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
    my $outer_obj = bless { id => 7 }, 'ZstdCtxReentrantObj';
    my $outer_data = { outer => $outer_obj, pad => ( 'OUTER_' x 80 ) };
    my $outer_blob = $re_enc->encode($outer_data);
    is( $inner_enc_blob_len, length($standalone_inner),
        're-entrant FREEZE clone preserves compress_level=9' );

    my $outer_got = $re_dec->decode($outer_blob);
    is( ref( $outer_got->{outer} ), 'ZstdCtxReentrantObj', 'outer Zstd THAW object class restored' );
    is( $outer_got->{outer}{id}, 7, 'outer Zstd THAW id preserved' );
    is( $outer_got->{outer}{inner}{id}, 7, 're-entrant Zstd THAW inner id preserved' );
    is( $outer_got->{outer}{inner}{nested}, ( 'ZSTD_NESTED_' x 80 ), 're-entrant Zstd THAW payload matches' );
}

# 5. Functional one-shot encode_sereal / decode_sereal (non-reusable path)
{
    my $orig = make_payload( 'compressible', 2048 );
    my $blob = encode_sereal( $orig, { compress => SRL_ZSTD, compress_threshold => 0 } );
    is_deeply( decode_sereal($blob), $orig, 'one-shot encode_sereal/decode_sereal zstd roundtrips' );
}

# 6. ithreads safety
SKIP: {
    skip 'Perl not compiled with useithreads', 2 unless $Config{useithreads};
    require threads;
    my @thrs = map {
        my $tid = $_;
        threads->create(sub {
            my $e = Sereal::Encoder->new( { compress => SRL_ZSTD, compress_threshold => 0 } );
            my $d = Sereal::Decoder->new();
            my $p = { tid => $tid, payload => ( "thr_$tid:" x 150 ) };
            my $g = $d->decode( $e->encode($p) );
            return ( $g->{tid} == $tid && $g->{payload} eq $p->{payload} ) ? 1 : 0;
        });
    } 1 .. 2;
    for my $i ( 0 .. $#thrs ) {
        is( $thrs[$i]->join(), 1, "ithreads zstd encode/decode thread " . ($i + 1) . " succeeded" );
    }
}

done_testing();
