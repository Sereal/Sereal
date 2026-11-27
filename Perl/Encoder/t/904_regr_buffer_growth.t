#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Sereal::Encoder qw(
    encode_sereal
    encode_sereal_with_header_data
    sereal_encode_with_object
    SRL_SNAPPY
    SRL_ZLIB
    SRL_ZSTD
);
use Sereal::Encoder::Constants qw(:all);
use Test::More;

{

    package Local::RegrBufFreeze;
    our $ENCODER;
    our $DIE_IN_FREEZE = 0;

    sub FREEZE {
        my ($self) = @_;
        die "intentional freeze failure" if $DIE_IN_FREEZE;
        return $ENCODER->encode( $self->{inner} );
    }

    sub THAW {
        my ( $class, $ser, $pkt ) = @_;
        my $inner = Sereal::Decoder->new->decode($pkt);
        return bless { inner => $inner }, $class;
    }
}

my $have_dec = have_encoder_and_decoder();
plan skip_all => 'Sereal::Decoder not available or incompatible' unless $have_dec;

require Sereal::Decoder;
my $dec = Sereal::Decoder->new();
my $enc = Sereal::Encoder->new();

# 1. Small-scalar arrays across sizes (1..50,000), checking both functional
#    encode_sereal (buffer theft path) and OO $enc->encode (reusable path).
for my $n ( 0, 1, 15, 16, 64, 127, 128, 1000, 10000, 50000 ) {
    my @ones = (1) x $n;
    my @seq  = $n ? ( 1 .. $n ) : ();

    for my $pair ( [ "ones_$n", \@ones ], [ "seq_$n", \@seq ] ) {
        my ( $label, $arr ) = @$pair;
        my $pkt_fn  = encode_sereal($arr);
        my $pkt_oo  = $enc->encode($arr);
        my $pkt_fwo = sereal_encode_with_object( $enc, $arr );

        is( $pkt_oo,  $pkt_fn, "array $label: OO matches functional wire bytes" );
        is( $pkt_fwo, $pkt_fn, "array $label: sereal_encode_with_object matches functional" );
        is_deeply( $dec->decode($pkt_fn), $arr, "array $label: roundtrip ok" );
    }
}

# 2. Small-scalar hashes across sizes (0..50,000) with canonical key order
#    so wire bytes from functional and OO paths are strictly comparable.
{
    my $enc_canon = Sereal::Encoder->new( { sort_keys => 1 } );
    for my $n ( 0, 1, 15, 16, 100, 1000, 10000, 50000 ) {
        my %h      = map { ( "k$_" => ( $_ % 16 ) ) } 1 .. $n;
        my $pkt_fn = encode_sereal( \%h, { sort_keys => 1 } );
        my $pkt_oo = $enc_canon->encode( \%h );

        is( $pkt_oo, $pkt_fn, "hash n=$n: OO matches functional wire bytes" );
        is_deeply( $dec->decode($pkt_fn), \%h, "hash n=$n: roundtrip ok" );
    }
}

# 3. Crossing the 64 KiB growth-policy boundary (2x < 64 KiB, 1.5x >= 64 KiB)
#    via both growing containers and large single strings.
{
    my $e = Sereal::Encoder->new();
    for my $len ( 65000, 65535, 65536, 65537, 98304, 131072, 262144 ) {
        my $s      = 'A' x $len;
        my $pkt_fn = encode_sereal($s);
        my $pkt_oo = $e->encode($s);
        is( $pkt_oo,               $pkt_fn, "large string len=$len: OO matches functional" );
        is( $dec->decode($pkt_fn), $s,      "large string len=$len: roundtrip ok" );
    }

    # Growing array of short strings crossing 64 KiB (~65 KB, ~120 KB)
    for my $count ( 6000, 12000 ) {
        my @arr    = map { sprintf( "str_%06d", $_ ) } 1 .. $count;
        my $pkt_fn = encode_sereal( \@arr );
        my $pkt_oo = $e->encode( \@arr );
        is( $pkt_oo, $pkt_fn, "growing string array count=$count: OO matches functional" );
        is_deeply(
            $dec->decode($pkt_fn), \@arr,
            "growing string array count=$count: roundtrip ok"
        );
    }
}

# 4. Compression (Snappy, Snappy incr, Zlib, Zstd) across 64 KiB and >1 MiB
{
    my @algos = (
        [ 'snappy',      { compress    => SRL_SNAPPY, compress_threshold => 0 } ],
        [ 'snappy_incr', { snappy_incr => 1,          snappy_threshold   => 0 } ],
        [ 'zlib',        { compress    => SRL_ZLIB,   compress_threshold => 0 } ],
        [ 'zstd',        { compress    => SRL_ZSTD,   compress_threshold => 0 } ],
    );
    my $compressible = ( "Sereal buffer growth compression payload! " x 3000 );      # ~120 KB
    my $huge_comp    = ( "Reusable encoder >2 MiB compression test!! " x 50000 );    # ~2.2 MB
    my $incompressible;
    {
        my $v = 123456789;
        my @chunks;
        for ( 1 .. 10000 ) {
            $v = ( $v * 1103515245 + 12345 ) & 0x7fffffff;
            push @chunks, pack( 'N', $v );
        }
        $incompressible = join( '', @chunks );    # 40 KB pseudo-random
    }

    for my $algo (@algos) {
        my ( $name, $opts ) = @$algo;
        my $ec = Sereal::Encoder->new($opts);
        for my $case (
            [ 'medium_comp', $compressible ],
            [ 'incompress',  $incompressible ],
            [ 'huge_comp',   $huge_comp ],
            [ 'tiny_after',  { ok => 1 } ],
            )
        {
            my ( $cname, $val ) = @$case;
            my $pkt_oo = $ec->encode($val);
            my $pkt_fn = encode_sereal( $val, $opts );
            is_deeply( $dec->decode($pkt_oo), $val, "$name $cname (OO): roundtrip ok" );
            is_deeply( $dec->decode($pkt_fn), $val, "$name $cname (fn): roundtrip ok" );
        }
    }
}

# 5. User header data across sizes (including >2 MiB header and >2 MiB body)
{
    my $eh        = Sereal::Encoder->new();
    my $huge_hdr  = { big => ( 'H' x 2_200_000 ) };
    my $huge_body = [ 'B' x 2_200_000, 42 ];

    for my $case (
        [ 'small_hdr_small_body', { k => 1 },  { h => 2 } ],
        [ 'huge_hdr_small_body',  { k => 3 },  $huge_hdr ],
        [ 'small_hdr_huge_body',  $huge_body,  { h => 4 } ],
        [ 'tiny_after_huge_hdr',  [ 1, 2, 3 ], [ 4, 5 ] ],
        )
    {
        my ( $label, $body, $hdr ) = @$case;
        my $pkt_oo = $eh->encode( $body, $hdr );
        my $pkt_fn = encode_sereal_with_header_data( $body, $hdr );
        is( $pkt_oo, $pkt_fn, "header $label: OO matches functional wire bytes" );
        is_deeply(
            $dec->decode_with_header($pkt_oo), [ $hdr, $body ],
            "header $label: roundtrip ok"
        );
    }
}

# 6. Reusable encoder: >2 MiB payload followed by tiny payloads, repeated reuse,
#    checksums, post-croak cleanup (body + header + FREEZE), and re-entrant FREEZE.
{
    my $re         = Sereal::Encoder->new();
    my $huge_str   = 'Z' x 2_300_000;
    my $huge_array = [ map { $_ } 1 .. 50000 ];

    # Repeated huge -> tiny -> medium -> huge -> tiny cycles
    for my $round ( 1 .. 3 ) {
        my $p1 = $re->encode( { round => $round, blob => $huge_str } );
        is_deeply(
            $dec->decode($p1),
            { round => $round, blob => $huge_str },
            "reuse round $round: >2 MiB blob roundtrip"
        );

        my $p2 = $re->encode( [ 1 .. 15 ] );
        is_deeply( $dec->decode($p2), [ 1 .. 15 ], "reuse round $round: tiny after >2 MiB" );

        my $p3 = $re->encode($huge_array);
        is_deeply( $dec->decode($p3), $huge_array, "reuse round $round: 50k int array" );

        my $p4 = $re->encode( { x => 'ok' } );
        is_deeply( $dec->decode($p4), { x => 'ok' }, "reuse round $round: small hash" );
    }

    # Protocol v6 checksum envelope on >2 MiB payload then tiny payload
    my $ce = Sereal::Encoder->new( {
        protocol_version => 6,
        checksum_key     => '0123456789abcdef',
    } );
    my $cd  = Sereal::Decoder->new( { checksum_key => '0123456789abcdef' } );
    my $cp1 = $ce->encode( [ $huge_str, 99 ] );
    is_deeply( $cd->decode($cp1), [ $huge_str, 99 ], 'checksum >2 MiB payload roundtrip' );
    my $cp2 = $ce->encode( { small => 1 } );
    is_deeply( $cd->decode($cp2), { small => 1 }, 'checksum tiny payload after >2 MiB' );

    # Post-croak cleanup after growing >2 MiB in body and in header
    my $croak_enc = Sereal::Encoder->new( { croak_on_bless => 1 } );
    my $ok        = eval { $croak_enc->encode( [ $huge_str, bless( {}, 'NoWay' ) ] ); 1 };
    ok( !$ok, 'croak_on_bless in >2 MiB body died' );
    is_deeply(
        $dec->decode( $croak_enc->encode( { clean => 1 } ) ),
        { clean => 1 },
        'reusable encoder clean after >2 MiB body croak'
    );

    $ok = eval { $croak_enc->encode( 'tiny', [ $huge_str, bless( {}, 'NoWayHdr' ) ] ); 1 };
    ok( !$ok, 'croak_on_bless in >2 MiB header died' );
    is_deeply(
        $dec->decode_with_header( $croak_enc->encode( 'body_ok', 'hdr_ok' ) ),
        [ 'hdr_ok', 'body_ok' ],
        'reusable encoder clean after >2 MiB header croak'
    );

    # Re-entrant FREEZE with >2 MiB outer, >2 MiB inner, and FREEZE dying after >2 MiB
    my $fe = Sereal::Encoder->new( { freeze_callbacks => 1 } );
    local $Local::RegrBufFreeze::ENCODER = $fe;

    my $f1  = bless { inner => [ 1 .. 50 ] }, 'Local::RegrBufFreeze';
    my $fp1 = $fe->encode( [ $huge_str, $f1 ] );
    is_deeply(
        $dec->decode($fp1),
        [ $huge_str, bless { inner => [ 1 .. 50 ] }, 'Local::RegrBufFreeze' ],
        're-entrant FREEZE: >2 MiB outer + small inner'
    );

    my $f2  = bless { inner => $huge_str }, 'Local::RegrBufFreeze';
    my $fp2 = $fe->encode( [ 'small_outer', $f2 ] );
    is_deeply(
        $dec->decode($fp2),
        [ 'small_outer', bless { inner => $huge_str }, 'Local::RegrBufFreeze' ],
        're-entrant FREEZE: small outer + >2 MiB inner'
    );

    {
        local $Local::RegrBufFreeze::DIE_IN_FREEZE = 1;
        my $fok = eval { $fe->encode( [ $huge_str, $f1 ] ); 1 };
        ok( !$fok, 'FREEZE dying after >2 MiB outer payload died as expected' );
    }
    is_deeply(
        $dec->decode( $fe->encode( [ 'recovered', 123 ] ) ),
        [ 'recovered', 123 ],
        'reusable encoder clean after >2 MiB FREEZE failure'
    );
}

# 7. Exact BUF_SPACE == 1 steal boundary, ~1 MiB steady-state clamp, and
#    >1 MiB incompressible Zstd + >1 MiB user header + v6 checksum combination.
{
    # Payloads whose exact packet length hits 62..65, 125..130, and 65534..65538 bytes
    for my $len ( 50 .. 60, 114 .. 125, 65520 .. 65538, 1_048_565 .. 1_048_585 ) {
        my $s   = 'X' x $len;
        my $pkt = encode_sereal($s);
        is( $dec->decode($pkt), $s, "exact boundary len=$len encode_sereal roundtrip" );
    }

    # Steady-state ~980 KiB .. 1,040 KiB packets on a reusable encoder
    my $re_1m = Sereal::Encoder->new();
    for my $sz ( 980_000, 1_000_000, 1_040_000, 990_000 ) {
        my $blob = 'M' x $sz;
        my $pkt  = $re_1m->encode( { data => $blob } );
        is_deeply(
            $dec->decode($pkt), { data => $blob },
            "steady-state ~1MiB ($sz B) reuse roundtrip"
        );
    }

    # Combined >1 MiB user header + >1 MiB incompressible Zstd body + v6 checksum
    my @chunks;
    my $v = 987654321;
    for ( 1 .. 300_000 ) {
        $v = ( $v * 1103515245 + 12345 ) & 0x7fffffff;
        push @chunks, pack( 'N', $v );
    }
    my $incomp_1_2m = join( '', @chunks );        # 1.2 MB incompressible
    my $hdr_1_1m    = { h => 'H' x 1_100_000 };
    my $ez          = Sereal::Encoder->new( {
        protocol_version   => 6,
        compress           => SRL_ZSTD,
        compress_threshold => 0,
        checksum_key       => 'fedcba9876543210',
    } );
    my $dz        = Sereal::Decoder->new( { checksum_key => 'fedcba9876543210' } );
    my $pkt_combo = $ez->encode( $incomp_1_2m, $hdr_1_1m );
    is_deeply(
        $dz->decode_with_header($pkt_combo),
        [ $hdr_1_1m, $incomp_1_2m ],
        'combined >1 MiB header + >1 MiB incompressible Zstd body + v6 checksum roundtrip'
    );
    is_deeply(
        $dz->decode_with_header( $ez->encode( 'tiny_after', { ok => 1 } ) ),
        [ { ok => 1 }, 'tiny_after' ],
        'reusable Zstd+checksum encoder clean after oversized combined packet'
    );
}

done_testing();
