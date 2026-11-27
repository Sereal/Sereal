#!perl
use strict;
use warnings;
use Config;
use File::Spec;
use Scalar::Util qw(reftype);

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet            qw(:all);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Test::More;

my $hdr = Header(6);

# 1. 1-byte and multi-byte varints at exact buffer boundaries
#    (end - pos == 0, 1, 10, 11, and > 11).
{
    # Empty varint right after tag (buf->pos == buf->end)
    for my $tag (
        SRL_HDR_VARINT, SRL_HDR_ZIGZAG, SRL_HDR_NEG_VARINT, SRL_HDR_BINARY,
        SRL_HDR_STR_UTF8
        )
    {
        my $ok = eval { decode_sereal( $hdr . chr($tag) ); 1 };
        ok( !$ok, sprintf( 'truncated empty varint after tag 0x%02x dies', $tag ) );
        like(
            $@, qr/end of packet reached before varint parsed/,
            sprintf( 'tag 0x%02x empty varint error', $tag ) );
    }

    # 1-byte varints at exact end of packet (end - pos == 1 < SRL_MAX_VARINT_LENGTH)
    for my $v ( 0, 1, 15, 16, 127 ) {
        my $pkt = $hdr . chr(SRL_HDR_VARINT) . varint($v);
        my $got = eval { decode_sereal($pkt) };
        is( $@,   '', "1-byte VARINT($v) at exact end decodes cleanly" );
        is( $got, $v, "1-byte VARINT($v) at exact end value matches" );
    }

    # 1-byte varints with exactly 9 trailing bytes (end - pos == 10 < 11)
    # and exactly 10 trailing bytes (end - pos == 11 == SRL_MAX_VARINT_LENGTH)
    for my $trail ( 9, 10 ) {
        my $pad = chr(SRL_HDR_POS) x $trail;
        for my $v ( 0, 1, 127 ) {
            my $pkt =
                  $hdr
                . chr(SRL_HDR_REFN)
                . chr(SRL_HDR_ARRAY)
                . varint( $trail + 1 )
                . chr(SRL_HDR_VARINT)
                . varint($v)
                . $pad;
            my $got = eval { decode_sereal($pkt) };
            is( $@, '', "1-byte VARINT($v) with end-pos=" . ( $trail + 1 ) . " decodes" );
            is(
                $got->[0], $v,
                "1-byte VARINT($v) with end-pos=" . ( $trail + 1 ) . " value matches"
            );
            is( scalar(@$got), $trail + 1, "1-byte VARINT($v) trailing array intact" );
        }
    }

    # Multi-byte varints across all widths at exact end, end - pos == 10, and end - pos >= 11
    my @multi = (
        128,
        16383,
        16384,
        ( 1 << 21 ) - 1,
        ( 1 << 21 ),
        ( 1 << 28 ) - 1,
        ( 1 << 28 ),
        0x7fffffff,
        0xffffffff,
    );
    if ( $Config{uvsize} >= 8 ) {
        no warnings 'portable';
        push @multi,
            ( 1 << 35 ) + 123,
            ( 1 << 49 ) + 456,
            ( 1 << 56 ) + 789,
            0x7fffffffffffffff,
            ~0;
    }

    for my $v (@multi) {
        my $vbytes = varint($v);
        my $vlen   = length($vbytes);

        # Exact end of packet (safe path)
        my $got_end = eval { decode_sereal( $hdr . chr(SRL_HDR_VARINT) . $vbytes ) };
        is( $@, '', "multi-byte VARINT($v) at exact end decodes" );
        ok(
            defined($got_end) && $got_end == $v,
            "multi-byte VARINT($v) at exact end value matches"
        );

        # Exact boundary end - pos == 10 (still safe path, 10 - $vlen trailing POS(0) tags)
        my $pad10 = 10 - $vlen;
        if ( $pad10 >= 0 ) {
            my $pkt10 =
                  $hdr
                . chr(SRL_HDR_REFN)
                . chr(SRL_HDR_ARRAY)
                . varint( $pad10 + 1 )
                . chr(SRL_HDR_VARINT)
                . $vbytes
                . ( chr(SRL_HDR_POS) x $pad10 );
            my $got10 = eval { decode_sereal($pkt10) };
            is( $@, '', "multi-byte VARINT($v) at end-pos==10 decodes" );
            ok(
                defined($got10) && $got10->[0] == $v,
                "multi-byte VARINT($v) at end-pos==10 value matches"
            );
        }

        # Exact boundary end - pos == 11 (nocheck path, 11 - $vlen trailing POS(0) tags)
        my $pad11 = 11 - $vlen;
        my $pkt11 =
              $hdr
            . chr(SRL_HDR_REFN)
            . chr(SRL_HDR_ARRAY)
            . varint( $pad11 + 1 )
            . chr(SRL_HDR_VARINT)
            . $vbytes
            . ( chr(SRL_HDR_POS) x $pad11 );
        my $got11 = eval { decode_sereal($pkt11) };
        is( $@, '', "multi-byte VARINT($v) at end-pos==11 decodes" );
        ok(
            defined($got11) && $got11->[0] == $v,
            "multi-byte VARINT($v) at end-pos==11 value matches"
        );
    }

    # Truncated multi-byte varint at buffer boundary (continuation bit set on final byte)
    for my $bad ( "\x80", "\x81\x80", "\x80" x ( $Config{uvsize} >= 8 ? 9 : 4 ) ) {
        my $ok = eval { decode_sereal( $hdr . chr(SRL_HDR_VARINT) . $bad ); 1 };
        ok( !$ok, 'truncated multi-byte varint at buffer boundary dies' );
        like(
            $@, qr/end of packet reached before varint parsed/,
            'truncated varint error preserved'
        );
    }

    # Unterminated varint with >= 11 bytes available (hits nocheck overrun error)
    my $unterm    = ( "\x80" x 11 );
    my $ok_unterm = eval { decode_sereal( $hdr . chr(SRL_HDR_VARINT) . $unterm ); 1 };
    ok( !$ok_unterm, 'unterminated 11-byte varint dies' );
    like(
        $@,
        qr/varint (?:not terminated in time|overflows U32)/,
        'unterminated varint nocheck error preserved'
    );

    # Overflowing varint on both safe path (end - pos == 10 on 64-bit / 5 on 32-bit)
    # and nocheck path (end - pos >= 11)
    my $over    = ( "\xff" x ( $Config{uvsize} >= 8 ? 9 : 4 ) ) . "\x7f";
    my $ok_safe = eval { decode_sereal( $hdr . chr(SRL_HDR_VARINT) . $over ); 1 };
    ok( !$ok_safe, 'overflowing varint at exact end dies' );
    like( $@, qr/varint too big/, 'overflowing varint safe-path error preserved' );

    my $ok_nocheck =
        eval { decode_sereal( $hdr . chr(SRL_HDR_VARINT) . $over . ( "\x00" x 11 ) ); 1 };
    ok( !$ok_nocheck, 'overflowing varint with >=11 bytes dies' );
    like( $@, qr/varint (?:too big|overflows U32)/, 'overflowing varint nocheck error preserved' );
}

# 2. Small ints (POS/NEG), NEG_VARINT, ZIGZAG, and srl_alias_iv out-of-line path
{
    my $arr = array( map { $_ < 0 ? chr( 32 + $_ ) : chr($_) } ( -16 .. 15 ) );
    my $pkt = $hdr . $arr;

    my $plain = decode_sereal($pkt);
    is_deeply( $plain, [ -16 .. 15 ], 'small ints -16..15 decode accurately' );

    my $dec_alias = Sereal::Decoder->new( { alias_smallint => 1 } );
    my $aliased   = $dec_alias->decode($pkt);
    is_deeply( $aliased, [ -16 .. 15 ], 'aliased small ints -16..15 decode accurately' );
    eval { $aliased->[0] = 999 };
    like( $@, qr/Modification of a read-only value/, 'aliased small int is readonly' );

    my $dec_varint_alias = Sereal::Decoder->new( { alias_varint_under => 100 } );
    my $v_pkt            = $hdr . array(
        chr(SRL_HDR_VARINT) . varint(50),
        chr(SRL_HDR_VARINT) . varint(100),
        chr(SRL_HDR_ZIGZAG) . varint(1),         # -1
        chr(SRL_HDR_ZIGZAG) . varint(100),       # 50
        chr(SRL_HDR_NEG_VARINT) . varint(17),    # -17
    );
    my $va = $dec_varint_alias->decode($v_pkt);
    is_deeply(
        $va, [ 50, 100, -1, 50, -17 ],
        'alias_varint_under decodes varints/zigzags/neg_varints'
    );
    eval { $va->[0] = 0 };
    like(
        $@, qr/Modification of a read-only value/,
        'varint under alias_varint_under is readonly'
    );
    eval { $va->[1] = 0 };
    is( $@, '', 'varint == alias_varint_under is mutable' );
    eval { $va->[4] = 0 };
    is( $@, '', 'negative varint below -16 is mutable' );
}

# 3. Strings, undefs (with/without use_undef), and all out-of-line cold tags
{
    my $s0  = short_string("");
    my $s31 = short_string( "x" x 31 );
    my $bin = chr(SRL_HDR_BINARY) . varint(40) . ( "y" x 40 );
    my $u8  = chr(SRL_HDR_STR_UTF8) . varint(4) . "\xc3\xa9\xc3\xa0";
    my $got = decode_sereal( $hdr . array( $s0, $s31, $bin, $u8 ) );
    is( $got->[0], "",             'empty short_binary' );
    is( $got->[1], "x" x 31,       'max len 31 short_binary' );
    is( $got->[2], "y" x 40,       'binary string' );
    is( $got->[3], "\x{e9}\x{e0}", 'utf8 string' );
    ok( utf8::is_utf8( $got->[3] ), 'utf8 flag preserved' );

    my $u_pkt = $hdr . array(
        chr(SRL_HDR_UNDEF),
        chr(SRL_HDR_CANONICAL_UNDEF),
        hash(
            short_string('a'), chr(SRL_HDR_UNDEF), short_string('b'), chr(SRL_HDR_CANONICAL_UNDEF)
        ),
    );
    my $u_mut = decode_sereal($u_pkt);
    is_deeply( $u_mut, [ undef, undef, { a => undef, b => undef } ], 'undef elements decode' );
    $u_mut->[0] = 1;
    is( $u_mut->[0], 1, 'non-use_undef array element is mutable' );

    my $dec_uu = Sereal::Decoder->new( { use_undef => 1 } );
    my $u_ro   = $dec_uu->decode($u_pkt);
    is_deeply( $u_ro, [ undef, undef, { a => undef, b => undef } ], 'use_undef elements decode' );
    eval { $u_ro->[0] = 1 };
    like( $@, qr/Modification of a read-only value/, 'use_undef array element uses PL_sv_undef' );
    eval { $u_ro->[2]{a} = 1 };
    like(
        $@, qr/Modification of (?:a read-only|non-creatable hash) value/,
        'use_undef hash value uses PL_sv_undef'
    );

    # Cold tags: FLOAT, DOUBLE, REGEXP, EXTEND (VSTRING_BINARY, VSTRING_UTF8, UNDEF_DEADLY), COPY
    my @cold_elems = (
        chr(SRL_HDR_FLOAT) . pack( 'f', 1.5 ),
        chr(SRL_HDR_DOUBLE) . pack( 'd', -3.25 ),
        chr(SRL_HDR_REFN) . chr(SRL_HDR_REGEXP) . short_string('foo\d+') . short_string('im'),
        chr(SRL_HDR_EXTEND) . chr(SRL_EXT_VSTRING_BINARY) . varint(6) . 'v1.2.3',
        chr(SRL_HDR_EXTEND) . chr(SRL_EXT_VSTRING_UTF8) . varint(8) . 'v1.2.300',
        chr(SRL_HDR_EXTEND) . chr(SRL_EXT_UNDEF_DEADLY),
    );
    my $copy_target_ofs = 1 + 1 + length( join( '', @cold_elems ) );
    push @cold_elems, short_string('copy'), chr(SRL_HDR_COPY) . varint($copy_target_ofs);

    my $cold = decode_sereal( $hdr . array(@cold_elems) );
    ok( abs( $cold->[0] - 1.5 ) < 1e-6,      'cold FLOAT tag' );
    ok( abs( $cold->[1] - (-3.25) ) < 1e-12, 'cold DOUBLE tag' );
    is( reftype( $cold->[2] ), 'REGEXP', 'cold REGEXP tag' );
    like( 'FOO123', $cold->[2], 'cold REGEXP modifiers work' );
    is( reftype( \$cold->[3] ),       'VSTRING', 'cold VSTRING_BINARY tag' );
    is( sprintf( '%vd', $cold->[3] ), '1.2.3',   'cold VSTRING_BINARY value' );
    is( reftype( \$cold->[4] ),       'VSTRING', 'cold VSTRING_UTF8 tag' );
    is( sprintf( '%vd', $cold->[4] ), '1.2.300', 'cold VSTRING_UTF8 value' );
    is( $cold->[5], undef,  'cold UNDEF_DEADLY decodes to undef under default policy' );
    is( $cold->[6], 'copy', 'copy target string decoded' );
    is( $cold->[7], 'copy', 'cold COPY tag decoded target string' );

    # Cold FLOAT_128 tag (v7 portable IEEE 754 binary128 +1.0 little-endian)
    my $f128_one = "\x00" x 14 . "\xff\x3f";
    my $got_f128 = decode_sereal( Header(7) . chr(SRL_HDR_FLOAT_128) . $f128_one );
    ok( abs( $got_f128 - 1.0 ) < 1e-12, 'cold FLOAT_128 tag' );

    my $res_ok = eval { decode_sereal( $hdr . chr(SRL_HDR_RESERVED) ); 1 };
    ok( !$res_ok, 'RESERVED tag rejected as unexpected' );
    like( $@, qr/Unexpected tag SRL_HDR_RESERVED/, 'RESERVED tag error preserved' );
}

# 4. Deep recursion (depth 200)
{
    my $tree = chr(SRL_HDR_VARINT) . varint(42);
    for my $i ( 1 .. 200 ) {
        $tree = ( $i % 2 ) ? array($tree) : hash( short_string('k'), $tree );
    }
    my $out = eval { decode_sereal( $hdr . $tree ) };
    is( $@, '', 'depth-200 nested array/hash tree decodes' );
    my $cur = $out;
    for my $i ( reverse 1 .. 200 ) {
        $cur = ( $i % 2 ) ? $cur->[0] : $cur->{k};
    }
    is( $cur, 42, 'depth-200 leaf value intact' );
}

done_testing();
