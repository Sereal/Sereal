#!perl
use strict;
use warnings;
use Config;
use File::Spec;
use Scalar::Util qw(blessed reftype);

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet            qw(:all);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Test::More;

my $hdr6 = Header(6);
my $hdr7 = Header(7);
my $hdr2 = Header(2);

sub pack_arrayref {
    my (@items) = @_;
    my $n = scalar @items;
    if ( $n <= 15 ) {
        return chr( SRL_HDR_ARRAYREF_LOW + $n ) . join( '', @items );
    }
    return chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint($n) . join( '', @items );
}

sub pack_plain_array {
    my (@items) = @_;
    return chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint( scalar @items ) . join( '', @items );
}

sub pack_short_bin {
    my ($s) = @_;
    my $len = length($s);
    die "too long for SHORT_BINARY: $len" if $len > 31;
    return chr( SRL_HDR_SHORT_BINARY_LOW + $len ) . $s;
}

sub pack_double_native {
    my ($d) = @_;
    return chr(SRL_HDR_DOUBLE) . pack( 'd', $d );
}

sub pack_double_le {
    my ($d) = @_;
    return chr(SRL_HDR_DOUBLE) . pack( 'd<', $d );
}

sub pack_double_be {
    my ($d) = @_;
    return chr(SRL_HDR_DOUBLE) . pack( 'd>', $d );
}

# 1. All untracked leaf fast-path tags in both ARRAYREF_n and REFN+ARRAY
{
    my @pos_tags    = map { chr( SRL_HDR_POS_LOW + $_ ) } 0 .. 15;
    my @neg_tags    = map { chr( SRL_HDR_NEG_LOW + $_ ) } 0 .. 15;    # -16..-1
    my @varint_vals = ( 0, 15, 16, 127, 128, 16384, 0x7fffffff );
    if ( $Config{uvsize} >= 8 ) {
        no warnings 'portable';
        push @varint_vals, 0x7fffffffffffffff, 0x8000000000000000, ~0;
    }
    else {
        push @varint_vals, 0x80000000, ~0;
    }
    my @varint_tags = map { chr(SRL_HDR_VARINT) . varint($_) } @varint_vals;
    my @sstr_vals   = map {
        my $l = $_;
        join '', map { chr( ( $_ * 17 + $l ) & 0xff ) } 0 .. $l - 1;
    } 0 .. 31;
    my @sstr_tags  = map { pack_short_bin($_) } @sstr_vals;
    my @undef_tags = ( chr(SRL_HDR_UNDEF), chr(SRL_HDR_CANONICAL_UNDEF) );
    my @dbl_vals   = ( 0.0, -0.0, 1.5, -3.25, 1e100, -1e-100 );
    my @dbl_tags   = map { pack_double_native($_) } @dbl_vals;

    my @all_tags = (
        @pos_tags, @neg_tags, @varint_tags, @sstr_tags, @undef_tags, @dbl_tags,
    );
    my @expected = (
        ( 0 .. 15 ), ( -16 .. -1 ), @varint_vals, @sstr_vals, ( undef, undef ), @dbl_vals,
    );

    my $got = decode_sereal( $hdr6 . pack_arrayref(@all_tags) );
    is( scalar(@$got), scalar(@expected), 'all leaf tags array length matches' );
    for my $i ( 0 .. $#expected ) {
        if ( !defined $expected[$i] ) {
            ok( !defined $got->[$i], "element $i is undef" );
        }
        elsif ( $i >= 16 + 16 + @varint_vals && $i < 16 + 16 + @varint_vals + @sstr_vals ) {
            is( $got->[$i], $expected[$i], "short_binary element $i matches" );
            ok( !utf8::is_utf8( $got->[$i] ), "short_binary element $i is not utf8" );
        }
        else {
            ok(
                defined( $got->[$i] ) && $got->[$i] == $expected[$i],
                "numeric element $i matches"
            );
        }
    }

    # Check UV > IV_MAX bit-exactness on 64-bit
    if ( $Config{uvsize} >= 8 ) {
        no warnings 'portable';
        my $uv_idx = 16 + 16 + @varint_vals - 2;    # 0x8000000000000000
        is(
            sprintf( '%u', $got->[$uv_idx] ), '9223372036854775808',
            'IV_MAX+1 UV preserved exactly'
        );
        is(
            sprintf( '%u', $got->[ $uv_idx + 1 ] ), '18446744073709551615',
            'UV_MAX preserved exactly'
        );
    }

    # Also test compact ARRAYREF_0..15 specifically
    for my $n ( 0 .. 15 ) {
        my @slice   = @all_tags[ 0 .. $n - 1 ];
        my $sub_got = decode_sereal( $hdr6 . pack_arrayref(@slice) );
        is( scalar(@$sub_got), $n, "ARRAYREF_$n length matches" );
        for my $i ( 0 .. $n - 1 ) {
            is( $sub_got->[$i], $expected[$i], "ARRAYREF_$n elem $i matches" );
        }
    }

    # Portable v7 little-endian doubles and big-endian v6 override
    my $v7_dbls = decode_sereal( $hdr7 . pack_plain_array( map { pack_double_le($_) } @dbl_vals ) );
    is_deeply( $v7_dbls, \@dbl_vals, 'v7 portable little-endian doubles in array fast path' );

    my $be_dec = Sereal::Decoder->new( { legacy_float_byteorder => 'big' } );
    my $v6_be = $be_dec->decode( $hdr6 . pack_plain_array( map { pack_double_be($_) } @dbl_vals ) );
    is_deeply( $v6_be, \@dbl_vals, 'v6 big-endian float override in array fast path' );

    # CANONICAL_UNDEF rejected on protocol < 3 inside array fast path
    my $ok_v2 = eval {
        decode_sereal(
            $hdr2 . pack_arrayref( chr( SRL_HDR_POS + 1 ), chr(SRL_HDR_CANONICAL_UNDEF) ) );
        1;
    };
    ok( !$ok_v2, 'CANONICAL_UNDEF in v2 array croaks' );
    like(
        $@, qr/CANONICAL_UNDEF tag requires protocol version 3/,
        'CANONICAL_UNDEF v2 error message'
    );
}

# 2. Mixed with tracked elements, ALIAS, COPY, nested containers, objects, PAD, booleans
{
    # Body offsets in v6 start after header: body_pos = pos - 1, so first body tag is offset 1.
    # Let's build: REFN ARRAY(16):
    #   [0] tracked POS(7)         @ offset 4
    #   [1] ALIAS(4)
    #   [2] tracked SHORT_BINARY("abc") @ offset 6 (takes 4 bytes: tag + "abc")
    #   [3] ALIAS(6)
    #   [4] COPY(6)
    #   [5] tracked VARINT(999)    @ offset 14 (tag + 2-byte varint)
    #   [6] ALIAS(14)
    #   [7] tracked DOUBLE(2.5)    @ offset 19 (tag + 8 bytes)
    #   [8] ALIAS(19)
    #   [9] tracked UNDEF          @ offset 30
    #   [10] ALIAS(30)
    #   [11] nested ARRAYREF_2 [1, "z"]
    #   [12] nested HASHREF_1 { k => 42 }
    #   [13] OBJECT("Foo") HASHREF_1 { a => -1 }
    #   [14] PAD + POS(3)
    #   [15] TRUE
    my $body = '';
    $body .= chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint(16);             # 3 bytes: offsets 1, 2, 3
    my $ofs_pos = length($body) + 1;
    $body .= chr( SRL_HDR_POS + 7 | TRACK_FLAG );                             # 1 byte
    $body .= chr(SRL_HDR_ALIAS) . varint($ofs_pos);                           # 2 bytes
    my $ofs_str = length($body) + 1;
    $body .= chr( ( SRL_HDR_SHORT_BINARY_LOW + 3 ) | TRACK_FLAG ) . 'abc';    # 4 bytes
    $body .= chr(SRL_HDR_ALIAS) . varint($ofs_str);                           # 2 bytes
    $body .= chr(SRL_HDR_COPY) . varint($ofs_str);                            # 2 bytes
    my $ofs_var = length($body) + 1;
    $body .= chr( SRL_HDR_VARINT | TRACK_FLAG ) . varint(999);                # 3 bytes
    $body .= chr(SRL_HDR_ALIAS) . varint($ofs_var);                           # 2 bytes
    my $ofs_dbl = length($body) + 1;
    $body .= chr( SRL_HDR_DOUBLE | TRACK_FLAG ) . pack( 'd', 2.5 );           # 9 bytes
    $body .= chr(SRL_HDR_ALIAS) . varint($ofs_dbl);                           # 2 bytes
    my $ofs_und = length($body) + 1;
    $body .= chr( SRL_HDR_UNDEF | TRACK_FLAG );                                                    # 1 byte
    $body .= chr(SRL_HDR_ALIAS) . varint($ofs_und);                                                # 2 bytes
    $body .= pack_arrayref( chr( SRL_HDR_POS + 1 ), pack_short_bin('z') );
    $body .= chr( SRL_HDR_HASHREF + 1 ) . pack_short_bin('k') . chr(SRL_HDR_VARINT) . varint(42);
    $body .=
          chr(SRL_HDR_OBJECT)
        . pack_short_bin('Foo')
        . chr( SRL_HDR_HASHREF + 1 )
        . pack_short_bin('a')
        . chr( SRL_HDR_NEG_LOW + 15 );
    $body .= chr(SRL_HDR_PAD) . chr( SRL_HDR_POS + 3 );
    $body .= chr(SRL_HDR_TRUE);

    my $got = decode_sereal( $hdr6 . $body );
    is( scalar(@$got), 16, 'mixed array length is 16' );
    is( $got->[0],     7,  'tracked POS decoded' );
    ok( \$got->[0] == \$got->[1], 'ALIAS to tracked POS shares SV' );
    is( $got->[2], 'abc', 'tracked SHORT_BINARY decoded' );
    ok( \$got->[2] == \$got->[3], 'ALIAS to tracked SHORT_BINARY shares SV' );
    is( $got->[4], 'abc', 'COPY of SHORT_BINARY matches value' );
    ok( \$got->[2] != \$got->[4], 'COPY of SHORT_BINARY is distinct SV' );
    is( $got->[5], 999, 'tracked VARINT decoded' );
    ok( \$got->[5] == \$got->[6], 'ALIAS to tracked VARINT shares SV' );
    is( $got->[7], 2.5, 'tracked DOUBLE decoded' );
    ok( \$got->[7] == \$got->[8],  'ALIAS to tracked DOUBLE shares SV' );
    ok( !defined $got->[9],        'tracked UNDEF decoded' );
    ok( \$got->[9] == \$got->[10], 'ALIAS to tracked UNDEF shares SV' );
    is_deeply( $got->[11], [ 1, 'z' ],  'nested ARRAYREF decoded' );
    is_deeply( $got->[12], { k => 42 }, 'nested HASHREF decoded' );
    is( blessed( $got->[13] ), 'Foo', 'nested OBJECT blessed' );
    is( $got->[13]{a},         -1,    'nested OBJECT content decoded' );
    is( $got->[14],            3,     'PAD + POS decoded' );
    ok( $got->[15], 'TRUE decoded' );
}

# 3. set_readonly and set_readonly_scalars
{
    my @uv_item = ( $Config{uvsize} >= 8 )
        ? (
        do { no warnings 'portable'; chr(SRL_HDR_VARINT) . varint(0x8000000000000001) }
        )
        : ( chr(SRL_HDR_VARINT) . varint(0x80000001) );

    my $pkt = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 5 ),
        chr( SRL_HDR_NEG_LOW + 10 ),
        chr(SRL_HDR_VARINT) . varint(500),
        @uv_item,
        pack_short_bin('hello'),
        chr(SRL_HDR_UNDEF),
        chr(SRL_HDR_CANONICAL_UNDEF),
        pack_double_native(4.25),
        pack_arrayref( chr( SRL_HDR_POS + 1 ) ),
    );

    my $ro_all = decode_sereal( $pkt, { set_readonly => 1 } );
    ok( Internals::SvREADONLY(@$ro_all),             'set_readonly: outer array is readonly' );
    ok( Internals::SvREADONLY( @{ $ro_all->[-1] } ), 'set_readonly: nested array is readonly' );
    for my $i ( 0 .. $#$ro_all ) {
        ok( Internals::SvREADONLY( $ro_all->[$i] ), "set_readonly: elem $i is readonly" );
    }

    my $ro_sc = decode_sereal( $pkt, { set_readonly_scalars => 1 } );
    ok( !Internals::SvREADONLY(@$ro_sc), 'set_readonly_scalars: outer ARRAYREF is writable' );
    ok(
        !Internals::SvREADONLY( @{ $ro_sc->[-1] } ),
        'set_readonly_scalars: nested ARRAYREF is writable'
    );
    for my $i ( 0 .. $#$ro_sc - 1 ) {
        ok(
            Internals::SvREADONLY( $ro_sc->[$i] ),
            "set_readonly_scalars: leaf elem $i is readonly"
        );
    }
    ok(
        !Internals::SvREADONLY( $ro_sc->[-1] ),
        'set_readonly_scalars: ref slot is not forced readonly'
    );
}

# 4. alias_smallint, alias_varint_under, and use_undef
{
    my $pkt = $hdr6 . pack_plain_array(
        ( map { chr( SRL_HDR_POS_LOW + $_ ) } 0 .. 15 ),
        ( map { chr( SRL_HDR_NEG_LOW + $_ ) } 0 .. 15 ),
        ( map { chr( SRL_HDR_POS_LOW + $_ ) } 0 .. 15 ),
        ( map { chr( SRL_HDR_NEG_LOW + $_ ) } 0 .. 15 ),
        chr(SRL_HDR_VARINT) . varint(0),
        chr(SRL_HDR_VARINT) . varint(15),
        chr(SRL_HDR_VARINT) . varint(16),
        chr(SRL_HDR_VARINT) . varint(99),
        chr(SRL_HDR_VARINT) . varint(99),
        chr(SRL_HDR_VARINT) . varint(100),
        chr(SRL_HDR_VARINT) . varint(100),
    );

    my $a1 = decode_sereal( $pkt, { alias_smallint => 1 } );
    for my $i ( 0 .. 31 ) {
        ok( \$a1->[$i] == \$a1->[ $i + 32 ], "alias_smallint: slot $i aliased to " . ( $i + 32 ) );
        ok( Internals::SvREADONLY( $a1->[$i] ), "alias_smallint: slot $i is readonly" );
    }
    ok( \$a1->[0] == \$a1->[64],  'alias_smallint: VARINT(0) shares alias with POS(0)' );
    ok( \$a1->[15] == \$a1->[65], 'alias_smallint: VARINT(15) shares alias with POS(15)' );
    ok( !Internals::SvREADONLY( $a1->[66] ), 'alias_smallint: VARINT(16) is not readonly' );

    my $a2 = decode_sereal( $pkt, { alias_varint_under => 100 } );
    ok( \$a2->[67] == \$a2->[68],            'alias_varint_under=100: VARINT(99) is aliased' );
    ok( Internals::SvREADONLY( $a2->[67] ),  'alias_varint_under=100: VARINT(99) is readonly' );
    ok( \$a2->[69] != \$a2->[70],            'alias_varint_under=100: VARINT(100) is NOT aliased' );
    ok( !Internals::SvREADONLY( $a2->[69] ), 'alias_varint_under=100: VARINT(100) is writable' );

    my $upkt = $hdr6 . pack_arrayref(
        chr(SRL_HDR_UNDEF),
        chr(SRL_HDR_CANONICAL_UNDEF),
        chr(SRL_HDR_UNDEF),
    );
    my $u_on = decode_sereal( $upkt, { use_undef => 1 } );
    ok(
        \$u_on->[0] == \undef && \$u_on->[1] == \undef && \$u_on->[2] == \undef,
        'use_undef=1: UNDEF and CANONICAL_UNDEF alias PL_sv_undef'
    );

    my $u_off = decode_sereal( $upkt, { use_undef => 0 } );
    ok(
        \$u_off->[0] != \$u_off->[1] && \$u_off->[0] != \undef,
        'use_undef=0: UNDEF elements are distinct scalars'
    );
    ok( !Internals::SvREADONLY( $u_off->[0] ), 'use_undef=0: UNDEF element is writable' );
}

# 5. max_string_length and max_recursion_depth exact boundaries
{
    my $str_ok_pkt = $hdr6 . pack_arrayref(
        pack_short_bin(''),
        pack_short_bin('abcde'),
    );
    is_deeply(
        decode_sereal( $str_ok_pkt, { max_string_length => 5 } ),
        [ '', 'abcde' ],
        'max_string_length=5 allows SHORT_BINARY of len 5'
    );

    my $str_bad_pkt = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 1 ),
        pack_short_bin('abcde'),
        pack_short_bin('abcdef'),
    );
    my $ok_str = eval { decode_sereal( $str_bad_pkt, { max_string_length => 5 } ); 1 };
    ok( !$ok_str, 'max_string_length=5 rejects SHORT_BINARY of len 6 mid-array' );
    like(
        $@, qr/Got input string with length 6, configured maximum is 5/,
        'max_string_length error matches'
    );

    my $flat_leaves = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 1 ),
        pack_short_bin('ab'),
        chr(SRL_HDR_UNDEF),
        chr(SRL_HDR_VARINT) . varint(1000),
        pack_double_native(1.5),
    );
    my $ok_d1 = eval { decode_sereal( $flat_leaves, { max_recursion_depth => 1 } ); 1 };
    ok( !$ok_d1, 'max_recursion_depth=1 rejects 1-level array of leaves' );
    like( $@, qr/Reached recursion limit \(1\)/, 'max_recursion_depth=1 error matches' );
    my $got_d2 = decode_sereal( $flat_leaves, { max_recursion_depth => 2 } );
    is_deeply(
        $got_d2, [ 1, 'ab', undef, 1000, 1.5 ],
        'max_recursion_depth=2 allows 1-level array of leaves'
    );

    my $nested_pkt = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 1 ),
        pack_arrayref( chr( SRL_HDR_POS + 2 ), pack_short_bin('x') ),
        chr( SRL_HDR_POS + 3 ),
    );
    my $ok_nd2 = eval { decode_sereal( $nested_pkt, { max_recursion_depth => 2 } ); 1 };
    ok( !$ok_nd2, 'max_recursion_depth=2 rejects 2-level nested array' );
    like( $@, qr/Reached recursion limit \(2\)/, 'max_recursion_depth=2 error matches' );
    is_deeply(
        decode_sereal( $nested_pkt, { max_recursion_depth => 3 } ),
        [ 1, [ 2, 'x' ], 3 ],
        'max_recursion_depth=3 allows 2-level nested array'
    );
}

# 6. Malformed / truncated leaf arrays mid-loop (cleanup / Valgrind test)
{
    my @bad_cases = ( [
            'truncated SHORT_BINARY mid-loop',
            $hdr6
                . chr( SRL_HDR_ARRAYREF_LOW + 3 )
                . pack_short_bin('12345')
                . chr( SRL_HDR_SHORT_BINARY_LOW + 5 ) . 'ab',
            qr/Unexpected termination of packet/,
        ],
        [
            'truncated VARINT mid-loop',
            $hdr6
                . chr( SRL_HDR_ARRAYREF_LOW + 3 )
                . chr( SRL_HDR_POS + 1 )
                . chr(SRL_HDR_VARINT)
                . "\x80\x80\x80",
            qr/end of packet reached before varint parsed/,
        ],
        [
            'truncated DOUBLE mid-loop',
            $hdr6
                . chr( SRL_HDR_ARRAYREF_LOW + 3 )
                . chr( SRL_HDR_POS + 1 )
                . chr(SRL_HDR_DOUBLE)
                . "\x00\x00\x00\x00",
            qr/Unexpected termination of packet/,
        ],
        [
            'SRL_RDR_DONE mid-loop after multi-byte SHORT_BINARY',

            # Array claims 4 elements, packet has 4 bytes after array tag: SHORT_BINARY(3) "abc" consumes all 4 bytes!
            $hdr6 . chr( SRL_HDR_ARRAYREF_LOW + 4 ) . pack_short_bin('abc'),
            qr/unexpected end of input stream while expecting a single value/,
        ],
        [
            'unknown tag mid-loop after leaf elements',
            $hdr6
                . chr( SRL_HDR_ARRAYREF_LOW + 4 )
                . chr( SRL_HDR_POS + 1 )
                . pack_short_bin('xy')
                . chr(SRL_HDR_RESERVED)
                . chr( SRL_HDR_POS + 2 ),
            qr/Unexpected tag SRL_HDR_RESERVED/,
        ],
    );

    for my $case (@bad_cases) {
        my ( $label, $pkt, $err_re ) = @$case;
        my $ok = eval { decode_sereal($pkt); 1 };
        ok( !$ok, "$label croaks" );
        like( $@, $err_re, "$label error matches" );
    }
}

# 7. Additional option interactions: ARRAYREF + use_undef + readonly,
#    validate_utf8 fallback, incremental bytes_consumed, and re-entrant THAW.
{
    # ARRAYREF_5 with use_undef + set_readonly / set_readonly_scalars + alias_smallint
    my $arr5 = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 2 ),
        chr(SRL_HDR_UNDEF),
        chr(SRL_HDR_CANONICAL_UNDEF),
        chr( SRL_HDR_NEG_LOW + 15 ),
        pack_short_bin('ro'),
    );
    for my $opt_name ( 'set_readonly', 'set_readonly_scalars' ) {
        my $d = Sereal::Decoder->new( {
            $opt_name      => 1,
            use_undef      => 1,
            alias_smallint => 1,
        } );
        my $got = $d->decode($arr5);
        ok(
            Internals::SvREADONLY( $got->[0] ),
            "ARRAYREF_5 $opt_name + alias_smallint: elem 0 readonly"
        );
        ok(
            \$got->[1] == \undef && \$got->[2] == \undef,
            "ARRAYREF_5 $opt_name + use_undef: aliases PL_sv_undef"
        );
        ok(
            Internals::SvREADONLY( $got->[3] ),
            "ARRAYREF_5 $opt_name + alias_smallint: elem 3 readonly"
        );
        ok( Internals::SvREADONLY( $got->[4] ), "ARRAYREF_5 $opt_name: short_binary readonly" );
        is( $got->[4], 'ro', "ARRAYREF_5 $opt_name: short_binary value matches" );
    }

    # validate_utf8 => 1 with valid & invalid STR_UTF8 inside leaf array
    my $valid_utf8_pkt = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 1 ),
        chr(SRL_HDR_STR_UTF8) . varint(3) . "\xe2\x98\x83",
        pack_short_bin('end'),
    );
    my $bad_utf8_pkt = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 1 ),
        chr(SRL_HDR_STR_UTF8) . varint(2) . "\xff\xfe",
    );
    my $d_utf8 = Sereal::Decoder->new( { validate_utf8 => 1 } );
    my $u_got  = $d_utf8->decode($valid_utf8_pkt);
    is( $u_got->[0], 1,          'validate_utf8 leaf array: POS matches' );
    is( $u_got->[1], "\x{2603}", 'validate_utf8 leaf array: valid UTF-8 string decoded' );
    ok( utf8::is_utf8( $u_got->[1] ), 'validate_utf8 leaf array: UTF-8 flag set' );
    is( $u_got->[2], 'end', 'validate_utf8 leaf array: trailing SHORT_BINARY matches' );
    my $u_ok = eval { $d_utf8->decode($bad_utf8_pkt); 1 };
    ok( !$u_ok, 'validate_utf8 leaf array: invalid STR_UTF8 croaks' );
    like( $@, qr/Invalid UTF8 byte sequence/, 'validate_utf8 leaf array: error message matches' );

    # incremental => 1 exact bytes_consumed on leaf array with trailing bytes
    my $leaf_pkt = $hdr6 . pack_arrayref(
        chr( SRL_HDR_POS + 4 ),
        pack_short_bin('xy'),
        chr(SRL_HDR_VARINT) . varint(300),
        chr(SRL_HDR_UNDEF),
    );
    my $d_inc  = Sereal::Decoder->new( { incremental => 1 } );
    my $stream = 'PREFIX' . $leaf_pkt . 'TRAILING';
    my $i_got  = $d_inc->decode_with_offset( $stream, 6 );
    is_deeply( $i_got, [ 4, 'xy', 300, undef ], 'incremental leaf array roundtrips' );
    is( $d_inc->bytes_consumed, length($leaf_pkt), 'incremental leaf array bytes_consumed exact' );

    # Re-entrant THAW preserves flags_readonly and alias_varint_under across _alike clone
    {

        package Local::RegrLeafThaw;
        our ( $DECODER, $INNER_PKT, $INNER_OUT );

        sub THAW {
            my ( $class, $ser, $payload ) = @_;
            $INNER_OUT = $DECODER->decode($INNER_PKT);
            return bless { outer => $payload }, $class;
        }
    }

    my $inner_leaf_pkt = $hdr6 . pack_plain_array(
        chr( SRL_HDR_POS + 0 ),
        chr( SRL_HDR_POS + 1 ),
        pack_short_bin('abc'),
        chr(SRL_HDR_UNDEF),
        chr(SRL_HDR_VARINT) . varint(99),
        chr(SRL_HDR_VARINT) . varint(99),
        chr(SRL_HDR_VARINT) . varint(100),
        chr(SRL_HDR_VARINT) . varint(100),
    );
    my $outer_thaw_pkt =
          $hdr6
        . chr(SRL_HDR_OBJECT_FREEZE)
        . pack_short_bin('Local::RegrLeafThaw')
        . pack_arrayref( chr( SRL_HDR_POS + 7 ) );

    for my $opt_name ( 'set_readonly', 'set_readonly_scalars' ) {
        my $d_thaw = Sereal::Decoder->new( {
            $opt_name          => 1,
            alias_varint_under => 100,
        } );
        local $Local::RegrLeafThaw::DECODER   = $d_thaw;
        local $Local::RegrLeafThaw::INNER_PKT = $inner_leaf_pkt;
        local $Local::RegrLeafThaw::INNER_OUT;

        my $outer = $d_thaw->decode($outer_thaw_pkt);
        my $in    = $Local::RegrLeafThaw::INNER_OUT;
        is( ref($outer), 'Local::RegrLeafThaw', "THAW re-entrancy ($opt_name): outer blessed" );
        is_deeply(
            $in, [ 0, 1, 'abc', undef, 99, 99, 100, 100 ],
            "THAW re-entrancy ($opt_name): inner leaf array values match"
        );
        for my $i ( 0 .. $#$in ) {
            ok(
                Internals::SvREADONLY( $in->[$i] ),
                "THAW re-entrancy ($opt_name): inner elem $i readonly"
            );
        }
        ok(
            \$in->[4] == \$in->[5],
            "THAW re-entrancy ($opt_name): alias_varint_under=100 aliases 99"
        );
        ok(
            \$in->[6] != \$in->[7],
            "THAW re-entrancy ($opt_name): alias_varint_under=100 does not alias 100"
        );
    }
}

done_testing();
