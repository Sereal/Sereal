#!perl
use strict;
use warnings;
use Config;
use File::Spec;
use Scalar::Util qw(dualvar reftype);

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet            qw(:all);
use Sereal::Encoder            qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);
use Test::More;

{

    package Local::FreezeUndef;
    sub FREEZE { return undef; }
    sub THAW   { my ( $class, $ser, @args ) = @_; return bless { thawed => \@args }, $class; }

    package Local::TiedHash;
    require Tie::Hash;
    our @ISA = ('Tie::StdHash');
}

my $have_dec = have_encoder_and_decoder();

# 1. Wire-level checks on undef vs canonical_undef (protocol >= 3)
{
    my $enc = Sereal::Encoder->new( { protocol_version => 6 } );

    # Fresh undef scalars in array & hash use CALL_SRL_DUMP_SV fast path -> SRL_HDR_UNDEF
    my $arr_pkt  = $enc->encode( [ undef, undef ] );
    my $body_pos = 6;                                  # 4-byte magic + 1-byte version + 1-byte header len (0)
    is(
        ord( substr( $arr_pkt, $body_pos, 1 ) ), SRL_HDR_ARRAYREF + 2,
        '2-elem array uses ARRAYREF+2'
    );
    is(
        ord( substr( $arr_pkt, $body_pos + 1, 1 ) ), SRL_HDR_UNDEF,
        'fresh array undef emits SRL_HDR_UNDEF'
    );
    is(
        ord( substr( $arr_pkt, $body_pos + 2, 1 ) ), SRL_HDR_UNDEF,
        'second fresh array undef emits SRL_HDR_UNDEF'
    );

    # Sparse array slots (NULL / PL_sv_undef) emit SRL_HDR_CANONICAL_UNDEF
    # Note: \@sparse has refcount 2 (pad + ref), so header is REFN + ARRAY + len(2) + elem0
    my @sparse;
    $sparse[1] = 7;
    my $sparse_pkt = $enc->encode( \@sparse );
    is( ord( substr( $sparse_pkt, $body_pos, 1 ) ), SRL_HDR_REFN, 'named array starts with REFN' );
    is(
        ord( substr( $sparse_pkt, $body_pos + 1, 1 ) ), SRL_HDR_ARRAY,
        'named array uses ARRAY tag'
    );
    is( ord( substr( $sparse_pkt, $body_pos + 2, 1 ) ), 2, 'sparse array length is 2' );
    is(
        ord( substr( $sparse_pkt, $body_pos + 3, 1 ) ), SRL_HDR_CANONICAL_UNDEF,
        'sparse array slot emits SRL_HDR_CANONICAL_UNDEF'
    );

    # Direct PL_sv_undef via top-level encode emits SRL_HDR_CANONICAL_UNDEF
    my $top_undef = $enc->encode(undef);
    is(
        ord( substr( $top_undef, $body_pos, 1 ) ),
        SRL_HDR_CANONICAL_UNDEF,
        'top-level PL_sv_undef emits SRL_HDR_CANONICAL_UNDEF'
    );

    # Shared undef scalar (SvREFCNT > 1 via references) must still be tracked and referenced
    my $shared_u = undef;
    my $ref_pkt  = $enc->encode( [ \$shared_u, \$shared_u ] );
    is(
        ord( substr( $ref_pkt, $body_pos + 1, 1 ) ), SRL_HDR_REFN,
        'first ref to shared undef uses REFN'
    );
    is(
        ord( substr( $ref_pkt, $body_pos + 2, 1 ) ), SRL_HDR_UNDEF | SRL_HDR_TRACK_FLAG,
        'shared undef referent is tracked'
    );
    is(
        ord( substr( $ref_pkt, $body_pos + 3, 1 ) ), SRL_HDR_REFP,
        'second ref to shared undef emits SRL_HDR_REFP'
    );

    # Reference to PL_sv_undef (\undef) emits REFN + SRL_HDR_CANONICAL_UNDEF
    my $ref_canon = $enc->encode( \undef );
    is( ord( substr( $ref_canon, $body_pos, 1 ) ), SRL_HDR_REFN, '\undef starts with REFN' );
    is(
        ord( substr( $ref_canon, $body_pos + 1, 1 ) ), SRL_HDR_CANONICAL_UNDEF,
        '\undef referent emits SRL_HDR_CANONICAL_UNDEF'
    );
}

# 2. Wire-level check that dedupe_strings and aliased_dedupe_strings trigger out-of-line dedupe
{
    my $long       = 'repeated_string_payload_for_dedupe';
    my $enc_dedupe = Sereal::Encoder->new( { dedupe_strings         => 1 } );
    my $enc_alias  = Sereal::Encoder->new( { aliased_dedupe_strings => 1 } );
    my $enc_plain  = Sereal::Encoder->new();

    my $p_plain  = $enc_plain->encode( [ $long, $long, $long ] );
    my $p_dedupe = $enc_dedupe->encode( [ $long, $long, $long ] );
    my $p_alias  = $enc_alias->encode( [ $long, $long, $long ] );

    cmp_ok(
        length($p_dedupe), '<', length($p_plain) - length($long),
        'dedupe_strings shrinks repeated strings'
    );
    cmp_ok(
        length($p_alias), '<', length($p_plain) - length($long),
        'aliased_dedupe_strings shrinks repeated strings'
    );
}

# 3. Cold out-of-line unsupported type handling (srl_handle_unsupported_svtype)
{
    my $code       = sub { 1 };
    my $enc_strict = Sereal::Encoder->new();
    eval { $enc_strict->encode($code) };
    like( $@, qr/not representable/, 'unsupported CODE ref croaks by default' );

    my $enc_undef = Sereal::Encoder->new( { undef_unknown => 1 } );
    my $pkt_u     = eval { $enc_undef->encode( [$code] ) };
    is( $@, '', 'undef_unknown encodes CODE ref without error' );

    my $enc_str = Sereal::Encoder->new( { stringify_unknown => 1 } );
    my $pkt_s   = eval { $enc_str->encode( [$code] ) };
    is( $@, '', 'stringify_unknown encodes CODE ref without error' );
}

SKIP: {
    skip 'Sereal::Decoder not available or incompatible', 1 unless $have_dec;

    require Sereal::Decoder;
    my $dec        = Sereal::Decoder->new();
    my $enc        = Sereal::Encoder->new();
    my $enc_v5     = Sereal::Encoder->new( { protocol_version       => 5 } );
    my $enc_dedupe = Sereal::Encoder->new( { dedupe_strings         => 1 } );
    my $enc_alias  = Sereal::Encoder->new( { aliased_dedupe_strings => 1 } );

    # 4. Small & large ints (1-byte varint < 0x80 fast path and multi-byte >= 0x80 path)
    my @ints = (
        -16 .. 15,
        -17, -64, -65, -127, -128, -129,  -16384, -32768, -2147483648,
        16,  63,  64,  127,  128,  16383, 16384,  2147483647,
    );
    if ( $Config{ivsize} >= 8 ) {
        push @ints, -9223372036854775807 - 1, 9223372036854775807, ~0;
    }
    is_deeply( $dec->decode( $enc->encode( \@ints ) ), \@ints, 'v6 roundtrip small & large ints' );
    is_deeply(
        $dec->decode( $enc_v5->encode( \@ints ) ), \@ints,
        'v5 roundtrip small & large ints (zigzag)'
    );

    # 5. Plain strings (empty, short <= 3, <= 31, > 31, UTF-8) with/without dedupe & alias
    my @strs = (
        '', 'a', 'abc', 'abcd',
        'short string', 'x' x 31, 'y' x 32,
        'long repeated string ' x 5,
        "\x{2603} snowman",
        'abc', 'abcd', 'long repeated string ' x 5, "\x{2603} snowman",
    );
    for my $pair (
        [ 'default',        $enc ],
        [ 'dedupe_strings', $enc_dedupe ],
        [ 'aliased_dedupe', $enc_alias ],
        )
    {
        my ( $label, $e ) = @$pair;
        my $rt = $dec->decode( $e->encode( \@strs ) );
        is_deeply( $rt, \@strs, "roundtrip plain strings ($label)" );
        ok( utf8::is_utf8( $rt->[8] ),  "roundtrip utf8 flag preserved ($label)" );
        ok( utf8::is_utf8( $rt->[12] ), "roundtrip repeated utf8 flag preserved ($label)" );
    }

    # 6. Dualvars & numeric-evaluated strings (POK+IOK, POK+NOK, POK+IOK+NOK)
    my $s_zero = '0';
    my $n_zero = $s_zero + 0;
    my $s_int  = '12345';
    my $n_int  = $s_int + 0;
    my $s_neg  = '-99';
    my $n_neg  = $s_neg + 0;

    my $dv_canonical     = dualvar( 42,           '42' );
    my $dv_noncanonical  = dualvar( 42,           'forty-two' );
    my $dv_leading_zero  = dualvar( 7,            '07' );
    my $dv_zero_noncanon = dualvar( 0,            '00' );
    my $dv_zero_empty    = dualvar( 0,            '' );
    my $dv_neg_noncanon  = dualvar( -5,           '-05' );
    my $dv_plus          = dualvar( 5,            '+5' );
    my $dv_float_str     = dualvar( 1.5,          '1.50' );
    my $dv_float_long    = dualvar( 123456789.25, '123456789.25' );

    # Scalar with POK + IOK + NOK (exercises scratch_sv comparison path)
    my $pok_iok_nok = '12345';
    my $tmp_num     = $pok_iok_nok + 0.0;
    my $tmp_int     = int($pok_iok_nok);

    my $rt_dv = $dec->decode(
        $enc->encode( [
                $s_zero,           $s_int,           $s_neg,
                $dv_canonical,     $dv_noncanonical, $dv_leading_zero,
                $dv_zero_noncanon, $dv_zero_empty,   $dv_neg_noncanon,
                $dv_plus,          $dv_float_str,    $dv_float_long,
                $pok_iok_nok,
            ] ) );
    is( $rt_dv->[0],  0,            'POK+IOK "0" roundtrips as 0' );
    is( $rt_dv->[1],  12345,        'POK+IOK "12345" roundtrips as 12345' );
    is( $rt_dv->[2],  -99,          'POK+IOK "-99" roundtrips as -99' );
    is( $rt_dv->[3],  42,           'canonical dualvar(42, "42") roundtrips as 42' );
    is( $rt_dv->[4],  'forty-two',  'non-canonical dualvar(42, "forty-two") preserves string' );
    is( $rt_dv->[5],  '07',         'leading-zero dualvar(7, "07") preserves string' );
    is( $rt_dv->[6],  '00',         'zero non-canonical dualvar(0, "00") preserves string' );
    is( $rt_dv->[7],  '',           'zero empty dualvar(0, "") preserves string' );
    is( $rt_dv->[8],  '-05',        'neg leading-zero dualvar(-5, "-05") preserves string' );
    is( $rt_dv->[9],  '+5',         'plus-prefixed dualvar(5, "+5") preserves string' );
    is( $rt_dv->[10], '1.50',       'non-canonical float dualvar preserves string' );
    is( $rt_dv->[11], 123456789.25, 'canonical long float dualvar roundtrips' );
    is( $rt_dv->[12], 12345,        'POK+IOK+NOK canonical int roundtrips' );

    # Non-canonical dualvars with dedupe_strings and aliased_dedupe_strings
    is_deeply(
        $dec->decode( $enc_dedupe->encode( [ $dv_noncanonical, $dv_noncanonical ] ) ),
        [ 'forty-two', 'forty-two' ],
        'dualvar with dedupe_strings roundtrips'
    );
    is_deeply(
        $dec->decode( $enc_alias->encode( [ $dv_noncanonical, $dv_noncanonical ] ) ),
        [ 'forty-two', 'forty-two' ],
        'dualvar with aliased_dedupe_strings roundtrips'
    );

    # Builtin booleans on Perl 5.36+ (copied booleans have SvREFCNT==1, SvPOK && SvIOK)
    if ( $] >= 5.036 ) {
        my $bt       = eval 'no warnings "experimental::builtin"; my $x = builtin::true();  $x';
        my $bf       = eval 'no warnings "experimental::builtin"; my $x = builtin::false(); $x';
        my $expected = chr( SRL_HDR_ARRAYREF_LOW + 2 ) . chr(SRL_HDR_YES) . chr(SRL_HDR_NO);
        my $wire_v5  = $enc_v5->encode( [ $bt, $bf ] );
        my $body_v5  = substr( $wire_v5, 6 );
        is( $body_v5, $expected, 'v5 copied builtin booleans emit SRL_HDR_YES and SRL_HDR_NO' );
        my $rt_bool = $dec->decode($wire_v5);
        ok( $rt_bool->[0] && !$rt_bool->[1], 'copied builtin booleans roundtrip' );

        my $enc_v4  = Sereal::Encoder->new( { protocol_version => 4 } );
        my $wire_v4 = $enc_v4->encode( [ $bt, $bf ] );
        my $body_v4 = substr( $wire_v4, 6 );
        isnt( $body_v4, $expected, 'v4 does not emit SRL_HDR_YES/SRL_HDR_NO for builtin booleans' );
        my $rt_v4 = $dec->decode($wire_v4);
        ok(
            $rt_v4->[0] && !$rt_v4->[1],
            'v4 copied builtin booleans still roundtrip as truthy/falsy'
        );
    }

    # SvOOK (offset string) and SvIsCOW (shared hash key copy) with SvREFCNT == 1
    {
        my $ook = "prefix_hello_ook";
        substr( $ook, 0, 7, '' );
        my %src_h = ( shared_cow_key_value => 1 );
        my ($cow) = keys %src_h;
        my $rt_sc = $dec->decode( $enc->encode( [ $ook, $cow ] ) );
        is_deeply(
            $rt_sc, [ 'hello_ook', 'shared_cow_key_value' ],
            'SvOOK and SvIsCOW scalars roundtrip on fast path'
        );
    }

    # 7. Cold hash paths: sorted_nomg, sort_keys_perl (srl_dump_hv_sv), tied unsorted & sorted
    my $h             = { z => 1, a => undef, m => 'mid', b => 2 };
    my $enc_sort      = Sereal::Encoder->new( { sort_keys => 1 } );
    my $enc_sort_perl = Sereal::Encoder->new( { sort_keys => 2 } );
    is_deeply( $dec->decode( $enc_sort->encode($h) ),      $h, 'sort_keys => 1 roundtrips' );
    is_deeply( $dec->decode( $enc_sort_perl->encode($h) ), $h, 'sort_keys => 2 (perl) roundtrips' );

    tie my %tied_h, 'Local::TiedHash';
    %tied_h = ( k1 => 'v1', k2 => undef, k3 => 42 );
    is_deeply(
        $dec->decode( $enc->encode( \%tied_h ) ), \%tied_h,
        'tied hash unsorted roundtrips'
    );
    is_deeply(
        $dec->decode( $enc_sort->encode( \%tied_h ) ), \%tied_h,
        'tied hash sorted roundtrips'
    );

    # 8. Cold scalar/object paths: regexp, vstring, blessed objects, FREEZE/THAW, deep tree
    my $re      = qr/^abc\s+123$/imsx;
    my $vs1     = v1.2.3;
    my $vs2     = v5.40.0;
    my $vs_u    = v1.2.300;
    my $obj1    = bless { a => 1 }, 'Local::PlainObj';
    my $obj2    = bless { b => 2 }, 'Local::PlainObj';
    my $rt_cold = $dec->decode(
        $enc->encode( [ 3.14159265358979, -0.125, $re, $vs1, $vs2, $vs_u, $obj1, $obj2 ] ) );
    ok( abs( $rt_cold->[0] - 3.14159265358979 ) < 1e-12, 'roundtrip float pi' );
    is( $rt_cold->[1],        -0.125,   'roundtrip float -0.125' );
    is( ref( $rt_cold->[2] ), 'Regexp', 'roundtrip regexp' );
    like( "ABC\n123", $rt_cold->[2], 'roundtrip regexp matches with modifiers' );
    is( reftype( \$rt_cold->[3] ),       'VSTRING',         'roundtrip v1.2.3 reftype' );
    is( sprintf( '%vd', $rt_cold->[3] ), '1.2.3',           'roundtrip v1.2.3 value' );
    is( sprintf( '%vd', $rt_cold->[4] ), '5.40.0',          'roundtrip v5.40.0 value' );
    is( sprintf( '%vd', $rt_cold->[5] ), '1.2.300',         'roundtrip utf8 v1.2.300 value' );
    is( ref( $rt_cold->[6] ),            'Local::PlainObj', 'first blessed object roundtrips' );
    is( ref( $rt_cold->[7] ), 'Local::PlainObj', 'second blessed object (OBJECTV) roundtrips' );

    my $enc_freeze = Sereal::Encoder->new( { freeze_callbacks => 1 } );
    my $rt_frozen  = $dec->decode( $enc_freeze->encode( bless {}, 'Local::FreezeUndef' ) );
    is_deeply(
        $rt_frozen, bless( { thawed => [undef] }, 'Local::FreezeUndef' ),
        'FREEZE returning undef roundtrips'
    );

    my $deep = 12345;
    for my $i ( 1 .. 200 ) {
        $deep = ( $i % 2 ) ? [ $deep, undef ] : { n => $deep, u => undef };
    }
    my $rt_deep = $dec->decode( $enc->encode($deep) );
    my $cur     = $rt_deep;
    for my $i ( reverse 1 .. 200 ) {
        $cur = ( $i % 2 ) ? $cur->[0] : $cur->{n};
    }
    is( $cur, 12345, 'roundtrip depth-200 nested array/hash with undefs' );
}

done_testing();
