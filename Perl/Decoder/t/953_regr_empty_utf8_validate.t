#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::Decoder qw(decode_sereal);
use Sereal::TestSet qw(Header have_encoder_and_decoder);

# Perl's is_utf8_string(s, 0) treats length 0 as "call strlen(s)".
# An empty STR_UTF8 (len == 0) in a packet containing subsequent non-UTF-8
# bytes or ending at a buffer boundary must not invoke is_utf8_string(..., 0).

my $dec_val = Sereal::Decoder->new( { validate_utf8 => 1 } );
my $dec_def = Sereal::Decoder->new;

# 1. Empty UTF-8 string followed by binary invalid-UTF-8 bytes in an array.
{
    my $pkt = "\x3d\xf3rl\x04\x00\x42\x27\x00\x62\xff\xfe";
    my $out = eval { $dec_val->decode($pkt) };
    is( $@, '', 'empty STR_UTF8 followed by binary \\xff\\xfe decodes with validate_utf8' );
    is( ref($out),     'ARRAY', 'decoded arrayref' );
    is( scalar(@$out), 2,       'decoded two elements' );
    is( $out->[0],     '',      'first element is empty string' );
    ok( utf8::is_utf8( $out->[0] ), 'first element retains UTF-8 flag' );
    is( $out->[1], "\xff\xfe", 'second element preserves raw binary bytes' );
    ok( !utf8::is_utf8( $out->[1] ), 'second element does not have UTF-8 flag' );
}

if ( have_encoder_and_decoder() ) {
    my $empty_utf8 = '';
    utf8::upgrade($empty_utf8);
    my $pkt = Sereal::Encoder::encode_sereal( [ $empty_utf8, "\xff\xfe" ] );
    my $out = eval { decode_sereal( $pkt, { validate_utf8 => 1 } ) };
    is( $@, '', 'Encoder roundtrip of [ empty_utf8, "\\xff\\xfe" ] succeeds with validate_utf8' );
    is( $out->[0], '', 'roundtrip empty string value' );
    ok( utf8::is_utf8( $out->[0] ), 'roundtrip empty string retains UTF-8 flag' );
    is( $out->[1], "\xff\xfe", 'roundtrip binary string value' );
    ok( !utf8::is_utf8( $out->[1] ), 'roundtrip binary string has no UTF-8 flag' );
}

# 2. Empty STR_UTF8 as the sole/final item of a packet under validate_utf8 => 1.
{
    my $sole_pkt = Header(4) . "\x27\x00";
    my $out      = eval { $dec_val->decode($sole_pkt) };
    is( $@,   '', 'sole empty STR_UTF8 packet decodes with validate_utf8' );
    is( $out, '', 'sole empty STR_UTF8 value is empty string' );
    ok( utf8::is_utf8($out), 'sole empty STR_UTF8 retains UTF-8 flag' );

    # Snappy-compressed (v4 incremental) packet whose uncompressed body is solely "\x27\x00".
    my $snappy_pkt = "\x3d\xf3rl\x24\x00\x04\x02\x04\x27\x00";
    $out = eval { $dec_val->decode($snappy_pkt) };
    is( $@,   '', 'snappy-compressed sole empty STR_UTF8 decodes with validate_utf8' );
    is( $out, '', 'snappy-compressed sole empty STR_UTF8 value is empty string' );
    ok( utf8::is_utf8($out), 'snappy-compressed sole empty STR_UTF8 retains UTF-8 flag' );

    # Non-NUL-terminated slice / trailing non-UTF-8 bytes after packet boundary.
    my $dec_inc = Sereal::Decoder->new( { validate_utf8 => 1, incremental => 1 } );
    my $buf     = "XX" . $sole_pkt . "\xff\xfe";
    $out = eval { $dec_inc->decode_with_offset( $buf, 2 ) };
    is( $@,   '', 'empty STR_UTF8 at end of incremental slice before \\xff\\xfe succeeds' );
    is( $out, '', 'incremental slice empty STR_UTF8 value is empty string' );
    ok( utf8::is_utf8($out), 'incremental slice empty STR_UTF8 retains UTF-8 flag' );
    is( $dec_inc->bytes_consumed, length($sole_pkt), 'bytes_consumed stops at packet end' );
}

# 3. Empty UTF-8 COPY classname followed by non-UTF-8 bytes in the packet.
{
    for my $tracked ( 0, 0x80 ) {
        my $str_tag = chr( 0x27 | $tracked );

        # v4 body: ARRAY(3), STR_UTF8(0) at body offset 2,
        #          OBJECT COPY(2) REFN ARRAY(0), SHORT_BINARY(2) "\xff\xfe"
        my $pkt = Header(4) . "\x43" . $str_tag . "\x00\x2c\x2f\x02\x28\x2b\x00\x62\xff\xfe";
        for my $case ( [ default => $dec_def ], [ validate_utf8 => $dec_val ] ) {
            my ( $label, $dec ) = @$case;
            my $out = eval { $dec->decode($pkt) };
            is( $@,        '', "empty UTF-8 COPY classname (tracked=$tracked, $label) succeeds" );
            is( ref($out), 'ARRAY', "empty COPY classname ($label) decoded arrayref" );
            is( $out->[0], '',      "empty COPY classname ($label) first element is empty string" );
            ok(
                utf8::is_utf8( $out->[0] ),
                "empty COPY classname ($label) first element is UTF-8"
            );
            is( ref( $out->[1] ), 'main', "empty COPY classname ($label) blesses into main" );
            is(
                $out->[2], "\xff\xfe",
                "empty COPY classname ($label) third element preserves binary bytes"
            );
        }
    }
}

# 4. Negative tests: non-empty invalid UTF-8 strings and COPY classnames still croak.
{
    for my $bad_pkt (
        [ 'sole non-empty invalid STR_UTF8'  => Header(4) . "\x27\x02\xff\xfe" ],
        [ 'array non-empty invalid STR_UTF8' => Header(4) . "\x42\x27\x02\xff\xfe\x60" ],
        )
    {
        my ( $name, $pkt ) = @$bad_pkt;
        my $ok = eval { $dec_val->decode($pkt); 1 };
        ok( !$ok, "$name still croaks with validate_utf8" );
        like( $@, qr/Invalid UTF8 byte sequence/, "$name reports Invalid UTF8 byte sequence" );
    }

    # Hash key at body offset 3 is an invalid STR_UTF8 "\xff\xfe" (hash keys do not
    # run validate_utf8), then OBJECT COPY(3) REFN ARRAY(0) triggers the COPY
    # classname check even when validate_utf8 is enabled.
    my $bad_copy_pkt = Header(4) . "\x42\x51\x27\x02\xff\xfe\x01\x2c\x2f\x03\x28\x2b\x00";
    for my $case ( [ default => $dec_def ], [ validate_utf8 => $dec_val ] ) {
        my ( $label, $dec ) = @$case;
        my $ok = eval { $dec->decode($bad_copy_pkt); 1 };
        ok( !$ok, "non-empty invalid UTF-8 COPY classname ($label) still croaks" );
        like(
            $@, qr/utf8 flagged classname is not actually utf8/,
            "non-empty invalid UTF-8 COPY classname ($label) reports expected panic"
        );
    }
}

done_testing();
