#!perl
use strict;
use warnings;
use Data::Dumper;
use Test::More;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet            qw(:all);
use Sereal::Decoder            qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);

# These tests are a manual attempt at seeing the decoder blow up on
# bad input. This obviously shouldn't segfault and neither leak
# memory.

plan tests => 124;
my ( $ok, $out, $err );

SCOPE: {
    check_fail( Header(), qr/Not a valid Sereal document/i, "Cannot decode just header" );

    my $badheaderpacket = "srX" . chr(SRL_PROTOCOL_VERSION) . chr(0) . integer(1);
    check_fail( $badheaderpacket, qr/Bad Sereal header/i, "Packet with invalid header blows up" );

    my $bad_nested_packet = Header() . array( integer(1), 7777 );
    check_fail( $bad_nested_packet, qr/Sereal: Error/, "Random crap in packet" );

    my $duplicate_key = "key-" . ( "x" x 80 );
    my $duplicate_key_token =
        chr(SRL_HDR_BINARY) . varint( length($duplicate_key) ) . $duplicate_key;
    my $duplicate_key_packet = Header() . hash(
        $duplicate_key_token, integer(1),
        $duplicate_key_token, integer(2),
    );
    my $duplicate_key_prefix = substr( $duplicate_key, 0, 16 );
    my $duplicate_key_suffix = substr( $duplicate_key, -16 );
    check_fail(
        $duplicate_key_packet,
        qr/duplicate key '\Q$duplicate_key_prefix\E' \.\.\. 52 bytes elided \.\.\. '\Q$duplicate_key_suffix\E' in hash/,
        "Duplicate-key error keeps a bounded key prefix",
    );

    my $not_worth_eliding_key = "y" x 59;
    my $not_worth_eliding_token =
        chr(SRL_HDR_BINARY) . varint( length($not_worth_eliding_key) ) . $not_worth_eliding_key;
    check_fail(
        Header() . hash(
            $not_worth_eliding_token, integer(1),
            $not_worth_eliding_token, integer(2),
        ),
        qr/duplicate key '\Q$not_worth_eliding_key\E' in hash/,
        "Duplicate-key error keeps the full key when elision is not shorter",
    );

    my $escaped_duplicate_key = "a'b\\\0";
    my $escaped_duplicate_key_token =
        chr(SRL_HDR_BINARY) . varint( length($escaped_duplicate_key) ) . $escaped_duplicate_key;
    my $escaped_duplicate_key_display = "a" . "\\'" . "b" . "\\\\" . "\\x00";
    check_fail(
        Header() . hash(
            $escaped_duplicate_key_token, integer(1),
            $escaped_duplicate_key_token, integer(2),
        ),
        qr/duplicate key '\Q$escaped_duplicate_key_display\E' in hash/,
        "Duplicate-key error escapes binary key bytes",
    );

    my $obj_packet =
        Header() . chr(SRL_HDR_OBJECT) . short_string("Foo") . chr(SRL_HDR_REFN) . integer(1);
    check_fail( $obj_packet, qr/refuse_obj/, "refusing objects option", { refuse_objects => 1 } );

    # strictly speaking not entirely correct; also: +16 for the snappy flag isn't exactly API
    my $h = SRL_MAGIC_STRING . chr( 1 + 16 ) . chr(0) . chr(SRL_HDR_UNDEF);
    check_fail( $h, qr/Snappy/, "refusing Snappy option", { refuse_snappy => 1 } );

    # Tests for limiting number of acceptable hash entries
    my $hash_packet = Header() . hash( map short_string($_), 1 .. 2000 );
    $h = decode_sereal($hash_packet);
    is( ref($h),             "HASH", "Deserializes as hash" );
    is( scalar( keys(%$h) ), 1000,   "Hash has 1000 entries" );
    $h = decode_sereal( $hash_packet, { max_num_hash_entries => 0 } );
    is( ref($h), "HASH", "Deserializes as hash (2)" );
    $h = decode_sereal( $hash_packet, { max_num_hash_entries => 1000 } );
    is( ref($h), "HASH", "Deserializes as hash (3)" );

    check_fail(
        $hash_packet, qr/Sereal: Error/, "Setting hash limit option (1)",
        { max_num_hash_entries => 1 } );
    check_fail(
        $hash_packet, qr/Sereal: Error/, "Setting hash limit option (999)",
        { max_num_hash_entries => 999 } );

    # Tests for limiting number of acceptable array entries
    my $array_packet = Header() . array( map short_string($_), 1 .. 1000 );
    my $ar           = decode_sereal($array_packet);
    is( ref($ar),     "ARRAY", "Deserializes as array" );
    is( scalar(@$ar), 1000,    "Array has 1000 entries" );
    $ar = decode_sereal( $array_packet, { max_num_array_entries => 0 } );
    is( ref($ar), "ARRAY", "Deserializes as array (2)" );
    $ar = decode_sereal( $array_packet, { max_num_array_entries => 1000 } );
    is( ref($ar), "ARRAY", "Deserializes as array (3)" );

    check_fail(
        $array_packet, qr/Sereal: Error/, "Setting array limit option (1)",
        { max_num_array_entries => 1 } );
    check_fail(
        $array_packet, qr/Sereal: Error/, "Setting array limit option (999)",
        { max_num_array_entries => 999 } );

    # Tests for limiting number of characters in a (NOT short) string
    my $string_packet = "\x3d\xf3\x72\x6c\x04\x00\x26\xe8\x07" . ( 'a' x 1000 );
    my $str           = decode_sereal($string_packet);
    is( ref($str),    "",   "Deserializes as scalar" );
    is( length($str), 1000, "String has 1000 characters" );
    $str = decode_sereal( $string_packet, { max_string_length => 0 } );
    is( ref($str), "", "Deserializes as string (2)" );
    $str = decode_sereal( $string_packet, { max_string_length => 1000 } );
    is( ref($str), "", "Deserializes as string (3)" );

    check_fail(
        $string_packet, qr/Sereal: Error/, "Setting string limit option (1)",
        { max_string_length => 1 } );
    check_fail(
        $string_packet, qr/Sereal: Error/, "Setting array limit option (999)",
        { max_string_length => 999 } );

    check_fail(
        Header() . short_string("ab"), qr/Sereal: Error/,
        "String length limit also applies to short strings",
        { max_string_length => 1 } );
    check_fail(
        Header() . hash( short_string("ab"), integer(1) ), qr/Sereal: Error/,
        "String length limit also applies to hash keys",
        { max_string_length => 1 } );
    check_fail(
        Header() . chr(SRL_HDR_OBJECT) . short_string("Foo") . chr(SRL_HDR_REFN) . integer(1),
        qr/Sereal: Error/, "String length limit also applies to class names",
        { max_string_length => 1 } );

    my $valid_packet = Header(2) . short_string("foo");
    my $foo          = decode_sereal($valid_packet);
    is( $foo, "foo", "Have valid test packet" );
    $valid_packet =~ s/^=srl/=\xF3rl/;
    $foo = eval { decode_sereal($valid_packet) };
    ok( !defined($foo), "SRL_MAGIC_STRING_HIGHBIT implies protocol v3 or higher." );

    substr( $valid_packet, 4, 1, chr(3) );
    $foo = eval { decode_sereal($valid_packet) };
    is( $foo, "foo", "Have valid test packet after asserting high bit in magic with protocol v3" );

    utf8::encode($valid_packet);
    check_fail( $valid_packet, qr/UTF-8/, "Sereal determined 'accidental' UTF8 upgrade" );
}

pass("Alive");    # done

sub check_fail {
    my ( $data, $err_like, $name, $options ) = @_;
    $options ||= {};

    my ( $ok, $out, $err );
    ( $ok, $out, $err ) = dec_func( $data, $options );
    expect_fail( $ok, $out, $err, $err_like, $name . "(func)" );
    ( $ok, $out, $err ) = dec_obj( $data, $options );
    expect_fail( $ok, $out, $err, $err_like, $name . "(OO)" );
}

sub expect_fail {
    my ( $ok, $out, $err, $err_like, $name ) = @_;
    ok( !$ok,           "$name, got exception" );
    ok( !defined($out), "$name, got no output" );
    if ( defined $err_like ) {
        like( $err, $err_like, "$name, matched exception" );
    }
    else {
        diag($err);
    }
}

sub dec_func {
    my ( $ok, $out );
    $ok = eval {
        $out = decode_sereal(@_);
        1;
    };
    my $err = $@ || 'Zombie error';
    return ( $ok, $out, $err );
}

sub dec_obj {
    my ( $ok, $out );
    my $obj = Sereal::Decoder->new( @_ > 1 ? $_[1] : {} );
    $ok = eval {
        $out = $obj->decode(@_);
        1;
    };
    my $err = $@ || 'Zombie error';
    return ( $ok, $out, $err );
}

