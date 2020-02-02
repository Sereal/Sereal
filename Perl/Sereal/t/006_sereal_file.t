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
use Sereal qw(
    get_sereal_decoder
    get_sereal_encoder
    clear_sereal_object_cache

    encode_sereal
    decode_sereal

    read_sereal
    read_sereal_file
    write_sereal
    write_sereal_file

    looks_like_sereal
    scalar_looks_like_sereal

    sereal_encode_with_object
    sereal_decode_with_object
    decode_sereal_with_header_data

    sereal_decode_with_header_with_object
    sereal_decode_only_header_with_object
    sereal_decode_only_header_with_offset_with_object
    sereal_decode_with_header_and_offset_with_object
    sereal_decode_with_offset_with_object

    SRL_UNCOMPRESSED
    SRL_SNAPPY
    SRL_ZLIB
    SRL_ZSTD
);

use Test::More tests => 30;
use File::Temp;
my $dir= File::Temp->newdir;
my $source= { foo => 1 };
{
    my $file= "$dir/test1.srl";
    {
        write_sereal_file( $file, $source );
        ok( -e $file, "write_sereal_file: file exists" );
    }
    {
        my $copy= read_sereal_file($file);
        is_deeply( $source, $copy, "read_sereal_file: simple read works" );
    }
    {
        read_sereal_file( $file, {}, my $copy );
        is_deeply( $source, $copy, "read_sereal_file: read to root works" );
    }
    {
        read_sereal_file( $file, undef, my $copy );
        is_deeply( $source, $copy, "read_sereal_file: read to root works (undef opts)" );
    }
}
{
    my $file= "$dir/test2.srl";
    {
        write_sereal( $file, $source, { compress => SRL_ZLIB } );
        ok( -e $file, "write_sereal: file exists" );
    }
    {
        my $copy= read_sereal($file);
        is_deeply( $source, $copy, "read_sereal: simple read works" );
    }
    {
        read_sereal( $file, {}, my $copy );
        is_deeply( $source, $copy, "read_sereal: read to root works" );
    }
    {
        read_sereal( $file, undef, my $copy );
        is_deeply( $source, $copy, "read_sereal: read to root works (undef opts)" );
    }
}

{
    my $encoder_0= get_sereal_encoder( { compress => SRL_ZLIB, compress_level => 9 } );
    my $decoder_0= get_sereal_decoder( { set_read_only => 1, use_undef => 1 } );
    is( ref($encoder_0), "Sereal::Encoder", "get_sereal_encoder returned an encoder" );
    is( ref($decoder_0), "Sereal::Decoder", "get_sereal_decoder returned a decoder" );
    my $encoder_1= get_sereal_encoder();
    my $decoder_1= get_sereal_decoder();
    is( ref($encoder_1), "Sereal::Encoder", "get_sereal_encoder returned an encoder" );
    is( ref($decoder_1), "Sereal::Decoder", "get_sereal_decoder returned a decoder" );
    ok( $encoder_0 != $encoder_1, "encoder_0 is different from encoder_1" );
    ok( $decoder_0 != $decoder_1, "decoder_0 is different from decoder_1" );
    my $encoder_2= get_sereal_encoder( { compress => SRL_ZLIB, compress_level => 9 } );
    my $decoder_2= get_sereal_decoder( { set_read_only => 1, use_undef => 1 } );
    is( ref($encoder_2), "Sereal::Encoder", "get_sereal_encoder returned an encoder" );
    is( ref($decoder_2), "Sereal::Decoder", "get_sereal_decoder returned a decoder" );
    ok( $encoder_0 == $encoder_2, "encoder_0 is same as encoder_2" );
    ok( $decoder_0 == $decoder_2, "decoder_0 is same as decoder_2" );
    is(
        clear_sereal_object_cache(), 4,
        "clear_sereal_object_cache returned the expected number of items"
    );
}

{
    my $encoded= encode_sereal( ["foo"] );
    my $decoded= decode_sereal($encoded);
    is( $decoded->[0],               "foo", "encode_sereal/decode_sereal seem to work" );
    is( looks_like_sereal($encoded), 4,     "functional looks_like_sereal() works as expected" );
    is(
        Sereal::Decoder->new()->looks_like_sereal($encoded), 4,
        "object method looks_like_sereal() works as expected"
    );
    is(
        Sereal::Decoder->looks_like_sereal($encoded), 4,
        "class method looks_like_sereal() works as expected"
    );
    is(
        scalar_looks_like_sereal($encoded), 4,
        "functional scalar_looks_like_sereal() works as expected"
    );
    my $eval_ok= eval q{
        scalar_looks_like_sereal("foo",$encoded);
        1;
    };
    my $error= $eval_ok ? "" : $@;
    is( $eval_ok, undef, "scalar_looks_like_sereal should die with two args" );
    like(
        $error, qr/^Too many arguments for Sereal::Decoder::scalar_looks_like_sereal/,
        "error looks as expected"
    );
}
{
    is( SRL_UNCOMPRESSED, 0, "SRL_UNCOMPRESSED has expected value" );
    is( SRL_SNAPPY,       1, "SRL_SNAPPY has expected value" );
    is( SRL_ZLIB,         2, "SRL_ZLIB has expected value" );
    is( SRL_ZSTD,         3, "SRL_ZSTD has expected value" );
}
