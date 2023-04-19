package Sereal::Encoder::Constants;
use strict;
use warnings;
require Exporter;
our @ISA= qw(Exporter);

our $VERSION= '5.004';

our ( @EXPORT_OK, %DEFINE, %TAG_INFO_HASH, @TAG_INFO_ARRAY );

our %EXPORT_TAGS= ( all => \@EXPORT_OK );

# start autoupdated section - do not modify directly

BEGIN {
    %DEFINE= (
        "SRL_HDR_ALIAS"                            => 46,
        "SRL_HDR_ARRAY"                            => 43,
        "SRL_HDR_ARRAYREF"                         => 64,
        "SRL_HDR_ARRAYREF_HIGH"                    => 79,
        "SRL_HDR_ARRAYREF_LOW"                     => 64,
        "SRL_HDR_BINARY"                           => 38,
        "SRL_HDR_CANONICAL_UNDEF"                  => 57,
        "SRL_HDR_COPY"                             => 47,
        "SRL_HDR_DOUBLE"                           => 35,
        "SRL_HDR_EXTEND"                           => 62,
        "SRL_HDR_FALSE"                            => 58,
        "SRL_HDR_FLOAT"                            => 34,
        "SRL_HDR_FLOAT_128"                        => 56,
        "SRL_HDR_HASH"                             => 42,
        "SRL_HDR_HASHREF"                          => 80,
        "SRL_HDR_HASHREF_HIGH"                     => 95,
        "SRL_HDR_HASHREF_LOW"                      => 80,
        "SRL_HDR_LONG_DOUBLE"                      => 36,
        "SRL_HDR_MANY"                             => 60,
        "SRL_HDR_NEG"                              => 16,
        "SRL_HDR_NEG_HIGH"                         => 31,
        "SRL_HDR_NEG_LOW"                          => 16,
        "SRL_HDR_NO"                               => 52,
        "SRL_HDR_OBJECT"                           => 44,
        "SRL_HDR_OBJECTV"                          => 45,
        "SRL_HDR_OBJECTV_FREEZE"                   => 51,
        "SRL_HDR_OBJECT_FREEZE"                    => 50,
        "SRL_HDR_PACKET_START"                     => 61,
        "SRL_HDR_PAD"                              => 63,
        "SRL_HDR_POS"                              => 0,
        "SRL_HDR_POS_HIGH"                         => 15,
        "SRL_HDR_POS_LOW"                          => 0,
        "SRL_HDR_REFN"                             => 40,
        "SRL_HDR_REFP"                             => 41,
        "SRL_HDR_REGEXP"                           => 49,
        "SRL_HDR_RESERVED"                         => 54,
        "SRL_HDR_RESERVED_HIGH"                    => 55,
        "SRL_HDR_RESERVED_LOW"                     => 54,
        "SRL_HDR_SHORT_BINARY"                     => 96,
        "SRL_HDR_SHORT_BINARY_HIGH"                => 127,
        "SRL_HDR_SHORT_BINARY_LOW"                 => 96,
        "SRL_HDR_STR_UTF8"                         => 39,
        "SRL_HDR_TRACK_FLAG"                       => 128,
        "SRL_HDR_TRUE"                             => 59,
        "SRL_HDR_UNDEF"                            => 37,
        "SRL_HDR_VARINT"                           => 32,
        "SRL_HDR_WEAKEN"                           => 48,
        "SRL_HDR_YES"                              => 53,
        "SRL_HDR_ZIGZAG"                           => 33,
        "SRL_MAGIC_STRING"                         => "=srl",
        "SRL_MAGIC_STRING_HIGHBIT"                 => "=\363rl",
        "SRL_MAGIC_STRING_HIGHBIT_UINT_BE"         => 1039364716,
        "SRL_MAGIC_STRING_HIGHBIT_UINT_LE"         => 1819472701,
        "SRL_MAGIC_STRING_HIGHBIT_UTF8"            => "=\303\263rl",
        "SRL_MAGIC_STRING_HIGHBIT_UTF8_UINT_BE"    => 1036235634,
        "SRL_MAGIC_STRING_HIGHBIT_UTF8_UINT_LE"    => 1924383549,
        "SRL_MAGIC_STRING_UINT_BE"                 => 1030976108,
        "SRL_MAGIC_STRING_UINT_LE"                 => 1819439933,
        "SRL_MAGIC_STRLEN"                         => 4,
        "SRL_MASK_ARRAYREF_COUNT"                  => 15,
        "SRL_MASK_HASHREF_COUNT"                   => 15,
        "SRL_MASK_SHORT_BINARY_LEN"                => 31,
        "SRL_NEG_MIN_SIZE"                         => 16,
        "SRL_POS_MAX_SIZE"                         => 15,
        "SRL_PROTOCOL_ENCODING_MASK"               => 240,
        "SRL_PROTOCOL_ENCODING_RAW"                => 0,
        "SRL_PROTOCOL_ENCODING_SNAPPY"             => 16,
        "SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL" => 32,
        "SRL_PROTOCOL_ENCODING_ZLIB"               => 48,
        "SRL_PROTOCOL_ENCODING_ZSTD"               => 64,
        "SRL_PROTOCOL_HDR_CONTINUE"                => 8,
        "SRL_PROTOCOL_HDR_USER_DATA"               => 1,
        "SRL_PROTOCOL_VERSION"                     => 5,
        "SRL_PROTOCOL_VERSION_BITS"                => 4,
        "SRL_PROTOCOL_VERSION_MASK"                => 15
    );

}

use constant \%DEFINE;
push @EXPORT_OK, keys %DEFINE;
@TAG_INFO_ARRAY= (

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "small positive integer - value in low 4 bits (identity)",
        "masked"     => 1,
        "masked_val" => 0,
        "name"       => "POS_0",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 0
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 1,
        "name"       => "POS_1",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 1
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 2,
        "name"       => "POS_2",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 2
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 3,
        "name"       => "POS_3",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 3
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 4,
        "name"       => "POS_4",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 4
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 5,
        "name"       => "POS_5",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 5
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 6,
        "name"       => "POS_6",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 6
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 7,
        "name"       => "POS_7",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 7
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 8,
        "name"       => "POS_8",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 8
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 9,
        "name"       => "POS_9",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 9
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 10,
        "name"       => "POS_10",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 10
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 11,
        "name"       => "POS_11",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 11
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 12,
        "name"       => "POS_12",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 12
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 13,
        "name"       => "POS_13",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 13
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 14,
        "name"       => "POS_14",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 14
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 15,
        "name"       => "POS_15",
        "type_name"  => "POS",
        "type_value" => 0,
        "value"      => 15
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "small negative integer - value in low 4 bits (k+32)",
        "masked"     => 1,
        "masked_val" => 16,
        "name"       => "NEG_16",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 16
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 15,
        "name"       => "NEG_15",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 17
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 14,
        "name"       => "NEG_14",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 18
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 13,
        "name"       => "NEG_13",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 19
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 12,
        "name"       => "NEG_12",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 20
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 11,
        "name"       => "NEG_11",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 21
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 10,
        "name"       => "NEG_10",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 22
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 9,
        "name"       => "NEG_9",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 23
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 8,
        "name"       => "NEG_8",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 24
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 7,
        "name"       => "NEG_7",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 25
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 6,
        "name"       => "NEG_6",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 26
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 5,
        "name"       => "NEG_5",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 27
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 4,
        "name"       => "NEG_4",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 28
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 3,
        "name"       => "NEG_3",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 29
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 2,
        "name"       => "NEG_2",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 30
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 1,
        "name"       => "NEG_1",
        "type_name"  => "NEG",
        "type_value" => 16,
        "value"      => 31
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<VARINT> - Varint variable length integer",
        "name"       => "VARINT",
        "type_name"  => "VARINT",
        "type_value" => 32,
        "value"      => 32
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<ZIGZAG-VARINT> - Zigzag variable length integer",
        "name"       => "ZIGZAG",
        "type_name"  => "ZIGZAG",
        "type_value" => 33,
        "value"      => 33
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<IEEE-FLOAT>",
        "name"       => "FLOAT",
        "type_name"  => "FLOAT",
        "type_value" => 34,
        "value"      => 34
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<IEEE-DOUBLE>",
        "name"       => "DOUBLE",
        "type_name"  => "DOUBLE",
        "type_value" => 35,
        "value"      => 35
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<IEEE-LONG-DOUBLE>",
        "name"       => "LONG_DOUBLE",
        "type_name"  => "LONG_DOUBLE",
        "type_value" => 36,
        "value"      => 36
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "None - Perl undef var; eg my \$var= undef;",
        "name"       => "UNDEF",
        "type_name"  => "UNDEF",
        "type_value" => 37,
        "value"      => 37
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<LEN-VARINT> <BYTES> - binary/(latin1) string",
        "name"       => "BINARY",
        "type_name"  => "BINARY",
        "type_value" => 38,
        "value"      => 38
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<LEN-VARINT> <UTF8> - utf8 string",
        "name"       => "STR_UTF8",
        "type_name"  => "STR_UTF8",
        "type_value" => 39,
        "value"      => 39
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<ITEM-TAG>    - ref to next item",
        "name"       => "REFN",
        "type_name"  => "REFN",
        "type_value" => 40,
        "value"      => 40
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<OFFSET-VARINT> - ref to previous item stored at offset",
        "name"       => "REFP",
        "type_name"  => "REFP",
        "type_value" => 41,
        "value"      => 41
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment" =>
            "<COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs",
        "name"       => "HASH",
        "type_name"  => "HASH",
        "type_value" => 42,
        "value"      => 42
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items",
        "name"       => "ARRAY",
        "type_name"  => "ARRAY",
        "type_value" => 43,
        "value"      => 43
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<STR-TAG> <ITEM-TAG> - class, object-item",
        "name"       => "OBJECT",
        "type_name"  => "OBJECT",
        "type_value" => 44,
        "value"      => 44
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment" =>
            "<OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item",
        "name"       => "OBJECTV",
        "type_name"  => "OBJECTV",
        "type_value" => 45,
        "value"      => 45
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<OFFSET-VARINT> - alias to item defined at offset",
        "name"       => "ALIAS",
        "type_name"  => "ALIAS",
        "type_value" => 46,
        "value"      => 46
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<OFFSET-VARINT> - copy of item defined at offset",
        "name"       => "COPY",
        "type_name"  => "COPY",
        "type_value" => 47,
        "value"      => 47
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<REF-TAG> - Weaken the following reference",
        "name"       => "WEAKEN",
        "type_name"  => "WEAKEN",
        "type_value" => 48,
        "value"      => 48
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<PATTERN-STR-TAG> <MODIFIERS-STR-TAG>",
        "name"       => "REGEXP",
        "type_name"  => "REGEXP",
        "type_value" => 49,
        "value"      => 49
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment" =>
            "<STR-TAG> <ITEM-TAG> - class, object-item. Need to call \"THAW\" method on class after decoding",
        "name"       => "OBJECT_FREEZE",
        "type_name"  => "OBJECT_FREEZE",
        "type_value" => 50,
        "value"      => 50
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment" =>
            "<OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT)",
        "name"       => "OBJECTV_FREEZE",
        "type_name"  => "OBJECTV_FREEZE",
        "type_value" => 51,
        "value"      => 51
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "SvIsBOOL() == PL_No,  5.36 and later only (json false)",
        "name"       => "NO",
        "type_name"  => "NO",
        "type_value" => 52,
        "value"      => 52
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "SvIsBOOL() == PL_Yes, 5.36 and later only (json true)",
        "name"       => "YES",
        "type_name"  => "YES",
        "type_value" => 53,
        "value"      => 53
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 0,
        "name"       => "RESERVED_0",
        "type_name"  => "RESERVED",
        "type_value" => 54,
        "value"      => 54
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 1,
        "name"       => "RESERVED_1",
        "type_name"  => "RESERVED",
        "type_value" => 54,
        "value"      => 55
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "quadmath _float128",
        "name"       => "FLOAT_128",
        "type_name"  => "FLOAT_128",
        "type_value" => 56,
        "value"      => 56
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "undef (PL_sv_undef) - \"the\" Perl undef (see notes)",
        "name"       => "CANONICAL_UNDEF",
        "type_name"  => "CANONICAL_UNDEF",
        "type_value" => 57,
        "value"      => 57
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "false (PL_sv_no)",
        "name"       => "FALSE",
        "type_name"  => "FALSE",
        "type_value" => 58,
        "value"      => 58
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "true  (PL_sv_yes)",
        "name"       => "TRUE",
        "type_name"  => "TRUE",
        "type_value" => 59,
        "value"      => 59
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment" =>
            "<LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3)",
        "name"       => "MANY",
        "type_name"  => "MANY",
        "type_value" => 60,
        "value"      => 60
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "(first byte of magic string in header)",
        "name"       => "PACKET_START",
        "type_name"  => "PACKET_START",
        "type_value" => 61,
        "value"      => 61
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<BYTE> - for additional tags",
        "name"       => "EXTEND",
        "type_name"  => "EXTEND",
        "type_value" => 62,
        "value"      => 62
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "(ignored tag, skip to next byte)",
        "name"       => "PAD",
        "type_name"  => "PAD",
        "type_value" => 63,
        "value"      => 63
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "[<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)",
        "masked"     => 1,
        "masked_val" => 0,
        "name"       => "ARRAYREF_0",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 64
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 1,
        "name"       => "ARRAYREF_1",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 65
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 2,
        "name"       => "ARRAYREF_2",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 66
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 3,
        "name"       => "ARRAYREF_3",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 67
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 4,
        "name"       => "ARRAYREF_4",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 68
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 5,
        "name"       => "ARRAYREF_5",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 69
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 6,
        "name"       => "ARRAYREF_6",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 70
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 7,
        "name"       => "ARRAYREF_7",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 71
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 8,
        "name"       => "ARRAYREF_8",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 72
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 9,
        "name"       => "ARRAYREF_9",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 73
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 10,
        "name"       => "ARRAYREF_10",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 74
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 11,
        "name"       => "ARRAYREF_11",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 75
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 12,
        "name"       => "ARRAYREF_12",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 76
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 13,
        "name"       => "ARRAYREF_13",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 77
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 14,
        "name"       => "ARRAYREF_14",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 78
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 15,
        "name"       => "ARRAYREF_15",
        "type_name"  => "ARRAYREF",
        "type_value" => 64,
        "value"      => 79
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment" =>
            "[<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)",
        "masked"     => 1,
        "masked_val" => 0,
        "name"       => "HASHREF_0",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 80
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 1,
        "name"       => "HASHREF_1",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 81
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 2,
        "name"       => "HASHREF_2",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 82
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 3,
        "name"       => "HASHREF_3",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 83
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 4,
        "name"       => "HASHREF_4",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 84
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 5,
        "name"       => "HASHREF_5",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 85
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 6,
        "name"       => "HASHREF_6",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 86
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 7,
        "name"       => "HASHREF_7",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 87
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 8,
        "name"       => "HASHREF_8",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 88
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 9,
        "name"       => "HASHREF_9",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 89
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 10,
        "name"       => "HASHREF_10",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 90
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 11,
        "name"       => "HASHREF_11",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 91
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 12,
        "name"       => "HASHREF_12",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 92
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 13,
        "name"       => "HASHREF_13",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 93
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 14,
        "name"       => "HASHREF_14",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 94
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 15,
        "name"       => "HASHREF_15",
        "type_name"  => "HASHREF",
        "type_value" => 80,
        "value"      => 95
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "comment"    => "<BYTES> - binary/latin1 string, length encoded in low 5 bits of tag",
        "masked"     => 1,
        "masked_val" => 0,
        "name"       => "SHORT_BINARY_0",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 96
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 1,
        "name"       => "SHORT_BINARY_1",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 97
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 2,
        "name"       => "SHORT_BINARY_2",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 98
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 3,
        "name"       => "SHORT_BINARY_3",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 99
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 4,
        "name"       => "SHORT_BINARY_4",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 100
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 5,
        "name"       => "SHORT_BINARY_5",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 101
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 6,
        "name"       => "SHORT_BINARY_6",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 102
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 7,
        "name"       => "SHORT_BINARY_7",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 103
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 8,
        "name"       => "SHORT_BINARY_8",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 104
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 9,
        "name"       => "SHORT_BINARY_9",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 105
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 10,
        "name"       => "SHORT_BINARY_10",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 106
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 11,
        "name"       => "SHORT_BINARY_11",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 107
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 12,
        "name"       => "SHORT_BINARY_12",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 108
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 13,
        "name"       => "SHORT_BINARY_13",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 109
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 14,
        "name"       => "SHORT_BINARY_14",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 110
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 15,
        "name"       => "SHORT_BINARY_15",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 111
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 16,
        "name"       => "SHORT_BINARY_16",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 112
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 17,
        "name"       => "SHORT_BINARY_17",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 113
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 18,
        "name"       => "SHORT_BINARY_18",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 114
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 19,
        "name"       => "SHORT_BINARY_19",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 115
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 20,
        "name"       => "SHORT_BINARY_20",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 116
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 21,
        "name"       => "SHORT_BINARY_21",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 117
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 22,
        "name"       => "SHORT_BINARY_22",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 118
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 23,
        "name"       => "SHORT_BINARY_23",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 119
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 24,
        "name"       => "SHORT_BINARY_24",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 120
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 25,
        "name"       => "SHORT_BINARY_25",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 121
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 26,
        "name"       => "SHORT_BINARY_26",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 122
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 27,
        "name"       => "SHORT_BINARY_27",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 123
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 28,
        "name"       => "SHORT_BINARY_28",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 124
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 29,
        "name"       => "SHORT_BINARY_29",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 125
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 30,
        "name"       => "SHORT_BINARY_30",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 126
    },

    # autoupdated by Sereal.git:Perl/shared/author_tools/update_from_header.pl do not modify directly!
    {
        "masked"     => 1,
        "masked_val" => 31,
        "name"       => "SHORT_BINARY_31",
        "type_name"  => "SHORT_BINARY",
        "type_value" => 96,
        "value"      => 127
    } );

$TAG_INFO_HASH{ chr $_ }= $TAG_INFO_ARRAY[$_] for 0 .. 127;
push @EXPORT_OK, qw(%TAG_INFO_HASH @TAG_INFO_ARRAY);

# stop autoupdated section - do not modify directly!
1;
