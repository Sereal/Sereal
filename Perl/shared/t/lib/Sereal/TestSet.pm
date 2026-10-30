package    # Hide from PAUSE
    Sereal::TestSet;

use strict;
use warnings;

use File::Spec;
use Scalar::Util qw(weaken);
use Test::More;
use Test::LongString;

#use Data::Dumper; # MUST BE LOADED *AFTER* THIS FILE (BUG IN PERL)
use Devel::Peek;
use Encode       qw(encode_utf8 is_utf8);
use Scalar::Util qw(reftype blessed refaddr);
use Config;
use Carp     qw(confess);
use Storable qw(dclone);
use Cwd;

# Dynamically load constants from whatever is being tested
our ( $Class, $ConstClass, $InRepo );

sub get_git_top_dir {
    my @dirs = ( 0, 1, 2, 4 );
    for my $d (@dirs) {
        my $tdir = File::Spec->catdir( map File::Spec->updir, 1 .. $d );
        my $gdir = File::Spec->catdir( $tdir,                 '.git' );
        return $tdir
            if -e $gdir;
    }
    return ();
}

BEGIN {
    if ( defined( my $top_dir = get_git_top_dir() ) ) {
        my @need = -e 'lib/Sereal.pm' || -e 'lib/Sereal/Encoder.pm'
            ? ( 'Encoder', 'Decoder' )
            : ('Decoder');
        for my $need (@need) {
            my $blib_dir = File::Spec->catdir( $top_dir, 'Perl', $need, "blib" );
            if ( -d $blib_dir ) {
                require blib;
                blib->import($blib_dir);
            }
        }
        $InRepo = 1;
    }
}

BEGIN {
    if ( -e "lib/Sereal.pm" ) {
        $Class = 'Sereal::Encoder';
    }
    elsif ( -e "lib/Sereal/Encoder.pm" ) {
        $Class = 'Sereal::Encoder';
    }
    elsif ( -e "lib/Sereal/Decoder.pm" ) {
        $Class = 'Sereal::Decoder';
    }
    elsif ( -e "lib/Sereal/Merger.pm" ) {
        $Class = 'Sereal::Merger';
    }
    elsif ( -e "lib/Sereal/Splitter.pm" ) {
        $Class = 'Sereal::Splitter';
    }
    else {
        die "Could not find an applicable Sereal constants location (in: ", cwd(), ")";
    }
    $ConstClass = $Class . "::Constants";
    eval "use $ConstClass ':all'; 1"
        or do {
        my $err = $@ || 'Zombie Error';
        die "Failed to load/import constants from '$ConstClass': $err";
        };
}

use Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(
    @BasicTests $Class $ConstClass
    Header
    TRACK_FLAG
    hobodecode
    integer short_string varint read_varint_at array array_fbit checksum_tag
    hash dump_bless
    have_encoder_and_decoder
    array_slot_ref
    regexp_modifier_test_cases
    check_for_dependency_issues
    run_roundtrip_tests
    run_roundtrip_version_tests
    run_roundtrip_corpus_tests
    run_roundtrip_profile_tests
    roundtrip_corpus_name
    write_test_files
    $use_objectv
    setup_tests
    _deep_cmp
    _test_deep_cmp
    _test
    _cmp_str
);

our %EXPORT_TAGS = ( all => \@EXPORT_OK );
our $use_objectv = 1;

use constant TRACK_FLAG => 128;

sub checksum_tag {
    my ( $type, $bytes, $key ) = @_;
    my %functions = ( 0 => 'siphash24', 1 => 'siphash24_128', 2 => 'siphash36_128' );
    my $name      = $functions{$type}  or die "Unknown checksum type $type";
    my $function  = $Class->can($name) or die "$Class does not provide $name";
    return $function->( $bytes, $key );
}

sub hobodecode {
    return unless defined $_[0];
    open my $fh, "| $^X -Mblib=../Encoder -Mblib=../Decoder author_tools/hobodecoder.pl -e"
        or die $!;
    print $fh $_[0];
    close $fh;
}

sub array_head {
    if ( $_[0] >= 16 ) {
        return chr(SRL_HDR_REFN) . chr(SRL_HDR_ARRAY) . varint( $_[0] );
    }
    else {
        return chr( SRL_HDR_ARRAYREF + $_[0] );
    }
}

sub array {
    array_head( 0 + @_ ) . join( "", @_ );
}

sub array_fbit {
    chr(SRL_HDR_REFN) . chr( SRL_HDR_ARRAY +TRACK_FLAG ) . varint( 0 + @_ ) . join( "", @_ );
}

sub hash_head {
    my $ret;
    my $len = int $_[0] / 2;
    if ( $len >= 16 ) {
        return chr(SRL_HDR_REFN) . chr(SRL_HDR_HASH) . varint($len);
    }
    else {
        return chr( SRL_HDR_HASHREF + $len );
    }
}

sub hash {
    hash_head( 0 + @_ ) . join( "", @_ );
}

sub dump_bless {

    # this hack does not support UTF8 class names, but that's not supported by
    # most releases of perl anyway
    (
        ref( $_[1] )
        ? (
            $use_objectv
            ? chr(SRL_HDR_OBJECTV) . varint( ${ $_[1] } )
            : chr(SRL_HDR_OBJECT) . chr(SRL_HDR_COPY) . varint( ${ $_[1] } ) )
        : chr(SRL_HDR_OBJECT)
            . (
            ( length( $_[1] ) >= SRL_MASK_SHORT_BINARY_LEN )
            ? chr(SRL_HDR_BINARY) . varint( length( $_[1] ) ) . $_[1]
            : chr( length( $_[1] ) + SRL_HDR_SHORT_BINARY_LOW ) . $_[1] ) ) . $_[0];
}

sub short_string {
    my ( $str, $alias ) = @_;
    $alias ||= 0;
    my $length = length($str);
    if ( $length > SRL_MASK_SHORT_BINARY_LEN ) {
        confess "String too long for short_string(), alias=$alias length=$length";
    }
    my $tag = SRL_HDR_SHORT_BINARY_LOW + length($str);
    if ( $tag > SRL_HDR_SHORT_BINARY_HIGH ) {
        confess
            "Tag value larger than SRL_HDR_SHORT_BINARY_HIGH, tag=$tag; alias=$alias; length=$length";
    }
    $tag |= SRL_HDR_TRACK_FLAG if $alias;
    if ( $tag > 255 ) {
        confess
            "Tag value over 255 in short_string(), tag=$tag; alias=$alias; length=$length; SRL_HDR_TRACK_FLAG=",
            SRL_HDR_TRACK_FLAG;
    }
    return chr($tag) . $str;
}

sub integer {
    if ( $_[0] < 0 ) {
        return $_[0] < -16
            ? die("zigzag not implemented in test suite")
            : chr( 0b0001_0000 + abs( $_[0] ) );
    }
    else {
        return $_[0] > 15
            ? varint( $_[0] )
            : chr( 0b0000_0000 + $_[0] );
    }
}

sub varint {
    my $n = shift;
    die "varint cannot be negative" if $n < 0;
    my $out = '';
    while ( $n >= 0x80 ) {
        $out .= chr( ( $n & 0x7f ) | 0x80 );
        $n >>= 7;
    }
    $out .= chr($n);
    return $out;
}

sub read_varint_at {
    my ( $bytes, $offset ) = @_;
    my $length = length($bytes);
    die "varint offset is outside input" if $offset < 0 || $offset > $length;

    my $uv_bits   = 8 * $Config{uvsize};
    my $max_bytes = int( ( $uv_bits + 6 ) / 7 );
    my ( $value, $shift ) = ( 0, 0 );
    for my $index ( 0 .. $max_bytes - 1 ) {
        die "truncated varint" if $offset >= $length;
        my $byte = ord( substr( $bytes, $offset++, 1 ) );
        my $part = $byte & 0x7f;
        if ( $index == $max_bytes - 1 ) {
            my $bits_left = $uv_bits - $shift;
            my $max_part  = ( 1 << $bits_left ) - 1;
            die "varint too big" if $part > $max_part;
        }
        $value |= $part << $shift;
        return ( $value, $offset ) unless $byte & 0x80;
        $shift += 7;
    }

    die "varint too long";
}

our $PROTO_VERSION;

sub Header {
    my $proto_version  = shift || $PROTO_VERSION || SRL_PROTOCOL_VERSION;
    my $user_data_blob = shift;
    my $mgc            = $proto_version > 2 ? SRL_MAGIC_STRING_HIGHBIT : SRL_MAGIC_STRING;
    my $hdr_base       = $mgc . chr($proto_version);
    if ( defined $user_data_blob ) {
        return $hdr_base . varint( 1 + length($user_data_blob) ) . chr(1) . $user_data_blob;
    }
    else {
        return $hdr_base . chr(0);
    }
}

sub offset {
    my ($str) = @_;
    Carp::confess("no protoversion") if !defined $PROTO_VERSION;
    if ( $PROTO_VERSION >= 2 ) {
        return length($str) + 1;
    }
    else {
        return length($str) + length Header($PROTO_VERSION);
    }
}

sub offseti {
    my ($i) = @_;
    if ( $PROTO_VERSION >= 2 ) {
        return $i + 1;
    }
    else {
        return $i + length Header($PROTO_VERSION);
    }
}

sub _permute {
    return [] unless @_;
    my $vals = shift;
    my @rest = _permute(@_);
    map {
        my $v = $_;
        map { [ $v, @$_ ] } @rest
    } @$vals;
}

sub permute_array {
    map { array(@$_) } _permute(@_);
}

sub debug_checks {
    my ( $data_ref, $encoded_ref, $decoded_ref, $debug ) = @_;
    if ( $debug or defined $ENV{DEBUG_SEREAL} ) {
        require Data::Dumper;
        note( "Original data was: " . Data::Dumper::Dumper($$data_ref) )
            if defined $data_ref;
        note( "Encoded data is: "
                . ( defined($$encoded_ref) ? Data::Dumper::qquote($$encoded_ref) : "<undef>" ) )
            if defined $encoded_ref;
        note( "Decoded data was: " . Data::Dumper::Dumper($$decoded_ref) )
            if defined $decoded_ref;
    }
    if ( defined $ENV{DEBUG_DUMP} ) {
        Dump($$data_ref)    if defined $data_ref;
        Dump($$encoded_ref) if defined $encoded_ref;
        Dump($$decoded_ref) if defined $decoded_ref;
    }
    if ( defined $ENV{DEBUG_HOBO} ) {
        hobodecode($$encoded_ref) if defined $encoded_ref;
    }
    exit() if $ENV{DEBUG_FAIL_FATAL};
}

our @BasicTests;

sub setup_tests {
    my ($proto_version) = @_;
    $PROTO_VERSION = $proto_version if defined $proto_version;
    my $ary_ref_for_repeating    = [ 5, 6 ];
    my $scalar_ref_for_repeating = \9;

    my $weak_thing; $weak_thing = [ \$weak_thing, 1 ]; weaken( $weak_thing->[0] );

    my $unicode1 = "Ba\xDF Ba\xDF"; my $unicode2 = "\x{168}nix! \x{263a}"; utf8::upgrade($unicode1);
    utf8::upgrade($unicode2);

    # each test is an array:
    # index 0 is the input to the encoder
    # index 1 is the output *without* header - or a sub which returns an expected output
    # index 2 is the name of the test
    # index 3 and on are alternate outputs (or subs which return alternate output(s))
    @BasicTests = (

        # warning: this hardcodes the POS/NEG headers
        [ -16,   chr(0b0001_0000),    "encode -16" ],
        [ -1,    chr(0b0001_1111),    "encode -1" ],
        [ 0,     chr(0b0000_0000),    "encode 0" ],
        [ 1,     chr(0b0000_0001),    "encode 1" ],
        [ 15,    chr(0b0000_1111),    "encode 15" ],
        [ undef, chr(SRL_HDR_UNDEF),  "encode undef" ],
        [ "",    short_string(""),    "encode empty string" ],
        [ "1",   short_string("1"),   "encode string '1'" ],
        [ "91a", short_string("91a"), "encode string '91a'" ],
        [
            "abc" x 1000, chr(SRL_HDR_BINARY) . varint(3000) . ( "abc" x 1000 ), "long ASCII string"
        ],
        [ \1,          chr(SRL_HDR_REFN) . chr(0b0000_0001), "scalar ref to int" ],
        [ [],          array(),                              "empty array ref" ],
        [ [ 1, 2, 3 ], array( chr(0b0000_0001), chr(0b0000_0010), chr(0b0000_0011) ), "array ref" ],
        [ 1000,        chr(SRL_HDR_VARINT) . varint(1000),                            "large int" ],
        [
            [ map { $_, undef } 1 .. 1000 ],
            array(
                ( map { chr($_) => chr(SRL_HDR_UNDEF) } ( 1 .. SRL_POS_MAX_SIZE ) ),
                (
                    map { chr(SRL_HDR_VARINT) . varint($_) => chr(SRL_HDR_UNDEF) }
                        ( ( SRL_POS_MAX_SIZE + 1 ) .. 1000 ) )
            ),
            "array ref with pos and varints and undef"
        ],
        [ {}, hash(), "empty hash ref" ],
        [
            { foo => "baaaaar" }, hash( short_string("foo"), short_string("baaaaar") ),
            "simple hash ref"
        ],
        [
            [qw(foooo foooo foooo)],
            sub {
                my $opt = shift;
                if ( $opt->{dedupe_strings} || $opt->{aliased_dedupe_strings} ) {
                    my $d   = array_head(3);
                    my $pos = offset($d);
                    my $tag = $opt->{aliased_dedupe_strings} ? SRL_HDR_ALIAS : SRL_HDR_COPY;
                    $d .=
                          short_string( "foooo", $opt->{aliased_dedupe_strings} ? 1 : 0 )
                        . chr($tag)
                        . varint($pos)
                        . chr($tag)
                        . varint($pos);
                    return $d;
                }
                else {
                    return array(
                        short_string("foooo"), short_string("foooo"),
                        short_string("foooo") );
                }
            },
            "ary ref with repeated string"
        ],
        [
            [ { foooo => "barrr" }, { barrr => "foooo" } ],
            sub {
                my $opt = shift;
                if ( ( $opt->{dedupe_strings} || $opt->{aliased_dedupe_strings} )
                    && !$opt->{no_shared_hashkeys} )
                {
                    my $d       = array_head(2) . hash_head(2);
                    my $key_pos = offset($d);
                    $d .= short_string("foooo");
                    my $value_pos = offset($d);
                    return
                          $d
                        . short_string("barrr")
                        . hash_head(2)
                        . chr(SRL_HDR_COPY)
                        . varint($value_pos)
                        . chr(SRL_HDR_COPY)
                        . varint($key_pos);
                }
                return array(
                    hash( short_string("foooo"), short_string("barrr") ),
                    hash( short_string("barrr"), short_string("foooo") ) );
            },
            "ary ref of hash refs without repeated strings"
        ],
        [
            [ { foooo => "foooo" }, { foooo2 => "foooo" } ],
            sub {
                my $opt = shift;
                if ( $opt->{dedupe_strings} || $opt->{aliased_dedupe_strings} ) {
                    my $tag     = $opt->{aliased_dedupe_strings} ? SRL_HDR_ALIAS : SRL_HDR_COPY;
                    my $d       = array_head(2) . hash_head(2);
                    my $key_pos = offset($d);
                    $d .= short_string("foooo");
                    my $pos = offset($d);
                    my $first_value =
                        $opt->{no_shared_hashkeys}
                        ? short_string("foooo")
                        : chr(SRL_HDR_COPY) . varint($key_pos);
                    substr( $first_value, 0, 1 ) = chr( ord($first_value) | TRACK_FLAG )
                        if $opt->{aliased_dedupe_strings};
                    $d .=
                          $first_value
                        . hash_head(2)
                        . short_string("foooo2")
                        . chr($tag)
                        . varint( $opt->{aliased_dedupe_strings}
                            || $opt->{no_shared_hashkeys} ? $pos : $key_pos );
                    return $d;
                }
                else {
                    return array(
                        hash( short_string("foooo"),  short_string("foooo") ),
                        hash( short_string("foooo2"), short_string("foooo") )
                        ),
                        ;
                }
            },
            "ary ref of hash refs with repeated strings"
        ],
        [
            $scalar_ref_for_repeating, chr(SRL_HDR_REFN) . chr(0b0000_1001),
            "scalar ref to constant"
        ],
        [
            [ $scalar_ref_for_repeating, $scalar_ref_for_repeating ],
            do {
                my $content = array_head(2);
                $content .= chr(SRL_HDR_REFN);
                my $pos = offset($content);
                $content .= chr(0b1000_1001) . chr(SRL_HDR_REFP) . varint($pos);
                $content;
            },
            "repeated substructure (REFP): scalar ref"
        ],
        [
            [ $ary_ref_for_repeating, $ary_ref_for_repeating ],
            do {
                my $content = array_head(2);
                my $pos     = offset($content) + 1;
                $content .=
                      array_fbit( chr(0b0000_0101), chr(0b0000_0110) )
                    . chr(SRL_HDR_REFP)
                    . varint($pos);
                $content;
            },
            "repeated substructure (REFP): array"
        ],
        [
            [ \$ary_ref_for_repeating, [ 1, $ary_ref_for_repeating ] ],
            do {
                my $content = array_head(2) . chr(SRL_HDR_REFN);
                my $pos     = offset($content) + 1;
                $content .= array_fbit(
                    chr(0b0000_0101),
                    chr(0b0000_0110) )
                    . array(
                    chr(0b0000_0001),
                    chr(SRL_HDR_REFP) . varint($pos) );
                $content;
            },
            "repeated substructure (REFP): asymmetric"
        ],
        [
            $weak_thing,
            chr(SRL_HDR_REFN)
                . chr( SRL_HDR_ARRAY + TRACK_FLAG )
                . varint(2)
                . chr(SRL_HDR_PAD)
                . chr(SRL_HDR_REFN)
                . chr(SRL_HDR_REFP)
                . varint( offseti(1) )
                . chr(0b0000_0001),
            "weak thing copy (requires PAD)"
        ],
        [
            \$weak_thing,
            chr(SRL_HDR_REFN)
                . chr( SRL_HDR_REFN + TRACK_FLAG )
                . chr(SRL_HDR_ARRAY)
                . varint(2)
                . chr(SRL_HDR_WEAKEN)
                . chr(SRL_HDR_REFP)
                . varint( offseti(1) )
                . chr(0b0000_0001),
            "weak thing ref"
        ],
        sub { \@_ }
            ->(
            $weak_thing,
            chr( SRL_HDR_REFN + TRACK_FLAG )
                . chr(SRL_HDR_ARRAY)
                . varint(2)
                . chr(SRL_HDR_WEAKEN)
                . chr(SRL_HDR_REFP)
                . varint( offseti(0) )
                . chr(0b0000_0001),
            "weak thing (aliased root)"
            ),
        [
            do { my @array; $array[0] = \$array[1]; $array[1] = \$array[0]; \@array },
            do {
                my $content = array_head(2);
                my $pos     = offset($content);
                $content
                    . chr( SRL_HDR_REFN + TRACK_FLAG )
                    . chr( SRL_HDR_REFP + TRACK_FLAG )
                    . varint($pos)
                    . chr(SRL_HDR_ALIAS)
                    . varint( $pos + 1 );
            },
            "scalar cross"
        ],
        [
            do {
                my @array; $array[0] = \$array[1]; $array[1] = \$array[0]; weaken( $array[1] );
                weaken( $array[0] ); \@array;
            },
            do {
                my $content = array_head(2);
                my $pos     = offset($content);
                $content
                    . chr( SRL_HDR_WEAKEN + TRACK_FLAG )
                    . chr(SRL_HDR_REFN)
                    . chr( SRL_HDR_WEAKEN + TRACK_FLAG )
                    . chr(SRL_HDR_REFP)
                    . varint($pos)
                    . chr(SRL_HDR_ALIAS)
                    . varint( $pos + 2 );
            },
            "weak scalar cross"
        ],
        [
            bless( [], "foo" ),
            dump_bless( array(), "foo" ),
            "bless [], 'foo' (2)"
        ],
        [
            do { my $qr = bless qr/foo/ix, "bar"; [ $qr, $qr ] },
            do {
                my $content = array_head(2);
                my $pos     = offset($content);
                join(
                    "", $content,
                    chr(SRL_HDR_OBJECT),
                    short_string("bar"),
                    chr(SRL_HDR_REFN),
                    chr( SRL_HDR_REGEXP + TRACK_FLAG ),
                    short_string("foo"),
                    short_string("ix"),
                    chr(SRL_HDR_REFP),
                    varint( $pos + 6 ),
                );
            },
            "blessed regexp with reuse"
        ],
        [
            do { my $o1 = bless [], "foo"; my $o2 = bless [], "foo"; [ $o1, $o2, $o1, $o2 ] },
            do {
                my $content = array_head(4) . chr(SRL_HDR_OBJECT);
                my $pos     = offset($content);
                join(
                    "", $content,
                    short_string("foo"),
                    chr(SRL_HDR_REFN) . chr( SRL_HDR_ARRAY + TRACK_FLAG ), varint(0),
                    chr( SRL_HDR_OBJECT + $use_objectv ),
                    $use_objectv ? () : chr(SRL_HDR_COPY),                 varint($pos),
                    chr(SRL_HDR_REFN) . chr( SRL_HDR_ARRAY + TRACK_FLAG ), varint(0),
                    chr(SRL_HDR_REFP),                                     varint( $pos + 5 ),
                    chr(SRL_HDR_REFP),                                     varint( $pos + 10 ),
                );
            },
            "blessed arrays with reuse"
        ],
        [
            [ bless( [], "foo" ), bless( [], "foo" ) ],
            do {
                my $content = array_head(2) . chr(SRL_HDR_OBJECT);
                my $pos     = offset($content);
                $content .= short_string("foo") . array() . dump_bless( array(), \$pos );
                $content;
            },
            "reused classname empty array"
        ],
        [
            bless( [ bless {}, "foo" ], "foo" ),
            do {
                my $content = chr(SRL_HDR_OBJECT);
                my $pos     = offset($content);
                $content .= short_string("foo") . array_head(1) . dump_bless( hash(), \$pos );
                $content;
            },
            "wrapped objects"
        ],
        [
            qr/foo/,
            dump_bless(
                chr(SRL_HDR_REFN) . chr(SRL_HDR_REGEXP) . short_string("foo") . short_string(""),
                "Regexp"
            ),
            "qr/foo/"
        ],
        [
            qr/(?i-xsm:foo)/,
            dump_bless(
                      chr(SRL_HDR_REFN)
                    . chr(SRL_HDR_REGEXP)
                    . short_string("(?i-xsm:foo)")
                    . short_string(""),
                "Regexp"
            ),
            "qr/(?i-xsm:foo)/"
        ],
        [
            qr/foo/i,
            dump_bless(
                chr(SRL_HDR_REFN) . chr(SRL_HDR_REGEXP) . short_string("foo") . short_string("i"),
                "Regexp"
            ),
            "qr/foo/i"
        ],
        [
            [ { foo => 1 }, { foo => 2 } ],
            sub {
                my $opt = shift;
                if ( $opt->{no_shared_hashkeys} ) {
                    return array(
                        hash(
                            short_string("foo"),
                            integer(1),
                        ),
                        hash(
                            short_string("foo"),
                            integer(2),
                        ),
                    );
                }
                else {
                    my $content = array_head(2);
                    return join(
                        "",
                        $content,
                        hash(
                            short_string("foo"),
                            integer(1),
                        ),
                        hash(
                            chr(SRL_HDR_COPY) . varint( offset($content) + 1 ),
                            integer(2),
                        ),
                    );
                }
            },
            "duplicate hash keys"
        ],
        [
            { $unicode1 => $unicode2 },
            hash(
                chr(SRL_HDR_STR_UTF8) . varint( bytes::length($unicode1) ) . encode_utf8($unicode1),
                chr(SRL_HDR_STR_UTF8) . varint( bytes::length($unicode2) ) . encode_utf8($unicode2),
            ),
            "simple unicode hash key and value"
        ],

        # Test true/false. Due to some edge case behavior in perl these two tests
        # produce different "expected" results depending on such things as how many
        # times we perform the test. Therefore we allow various "alternates" to
        # be produced. An example of the underlying weirdness is that on an unthreaded
        # linux perl 5.14 the two tests have their expected output first, which
        # as you will note is different for the first and second call, despite the underlying
        # code being the same both times.
        #
        # So for instance the first test need not have the last two options, at least
        # on perl 5.14, but the second test requires one of those options. Working around
        # perl bugs sucks.
        [
            sub { \@_ }
                ->( !1, !0 ),
            array( chr(SRL_HDR_FALSE), chr(SRL_HDR_TRUE) ),    # this is the "correct" response.
            "true/false (prefered order)",
            permute_array( [
                    short_string(""),
                    chr(SRL_HDR_FALSE),
                ],
                [
                    chr(SRL_HDR_TRUE),
                    short_string("1"),
                    integer(1) ]
            ),                                                 # this is what threaded perls will probably match
        ],
        [
            sub { \@_ }
                ->( !1, !0 ),
            array( short_string(""), short_string("1") ),      # this is the expected value on perl 5.14 unthreaded
            "true/false (reversed alternates)",
            permute_array( [
                    short_string(""),
                    chr(SRL_HDR_FALSE)
                ],
                [
                    chr(SRL_HDR_TRUE),
                    integer(1),
                    short_string("1") ]
            ),
        ],
    );
}

sub regexp_modifier_test_cases {
    # First stable releases: /msix are present in Perl 5.000, /p is 5.10,
    # /dluaa is 5.14, /n is 5.22 and /xx is 5.26. Check the development
    # introductions too: a reserved flag bit does not imply working /xx.
    # Perl origin/blead commits: 87e95b7ff3 (/p), 94b03d7d32 (charset
    # suffixes), 41d7c59e80 (/n), and 77c8f26370 (/xx).
    my $charset       = $] >= 5.013010;
    my $nocapture     = $] >= 5.021008;
    my $extended_more = $] >= 5.025009;
    return (
        [ 'm',         '^b',           "a\nb",     1, 1 ],
        [ 's',         'a.b',          "a\nb",     1, 1 ],
        [ 'i',         'a',            'A',        1, 1 ],
        [ 'x',         'a b',          'ab',       1, 1 ],
        [ 'p',         'a',            'a',        1, $] >= 5.010 ],
        [ 'd',         '\\w',          "\xe9",     0, $charset ],
        [ 'u',         '\\w',          "\xe9",     1, $charset ],
        [ 'ui',        'foo\\xdf',     'fooss',    1, $charset ],
        [ 'l',         '[[:alpha:]]',  'a',        1, $charset ],
        [ 'a',         '\\w',          "\xe9",     0, $charset ],
        [ 'ai',        'k',            "\x{212a}", 1, $charset ],
        [ 'aai',       'k',            "\x{212a}", 0, $charset ],
        [ 'n',         '(a)',          'a',        1, $nocapture ],
        [ 'xx',        '[ a]',         ' ',        0, $extended_more ],
        [ 'xx',        '[ a]',         'a',        1, $extended_more ],
        [ 'xxx',       '[ a]',         ' ',        0, $extended_more ],
        [ 'uimsxnp',   '^(a) . b$',    "A\nb",     1, $charset && $nocapture ],
        [ 'aaimsxxnp', '^([ a]) . b$', "A\nb",     1, $extended_more ],
        [ '',          '(?i:a)',       'A',        1, 1 ],
    );
}

# Inspect an array slot without asking av_fetch() for an lvalue. Older
# Perls replace PL_sv_undef with a fresh scalar when evaluating \$array->[$i],
# which would destroy the alias identity these tests are inspecting.
sub array_slot_ref {
    my ( $array, $index ) = @_;
    require B;
    my @slots = B::svref_2object($array)->ARRAY;
    my $slot = $slots[$index];
    return \undef if ref($slot) eq 'B::SPECIAL' && $$slot == 1;
    return $slot->object_2svref;
}

sub have_encoder_and_decoder {
    my $ret = check_for_dependency_issues(@_);
    diag $ret if $ret;
    return !$ret;
}

my %have_version;

sub check_for_dependency_issues {
    my ($min_v) = @_;
    foreach my $class ( "Sereal::Encoder", "Sereal::Decoder" ) {
        eval "use $class; 1" or next;
        $have_version{$class} = $class->VERSION;
    }
    my $Other =
        ( $Class eq "Sereal::Decoder" )
        ? "Sereal::Encoder"
        : "Sereal::Decoder";

    my $this_version = $have_version{$Class}
        or die "panic: No version for the module ($Class) we are building?";

    my $other_version = $have_version{$Other}
        or return "No $Other available to test with.";

    if ( !$min_v and $0 =~ m![\\/]v(\d+)[\\/]! ) {
        $min_v = $1;
        if ( ( $Config{usequadmath} || $Config{uselongdouble} ) && $min_v < 5 ) {
            $min_v = 5;
        }
    }
    if ( $min_v and $other_version < $min_v ) {
        return "This test requires version $min_v for $Other but we have version $other_version";
    }
    return "";
}

# max iv/uv logic taken from Storable tests
my $max_uv    = ~0;
my $max_uv_m1 = ~0 ^ 1;

# Express it in this way so as not to use any addition, as 5.6 maths would
# do this in NVs on 64 bit machines, and we're overflowing IVs so can't use
# use integer.
my $max_iv_p1  = $max_uv ^ ( $max_uv >> 1 );
my $lots_of_9C = do {
    my $temp = sprintf "%#x", ~0;
    $temp =~ s/ff/9c/g;
    local $^W;
    no warnings;
    eval $temp;
};
my $max_iv = ~0 >> 1;
my $min_iv = do { use integer; -$max_iv - 1 };    # 2s complement assumption

my @numstr = map { ; no warnings; $_ < 0 and warn "this shouldnt happpen"; $_ } (
    "    1    ",  "0.0",  "00000.0000", "0.0.0.0", ".0", "    .0", " 22",
    "01",         "01.1", "   0   ",    ".0", "0.001", ".1", "  .1", ".2", "00", ".00",
    "0 but true", "0E0"
);
my $eng0e0 = "0e0";
my $eng0e1 = "0e1";
my $eng2   = "1e3";

my $sum = $eng0e0 + $eng0e1 + $eng2;

sub encoder_required {
    my ( $ver, $name ) = @_;
    return "" . ( !defined($Sereal::Encoder::VERSION) || $Sereal::Encoder::VERSION < $ver ? "TODO " : "" ) . $name;
}

sub _get_roundtrip_tests {
    my @ScalarRoundtripTests = (

        # name, structure, optional minimum UV width for stored integer fixtures
        [ "undef",                           undef ],
        [ "small int",                       3 ],
        [ "small negative int",              -8 ],
        [ "largeish int",                    100000 ],
        [ "largeish negative int -302001",   -302001 ],
        [ "largeish negative int -1234567",  -1234567 ],
        [ "largeish negative int -12345678", -12345678 ],
        [ "negative int -16",                -16 ],
        [ "negative int -17",                -17 ],
        [ "negative int -33",                -33 ],

        (
            # The stored corpus encodes these as integers on a 64-bit Perl.
            # A native 32-bit live encoder can instead use a floating value.
            map {
                [ "integer: $_", 0 + $_,
                    $_ < -2147483648 || $_ > 4294967295 ? 64 : 32 ]
            } (

                # IV bounds of 8 bits
                -1, 0, 1, -127, -128, -129, 42, 126, 127, 128, 129, 254, 255, 256, 257,

                # IV bounds of 32 bits
                -2147483647, -2147483648, -2147483649, 2147483646, 2147483647, 2147483648,

                # IV bounds
                $min_iv, do { use integer; $min_iv + 1 }, do { use integer; $max_iv - 1 },
                $max_iv,

                # UV bounds at 32 bits
                0x7FFFFFFF, 0x80000000, 0x80000001, 0xFFFFFFFF, 0xDEADBEEF,

                # UV bounds
                $max_iv_p1, $max_uv_m1, $max_uv, $lots_of_9C,
                $eng0e0,    $eng0e1,    $eng2,
            )
        ),
        ( map { [ "float $_", 0 + $_ ] } ( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ) ),
        [ "short ascii string",  "fooo" ],
        [ "short latin1 string", "Müller" ],
        [
            "short utf8 string",
            do { use utf8; " עדיין ח" }
        ],

        (
            map { [
                    "long ascii string 'a' x $_",
                    do { "a" x $_ }
                ]
            } (
                9999, 10000, 10001,
                1023, 1024,  1025,
                8191, 8192,  8193,
            )
        ),
        (
            map { [
                    "long ascii string 'ab' x $_",
                    do { "ab" x $_ }
                ]
            } (
                9999, 10000, 10001,
                1023, 1024,  1025,
                8191, 8192,  8193,
            )
        ),
        (
            map { [
                    "long ascii string 'abc' x $_",
                    do { "abc" x $_ }
                ]
            } (
                9999, 10000, 10001,
                1023, 1024,  1025,
                8191, 8192,  8193,
            )
        ),
        (
            map { [
                    "long ascii string 'abcd' x $_",
                    do { "abcd" x $_ }
                ]
            } (
                9999, 10000, 10001,
                1023, 1024,  1025,
                8191, 8192,  8193,
            )
        ),
        (
            map { [
                    encoder_required( 3.005002, " troublesome num/strs '$_'" ),
                    $_
                ]
            } @numstr
        ),
        [ "long latin1 string", "üll" x 10000 ],
        [
            "long utf8 string",
            do { use utf8; " עדיין חשב" x 10000 }
        ],
        [
            "long utf8 string with only ascii",
            do { use utf8; "foo" x 10000 }
        ],
        [
            "long utf8 string with only latin1 subset",
            do { use utf8; "üll" x 10000 }
        ],

        [ "simple regexp",                qr/foo/ ],
        [ "regexp with inline modifiers", qr/(?i-xsm:foo)/ ],
        [ "regexp with modifiers",        qr/foo/i ],
        [ "float",                        123013.139 ],
        [ "negative float",               -1234.59 ],
        [ "small float 0.41",             0.41 ],
        [ "negative small float -0.13",   -0.13 ],
        [ "small int",                    123 ],
        [ "empty string",                 '' ],
        [ "simple array",                 [] ],
        [ "empty hash",                   {} ],
        [ "simple hash",                  { foo => 'bar' } ],
        [ "undef value",                  { foo => bar => baz => undef } ],
        [ "simple array",                 [1] ],
        [ "nested simple",                [ 1, [2] ] ],
        [ "deep nest", [ 1, 2, [ 3, 4, { 5 => 6, 7 => { 8 => [] }, 9 => {} }, {}, [] ] ] ],
        [
            "complex hash", {
                foo          => 123,
                bar          => -159, pi => 3,
                'baz'        => "foo",
                'bop \''     => "\10",      'bop \'\\' => "\x{100}",
                'bop \'x\\x' => "x\x{100}", 'bing'     => "x\x{100}",
                x            => 'y', z => 'p', i => '1', l => " \10", m => "\10 ", n => " \10 ",
            }
        ],
        [
            "complex hash with float", {
                foo          => 123,
                bar          => -159.23, a_pi => 3.14159,
                'baz'        => "foo",
                'bop \''     => "\10",      'bop \'\\' => "\x{100}",
                'bop \'x\\x' => "x\x{100}", 'bing'     => "x\x{100}",
                x            => 'y', z => 'p', i => '1', l => " \10", m => "\10 ", n => " \10 ",
            }
        ],
        [
            "more complex", {
                foo      => [123],
                "bar"    => [ -159, n => 3, { 'baz' => "foo", }, ],
                'bop \'' => {
                    "\10" => {
                        'bop \'\\' => "\x{100}",
                        h          => {
                            'bop \'x\\x' => "x\x{100}", 'bing' => "x\x{100}",
                            x            => 'y',
                        },
                        z => 'p',
                    },
                    i => '1',
                },
                l => " \10",
                m => "\10 ",
                n => " \10 ",
                o => undef,
                p => undef,
                q => \undef,
                r => \$eng0e0,
                u => \$eng0e1,
                w => \$eng2
            }
        ],
        [
            "more complex with float", {
                foo      => [123],
                "bar"    => [ -159.23, a_pi => 3.14159, { 'baz' => "foo", }, ],
                'bop \'' => {
                    "\10" => {
                        'bop \'\\' => "\x{100}",
                        h          => {
                            'bop \'x\\x' => "x\x{100}", 'bing' => "x\x{100}",
                            x            => 'y',
                        },
                        z => 'p',
                    },
                    i => '1',
                },
                l => " \10",
                m => "\10 ",
                n => " \10 ",
                o => undef,
                p => undef,
                q => \undef,
                r => \$eng0e0,
                u => \$eng0e1,
                w => \$eng2
            }
        ],
        [ 'var strings',       [ "\$", "\@", "\%" ] ],
        [ "quote keys",        { "" => '"', "'" => "" } ],
        [ "ref to foo",        \"foo" ],
        [ "double ref to foo", \\"foo" ],
        [ "refy array",        \\["foo"] ],
        [ "reffy hash",        \\\{ foo => \"bar" } ],
        [ "blessed array",     bless( \[], "foo" ) ],
        [ "utf8 string",       "123\\277ABC\\x{DF}456" ],
        [ "escaped string",    "\\012\345\267\145123\\277ABC\\x{DF}456" ],
        [ "more escapes",      "\\0123\0124" ],
        [ "ref to undef",      \undef ],
        [ "negative big num",  -4123456789, 64 ],
        [ "positive big num",  4123456789 ],
        [ "eng-ref",           [ \$eng0e0, \$eng0e1, \$eng2 ] ],
        [ "undef",             [ \undef,   \undef ] ],
    );

    if ( $PROTO_VERSION >= 6 ) {
        push @ScalarRoundtripTests,
            [ "binary Perl v-string", v01.002.003 ],
            [ "UTF8 Perl v-string",   v01.002.0300 ];
    }

    my @blessed_array_check1;
    $blessed_array_check1[0] = "foo";
    $blessed_array_check1[1] = bless \$blessed_array_check1[0], "BlessedArrayCheck";
    $blessed_array_check1[2] = \$blessed_array_check1[0];

    my @blessed_array_check2 = ( 3, 0, 0, 3 );
    $blessed_array_check2[1] = \$blessed_array_check2[0];
    $blessed_array_check2[2] = \$blessed_array_check2[3];
    bless \$blessed_array_check2[0], "BlessedArrayCheck";
    bless \$blessed_array_check2[3], "BlessedArrayCheck";

    my @sc_array = ( 1, 1 );
    $sc_array[0] = bless \$sc_array[1], "BlessedArrayLeft";
    $sc_array[1] = bless \$sc_array[0], "BlessedArrayRight";

    my @RoundtripTests = (
        @ScalarRoundtripTests,
        [ encoder_required( 3.006006, "BlessedArrayCheck 1" ),        \@blessed_array_check1 ],
        [ encoder_required( 3.006006, "BlessedArrayCheck 2" ),        \@blessed_array_check2 ],
        [ encoder_required( 3.006006, "Scalar Cross Blessed Array" ), \@sc_array ],

        [
            "[{foo => 1}, {foo => 2}] - repeated hash keys",
            [ { foo => 1 }, { foo => 2 } ]
        ],

        ( map { [ "scalar ref to " . $_->[0],        ( \( $_->[1] ) ), $_->[2] ] } @ScalarRoundtripTests ),
        ( map { [ "nested scalar ref to " . $_->[0], ( \\( $_->[1] ) ), $_->[2] ] } @ScalarRoundtripTests ),
        ( map { [ "array ref to " . $_->[0],         ( [ $_->[1] ] ), $_->[2] ] } @ScalarRoundtripTests ),
        ( map { [ "hash ref to " . $_->[0], ( { foo => $_->[1] } ), $_->[2] ] } @ScalarRoundtripTests ),

        # ---
        (
            map { [ "array ref to duplicate " . $_->[0], ( [ $_->[1], $_->[1] ] ), $_->[2] ] }
                @ScalarRoundtripTests
        ),
        (
            map {
                [
                    "AoA of duplicates " . $_->[0],
                    ( [
                            $_->[1], [ $_->[1], $_->[1] ], $_->[1], [ $_->[1], $_->[1], $_->[1] ],
                            $_->[1] ] ), $_->[2] ]
            } @ScalarRoundtripTests
        ),

        # ---
        (
            map { [
                    "array ref to aliases " . $_->[0], (
                        sub { \@_ }
                            ->( $_->[1], $_->[1] ) ), $_->[2] ]
            } @ScalarRoundtripTests
        ),
        (
            map {
                [
                    "array ref to scalar refs to same " . $_->[0],
                    ( [ \( $_->[1] ), \( $_->[1] ) ] ), $_->[2] ]
            } @ScalarRoundtripTests
        ),
    );
    if ( eval "use Array::RefElem (av_store hv_store); 1" ) {
        my $x = "alias!";
        my ( @av, %hv );
        av_store( @av, 0, $x );
        av_store( @av, 1, $x );
        hv_store( %hv, "x", $x );
        hv_store( %hv, "y", $x );
        push @RoundtripTests,
            [ \@av, "alias in array" ],
            [ \%hv, "alias in hash" ],
            [ [ \@av, \%hv, \$x ], "alias hell" ];
    }
    return @RoundtripTests;
}

sub run_roundtrip_tests {
    my ( $name, $opts ) = @_;

    my $proto_version;
    if ( $0 =~ m![\\/]v(\d+)[\\/]! ) {
        $proto_version = $1;
    }
    else {
        die "Failed to detect version\n";
    }

    my $suffix = "_v$proto_version";

    # older v2.x and v1.0 should ingore this.
    $opts->{protocol_version} = $proto_version;
    if ( $proto_version == 1 ) {
        $opts->{use_protocol_v1} = 1;
    }
    setup_tests($proto_version);
    run_roundtrip_tests_internal( $name . $suffix, $opts );
}

sub roundtrip_corpus_name {
    my ( $index, $name ) = @_;
    $name =~ s/TODO //g;
    return sprintf( 'roundtrip/%05u/%s', $index + 1, $name );
}

sub _protocol_version_from_test_path {
    return $1 if $0 =~ m![\\/]v(\d+)[\\/]!;
    die "Failed to detect protocol version from test path\n";
}

sub _installed_encoder_protocol_version {
    return unless eval {
        require Sereal::Encoder;
        require Sereal::Encoder::Constants;
        1;
    };
    return eval { Sereal::Encoder::Constants::SRL_PROTOCOL_VERSION() };
}

sub _should_run_live_encoder_roundtrips {
    my ($proto_version) = @_;
    return 1 if $Class eq 'Sereal::Encoder';
    my $encoder_protocol = _installed_encoder_protocol_version();
    return defined($encoder_protocol) && $encoder_protocol == $proto_version;
}

sub _run_live_roundtrip_layer {
    my ( $description, $proto_version, $callback ) = @_;
    subtest $description => sub {
        unless ( _should_run_live_encoder_roundtrips($proto_version) ) {
            plan skip_all => "live Encoder round trips are covered by the matching protocol suite";
        }
        require Sereal::Encoder;
        require Sereal::Decoder;
        $callback->();
        done_testing();
    };
}

sub run_roundtrip_version_tests {
    my ($name) = @_;
    my $proto_version = _protocol_version_from_test_path();
    my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
    require Sereal::TestSet::Corpus;
    my $source = "roundtrip-v$proto_version-plain";
    die "Missing required packet corpus source '$source'"
        unless Sereal::TestSet::Corpus::has_source( $corpus, $source );

    run_roundtrip_corpus_tests($corpus);
    _run_live_roundtrip_layer(
        "live Encoder round trips (protocol v$proto_version)",
        $proto_version,
        sub { run_roundtrip_tests($name) },
    );
}

sub run_roundtrip_corpus_tests {
    my ( $root, $profile, $decoder_opts ) = @_;
    $profile ||= 'plain';
    my $proto_version = _protocol_version_from_test_path();

    setup_tests($proto_version);
    require Sereal::Decoder;
    require Sereal::TestSet::Corpus;
    my $source = "roundtrip-v$proto_version-$profile";
    my %available = map { $_ => 1 } Sereal::TestSet::Corpus::packet_names( $root, $source );
    my $decoder = Sereal::Decoder->new( {
        %{ $decoder_opts || {} },
    } );
    my @tests   = _get_roundtrip_tests();
    for my $index ( 0 .. $#tests ) {
        my ( $name, $expected, $required_uv_bits ) = @{ $tests[$index] };
        my $logical_name = roundtrip_corpus_name( $index, $name );
        if ( $required_uv_bits && 8 * $Config{uvsize} < $required_uv_bits ) {
            SKIP: {
                skip( "$name (stored integer requires $required_uv_bits-bit UVs)", 2 );
            }
            next;
        }
        unless ( $available{$logical_name} ) {
            SKIP: {
                skip( "$name (not included in the compact corpus)", 2 );
            }
            next;
        }
        my $packet = Sereal::TestSet::Corpus::read_packet( $root, $source, $logical_name );
        my ( $ok, $decoded, $error );
        {
            local $@;
            $ok = eval { $decoded = $decoder->decode($packet); 1 };
            $error = $@ unless $ok;
        }
        ok( $ok, "$name (stored encoder corpus, decoding)" ) or do { diag $error; next };
        deep_cmp( $decoded, $expected, "$name (stored encoder corpus, decoded vs data)" );
    }
}

sub run_roundtrip_profile_tests {
    my ( $profile, $opts, $decoder_opts ) = @_;
    my $corpus = File::Spec->catdir( 't', 'data', 'packet_corpus' );
    require Sereal::TestSet::Corpus;
    my $has_corpus = Sereal::TestSet::Corpus::has_source(
        $corpus, "roundtrip-v6-$profile"
    );
    plan skip_all => "Missing required stored v6 $profile corpus" unless $has_corpus;
    run_roundtrip_corpus_tests( $corpus, $profile, $decoder_opts );
    _run_live_roundtrip_layer( "live Encoder round trips (v6 $profile)", 6, sub {
        $opts = $opts->() if ref($opts) eq 'CODE';
        run_roundtrip_tests( $profile, $opts );
    } );
}

sub _test {
    my ( $msg, $v1, $v2 ) = @_;

    # require Data::Dumper not needed, called in parent frame
    if ( $v1 ne $v2 ) {
        my $q1 = Data::Dumper::qquote($v1);
        my $q2 = Data::Dumper::qquote($v2);
        return "$msg: $q1 ne $q2";
    }
    return;
}

sub _cmp_str {
    my ( $v1, $v2 ) = @_;
    my $v1_is_utf8 = is_utf8($v1);
    my $v2_is_utf8 = is_utf8($v2);

    Encode::_utf8_off($v1);    # turn off utf8, in case it is corrupt
    Encode::_utf8_off($v2);    # turn off utf8, in case it is corrupt
    if ( $v1 eq $v2 ) {
        return;
    }
    my $diff_start = 0;
    $diff_start++
        while $diff_start < length($v1)
        and $diff_start < length($v2)
        and substr( $v1, $diff_start, 1 ) eq substr( $v2, $diff_start, 1 );
    my $diff_end = length($v1) < length($v2) ? length($v1) : length($v2);

    $diff_end--
        while $diff_end > $diff_start
        and $diff_end > $diff_start
        and substr( $v1, $diff_end - 1, 1 ) eq substr( $v2, $diff_end - 1, 1 );
    my $length_to_show = $diff_end - $diff_start;

    my $max_context_len = 10;
    my $max_diff_len    = 30;

    $length_to_show = $max_diff_len if $length_to_show > $max_diff_len;

    # require Data::Dumper not needed, called in parent frame
    my $q1            = Data::Dumper::qquote( substr( $v1, $diff_start, $length_to_show ) );
    my $q2            = Data::Dumper::qquote( substr( $v2, $diff_start, $length_to_show ) );
    my $context_start = $diff_start > $max_context_len ? $diff_start - $max_context_len : 0;

    if ( $context_start < $diff_start ) {
        $q1 =
              Data::Dumper::qquote( substr( $v1, $context_start, $diff_start - $context_start ) )
            . " . "
            . $q1;
        $q2 =
              Data::Dumper::qquote( substr( $v2, $context_start, $diff_start - $context_start ) )
            . " . "
            . $q2;
    }

    if ( $context_start > 0 ) {
        $q1 = "...$q1";
        $q2 = "...$q2";
    }
    if ( $length_to_show < $max_diff_len ) {
        $q1 .= " . "
            . Data::Dumper::qquote(
            substr( $v1, $diff_start + $length_to_show, $max_diff_len - $length_to_show ) )
            if $diff_start + $length_to_show < length($v1);
        $q2 .= " . "
            . Data::Dumper::qquote(
            substr( $v2, $diff_start + $length_to_show, $max_diff_len - $length_to_show ) )
            if $diff_start + $length_to_show < length($v2);
    }
    if ( $diff_start + $max_diff_len <= length($v1) ) {
        $q1 .= "...";
    }
    if ( $diff_start + $max_diff_len <= length($v2) ) {
        $q2 .= "...";
    }
    my $pad = length($q1) > length($q2) ? length($q1) : length($q2);
    my $lpad =
          length( length($v1) ) > length( length($v2) )
        ? length( length($v1) )
        : length( length($v2) );

    my $issues = "";
    $issues .= "; utf8 mismatch"   if $v1_is_utf8 != $v2_is_utf8;
    $issues .= "; length mismatch" if length($v1) != length($v2);

    my $ret = sprintf(
              "strings different\n"
            . "first string difference at octet offset %d%s\n"
            . " got-octets = %*s (octets: %*d, utf8-flag: %d)\n"
            . "want-octets = %*s (octets: %*d, utf8-flag: %d)\n", $diff_start, $issues,
        -$pad, $q1, $lpad, length($v1), $v1_is_utf8,
        -$pad, $q2, $lpad, length($v2), $v2_is_utf8,
    );
    return $ret;
}

sub _vstring_spelling {
    require B;
    my $sv = B::svref_2object( $_[0] );
    for my $magic ( $sv->MAGIC ) {
        return $magic->PTR if $magic->TYPE eq 'V';
    }
    return;
}

our $deep_cmp_name;    # set by deep_cmp()

sub _test_deep_cmp {
    local $deep_cmp_name = "_test_deep_cmp";
    return _deep_cmp(@_);
}

sub _deep_cmp {
    my ( $x, $y, $seenx, $seeny ) = @_;
    $seenx ||= {};
    $seeny ||= {};
    my $cmp;

    $cmp = _test( "defined mismatch", defined($x), defined($y) )
        and return $cmp;
    defined($x)
        or return "";
    $cmp =
           _test( "seen scalar ", ++$seenx->{ refaddr \$_[0] }, ++$seeny->{ refaddr \$_[1] } )
        || _test( "boolean mismatch", !!$x,      !!$y )
        || _test( "isref mismatch",   !!ref($x), !!ref($y) )
        and return $cmp;

    if ( ref $x ) {
        $cmp =
               _test( "seen ref", ++$seenx->{ refaddr $x}, ++$seeny->{ refaddr $y} )
            || _test( "reftype mismatch", reftype($x),       reftype($y) )
            || _test( "class mismatch",   !blessed($x),      !blessed($y) )
            || _test( "class different",  blessed($x) || "", blessed($y) || "" )
            and return $cmp;
        return ""
            if $x == $y
            or $seenx->{ refaddr $x} > 1;

        if ( reftype($x) eq "HASH" ) {
            $cmp = _test( "keycount mismatch", 0 + keys(%$x), 0 + keys(%$y) )
                and return $cmp;
            foreach my $key ( keys %$x ) {
                return "key missing '$key'" unless exists $y->{$key};
                $cmp = _deep_cmp( $x->{$key}, $y->{$key}, $seenx, $seeny )
                    and return $cmp;
            }
        }
        elsif ( reftype($x) eq "ARRAY" ) {
            $cmp = _test( "arraysize mismatch", 0 + @$x, 0 + @$y )
                and return $cmp;
            foreach my $idx ( 0 .. $#$x ) {
                $cmp = _deep_cmp( $x->[$idx], $y->[$idx], $seenx, $seeny )
                    and return $cmp;
            }
        }
        elsif (reftype($x) eq "SCALAR"
            or reftype($x) eq "REF"
            or reftype($x) eq "VSTRING" )
        {
            return _deep_cmp( $$x, $$y, $seenx, $seeny );
        }
        elsif ( reftype($x) eq "REGEXP" ) {
            $cmp = _test( "regexp different", "$x", "$y" )
                and return $cmp;
        }
        else {
            die "Unknown reftype '", reftype($x) . "'";
        }
    }
    else {
        my $x_is_vstring = ( reftype( \$x ) || "" ) eq "VSTRING";
        my $y_is_vstring = ( reftype( \$y ) || "" ) eq "VSTRING";
        $cmp = _test( "vstring mismatch", $x_is_vstring, $y_is_vstring )
            and return $cmp;
        if (    $deep_cmp_name !~ /v5/
            and $deep_cmp_name =~ /float/
            and $x             =~ /^-?\d+\.\d+\z/
            and $y             =~ /^-?\d+\.\d+\z/ )
        {
            if ( $deep_cmp_name =~ /(-?\d+\.(\d+))/ ) {
                my $maxf = length $1;
                my $minf = length $2;
                $_ = sprintf "%*.*f", $maxf, $minf, $_ for $x, $y;
            }
            else {
                $_ = sprintf "%f", $_ for $x, $y;
            }
        }
        $cmp = _cmp_str( $x, $y )
            and return $cmp;
        if ($x_is_vstring) {
            $cmp = _test( "vstring spelling", _vstring_spelling( \$x ), _vstring_spelling( \$y ) )
                and return $cmp;
        }
    }
    return "";
}

sub deep_cmp {
    my ( $v1, $v2, $name ) = @_;
    local $deep_cmp_name = $name;
    my $diff = _deep_cmp( $v1, $v2 );
    if ($diff) {
        my ( $reason, $diag ) = split /\n/, $diff, 2;
        fail("$name - $reason");
        diag("$name - $diag") if $diag;
        return;
    }
    return 1;
}

sub _roundtrip_decoder_options {
    my ($encoder_options) = @_;
    my %decoder_keys = map { $_ => 1 } qw(
        alias_smallint alias_varint_under incremental max_num_hash_entries
        max_recursion_depth no_bless_objects refuse_objects refuse_snappy
        refuse_zlib set_readonly set_readonly_scalars use_undef validate_utf8
        refuse_zstd max_num_array_entries max_string_length max_uncompressed_size
        no_thaw_objects thaw_allow_classes thaw_deny_action checksum_key_v1
        checksum_key_v2 checksum_policy checksum_key_v3 undef_deadly_policy
    );
    my %decoder_options = map { $_ => $encoder_options->{$_} }
        grep { exists $decoder_keys{$_} } keys %{ $encoder_options || {} };
    return \%decoder_options;
}

sub _roundtrip_encoder_options {
    my ($options) = @_;
    my %encoder_keys = map { $_ => 1 } qw(
        aliased_dedupe_strings canonical canonical_refs compress compress_level
        compress_threshold croak_on_bless dedupe_strings freeze_callbacks
        max_recursion_depth no_bless_objects no_shared_hashkeys protocol_version
        snappy snappy_incr snappy_threshold sort_keys stringify_unknown undef_unknown
        use_protocol_v1 warn_unknown use_standard_double freeze_allow_classes
        freeze_deny_action checksum_key_v1 checksum_key_v2 checksum_policy
        checksum_key_v3
    );
    my %encoder_options = map { $_ => $options->{$_} }
        grep { exists $encoder_keys{$_} } keys %{ $options || {} };
    return \%encoder_options;
}

sub run_roundtrip_tests_internal {
    my ( $ename, $opt, $encode_decode_callbacks ) = @_;
    require Data::Dumper;

    my $skip_long_string_tests = 0;
    if (
        4.015 <= $Sereal::Decoder::VERSION && $Sereal::Decoder::VERSION <= 4.019
        )
    {
        my $perl_is_debugging = $Config{"ccflags"} =~ /-DDEBUGGING/;
        my $perl_is_threaded  = $Config{"useithreads"} || $Config{"usethreads"};
        if ( $perl_is_debugging and $perl_is_threaded ) {
            diag "Skipping round trip tests on this perl!\n",
                "Sereal::Decoder v4.015 - v4.019 has a known fault when used with threaded perls.\n",
                "This fault will trigger assertion failures on DEBUGGING builds like this perl.\n",
                "You are *STRONGLY* advised to upgrade to Sereal::Decoder 4.023 *IMMEDIATELY!\n";
            $skip_long_string_tests = 1;
            return;
        }
    }

    my $failed = 0;

    my $decoder_opt = _roundtrip_decoder_options($opt);
    my $encoder_opt = _roundtrip_encoder_options($opt);
    my $decoder = Sereal::Decoder->new($decoder_opt);
    my $encoder = Sereal::Encoder->new($encoder_opt);
    my %seen_name;
    my @RoundtripTests = _get_roundtrip_tests();
    foreach my $rt (@RoundtripTests) {
        my ( $name, $data ) = @$rt;

        if ( $name =~ /regexp/i && defined($Sereal::Encoder::VERSION)
            && $Sereal::Encoder::VERSION < 4.025 )
        {
            SKIP: {
                skip( "live Encoder $Sereal::Encoder::VERSION does not preserve regexp references", 5 );
            }
            next;
        }

        if ( $failed > 20 ) {
            fail("too many test failures to continue");
            last;
        }

        TODO:
        foreach my $meth ( [
                'object-oriented',
                sub { $encoder->encode( $_[0] ) },
                sub { $decoder->decode( $_[0] ) }
            ],
            [
                'functional simple',
                sub { Sereal::Encoder::encode_sereal( $_[0], $encoder_opt ) },
                sub { Sereal::Decoder::decode_sereal( $_[0], $decoder_opt ) }
            ],
            [
                'functional with object',
                sub { Sereal::Encoder::sereal_encode_with_object( $encoder, $_[0] ) },
                sub { Sereal::Decoder::sereal_decode_with_object( $decoder, $_[0] ) }
            ],
            [
                'header-body',
                sub { $encoder->encode( $_[0], 123456789 ) },    # header data is abitrary to stand out for debugging
                sub { $decoder->decode( $_[0] ) }
            ],
            [
                'header-only',
                sub { $encoder->encode( 987654321, $_[0] ) },    # body data is abitrary to stand out for debugging
                sub { $decoder->decode_only_header( $_[0] ) }
            ],
            )
        {
            my ( $mname, $enc, $dec ) = @$meth;

            local $TODO = $name =~ /TODO/ ? $name : undef;

            next if $mname =~ /header/ and $opt->{use_protocol_v1};

            my $encoded;
            eval { $encoded = $enc->($data); 1 } or do {
                my $err = $@ || 'Zombie error';
                fail("$name ($ename, $mname, encoding failed)");
                $failed++;
            };

            defined($encoded)
                or do {
                fail("$name ($ename, $mname, encoded defined)");
                debug_checks( \$data, \$encoded, undef );
                $failed++;
                next;    #test
                };

            my $decoded;
            eval { $decoded = $dec->($encoded); 1 } or do {
                my $err = $@ || 'Zombie error';
                fail("$name ($ename, $mname, decoding failed)");
                $failed++;
                next;
            };

            defined($decoded) == defined($data)
                or do {
                fail("$name ($ename, $mname, decoded definedness)");
                debug_checks( \$data, \$encoded, undef );
                $failed++;
                next;    #test
                };

            # Second roundtrip
            my $encoded2;
            eval { $encoded2 = $enc->($decoded); 1 } or do {
                my $err = $@ || 'Zombie error';
                fail("$name ($ename, $mname, second encoding failed)");
                $failed++;
                next;    #test
            };

            defined $encoded2
                or do {
                fail("$name ($ename, $mname, encoded2 defined)");
                $failed++;
                next;    #test
                };

            my $decoded2;
            eval { $decoded2 = $dec->($encoded2); 1 } or do {
                my $err = $@ || 'Zombie error';
                fail("$name ($ename, $mname, second decoding failed)");
                $failed++;
                next;    #test
            };

            defined($decoded2) == defined($data)
                or do {
                fail("$name ($ename, $mname, decoded2 defined)");
                $failed++;
                next;    #test
                };

            # Third roundtrip
            my $encoded3;
            eval { $encoded3 = $enc->($decoded2); 1 } or do {
                my $err = $@ || 'Zombie error';
                fail("$name ($ename, $mname, third encoding failed)");
                $failed++;
                next;    #test
            };

            defined $encoded3
                or do {
                fail("$name ($ename, $mname, encoded3 defined)");
                $failed++;
                next;    #test
                };

            my $decoded3;
            eval { $decoded3 = $dec->($encoded3); 1 } or do {
                my $err = $@ || 'Zombie error';
                fail("$name ($ename, $mname, third decoding failed)");
                $failed++;
                next;    #test
            };

            defined($decoded3) == defined($data)
                or do {
                fail("$name ($ename, $mname, decoded3 defined)");
                $failed++;
                next;    #test
                };

            deep_cmp( $decoded, $data, "$name ($ename, $mname, decoded vs data)" )
                or do { $failed++; next };    #test
            deep_cmp( $decoded2, $data, "$name ($ename, $mname, decoded2 vs data)" )
                or do { $failed++; next };    #test
            deep_cmp( $decoded2, $decoded, "$name ($ename, $mname, decoded2 vs decoded)" )
                or do { $failed++; next };    #test
            deep_cmp( $decoded3, $data, "$name ($ename, $mname, decoded3 vs data)" )
                or do { $failed++; next };    #test
            deep_cmp( $decoded3, $decoded, "$name ($ename, $mname, decoded3 vs decoded)" )
                or do { $failed++; next };    #test
            deep_cmp( $decoded3, $decoded2, "$name ($ename, $mname, decoded3 vs decoded2)" )
                or do { $failed++; next };    #test

            if ( $ename =~ /canon/ ) {
                deep_cmp( $encoded2, $encoded, "$name ($ename, $mname, encoded2 vs encoded)" )
                    or do { $failed++; next };    #test
                deep_cmp( $encoded3, $encoded2, "$name ($ename, $mname, encoded3 vs encoded2)" )
                    or do { $failed++; next };    #test
                deep_cmp( $encoded3, $encoded, "$name ($ename, $mname, encoded3 vs encoded)" )
                    or do { $failed++; next };    #test
            }
            pass("$name ($ename, $mname)");
        }    # end method type
    }    # end test type
}

# dumb data-to-file dumper
sub _write_file {
    my ( $file, $data ) = @_;
    open my $fh, ">", $file
        or die "Failed to open file '$file' for writing: $!";
    binmode($fh);
    print $fh $data;
    close $fh;
}

# For bootstrapping other language implementations' tests
our $COMPRESS;

sub write_test_files {
    my ( $dir, $version ) = @_;
    require File::Path;
    File::Path::mkpath($dir);
    my $make_data_file_name =
        sub { File::Spec->catfile( $dir, sprintf( "test_data_%05u", shift ) ) };
    my $make_name_file_name =
        sub { File::Spec->catfile( $dir, sprintf( "test_name_%05u", shift ) ) };

    setup_tests($version);
    foreach my $testno ( 1 .. @BasicTests ) {
        my $t    = $BasicTests[ $testno - 1 ];
        my $data = ref( $t->[1] ) eq 'CODE' ? $t->[1]->() : $t->[1];

        _write_file( $make_data_file_name->($testno), Header($PROTO_VERSION) . $data );
        _write_file( $make_name_file_name->($testno), $t->[2] . "\n" );
    }

    my $encoder = Sereal::Encoder->new( {
        protocol_version => $PROTO_VERSION,
        compress         => $COMPRESS || Sereal::Encoder::SRL_UNCOMPRESSED(),
    } );
    my @RoundtripTests = _get_roundtrip_tests();
    foreach my $i ( 0 .. $#RoundtripTests ) {
        my $testno = @BasicTests + $i + 1;
        my $t      = $RoundtripTests[$i];

        _write_file( $make_data_file_name->($testno), $encoder->encode( $t->[1] ) );
        _write_file( $make_name_file_name->($testno), $t->[0] . "\n" );
    }
}

1;
