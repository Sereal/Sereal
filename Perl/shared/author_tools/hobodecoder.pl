#!perl
use strict;
use warnings;
use Data::Dumper;
use Config;

use Getopt::Long qw(GetOptions);
use Encode       qw(encode_utf8 decode_utf8);
our @constants;
no warnings 'recursion';

BEGIN {
    my $add_use_blib = "";
    my $use          = "";
    my @check;
    for my $type ( "Decoder", "Encoder" ) {
        if ( -e "blib/lib/Sereal/$type/Constants.pm" ) {
            $add_use_blib = "use blib;";
            @check        = ($type);
            last;
        }
        push @check, $type;
    }

    my @err;
    foreach my $check (@check) {
        if (
            eval(
                my $code = sprintf '
                %s
                use Sereal::%s::Constants qw(:all);
                @constants= @Sereal::%s::Constants::EXPORT_OK;
                print "Loaded constants from $INC{q(Sereal/%s/Constants.pm)}\n";
                1;
            ', $add_use_blib, ($check) x 3
            ) )
        {
            @err = ();
            last;
        }
        else {
            push @err, "Error:", $@ || "Zombie Error", "\nCode:\n$code";
        }
    }
    die @err if @err;
}

my $done;
my $data;
my $packet_remainder;
my $hlen   = -1;
my $indent = "";
my $opt;
my %siphash_function;

sub _load_siphash_function {
    my ($version) = @_;
    return $siphash_function{$version} if $siphash_function{$version};
    for my $module (qw(Sereal::Decoder Sereal::Encoder)) {
        ( my $file = "$module.pm" ) =~ s{::}{/}g;
        next unless eval { require $file; 1 };
        my $function = $version == 0 ? 'siphash24' : $version == 1 ? 'siphash24_128' : 'siphash36_128';
        my $code     = $module->can($function) or next;
        return $siphash_function{$version} = $code;
    }
    die "Could not load a Sereal module with SipHash support\n";
}

sub _constant_time_equal {
    my ( $left, $right ) = @_;
    return 0 unless length($left) == length($right);
    my $different = 0;
    for my $i ( 0 .. length($left) - 1 ) {
        $different |= ord( substr( $left, $i, 1 ) ) ^ ord( substr( $right, $i, 1 ) );
    }
    return $different == 0;
}

sub _chop_data_prefix {
    my ($len) = @_;
    die "Unexpected end of packet" unless length($data) >= $len;
    return substr( $data, 0, $len, '' );
}

sub parse_header {
    $data =~ s/^(=[s\xF3]rl)(.)// or die "invalid header: $data";
    $done .= $1 . $2;
    my $flags = $2;
    my $len   = varint();
    my $hdr   = _chop_data_prefix($len);

    my $proto_version = ord($flags) & SRL_PROTOCOL_VERSION_MASK;
    print "Sereal protocol version: $proto_version\n";
    if ( length($hdr) ) {
        print "Header($len): " . join( " ", map ord, split //, $hdr ) . "\n";
        if ( $proto_version >= 2 && ( ord( substr( $hdr, 0, 1 ) ) & 1 ) ) {    # if first bit set => user header data
            $done .= substr( $hdr, 0, 1 );
            print "Found user data in header:\n";
            my $tmp_data = $data;                                              # dance necessary because $data is treated as a global :( hobo, hobo, hobo!
            $data = substr( $hdr, 1 );
            parse_sv("  ");
            $data = $tmp_data;
            print "End of user data in header. Body:\n";
        }
        else {
            $done .= $hdr;
        }
    }
    else {
        print "Empty Header.\n";
    }

    my $encoding    = ord($flags) & SRL_PROTOCOL_ENCODING_MASK;
    my $checksummed = ( $encoding & SRL_PROTOCOL_ENCODING_CHECKSUM ) != 0;
    $encoding &= SRL_PROTOCOL_ENCODING_BASE_MASK;

    printf "%i %i %i\n", $encoding, ord(SRL_PROTOCOL_ENCODING_MASK), ord($flags);
    $packet_remainder = undef;
    if ($checksummed) {
        die "Checksummed packets require protocol version 6 or later\n"
            if $proto_version < 6;

        my $checksum_type = ord( _chop_data_prefix(1) );
        $done .= chr($checksum_type);
        my $tag_len         = varint();
        my $checksum_prefix = $done;
        my $tag             = _chop_data_prefix($tag_len);
        $done .= $tag;
        my $payload_len_start = length($done);
        my $payload_len       = varint();
        my $payload_len_bytes = substr( $done, $payload_len_start );
        die "Checksummed payload length exceeds available input\n"
            if $payload_len > length($data);
        $packet_remainder = substr( $data, $payload_len, length($data) - $payload_len, '' );
        my $wire_payload = $data;
        printf "Checksum type %u, tag length %u bytes, encoded payload length %u bytes.\n",
            $checksum_type, $tag_len, $payload_len;
        my $expected_len =
            $checksum_type == 0 ? 8 : ( $checksum_type == 1 || $checksum_type == 2 ) ? 16 : undef;
        die "Invalid SipHash checksum length $tag_len for type $checksum_type\n"
            if defined($expected_len) && $tag_len != $expected_len;
        my $key =
              $checksum_type == 0 ? $opt->{checksum_key_v1}
            : $checksum_type == 1 ? $opt->{checksum_key_v2}
            : $checksum_type == 2 ? $opt->{checksum_key_v3}
            :                       undef;

        if ( defined($expected_len) && defined($key) ) {
            my $siphash = _load_siphash_function($checksum_type);
            my $actual  = $siphash->( $checksum_prefix . $payload_len_bytes . $wire_payload, $key );
            die "SipHash checksum verification failed\n"
                unless _constant_time_equal( $tag, $actual );
            printf "SipHash-%s checksum verified: %s\n",
                $checksum_type == 0 ? '2-4' : $checksum_type == 1 ? '2-4-128' : '3-6-128',
                unpack( 'H*', $tag );
        }
        else {
            printf "SipHash tag (%u bytes, %s): %s\n",
                $tag_len,
                defined($expected_len) ? 'not verified (no matching key)' : 'unknown type, skipped',
                unpack( 'H*', $tag );
        }
    }
    if ( $encoding == SRL_PROTOCOL_ENCODING_RAW ) {
        print "Header says: Document body is uncompressed.\n";
    }
    elsif ( $encoding == SRL_PROTOCOL_ENCODING_SNAPPY ) {
        print "Header says: Document body is Snappy-compressed.\n";
        require Compress::Snappy;
        my $out = Compress::Snappy::decompress($data);
        $data = $out;
    }
    elsif ( $encoding == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL ) {
        print "Header says: Document body is Snappy-compressed (incremental).\n";
        my $compressed_len = varint();
        require Compress::Snappy;
        my $out = Compress::Snappy::decompress($data);
        $data = $out;
    }
    elsif ( $encoding == SRL_PROTOCOL_ENCODING_ZLIB ) {
        print "Header says: Document body is ZLIB-compressed.\n";
        my $uncompressed_len = varint();
        my $compressed_len   = varint();
        require Compress::Zlib;
        my $out = Compress::Zlib::uncompress($data);
        $data = $out;
    }
    elsif ( $encoding == SRL_PROTOCOL_ENCODING_ZSTD ) {
        print "Header says: Document body is ZSTD-compressed.\n";
        my $compressed_len = varint();
        require Compress::Zstd;
        my $out = Compress::Zstd::decompress($data);
        $data = $out;
    }
    else {
        die "Invalid encoding '" . ( $encoding >> SRL_PROTOCOL_VERSION_BITS ) . "'";
    }
    $hlen = length($done);
}

my ( $len_f, $len_d, $len_D, $len_F );

sub parse_float {
    $len_f ||= length( pack( "f", 0 ) );
    my $v = _chop_data_prefix($len_f);
    $done .= $v;
    return unpack( "f", $v );
}

sub parse_float_128 {
    $len_F ||= length( pack( "F", 0 ) );
    my $v = _chop_data_prefix($len_F);
    $done .= $v;
    return unpack( "F", $v );
}

sub parse_double {
    $len_d ||= length( pack( "d", 0 ) );
    my $v = _chop_data_prefix($len_d);
    $done .= $v;
    return unpack( "d", $v );
}

sub parse_long_double {
    $len_D ||= eval { length( pack( "D", 0.0 ) ) };
    die "Long double not supported" unless $len_D;
    my $v = _chop_data_prefix($len_D);
    $done .= $v;
    return unpack( "D", $v );
}

my $fmt1       = "%06d/%06d: %02x%1s %03s %s";
my $fmt2       = "%-6s %-6s  %-2s%1s %-3s %s";
my $lead_items = 5;                              # 1 less than the fmt2

sub parse_sv {
    my ($ind) = @_;

    my $p = length($done);
    my $t = _chop_data_prefix(1);
    $done .= $t;
    my $o    = ord($t);
    my $bv   = $o;
    my $high = $o >= 128;
    $o -= 128 if $high;
    printf $fmt1, $p, $p - $hlen + 1, $o, $high ? '*' : ' ', $bv, $ind;

    if ( $o == SRL_HDR_VARINT ) {
        printf "VARINT: %u\n", varint();
    }
    elsif ( $o == SRL_HDR_ZIGZAG ) {
        printf "ZIGZAG: %d\n", zigzag();
    }
    elsif ( $o == SRL_HDR_NEG_VARINT ) {
        my $magnitude = varint();
        die "NEG_VARINT magnitude must be nonzero" unless $magnitude;
        printf "NEG_VARINT: -%u\n", $magnitude;
    }
    elsif ( SRL_HDR_POS_LOW <= $o && $o <= SRL_HDR_POS_HIGH ) {
        printf "POS: %u\n", $o;
    }
    elsif ( SRL_HDR_NEG_LOW <= $o && $o <= SRL_HDR_NEG_HIGH ) {
        $o = $o - 32;
        printf "NEG: %i\n", $o;
    }
    elsif ( $o >= SRL_HDR_SHORT_BINARY_LOW ) {
        $o -= SRL_HDR_SHORT_BINARY_LOW;
        my $len = $o;
        my $str = _chop_data_prefix($len);
        $done .= $str;
        printf "SHORT_BINARY(%u): '%s' (%s)\n", $len, encode_utf8($str), unpack( "H*", $str );
    }
    elsif ( $o == SRL_HDR_BINARY || $o == SRL_HDR_STR_UTF8 ) {
        my $l   = varint();
        my $str = _chop_data_prefix($l);    # fixme UTF8
        $done .= $str;
        $str = decode_utf8($str) if $o == SRL_HDR_STR_UTF8;
        printf(
            ( $o == SRL_HDR_STR_UTF8 ? "STR_UTF8" : "BINARY" ) . "(%u): '%s' (%s)\n", $l,
            encode_utf8($str), unpack( "H*", encode_utf8($str) ) );
    }
    elsif ( $o == SRL_HDR_EXTEND ) {
        my $ext = ord( _chop_data_prefix(1) );
        $done .= chr($ext);
        if ( $ext == SRL_EXT_UNDEF_DEADLY ) {
            print "UNDEF_DEADLY (undef; must not be emitted by an encoder)\n";
        }
        else {
            die "Unknown extended tag ID $ext\n"
                unless $ext == SRL_EXT_VSTRING_BINARY || $ext == SRL_EXT_VSTRING_UTF8;

            # The VSTRING payload is its original spelling. The extension ID
            # describes the encoding of the PV reconstructed from that spelling.
            my $l   = varint();
            my $str = _chop_data_prefix($l);
            $done .= $str;
            $str = decode_utf8($str) if $ext == SRL_EXT_VSTRING_UTF8;
            printf "VSTRING-%s spelling(%u): '%s' (%s)\n",
                $ext == SRL_EXT_VSTRING_UTF8 ? "UTF8" : "BINARY",
                $l, encode_utf8($str), unpack( "H*", encode_utf8($str) );
        }
    }
    elsif ( $o == SRL_HDR_FLOAT ) {
        printf "FLOAT(%f)\n", parse_float();
    }
    elsif ( $o == SRL_HDR_DOUBLE ) {
        printf "DOUBLE(%f)\n", parse_double();
    }
    elsif ( $o == SRL_HDR_LONG_DOUBLE ) {
        printf "LONG_DOUBLE(%f)\n", parse_long_double();
    }
    elsif ( $o == SRL_HDR_FLOAT_128 ) {
        printf "FLOAT_128(%f)\n", parse_float_128();
    }
    elsif ( $o == SRL_HDR_REFN ) {
        printf "REFN\n";
        parse_sv( $ind . "  " );
    }
    elsif ( $o == SRL_HDR_REFP ) {
        my $len = varint();
        printf "REFP(%u)\n", $len;
    }
    elsif ( $o == SRL_HDR_COPY ) {
        my $len = varint();
        printf "COPY(%u)\n", $len;
    }
    elsif ( SRL_HDR_ARRAYREF_LOW <= $o && $o <= SRL_HDR_ARRAYREF_HIGH ) {
        printf "ARRAYREF";
        parse_av( $ind, $o );
    }
    elsif ( $o == SRL_HDR_ARRAY ) {
        printf "ARRAY";
        parse_av($ind);
    }
    elsif ( SRL_HDR_HASHREF_LOW <= $o && $o <= SRL_HDR_HASHREF_HIGH ) {
        printf "HASHREF";
        parse_hv( $ind, $o );
    }
    elsif ( $o == SRL_HDR_HASH ) {
        printf "HASH";
        parse_hv($ind);
    }
    elsif ( $o == SRL_HDR_CANONICAL_UNDEF ) {
        printf "CANONICAL_UNDEF\n";
    }
    elsif ( $o == SRL_HDR_UNDEF ) {
        printf "UNDEF\n";
    }
    elsif ( $o == SRL_HDR_WEAKEN ) {
        printf "WEAKEN\n";
        parse_sv($ind);
    }
    elsif ( $o == SRL_HDR_PAD ) {
        printf "[PAD]\n";
        parse_sv($ind);
    }
    elsif ( $o == SRL_HDR_ALIAS ) {
        my $ofs = varint();
        printf "ALIAS(%u)\n", $ofs;
    }
    elsif ( $o == SRL_HDR_OBJECTV ) {
        my $ofs = varint();
        printf "OBJECTV(%d)\n",                       $ofs;
        printf "$fmt2  Value:\n", ("") x $lead_items, $ind;
        parse_sv( $ind . "    " );
    }
    elsif ( $o == SRL_HDR_OBJECTV_FREEZE ) {
        my $ofs = varint();
        printf "OBJECTV_FREEZE(%d)\n",                $ofs;
        printf "$fmt2  Value:\n", ("") x $lead_items, $ind;
        parse_sv( $ind . "    " );
    }
    elsif ( $o == SRL_HDR_OBJECT ) {
        printf "OBJECT\n";
        printf "$fmt2  Class:\n", ("") x $lead_items, $ind;
        parse_sv( $ind . "    " );
        printf "$fmt2  Value:\n", ("") x $lead_items, $ind;
        parse_sv( $ind . "    " );
    }
    elsif ( $o == SRL_HDR_OBJECT_FREEZE ) {
        printf "OBJECT_FREEZE\n";
        printf "$fmt2  Class:\n", ("") x $lead_items, $ind;
        parse_sv( $ind . "    " );
        printf "$fmt2  Value:\n", ("") x $lead_items, $ind;
        parse_sv( $ind . "    " );
    }
    elsif ( $o == SRL_HDR_REGEXP ) {
        printf "REGEXP\n";
        parse_sv( $ind . "  " );
        parse_sv( $ind . "  " );
    }
    elsif ( $o == SRL_HDR_FALSE ) {
        printf "FALSE\n";
    }
    elsif ( $o == SRL_HDR_TRUE ) {
        printf "TRUE\n";
    }
    elsif ( $o == SRL_HDR_NO ) {
        printf "NO (Perl boolean false)\n";
    }
    elsif ( $o == SRL_HDR_YES ) {
        printf "YES (Perl boolean true)\n";

    }
    else {
        printf "<UNKNOWN>\n";
        die sprintf "unsupported type: 0x%02x (%d) %s: %s", $o, $o,
            Data::Dumper::qquote($t),
            Data::Dumper->new( [ $TAG_INFO_ARRAY[$o] ] )->Terse(1)->Dump();
    }
    return 0;
}

sub parse_av {
    my ( $ind, $o ) = @_;
    my $len = defined $o ? $o & 15 : varint();
    printf "(%u)\n", $len;
    $ind .= "  ";
    while ( $len-- ) {
        parse_sv( $ind, \$len );
    }
}

sub parse_hv {
    my ( $ind, $o ) = @_;
    my $len = ( defined $o ? $o & 15 : varint() );
    printf "(%u)\n", $len;
    $ind .= "  ";
    while ( $len-- ) {
        printf "$fmt2%s:\n", ("") x $lead_items, $ind, "KEY";
        parse_sv( $ind . "  " );
        printf "$fmt2%s:\n", ("") x $lead_items, $ind, "VALUE";
        parse_sv( $ind . "  " );
    }
}

# super inefficient
sub varint {
    my $uv_bits   = 8 * $Config{uvsize};
    my $max_bytes = int( ( $uv_bits + 6 ) / 7 );
    my ( $x, $lshift ) = ( 0, 0 );
    for my $index ( 0 .. $max_bytes - 1 ) {
        die "premature end of varint" unless length $data;
        my $c = ord( _chop_data_prefix(1) );
        $done .= chr($c);
        my $part = $c & 0x7f;
        if ( $index == $max_bytes - 1 ) {
            my $bits_left = $uv_bits - $lshift;
            my $max_part  = ( 1 << $bits_left ) - 1;
            die "varint too big" if $part > $max_part;
        }
        $x |= $part << $lshift;
        return $x unless $c & 0x80;
        $lshift += 7;
    }
    die "varint too long";
}

sub _zigzag {
    my $n = $_[0];
    return $n & 1 ? -( ( $n >> 1 ) + 1 ) : ( $n >> 1 );
}

sub zigzag {
    return _zigzag( varint() );
}

GetOptions(
    $opt = {},
    'e|stderr',
    'checksum-v0-key=s',
    'checksum-v1-key=s',
    'checksum-v2-key=s',
    'checksum-v0-key-hex=s',
    'checksum-v1-key-hex=s',
    'checksum-v2-key-hex=s',
);

for my $version ( 0, 1, 2 ) {
    my $plain_name   = "checksum_key_v" . ( $version + 1 );
    my $hex_name     = "checksum_key_v" . ( $version + 1 ) . "_hex";
    my $plain_option = "checksum-v${version}-key";
    my $hex_option   = "checksum-v${version}-key-hex";
    die "Specify only one of --$plain_option and --$hex_option\n"
        if defined( $opt->{$plain_option} ) && defined( $opt->{$hex_option} );
    if ( defined $opt->{$hex_option} ) {
        die "--$hex_option expects an even number of hexadecimal digits\n"
            if $opt->{$hex_option} =~ /[^0-9a-fA-F]/ || length( $opt->{$hex_option} ) % 2;
        $opt->{$plain_name} = pack( 'H*', $opt->{$hex_option} );
    }
    elsif ( defined $opt->{$plain_option} ) {
        $opt->{$plain_name} = $opt->{$plain_option};
    }
    die "--checksum-v${version}-key must be exactly 16 bytes\n"
        if defined( $opt->{$plain_name} ) && length( $opt->{$plain_name} ) != 16;
}

$| = 1;
if ( $opt->{e} ) {
    select(STDERR);
}

local $/ = undef;
$data = <STDIN>;

open my $fh, "| od -tu1c" or die $!;
print $fh $data;
close $fh;

print "\n\nTotal length: " . length($data) . "\n\n";

while ( length $data ) {
    parse_header();
    print "--- End header\n";
    $done = parse_sv("");
    print "--- End Document\n";
    $data             = $packet_remainder if defined $packet_remainder;
    $packet_remainder = undef;
}
