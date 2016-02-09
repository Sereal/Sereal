#!perl
use strict;
use warnings;
use Data::Dumper;

use Getopt::Long qw(GetOptions);
use Encode qw(encode_utf8 decode_utf8);
our @constants;
no warnings 'recursion';
BEGIN {
    my $add_use_blib= "";
    my $use= "";
    my @check;
    for my $type ("Decoder","Encoder") {
        if (-e "blib/lib/Sereal/$type/Constants.pm") {
            $add_use_blib="use blib;";
            @check= ($type);
            last;
        }
        push @check, $type;
    }

    my @err;
    foreach my $check (@check) {
        if (eval(my $code= sprintf '
                %s
                use Sereal::%s::Constants qw(:all);
                @constants= @Sereal::%s::Constants::EXPORT_OK;
                print "Loaded constants from $INC{q(Sereal/%s/Constants.pm)}\n";
                1;
            ', $add_use_blib, ($check) x 3))
        {
            @err= ();
            last;
        } else {
            push @err, "Error:",$@ || "Zombie Error","\nCode:\n$code";
        }
    }
    die @err if @err;
}

my $done;
my $data;
my $hlen = -1;
my $indent = "";

sub _chop_data_prefix {
    my ($len)= @_;
    die "Unexpected end of packet" unless length($data) >= $len;
    return substr($data,0,$len,'');
}

sub parse_header {
  $data =~ s/^(=[s\xF3]rl)(.)// or die "invalid header: $data";
  $done .= $1 . $2;
  my $flags = $2;
  my $len = varint();
  my $hdr = _chop_data_prefix( $len );

  my $proto_version = ord($flags) & SRL_PROTOCOL_VERSION_MASK;
  print "Sereal protocol version: $proto_version\n";
  if (length($hdr)) {
    print "Header($len): " . join(" ", map ord, split //, $hdr) . "\n";
    if ($proto_version >= 2 && (ord(substr($hdr, 0, 1)) & 1) ) { # if first bit set => user header data
      print "Found user data in header:\n";
      my $tmp_data = $data; # dance necessary because $data is treated as a global :( hobo, hobo, hobo!
      $data = substr($hdr, 1);
      parse_sv("  ");
      $data = $tmp_data;
      print "End of user data in header. Body:\n";
    }
  }
  else {
    print "Empty Header.\n";
  }

  my $encoding= ord($flags) & SRL_PROTOCOL_ENCODING_MASK;

  printf "%i %i %i\n", $encoding, ord(SRL_PROTOCOL_ENCODING_MASK), ord($flags);
  if ($encoding == SRL_PROTOCOL_ENCODING_RAW) {
    print "Header says: Document body is uncompressed.\n";
  } elsif ($encoding == SRL_PROTOCOL_ENCODING_SNAPPY) {
    print "Header says: Document body is Snappy-compressed.\n";
    require Compress::Snappy;
    my $out = Compress::Snappy::decompress($data);
    $data = $out;
  } elsif ($encoding == SRL_PROTOCOL_ENCODING_SNAPPY_INCREMENTAL) {
    print "Header says: Document body is Snappy-compressed (incremental).\n";
    my $compressed_len = varint();
    require Compress::Snappy;
    my $out = Compress::Snappy::decompress($data);
    $data = $out;
  } elsif ($encoding == SRL_PROTOCOL_ENCODING_ZLIB) {
    print "Header says: Document body is ZLIB-compressed.\n";
    my $uncompressed_len = varint();
    my $compressed_len = varint();
    require Compress::Zlib;
    my $out = Compress::Zlib::uncompress($data);
    $data = $out;
  } else {
    die "Invalid encoding '" . ($encoding >> SRL_PROTOCOL_VERSION_BITS) . "'";
  }
  $hlen= length($done);
}

my ($len_f, $len_d, $len_D);
sub parse_float {
    $len_f||= length(pack("f",0));
    my $v= _chop_data_prefix( $len_f );
    $done .= $v;
    return unpack("f",$v);
}
sub parse_double {
    $len_d||= length(pack("d",0));
    my $v= _chop_data_prefix( $len_d );
    $done .= $v;
    return unpack("d",$v);
}
sub parse_long_double {
    $len_D ||= eval { length(pack("D",0.0)) };
    die "Long double not supported" unless $len_D;
    my $v= _chop_data_prefix( $len_D );
    $done .= $v;
    return unpack("D",$v);
}

my $fmt1= "%06u/%06u: %02x%1s %03s %s";
my $fmt2= "%-6s %-6s  %-2s%1s %-3s %s";
my $lead_items= 5; # 1 less than the fmt2
sub parse_sv {
  my ($ind) = @_;

  my $p= length($done);
  my $t = _chop_data_prefix( 1 );
  $done .= $t;
  my $o = ord($t);
  my $bv= $o;
  my $high = $o >= 128;
  $o -= 128 if $high;
  printf $fmt1, $p, $p-$hlen+1, $o, $high ? '*' : ' ', $bv, $ind;

  if ($o == SRL_HDR_VARINT) {
    printf "VARINT: %u\n", varint();
  }
  elsif ($o == SRL_HDR_ZIGZAG) {
    printf "ZIGZAG: %d\n", zigzag();
  }
  elsif (SRL_HDR_POS_LOW <= $o && $o <= SRL_HDR_POS_HIGH) {
    printf "POS: %u\n", $o;
  }
  elsif (SRL_HDR_NEG_LOW <= $o && $o <= SRL_HDR_NEG_HIGH) {
    $o = $o - 32;
    printf "NEG: %i\n", $o;
  }
  elsif ($o >= SRL_HDR_SHORT_BINARY_LOW) {
    $o -= SRL_HDR_SHORT_BINARY_LOW;
    my $len = $o;
    my $str = _chop_data_prefix( $len );
    $done .= $str;
    printf "SHORT_BINARY(%u): '%s' (%s)\n", $len, encode_utf8($str), unpack("H*", $str);
  }
  elsif ($o == SRL_HDR_BINARY || $o == SRL_HDR_STR_UTF8) {
    my $l = varint();
    my $str = _chop_data_prefix( $l ); # fixme UTF8
    $done .= $str;
    $str= decode_utf8($str) if $o == SRL_HDR_STR_UTF8;
    printf( ($o == SRL_HDR_STR_UTF8 ? "STR_UTF8" : "BINARY")."(%u): '%s' (%s)\n", $l, encode_utf8($str), unpack("H*", encode_utf8($str)));
  }
  elsif ($o == SRL_HDR_FLOAT) {
    printf "FLOAT(%f)\n", parse_float();
  }
  elsif ($o == SRL_HDR_DOUBLE) {
    printf "DOUBLE(%f)\n", parse_double();
  }
  elsif ($o == SRL_HDR_LONG_DOUBLE) {
    printf "LONG_DOUBLE(%f)\n", parse_long_double();
  }
  elsif ($o == SRL_HDR_REFN) {
    printf "REFN\n";
    parse_sv($ind . "  ");
  }
  elsif ($o == SRL_HDR_REFP) {
    my $len = varint();
    printf "REFP(%u)\n", $len;
  }
  elsif ($o == SRL_HDR_COPY) {
    my $len = varint();
    printf "COPY(%u)\n", $len;
  }
  elsif (SRL_HDR_ARRAYREF_LOW <= $o && $o <= SRL_HDR_ARRAYREF_HIGH) {
    printf "ARRAYREF";
    parse_av($ind,$o);
  }
  elsif ($o == SRL_HDR_ARRAY) {
    printf "ARRAY";
    parse_av($ind);
  }
  elsif (SRL_HDR_HASHREF_LOW <= $o && $o <= SRL_HDR_HASHREF_HIGH) {
    printf "HASHREF";
    parse_hv($ind,$o);
  }
  elsif ($o == SRL_HDR_HASH) {
    printf "HASH";
    parse_hv($ind);
  }
  elsif ($o == SRL_HDR_CANONICAL_UNDEF) {
    printf "CANONICAL_UNDEF\n";
  }
  elsif ($o == SRL_HDR_UNDEF) {
    printf "UNDEF\n";
  }
  elsif ($o == SRL_HDR_WEAKEN) {
    printf "WEAKEN\n";
    parse_sv($ind);
  }
  elsif ($o == SRL_HDR_PAD) {
    printf "PAD\n";
  }
  elsif ($o == SRL_HDR_ALIAS) {
    my $ofs= varint();
    printf "ALIAS(%u)\n", $ofs;
  }
  elsif ($o == SRL_HDR_OBJECTV) {
    my $ofs= varint();
    printf "OBJECTV(%d)\n", $ofs;
    printf  "$fmt2  Value:\n",("") x $lead_items, $ind;
    parse_sv($ind."    ");
  }
  elsif ($o == SRL_HDR_OBJECTV_FREEZE) {
    my $ofs= varint();
    printf "OBJECTV_FREEZE(%d)\n", $ofs;
    printf  "$fmt2  Value:\n",("") x $lead_items, $ind;
    parse_sv($ind."    ");
  }
  elsif ($o == SRL_HDR_OBJECT) {
    printf "OBJECT\n";
    printf  "$fmt2  Class:\n",("") x $lead_items, $ind;
    parse_sv($ind."    ");
    printf  "$fmt2  Value:\n",("") x $lead_items, $ind;
    parse_sv($ind."    ");
  }
  elsif ($o == SRL_HDR_OBJECT_FREEZE) {
    printf "OBJECT_FREEZE\n";
    printf  "$fmt2  Class:\n",("") x $lead_items, $ind;
    parse_sv($ind."    ");
    printf  "$fmt2  Value:\n",("") x $lead_items, $ind;
    parse_sv($ind."    ");
  }
  elsif ($o == SRL_HDR_REGEXP) {
    printf "REGEXP\n";
    parse_sv($ind."  ");
    parse_sv($ind."  ");
  }
  elsif ($o == SRL_HDR_FALSE) {
    printf "FALSE\n";
  }
  elsif ($o == SRL_HDR_TRUE) {
    printf "TRUE\n";

  }
  else {
    printf "<UNKNOWN>\n";
    die sprintf "unsupported type: 0x%02x (%d) %s: %s", $o, $o,
        Data::Dumper::qquote($t), Data::Dumper->new([$TAG_INFO_ARRAY[$o]])->Terse(1)->Dump();
  }
  return 0;
}

sub parse_av {
  my ($ind,$o) = @_;
  my $len = defined $o ? $o & 15 : varint();
  printf "(%u)\n", $len;
  $ind .= "  ";
  while ($len--) {
    parse_sv($ind,\$len);
  }
}

sub parse_hv {
  my ($ind, $o) = @_;
  my $len = (defined $o ? $o & 15 : varint());
  printf "(%u)\n", $len;
  $ind .= "  ";
  while ($len--) {
    printf  "$fmt2%s:\n",("") x $lead_items, $ind, "KEY";
    parse_sv($ind."  ");
    printf  "$fmt2%s:\n",("") x $lead_items, $ind,  "VALUE";
    parse_sv($ind."  ");
  }
}


# super inefficient
sub varint {
  my $x = 0;
  my $lshift = 0;
  while (length($data) && ord(substr($data, 0, 1)) & 0x80) {
    my $c = ord(_chop_data_prefix( 1 ));
    $done .= chr($c);
    $x += ($c & 0x7F) << $lshift;
    $lshift += 7;
  }
  if (length($data)) {
    my $c = ord(_chop_data_prefix( 1 ));
    $done .= chr($c);
    $x += $c << $lshift;
  }
  else {
    die "premature end of varint";
  }
  return $x;
}

sub _zigzag {
    my $n= $_[0];
    return $n & 1 ? -(($n >> 1)+1) : ($n >> 1);
}
sub zigzag {
    return _zigzag(varint());
}

GetOptions(
  my $opt = {},
  'e|stderr',
);

$| = 1;
if ($opt->{e}) {
  select(STDERR);
}

local $/ = undef;
$data = <STDIN>;

open my $fh, "| od -tu1c" or die $!;
print $fh $data;
close $fh;

print "\n\nTotal length: " . length($data) . "\n\n";

parse_header();
while (length $data) {
  $done = parse_sv("");
}
