#!perl
use strict;
use warnings;
use Data::Dumper;

use Getopt::Long qw(GetOptions);
BEGIN {
    my $err;
    eval '
        use Sereal::Encoder::Constants qw(:all);
        1;
    ' or do { $err= $@; eval '
        use Sereal::Decoder::Constants qw(:all);
        1;
    ' } or die "No encoder/decoder constants: $err\n$@";
}

my $done;
my $data;
my $hlen;
my $indent = "";
my %const_names = map {$_ => eval "$_"} @Sereal::Constants::EXPORT_OK;

sub parse_header {
  $data =~ s/^(=srl)(.)// or die "invalid header: $data";
  $done .= $1 . $2;
  my $flags = $2;
  my $len = varint();
  my $hdr = substr($data, 0, $len, '');

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

  if ($encoding == SRL_PROTOCOL_ENCODING_SNAPPY) {
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
  } elsif ($encoding) {
    die "Invalid encoding '" . ($encoding >> SRL_PROTOCOL_VERSION_BITS) . "'";
  }
  $hlen= length($done);
}

my ($len_f, $len_d, $len_D);
sub parse_float {
    $len_f||= length(pack("f",0));
    my $v= substr($data,0,$len_f,"");
    $done .= $v;
    return unpack("f",$v);
}
sub parse_double {
    $len_d||= length(pack("d",0));
    my $v= substr($data,0,$len_d,"");
    $done .= $v;
    return unpack("d",$v);
}
sub parse_long_double {
    $len_D||= eval { length(pack("D",0)) };
    die "Long double not supported" unless $len_D;
    my $v= substr($data,0,$len_D,"");
    $done .= $v;
    return unpack("D",$v);
}

my $fmt1= "%06u/%06u: %02x%1s %03s %s";
my $fmt2= "%-6s %-6s  %-2s%1s %-3s %s";
my $lead_items= 5; # 1 less than the fmt2
sub parse_sv {
  my ($ind) = @_;

  my $p= length($done);
  my $t = substr($data, 0, 1, '');
  $done .= $t;
  my $o = ord($t);
  my $bv= $o;
  my $high = $o > 128;
  $o -= 128 if $high;
  printf $fmt1, $p, $p-$hlen+1, $o, $high ? '*' : ' ', $bv, $ind;

  if ($o == SRL_HDR_VARINT) {
    printf "VARINT: %u\n", varint();
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
    my $str = substr($data, 0, $len, '');
    $done .= $str;
    printf "SHORT_BINARY(%u): '%s'\n", $len, $str;
  }
  elsif ($o == SRL_HDR_BINARY || $o == SRL_HDR_STR_UTF8) {
    my $l = varint();
    my $str = substr($data, 0, $l, ""); # fixme UTF8
    $done .= $str;
    printf( ($o == SRL_HDR_STR_UTF8 ? "STR_UTF8" : "BINARY")."(%u): '%s'\n", $l, $str);
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
    die "unsupported type: $o ($t): $const_names{$o}";
  }
  return 0;
}

sub parse_av {
  my ($ind,$o) = @_;
  my $len = defined $o ? $o & 15 : varint();
  printf "(%u)\n", $len;
  $ind .= "  ";
  while ($len--) {
    my $t = substr($data, 0, 1);
    my $o = ord($t);
      parse_sv($ind);
  }
}

sub parse_hv {
  my ($ind, $o) = @_;
  my $len = (defined $o ? $o & 15 : varint()) * 2;
  printf "(%u)\n", $len;
  $ind .= "  ";
  my $flipflop = 0;
  while ($len--) {
    my $t = substr($data, 0, 1);
    my $o = ord($t);
    print( "               ", $ind, ($flipflop++ % 2 == 1 ? "VALUE" : "KEY"), ":\n" );
    parse_sv($ind."  ");
  }
}


# super inefficient
sub varint {
  my $x = 0;
  my $lshift = 0;
  while (length($data) && ord(substr($data, 0, 1)) & 0x80) {
    my $c = ord(substr($data, 0, 1, ''));
    $done .= chr($c);
    $x += ($c & 0x7F) << $lshift;
    $lshift += 7;
  }
  if (length($data)) {
    my $c = ord(substr($data, 0, 1, ''));
    $done .= chr($c);
    $x += $c << $lshift;
  }
  else {
    die "premature end of varint";
  }
  return $x;
}

GetOptions(
  my $opt = {},
  'e|stderr',
);

$| = 1;
if ($opt->{e}) {
  select(STDERR);
}

#print Dumper \%const_names; exit;

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
