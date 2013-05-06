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

GetOptions(
  my $opt = {},
  'e|stderr',
);

$| = 1;
if ($opt->{e}) {
  select(STDERR);
}

my %const_names = map {$_ => eval "$_"} @Sereal::Constants::EXPORT_OK;
#print Dumper \%const_names; exit;

local $/ = undef;
my $data = <STDIN>;

open my $fh, "| od -tu1c" or die $!;
print $fh $data;
close $fh;

print "\n\nTotal length: " . length($data) . "\n\n";

my $indent = "";
my $done;
parse_header();
while (length $data) {
  my $done = parse_sv("");
}

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
    die "Incremental Snappy encoding not implemented in hobodecoder. (yet. It's easy to do.)";
  }
  elsif ($encoding) {
    die "Invalid encoding '" . ($encoding >> SRL_PROTOCOL_VERSION_BITS) . "'";
  }
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

sub parse_sv {
  my ($ind) = @_;

  my $p= length($done);
  my $t = substr($data, 0, 1, '');
  $done .= $t;
  my $o = ord($t);
  my $bv= $o;
  my $high = $o > 128;
  $o -= 128 if $high;
  if ($o == SRL_HDR_VARINT) {
    printf "%06u: %02x %03s %sVARINT: %u\n", $p, $o, $bv, $ind, varint();
  }
  elsif (SRL_HDR_POS_LOW <= $o && $o <= SRL_HDR_POS_HIGH) {
    printf "%06u: %02x %03s %sPOS: %u\n", $p, $o, $bv, $ind, $o;
  }
  elsif (SRL_HDR_NEG_LOW <= $o && $o <= SRL_HDR_NEG_HIGH) {
    $o = $o - 32;
    printf "%06u: %02x %03s %sNEG: %i\n", $p, $o, $bv, $ind, $o;
  }
  elsif ($o >= SRL_HDR_SHORT_BINARY_LOW) {
    $o -= SRL_HDR_SHORT_BINARY_LOW;
    my $len = $o;
    my $str = substr($data, 0, $len, '');
    $done .= $str;
    printf "%06u: %02x %03s %sSHORT_BINARY(%u): '%s'\n", $p, $o, $bv, $ind, $len, $str;
  }
  elsif ($o == SRL_HDR_BINARY || $o == SRL_HDR_STR_UTF8) {
    my $l = varint();
    my $str = substr($data, 0, $l, ""); # fixme UTF8
    $done .= $str;
    printf "%06u: %02x %03s %s".($o == SRL_HDR_STR_UTF8 ? "STR_UTF8" : "BINARY")."(%u): '%s'\n", $p, $o, $bv, $ind, $l, $str;
  }
  elsif ($o == SRL_HDR_FLOAT) {
    printf "%06u: %02x %03s %sFLOAT(%f)\n", $p, $o, $bv, $ind, parse_float();
  }
  elsif ($o == SRL_HDR_DOUBLE) {
    printf "%06u: %02x %03s %sDOUBLE(%f)\n", $p, $o, $bv, $ind, parse_double();
  }
  elsif ($o == SRL_HDR_LONG_DOUBLE) {
    printf "%06u: %02x %03s %sLONG_DOUBLE(%f)\n", $p, $o, $bv, $ind, parse_long_double();
  }
  elsif ($o == SRL_HDR_REFN) {
    printf "%06u: %02x %03s %sREFN\n", $p, $o, $bv, $ind;
    parse_sv($ind . "  ");
  }
  elsif ($o == SRL_HDR_REFP) {
    my $len = varint();
    printf "%06u: %02x %03s %sREFP(%u)\n", $p, $o, $bv, $ind, $len;
  }
  elsif ($o == SRL_HDR_COPY) {
    my $len = varint();
    printf "%06u: %02x %03s %sCOPY(%u)\n", $p, $o, $bv, $ind, $len;
  }
  elsif (SRL_HDR_ARRAYREF_LOW <= $o && $o <= SRL_HDR_ARRAYREF_HIGH) {
    printf "%06u: %02x %03s %sARRAYREF", $p, $o, $bv, $ind;
    parse_av($ind,$o);
  }
  elsif ($o == SRL_HDR_ARRAY) {
    printf "%06u: %02x %03s %sARRAY", $p, $o, $bv, $ind;
    parse_av($ind);
  }
  elsif (SRL_HDR_HASHREF_LOW <= $o && $o <= SRL_HDR_HASHREF_HIGH) {
    printf "%06u: %02x %03s %sHASHREF", $p, $o, $bv, $ind;
    parse_hv($ind,$o);
  }
  elsif ($o == SRL_HDR_HASH) {
    printf "%06u: %02x %03s %sHASH", $p, $o, $bv, $ind;
    parse_hv($ind);
  }
  elsif ($o == SRL_HDR_UNDEF) {
    printf "%06u: %02x %03s %sUNDEF\n", $p, $o, $bv, $ind;
  }
  elsif ($o == SRL_HDR_WEAKEN) {
    printf "%06u: %02x %03s %sWEAKEN\n", $p, $o, $bv, $ind;
    parse_sv($ind);
  }
  elsif ($o == SRL_HDR_PAD) {
    printf "%06u: %02x %03s %sPAD\n", $p, $o, $bv, $ind;
  }
  elsif ($o == SRL_HDR_ALIAS) {
    my $ofs= varint();
    printf "%06u: %02x %03s %sALIAS(%u)\n", $p, $o, $bv, $ind, $ofs;
  }
  elsif ($o == SRL_HDR_OBJECTV) {
    my $ofs= varint();
    printf "%06u: %02x %03s %sOBJECTV(%d)\n", $p, $o, $bv, $ind, $ofs;
    printf  "%6s  %2s %3s %s  Value:\n",("") x 3, $ind."  ";
    parse_sv($ind."    ");
  }
  elsif ($o == SRL_HDR_OBJECT) {
    printf "%06u: %02x %03s %sOBJECT\n", $p, $o, $bv, $ind;
    printf  "%6s  %2s %3s %s  Class:\n",("") x 3, $ind."  ";
    parse_sv($ind."    ");
    printf  "%6s  %2s %3s %s  Value:\n",("") x 3, $ind."  ";
    parse_sv($ind."    ");
  }
  elsif ($o == SRL_HDR_REGEXP) {
    printf "%06u: %02x %03s %sREGEXP\n", $p, $o, $bv, $ind;
    parse_sv($ind."  ");
    parse_sv($ind."  ");
  }
  elsif ($o == SRL_HDR_FALSE) {
    printf "%06u: %02x %03s %sFALSE\n", $p, $o, $bv, $ind;
  }
  elsif ($o == SRL_HDR_TRUE) {
    printf "%06u: %02x %03s %sTRUE\n", $p, $o, $bv, $ind;

  }
  else {
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
