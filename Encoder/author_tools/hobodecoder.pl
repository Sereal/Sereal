#!perl
use strict;
use warnings;
use Data::Dumper;

use Getopt::Long qw(GetOptions);
use Sereal::Encoder;
use Sereal::Encoder::Constants qw(:all);

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

$data =~ s/\s*$//m;
$data =~ s/^\s*//s;
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
  $data =~ s/^(srl.)// or die "invalid header: $data";
  $done .= $1;
  my $len = varint();
  my $hdr = substr($data, 0, $len);
  if (length($hdr)) {
    print "Header($len): " . join(" ", map ord, split //, $hdr) . "\n";
  }
  else {
    print "Empty Header.\n";
  }
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
  elsif ($o <= 15) {
    printf "%06u: %02x %03s %sPOS: %u\n", $p, $o, $bv, $ind, $o;
  }
  elsif ($o <= 31) {
    $o = 15-$o;
    printf "%06u: %02x %03s %sNEG: %i\n", $p, $o, $bv, $ind, $o;
  }
  elsif ($o >= 64) {
    $o -= 64;
    my $len = $o;
    my $str = substr($data, 0, $len, '');
    $done .= $str;
    printf "%06u: %02x %03s %sASCII(%u): '%s'\n", $p, $o, $bv, $ind, $len, $str;
  }
  elsif ($o == SRL_HDR_STRING || $o == SRL_HDR_STRING_UTF8) {
    my $l = varint();
    my $str = substr($data, 0, $l, ""); # fixme UTF8
    $done .= $str;
    printf "%06u: %02x %03s %sSTRING".($o == SRL_HDR_STRING_UTF8 ? "_UTF8" : "")."(%u): '%s'\n", $p, $o, $bv, $ind, $l, $str;
  }
  elsif ($o == SRL_HDR_REF) {
    my $whence = varint();
    printf "%06u: %02x %03s %sREF(%u)\n", $p, $o, $bv, $ind, $whence;
    if ($whence == 0) {
      parse_sv($ind . "  ");
    }
  }
  elsif ($o == SRL_HDR_REUSE) {
    my $len = varint();
    printf "%06u: %02x %03s %sREUSE(%u)\n", $p, $o, $bv, $ind, $len;
  }
  elsif ($o == SRL_HDR_COPY) {
    my $len = varint();
    printf "%06u: %02x %03s %sCOPY(%u)\n", $p, $o, $bv, $ind, $len;
  }
  elsif ($o == SRL_HDR_ARRAY) {
    printf "%06u: %02x %03s %sARRAY", $p, $o, $bv, $ind;
    parse_av($ind);
  }
  elsif ($o == SRL_HDR_HASH) {
    printf "%06u: %02x %03s %sHASH", $p, $o, $bv, $ind;
    parse_hv($ind);
  }
  elsif ($o == SRL_HDR_TAIL) {
    printf "%06u: %02x %03s %sTAIL\n", $p, $o, $bv, $ind;
    return 1;
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
  elsif ($o == SRL_HDR_BLESS) {
    printf "%06u: %02x %03s %sBLESS\n", $p, $o, $bv, $ind;
    parse_sv($ind."  ");
    parse_sv($ind."  ");
  }
  elsif ($o == SRL_HDR_REGEXP) {
    printf "%06u: %02x %03s %sREGEXP\n", $p, $o, $bv, $ind;
    parse_sv($ind."  ");
    parse_sv($ind."  ");
  }
  else {
    die "unsupported type: $o ($t): $const_names{$o}";
  }
  return 0;
}

sub parse_av {
  my ($ind) = @_;
  my $len = varint();
  printf "(%u)\n", $len;
  $ind .= "  ";
  while (1) {
    my $t = substr($data, 0, 1);
    my $o = ord($t);
    if ($o == SRL_HDR_TAIL) {
      printf "%06u: %02x %03s %sTAIL\n", length($done), SRL_HDR_TAIL, SRL_HDR_TAIL, $ind;
      $done .= substr($data, 0, 1, "");
      last;
    }
    else {
      parse_sv($ind);
    }
  }
}

sub parse_hv {
  my ($ind) = @_;
  my $len = varint();
  printf "(%u)\n", $len;
  $ind .= "  ";
  my $flipflop = 0;
  while (1) {
    my $t = substr($data, 0, 1);
    my $o = ord($t);
    if ($o == SRL_HDR_TAIL) {
      printf "%06u: %02x %03s %sTAIL\n", length($done), SRL_HDR_TAIL, SRL_HDR_TAIL, $ind;
      $done .= substr($data, 0, 1, "");
      last;
    }
    else {
      print( "        ", $ind, ($flipflop++ % 2 == 0 ? "VALUE" : "KEY"), ":\n" );
      parse_sv($ind."  ");
    }
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
