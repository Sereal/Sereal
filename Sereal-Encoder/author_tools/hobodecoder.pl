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

my $dref = \$data;
my $indent = "";
my $pos = -1;
parse_header($dref, \$pos);
while (defined $data and $data ne '') {
  my $done = parse_sv($dref, \$pos, $indent);
  last if $done;
}

sub parse_header {
  my $dr = shift;
  my $ri = shift;
  $$dr =~ s/^srl.// or die "invalid header";
  my $len = varint($dr, $ri);
  my $hdr = substr($$dr, 0, $len);
  if (defined $hdr and length($hdr)) {
    print "Header($len): " . join(" ", map ord, split //, $hdr) . "\n";
  }
  else {
    print "Empty Header.\n";
  }
  $$ri += $len + 4;
}

sub parse_sv {
  my ($dr, $ri, $ind) = @_;

  my $p = $$ri;
  my $t = substr($$dr, 0, 1, '');
  $$ri++;
  my $o = ord($t);
  my $high = $o > 128;
  $o -= 128 if $high;
  if ($o == SRL_HDR_VARINT) {
    printf "%06u, %sVARINT: %u\n", $p, $ind, varint($dr, $ri);
  }
  elsif ($o <= 15) {
    printf "%06u, %sPOS: %u\n", $p, $ind, $o;
  }
  elsif ($o <= 31) {
    $o = 15-$o;
    printf "%06u, %sNEG: %i\n", $p, $ind, $o;
  }
  elsif ($o > 64) {
    $o -= 64;
    my $len = $o;
    $$ri += $len;
    my $str = substr($$dr, 0, $len, '');
    printf "%06u, %sASCII(%u): '%s'\n", $p, $ind, $len, $str;
  }
  elsif ($o == SRL_HDR_STRING || $o == SRL_HDR_STRING_UTF8) {
    my $l = varint($dr, $ri);
    my $str = substr($$dr, 0, $l, ""); # fixme UTF8
    printf "%06u, %sSTRING".($o == SRL_HDR_STRING_UTF8 ? "_UTF8" : "")."(%u): '%s'\n", $p, $ind, $l, $str;
    $$ri += $l;
  }
  elsif ($o == SRL_HDR_REF) {
    my $whence = varint($dr, $ri);
    printf "%06u, %sREF(%u)\n", $p, $ind, $whence;
    if ($whence == 0) {
      parse_sv($dr, $ri, $ind."  ");
    }
  }
  elsif ($o == SRL_HDR_REUSE) {
    my $len = varint($dr, $ri);
    printf "%06u, %sREUSE(%u)\n", $p, $ind, $len;
  }
  elsif ($o == SRL_HDR_COPY) {
    my $len = varint($dr);
    printf "%06u, %sCOPY(%u)\n", $p, $ind, $len;
  }
  elsif ($o == SRL_HDR_ARRAY) {
    parse_av($dr, $ri, $ind);
  }
  elsif ($o == SRL_HDR_HASH) {
    parse_hv($dr, $ri, $ind);
  }
  elsif ($o == SRL_HDR_TAIL) {
    printf "%06u, %sTAIL\n", $p, $ind;
    return 1;
  }
  elsif ($o == SRL_HDR_UNDEF) {
    printf "%06u, %sUNDEF\n", $p, $ind;
  }
  elsif ($o == SRL_HDR_WEAKEN) {
    printf "%06u, %sWEAKEN\n", $p, $ind;
  }
  elsif ($o == SRL_HDR_PAD) {
    printf "%06u, %sPAD\n", $p, $ind;
  }
  elsif ($o == SRL_HDR_ALIAS) {
    my $ofs= varint($dr);
    printf "%06u, %sALIAS(%u)\n", $p, $ind, $ofs;
  }
  else {
    die "unsupported type: $o ($t): $const_names{$o}";
  }
  return 0;
}

sub parse_av {
  my ($dr, $ri, $ind) = @_;
  my $p = $$ri;
  my $len = varint($dr, $ri);
  printf "%06u, %sARRAY(%u)\n", $p, $ind, $len;
  $ind .= "  ";
  $$ri++;
  while (1) {
    my $t = substr($$dr, 0, 1);
    my $o = ord($t);
    if ($o == SRL_HDR_TAIL) {
      printf "%06u, %sTAIL\n", $$ri, $ind;
      substr($$dr, 0, 1, "");
      $$ri++;
      last;
    }
    else {
      parse_sv($dr, $ri, $ind);
    }
  }
}

sub parse_hv {
  my ($dr, $ri, $ind) = @_;
  my $p = $$ri;
  my $len = varint($dr, $ri);
  printf "%06u, %sHASH(%u)\n", $p, $ind, $len;
  $ind .= "  ";
  $$ri++; # for the hash type token
  my $flipflop = 0;
  while (1) {
    my $t = substr($$dr, 0, 1);
    my $o = ord($t);
    if ($o == SRL_HDR_TAIL) {
      printf "%06u, %sTAIL\n", $$ri, $ind;
      substr($$dr, 0, 1, "");
      $$ri++;
      last;
    }
    else {
      print( "        ", $ind, ($flipflop++ % 2 == 0 ? "VALUE" : "KEY"), ":\n" );
      parse_sv($dr, $ri, $ind."  ");
    }
  }
}


# super inefficient
sub varint {
  my $dr = shift;
  my $ri = shift;
  my $x = 0;
  my $lshift = 0;
  while (length($$dr) && ord(substr($$dr, 0, 1)) & 0x80) {
    my $c = ord(substr($$dr, 0, 1, ''));
    ++$$ri if $ri;
    $x += ($c & 0x7F) << $lshift;
    $lshift += 7;
  }
  if (length($$dr)) {
    ++$$ri if $ri;
    my $c = ord(substr($$dr, 0, 1, ''));
    $x += $c << $lshift;
  }
  else {
    die "premature end of varint";
  }
  return $x;
}
