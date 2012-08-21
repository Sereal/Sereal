use strict;
use warnings;
use Data::Dumper;

use Sereal::Encoder;
use Sereal::Encoder::Constants qw(:all);

$| = 1;

my %const_names = map {$_ => eval "$_"} @Sereal::Constants::EXPORT_OK;
#print Dumper \%const_names; exit;

local $/ = undef;
my $data = <STDIN>;

$data =~ s/\s*$//m;
$data =~ s/^\s*//s;
open my $fh, "| od -tu1c" or die $!;
print $fh $data;
close $fh;

print "\n";

my $dref = \$data;
my $indent = "";
parse_header($dref);
while (defined $data and $data ne '') {
  my $done = parse_sv($dref, $indent);
  last if $done;
}

sub parse_header {
  my $dr = shift;
  $$dr =~ s/^srl.// or die "invalid header";
  my $len = varint($dr);
  my $hdr = substr($$dr, $len);
  if (defined $hdr and length($hdr)) {
    print "Header: " . join(" ", map ord, split //, $hdr) . "\n";
  }
  else {
    print "Empty Header.\n";
  }
}

sub parse_sv {
  my $dr = shift;
  my $ind = shift;
  my $t = substr($$dr, 0, 1, '');
  my $o = ord($t);
  my $high = $o > 128;
  $o -= 128 if $high;
  if ($o == SRL_HDR_VARINT) {
    print $ind, "VARINT: " . varint($dr) . "\n";
  }
  elsif ($o <= 15) {
    print $ind, "POS: $o\n";
  }
  elsif ($o <= 31) {
    $o = 15-$o;
    print $ind, "NEG: $o\n";
  }
  elsif ($o == SRL_HDR_UNDEF) {
    print $ind, "UNDEF\n";
  }
  elsif ($o > 64) {
    $o -= 64;
    my $len = $o;
    my $str = substr($$dr, 0, $len, '');
    print $ind, "ASCII($len): '$str'\n";
  }
  elsif ($o == SRL_HDR_STRING || $o == SRL_HDR_STRING_UTF8) {
    my $l = varint($dr);
    print $ind, "STRING".($o == SRL_HDR_STRING_UTF8 ? "_UTF8" : "")."($l): '";
    my $str = substr($$dr, 0, $l, ""); # fixme UTF8
    print $str, "'\n";
  }
  elsif ($o == SRL_HDR_REF) {
    print $ind, "REF\n";
    parse_sv($dr, $ind."  ");
  }
  elsif ($o == SRL_HDR_REUSE) {
    my $len = varint($dr);
    print $ind, "REUSE($len)\n";
  }
  elsif ($o == SRL_HDR_COPY) {
    my $len = varint($dr);
    print $ind, "COPY($len)\n";
  }
  elsif ($o == SRL_HDR_ARRAY) {
    print $ind, "ARRAY\n";
    parse_av($dr, $ind."  ");
  }
  elsif ($o == SRL_HDR_TAIL) {
    print $ind, "TAIL\n";
    return 1;
  }
  else {
    die "unsupported type: $o ($t): $const_names{$o}";
  }
  return 0;
}

sub parse_av {
  my ($dr, $ind) = @_;
  while (1) {
    my $t = substr($$dr, 0, 1);
    my $o = ord($t);
    if ($o == SRL_HDR_TAIL) {
      print $ind, "TAIL\n";
      substr($$dr, 0, 1, "");
      last;
    }
    else {
      parse_sv($dr, $ind);
    }
  }
}

# super inefficient
sub varint {
  my $dr = shift;
  my $x = 0;
  my $lshift = 0;
  while (length($$dr) && ord(substr($$dr, 0, 1)) & 0x80) {
    my $c = ord(substr($$dr, 0, 1, ''));
    $x += ($c & 0x7F) << $lshift;
    $lshift += 7;
  }
  if (length($$dr)) {
    my $c = ord(substr($$dr, 0, 1, ''));
    $x += $c << $lshift;
  }
  else {
    die "premature end of varint";
  }
  return $x;
}
