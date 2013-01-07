use strict;
use warnings;

use lib 't/lib';
use Sereal::TestSet qw(:all);

my $done;
my $data = varint(120000013);
$data =~ s/(.)$/chr(128+ord($1))/se;
$data .= chr(128) for 1..10;
$data .= chr(0) for 1;

warn length($data);
warn varint_dec();
warn length($done);

sub varint_dec {
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

