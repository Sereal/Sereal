#!perl
use strict;
use warnings;
use Test::More;

BEGIN {
  use Config;
  if (! $Config{'useithreads'}) {
    print("1..0 # SKIP Perl not compiled with 'useithreads'\n");
    exit(0);
  }
  elsif ($] < 5.008007) {
    print("1..0 # SKIP Sereal not thread safe on Perls before 5.8.7\n");
    exit(0);
  }
}

use Sereal::Decoder;
plan tests => 1;
use threads;

sub foo {}
SCOPE: {
  my $enc = Sereal::Decoder->new;

  my $thr = threads->new(\&foo);
  $thr->join;
}

pass();
