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
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);

use Sereal::Encoder;
plan tests => 1;
use threads;
use threads::shared;

sub foo {}
SCOPE: {
    my $dat= shared_clone([undef]);
    my $enc = Sereal::Encoder->new;

    my $thr = threads->new(\&foo);
    $thr->join;
    my $encoded= $enc->encode($dat);
}

pass();
