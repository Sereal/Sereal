#!perl
use strict;
use warnings;
# most be loaded before Sereal::TestSet
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(:all);

use Sereal::Encoder qw(encode_sereal);
use Sereal::Encoder::Constants qw(:all);

use Data::Dumper; # must be loaded AFTER the test set (bug in perl)

# These tests are extraordinarily basic, badly-done and really just
# for basic sanity testing during development.

use Test::More;

run_tests("plain");
run_tests("no_shared_hk", {no_shared_hashkeys => 1});
run_tests("dedupe_strings", {dedupe_strings => 1});
run_tests("aliased_dedupe_strings", {aliased_dedupe_strings => 1});
done_testing();

sub run_tests {
  my ($extra_name, $opt_hash) = @_;
  setup_tests(4);
  foreach my $bt (@BasicTests) {
    my (undef, $expect, $name, @alternate) = @$bt;

    $name="unnamed" if not defined $name;
    #next unless $name=~/PAD/;

    for my $x ( $expect, @alternate ) {
        $x = $x->($opt_hash) if ref($x) eq 'CODE';
        # add the header ...
        $x = Header() . $x;
    }

    my $enc = Sereal::Encoder->new($opt_hash ? $opt_hash : ());
    my $out;
    eval{
        $out= $enc->encode($bt->[0]); # must use bt here or we get a copy
        1;
    } or die "Failed to encode: \n$@\n". Data::Dumper::Dumper($bt->[0]);
    ok(defined $out, "($extra_name) defined: $name")
        or next;

    my $alt= "";
    if ($out ne $expect) {
        foreach my $accept (@alternate) {
            if ($out eq $accept) {
                $expect= $accept;
                $alt= " - alternate";
                last;
            }
        }
    }
    is(Data::Dumper::qquote($out), Data::Dumper::qquote($expect), "($extra_name) correct: $name" . $alt)
      or do {
        if ($ENV{DEBUG_SEREAL}) {
          print STDERR "\nEXPECTED:\n";
          hobodecode($expect);
          print STDERR "\nGOT:\n";
          hobodecode($out);
          print STDERR "\n";
        }
      };
  }
}

