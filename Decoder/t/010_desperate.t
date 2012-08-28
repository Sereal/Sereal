#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use Data::Dumper;
use File::Spec;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
  lib->import('lib')
    if !-d 't';
}

use Sereal::TestSet qw(:all);

# These tests are extraordinarily basic, badly-done and really just
# for basic sanity testing during development.

use Test::More;

run_tests("plain");
done_testing();
note("All done folks!");

sub run_tests {
  my ($extra_name, $opt_hash) = @_;
  foreach my $bt (@BasicTests) {
    my ($in, $exp, $name) = @$bt;

    $exp = $exp->($opt_hash) if ref($exp) eq 'CODE';
    $exp = "$Header$exp";

    my $out;
    my $ok= eval { $out = decode_sereal($exp); 1};
    ok($ok,"($extra_name) did not die: $name")
        or do {
            diag "$@"||"Zombie error";
            diag "input=", Data::Dumper::qquote($exp);
            next;
        };
    ok(defined($out)==defined($in), "($extra_name) defined: $name");
    is_deeply($out, $in,"($extra_name) is_deeply: $name");
  }
}

