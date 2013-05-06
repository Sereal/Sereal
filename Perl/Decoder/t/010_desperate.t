#!perl
use strict;
use warnings;
use Sereal::Decoder qw(decode_sereal);
use Sereal::Decoder::Constants qw(:all);
use File::Spec;
use Devel::Peek;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);

use Data::Dumper; # this thing seems to change the meaning of true?!

# These tests are extraordinarily basic, badly-done and really just
# for basic sanity testing during development.

use Test::More;

run_tests("plain");
done_testing();
note("All done folks!");

sub run_tests {
    my ($extra_name, $opt_hash) = @_;
    my $dec = Sereal::Decoder->new($opt_hash ? $opt_hash : ());
    foreach my $bt (@BasicTests) {
        my ($in, $exp, $name) = @$bt;

        next if $ENV{SEREAL_TEST} and $ENV{SEREAL_TEST} ne $name;

        $exp = $exp->($opt_hash) if ref($exp) eq 'CODE';
        $exp = Header() . $exp;

        my ($out, $out2, $out3);
        my $ok= eval { decode_sereal($exp, $opt_hash ? $opt_hash : undef, $out); 1};
        my $err = $@ || 'Zombie error';

        ok($ok,"($extra_name) did not die: $name")
            or do {
                diag $err;
                diag "input=", Data::Dumper::qquote($exp);
                next;
            };
        ok(defined($out)==defined($in), "($extra_name) defined: $name");
        is_deeply($out, $in,"($extra_name) is_deeply: $name");

        my $ok2= eval { $out2 = $dec->decode($exp,$out); 1 };
        my $err2 = $@ || 'Zombie error';
        ok($ok2,"($extra_name, OO) did not die: $name")
            or do {
                diag $err2;
                diag "input=", Data::Dumper::qquote($exp);
                next;
            };
        ok(defined($out2)==defined($in), "($extra_name, OO) defined: $name");
        is_deeply($out2, $in,"($extra_name, OO) is_deeply: $name");

        SKIP: {
            skip "Weak thing alias test cannot work without pass-down", 1
                if $name eq 'weak thing (aliased root)';
            my $ok3= eval { $out3 = decode_sereal($exp); 1};
            my $err3 = $@ || 'Zombie error';

            ok($ok,"($extra_name, non-pass-down) did not die: $name")
                or do {
                    diag $err3;
                    diag "input=", Data::Dumper::qquote($exp);
                    next;
                };
            ok(defined($out3)==defined($in), "($extra_name, non-pass-down) defined: $name");
            is_deeply($out3, $in,"($extra_name, non-pass-down) is_deeply: $name");
        }
    }
}

