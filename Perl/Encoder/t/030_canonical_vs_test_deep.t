#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet;
use Test::More;
use Sereal::Encoder qw(encode_sereal);
use version;
my %tests = (
    # IMPORTANT: If you add new types of cases here please update the
    # "CANONICAL REPRESENTATION" documentation.
    utf8_flag_on_ascii_string => [
        sub {
            return "en";
        },
        sub {
            my $en = "en";
            utf8::upgrade($en);
            return $en;
        },
    ],
    IV_string_value => [
        sub { "12345" },
        sub { "12345" + 0 },
    ],
    NV_string_value => [
        sub { "12.345" },
        sub { "12.345" + 0 },
    ],
);

eval {
    require Test::Deep::NoTest;
    die "PANIC: We expect at least Test::Deep 0.110 (and Test::Deep::NoTest doesn't support ->VERSION(...)"
        unless version->new(Test::Deep->VERSION) >= version->new('0.110');
    1;
} or do {
    my $error = $@ || "Zombie Error";
    plan skip_all => "We are skipping all our tests because we don't have a suitable Test::Deep here, got error: $error";
};
plan tests => keys(%tests) * 2;

for my $test (keys %tests) {
    my ($x, $y) = @{$tests{$test}};
    my $x_value = $x->();
    my $y_value = $y->();

    my $x_value_sereal = encode_sereal($x_value, {canonical => 1});
    my $y_value_sereal = encode_sereal($y_value, {canonical => 1});

    cmp_ok($x_value_sereal, 'ne', $y_value_sereal, "The $test values are not the same under Sereal");
    ok(Test::Deep::eq_deeply($x_value, $y_value), "The $test values are the same under Test::Deep though");
}
