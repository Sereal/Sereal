#!perl

use strict;
use warnings;
use Test::More;
use File::Spec;
use Encode;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(have_encoder_and_decoder);
BEGIN {
    my $ok = have_encoder_and_decoder();
    if (not $ok) {
        plan skip_all => 'Did not find right version of encoder';
        done_testing();
        exit(0);
    }
}
require Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);

# Each test below will use the supplied encoder against
# the supplied input data structure and will compare how
# the decoder behaves with its output marked as utf8 or not
my @tests = (
    {
        # First round of tests, with a snappy-compressed structure,
        # crafted to yield high-bit data points
        encoder => Sereal::Encoder->new({ snappy => 1, snappy_threshold => 0 }),
        input => {
            foo => 'bar',
            f111 => 'bar',
            f1111 => 'bar',
            f11111 => 'bar',
            f111111 => 'bar',
            f1111111 => 'bar',
            f11111111 => 'bar',
            f111111111 => 'bar',
            f1111111111 => 'bar',
            f11111111111 => 'bar',
            f111111111111 => 'bar',
            f1111111111111 => 'bar',
            f11111111111111 => 'bar',
            f111111111111111 => 'bar',
            f1111111111111111 => 'bar',
            f11111111111111111 => 'bar',
        },
    },
    {
        # Second round of testing, this time do not use snappy, but
        # encode directly utf8 data
        encoder => Sereal::Encoder->new,
        input => { therefore => "\x{2234}" },
    },
);
plan tests => 9 * @tests;

# The testing routine
sub encode_and_encode {
    my ($encoder, $input) = @_;
    my $s1 = $encoder->encode($input);
    my $s2 = $s1;
    ok(!utf8::is_utf8($s1), "encoder returns a string without the utf8 flag");
    $s2 = encode("utf8", $s2);
    Encode::_utf8_on($s2);
    ok(utf8::is_utf8($s2), "the copy of the string has the utf8 flag turned on");
    is($s1, $s2, "the strings are still the same for perl");

    my $output;
    my $ok = eval { decode_sereal($s1, { validate_utf8 => 1 }, $output); 1 };
    my $err = $@ || 'Zombie error';
    ok($ok, "did not die while decoding the first string") or diag $err;
    is(ref $output, 'HASH', "correctly decoded to a hashref");
    is_deeply($output, $input, "correctly decoded");

    undef $output;
    $ok = eval { decode_sereal($s2, { validate_utf8 => 1 }, $output); 1 };
    $err = $@ || 'Zombie error';
    ok($ok, "did not die while decoding the utf8 string") or diag $err;
    is(ref $output, 'HASH', "correctly decoded to a hashref");
    is_deeply($output, $input, "correctly decoded");
}

for my $t (@tests) {
    encode_and_encode($t->{encoder}, $t->{input});
}
