#!perl
use strict;
use warnings;

use Test::More;
use Sereal::Path::Tie;
use Sereal::Encoder qw/encode_sereal/;

my $data = {
    foo => {},
    bar => undef,
};

my $tie = Sereal::Path::Tie::parse(encode_sereal($data));
is_deeply($tie->{foo}{test},    $data->{foo}{test}, 'data->{foo}{test}');
is_deeply($tie->{bar}{test},    $data->{bar}{test}, 'data->{bar}{test}');
is_deeply($tie->{baz}{test},    $data->{baz}{test}, 'data->{baz}{test}');
done_testing();
