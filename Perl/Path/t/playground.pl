#!perl

use strict;
use warnings;

use Data::Dumper;
use Sereal qw/encode_sereal decode_sereal/;
use Time::HiRes;
use Sereal::Path::Tie;

my $content = do {
    $/ = undef;
    <STDIN>;
};

my $start = Time::HiRes::time;
for (1..100000) {
    my $val = Sereal::Path::Tie::parse($content);
    # my $val = decode_sereal($content);
    my $v = $val->{tuning}{wallclock};
}

my $end = Time::HiRes::time;
printf "elapsed %d ms\n", ($end - $start) * 1000;
exit 0;
