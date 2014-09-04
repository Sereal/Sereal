#!perl
use strict;
use warnings;

use Test::More;

use Sereal::Merger;
use Sereal::Encoder qw/encode_sereal/;

sub binary2hex {
    my $unpacked = unpack("H*", $_[0]);
    return join(' ', grep { $_ } split(/(..)/, $unpacked)) . "\n";
}

{
    my $test = Sereal::Merger::test_me();
    print "--[$test]\n";
}

{
    my $test = Sereal::Merger->new({});
    print Dumper($test); use Data::Dumper;
    my $enc = encode_sereal(['x' x 33, 'x' x 33]);
    #my $enc = encode_sereal([{ foo => 'barbarbar' }, { foo => 'barbarbar' }, { foo => 'barbarbar' }], { dedupe_strings => 1 });
    #my $enc = encode_sereal(['foo']);
    my $unpacked = unpack("H*", $enc);
    print binary2hex($enc) . "\n";

    $test->append($enc);
    #$test->append($enc);
    my $merged = $test->finish();

    print "\n" . binary2hex($merged) . "\n";
}

pass;
done_testing;
