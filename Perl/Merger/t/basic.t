#!perl
use strict;
use warnings;

use Test::More;

use Sereal::Merger;
use Sereal::Encoder qw/encode_sereal/;

{
    my $test = Sereal::Merger::test_me();
    print "--[$test]\n";
}

{
    my $test = Sereal::Merger->new({});
    print Dumper($test); use Data::Dumper;
    my $enc = encode_sereal([{ foo => 'barbarbar' }, { foo => 'barbarbar' }, { foo => 'barbarbar' }], { dedupe_strings => 1 });
    my $unpacked = unpack("H*", $enc);
    print join(' ', grep { $_ } split(/(..)/, $unpacked)) . "\n";

    $test->append($enc);
    #my $merged = $test->finish();
    #print STDERR "finish returned $merged\n";
}

pass;
done_testing;
