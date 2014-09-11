#!perl
use strict;
use warnings;

use Test::More;
use Time::HiRes;
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
    local $/ = undef;
    my @files = sort { $a cmp $b } glob('~/1/*.srl');
    #@files = splice(@files, 0, 2);

    my @data = map {
        #print "read file $_\n";
        open(my $fh, '<', $_) or die $!;
        my $content = <$fh>;
        $content;
    } @files;

    #print "sleep 5 sec\n";
    #sleep(5);

    my $start = Time::HiRes::time();

    #for (1..10) {
        my $test = Sereal::Merger->new({});
        foreach (@data) {
            $test->append($_);
        };

        my $merged = $test->finish();
    #}

    my $elapsed = Time::HiRes::time() - $start;
    printf "Merging took %.2fs\n", $elapsed;
    open (my $fh, '>', 'data.srl') or die $!;
    print $fh $merged;
    #print "\n" . binary2hex($merged) . "\n";
}

pass;
done_testing;
