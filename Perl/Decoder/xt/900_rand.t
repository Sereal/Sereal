use strict;
use warnings;

use Test::More;
use Test::Warn;
use Sereal::Decoder qw(:all);
use Sereal::Decoder::Constants qw(:all);
use lib ('t/lib', 'lib');
use Sereal::TestSet qw(hobodecode);
use Devel::Peek;


my $seed= $ENV{SRAND} || (time ^ int rand(1000000) ^ $$);
srand($seed);
warn("Seed for this test was: $seed\n");


my $header = SRL_MAGIC_STRING . chr(1) . chr(0);
my $header_v2 = SRL_MAGIC_STRING . chr(2) . chr(0);
my $header_snappy = SRL_MAGIC_STRING . chr(1+16) . chr(0);
my $decoder = Sereal::Decoder->new;
for my $i (1..2e9) {
    my $content = '';
    my $len = 200;
    #my $len = int( rand() * 200 );
    $content .= chr( int( rand() * 256 ) ) for 1..$len;

    foreach my $hdr ( ['v1', $header],
                      ['v2', $header_v2],
        #                 ['snappy', $header_snappy] # snappy not safe to random crapola (pity)
    )
    {
        my $out;
        my $ok;
        my $data = "$hdr->[1]$content";
        while ($len-->0) {
            if ($ENV{DEBUG_SEREAL}) {
                #note("'$data'");
                Dump($data);
                #hobodecode($data);
            }
            warnings_are { eval {$decoder->decode($data)} } [], "Alive and no warnings ($i, $len, $hdr->[0])";
            chop $data;
        }
    }
}

pass("Alive");
done_testing();

