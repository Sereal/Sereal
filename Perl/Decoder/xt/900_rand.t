use strict;
use warnings;

use Test::More;
use Test::Warn;
use Sereal::Decoder            qw(:all);
use Sereal::Decoder::Constants qw(:all);
use lib ( 't/lib', 'lib' );
use Sereal::TestSet qw(hobodecode);
use Devel::Peek;

my $seed = $ENV{SRAND} || ( time ^ int rand(1000000) ^ $$ );
srand($seed);
warn("Seed for this test was: $seed\n");

my $header        = SRL_MAGIC_STRING . chr(1) . chr(0);
my $header_v2     = SRL_MAGIC_STRING . chr(2) . chr(0);
my $header_snappy = SRL_MAGIC_STRING . chr( 1 + 16 ) . chr(0);
my $decoder       = Sereal::Decoder->new;
my $iterations    = $ENV{SEREAL_RAND_ITERATIONS} || 1000;
for my $i ( 1 .. $iterations ) {
    my $content = '';
    my $content_len = 200;

    #my $content_len = int( rand() * 200 );
    $content .= chr( int( rand() * 256 ) ) for 1 .. $content_len;

    warnings_are {
        foreach my $hdr (
            [ 'v1', $header ],
            [ 'v2', $header_v2 ],

            #                 ['snappy', $header_snappy] # snappy not safe to random crapola (pity)
            )
        {
            my $out;
            my $ok;
            my $data = "$hdr->[1]$content";
            my $len = $content_len;
            while ( $len-- > 0 ) {
                if ( $ENV{DEBUG_SEREAL} ) {

                    #note("'$data'");
                    Dump($data);

                    #hobodecode($data);
                }
                eval { $decoder->decode($data) };
                chop $data;
            }
        }
    } [], "Alive and no warnings ($i)";
}

pass("Alive");
done_testing();

