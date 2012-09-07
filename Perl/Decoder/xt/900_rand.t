use strict;
use warnings;

use Test::More;

use Sereal::Decoder qw(:all);
use Sereal::Decoder::Constants qw(:all);
use Devel::Peek;


my $seed= $ENV{SRAND} || (time ^ int rand(1000000) ^ $$);
srand($seed);
warn("Seed for this test was: $seed\n");


my $header = SRL_MAGIC_STRING . chr(1) . chr(0);
my $header_snappy = SRL_MAGIC_STRING . chr(1+16) . chr(0);
for my $i (1..1000) {
    my $content = '';
    my $len = int( rand() * 200 );
    $content .= chr( int( rand() * 256 ) ) for 1..$len;

    foreach my $hdr (['plain', $header],
        #                 ['snappy', $header_snappy] # snappy not safe to random crapola (pity)
    )
    {
        my $out;
        my $ok;
        my $data = "$hdr->[1]$content";
        if ($ENV{DEBUG_SEREAL}) {
            note("'$data'");
            open my $pipe,
                "| perl -Mblib=../Encoder/blib -Mblib=../Decoder/blib author_tools/hobodecoder.pl -e"
                or die $!;
            print $pipe $data;
        }
        $ok = eval { $out = decode_sereal($data); 1 };
        pass("Alive $i, $hdr->[0]");
    }
}

pass("Alive");
done_testing();

