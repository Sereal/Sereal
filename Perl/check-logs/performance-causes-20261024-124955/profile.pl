use strict;
use warnings;
use Sereal::Encoder;
my $data=do '/home/demerphq/git_tree/Sereal/Perl/check-logs/performance-dumbbench-20261024-123314/fixture.pl';
die $@ unless $data;
my $enc=Sereal::Encoder->new({protocol_version=>5,checksum_v0_key=>undef});
$enc->encode($data) for 1..20000;
