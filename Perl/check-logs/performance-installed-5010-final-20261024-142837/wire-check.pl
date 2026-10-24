use strict;
use warnings;
use Sereal::Encoder;
use Scalar::Util qw(dualvar);
my $uvmax = ~0;
my $ivmax = $uvmax >> 1;
my @values = (0, 1, 15, 16, 127, 128, 255, 256, 16383, 16384,
    65535, 65536, 2147483647, $ivmax, $uvmax, -1, -16, -17, -$ivmax - 1);
for my $v (1 .. 6) {
    my $enc = Sereal::Encoder->new({protocol_version => $v, checksum_v0_key => undef});
    for my $value (@values) {
        my $shared = $value;
        for my $data ([$value], [$value, \$shared, \$shared]) {
            print unpack('H*', $enc->encode($data)), "\n";
        }
    }
    print unpack('H*', $enc->encode([dualvar(37, 'use the string')])), "\n";
}
