use strict;
use warnings;
use Sereal::Encoder;
use Time::HiRes qw(clock_gettime CLOCK_MONOTONIC);
use JSON::PP qw(encode_json);
for my $n (100, 1000) {
    for my $sort (0,1) {
        my $data = {map {("key-$_" => bless({name=>'blargh'},'Foo'))} 1..$n};
        my $enc = Sereal::Encoder->new({protocol_version=>5, checksum_v0_key=>undef,
            freeze_callbacks=>1, sort_keys=>$sort});
        my $bytes=length $enc->encode($data);
        $enc->encode($data) for 1..3;
        my @samples;
        for (1..9) {
            my $start=clock_gettime(CLOCK_MONOTONIC);
            $enc->encode($data);
            push @samples, clock_gettime(CLOCK_MONOTONIC)-$start;
        }
        my @ordered=sort {$a<=>$b} @samples;
        print encode_json({encoder=>$Sereal::Encoder::VERSION, count=>$n, sorted=>$sort,
            median_ms=>$ordered[4]*1000, bytes=>$bytes, samples_seconds=>\@samples}),"\n";
    }
}
package Foo;
sub FREEZE { return $_[0]{name} }
