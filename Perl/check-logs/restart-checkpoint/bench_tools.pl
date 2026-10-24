use strict;
use warnings;
use Time::HiRes qw(time);
use Sereal::Encoder;
use Sereal::Decoder;
use Sereal::Merger;
use Sereal::Splitter;
use Sereal::Path;
use Getopt::Long qw(GetOptions);

my ($case, $count, $iterations, $loops) = ('merger', 10000, 9, 5);
GetOptions('case=s' => \$case, 'count=i' => \$count,
    'iterations=i' => \$iterations, 'loops=i' => \$loops) or die 'bad options';
my $encoder = Sereal::Encoder->new({ protocol_version => 4 });
my $decoder = Sereal::Decoder->new;
my $data = [ map { +{ id => $_, name => "name-$_", tags => [1, 2, 3] } } 1 .. $count ];
my $packet = $encoder->encode($data);
my ($operation, $check);
if ($case eq 'merger') {
    my @docs;
    for (my $i = 0; $i < $count; $i += 500) {
        push @docs, $encoder->encode([ @$data[$i .. ($i + 499 < $count ? $i + 499 : $count - 1)] ]);
    }
    $operation = sub {
        my $merger = Sereal::Merger->new({ protocol_version => 4 });
        $merger->append($_) for @docs;
        return $merger->finish;
    };
    $check = sub {
        my $got = $decoder->decode($_[0]);
        die 'merged contents differ' unless @$got == @docs
            && $got->[0][0]{id} == 1 && $got->[-1][-1]{id} == $count;
    };
} elsif ($case eq 'splitter') {
    $operation = sub {
        my $splitter = Sereal::Splitter->new({ input => $packet, chunk_size => 16384 });
        my @chunks;
        while (defined(my $chunk = $splitter->next_chunk)) { push @chunks, $chunk }
        return \@chunks;
    };
    $check = sub {
        my @got = map { @{ $decoder->decode($_) } } @{ $_[0] };
        die 'split contents differ' unless @got == $count
            && $got[0]{id} == 1 && $got[-1]{id} == $count;
    };
} elsif ($case eq 'path-last') {
    $operation = sub { Sereal::Path->new($packet)->value('$[' . ($count - 1) . '].name') };
    $check = sub { die 'path result differs' unless $_[0] eq "name-$count" };
} elsif ($case eq 'path-all') {
    $operation = sub { [ Sereal::Path->new($packet)->values('$[*].id') ] };
    $check = sub {
        die 'path results differ' unless @{ $_[0] } == $count
            && $_[0][0] == 1 && $_[0][-1] == $count;
    };
} else { die "unknown case $case" }
my $warm = $operation->();
$check->($warm);
my $bytes = ref($warm) eq 'ARRAY' ? length(join('', @$warm)) : length($warm);
my @samples;
for (1 .. $iterations) {
    my $start = time;
    for (1 .. $loops) { my $out = $operation->() }
    push @samples, (time - $start) / $loops;
}
@samples = sort { $a <=> $b } @samples;
printf "case=%s count=%d iterations=%d loops=%d bytes=%d median_ms=%.6f min_ms=%.6f\n",
    $case, $count, $iterations, $loops, $bytes, 1000 * $samples[int(@samples / 2)], 1000 * $samples[0];
printf "Perl=%s Encoder=%s Decoder=%s Merger=%s Splitter=%s Path=%s\n", $^V,
    $Sereal::Encoder::VERSION, $Sereal::Decoder::VERSION, $Sereal::Merger::VERSION,
    $Sereal::Splitter::VERSION, $Sereal::Path::VERSION;
