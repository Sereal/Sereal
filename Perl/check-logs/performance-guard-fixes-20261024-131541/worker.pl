use strict;
use warnings;
use JSON::PP qw(encode_json);
use Time::HiRes qw(clock_gettime CLOCK_MONOTONIC);
use Sereal::Encoder;
use Sereal::Decoder;
$| = 1;
my ($case, $operation, $fixture) = @ARGV;
my $data;
if ($case eq 'records') {
    $data = [map {+{id => $_, name => "name-$_", tags => [1,2,3], meta => {a => 1, b => [$_]}}} 1..200000];
} elsif ($case eq 'arrays') {
    $data = [map {[$_]} 1..300000];
} elsif ($case eq 'scalars') {
    $data = [1..200000];
} elsif ($case eq 'keys') {
    $data = {map {("key-$_" => $_)} 1..300000};
} elsif ($case =~ /^medium/) {
    $ENV{SEREAL_FIXTURE_PLAIN} = ($case eq 'medium-plain');
    $data = do $fixture;
    die "fixture $fixture: $@ $!" unless $data;
} elsif ($case =~ /^sorted-freeze-(1000)$/) {
    $data = {map {("key-$_" => bless({name=>'blargh'}, 'Foo'))} 1..$1};
} elsif ($case =~ /^freeze-(1|100)$/) {
    $data = [map {bless {name => 'blargh'}, 'Foo'} 1..$1];
} else { die "bad case $case" }
my $enc = Sereal::Encoder->new({protocol_version => 5, checksum_v0_key => undef,
    freeze_callbacks => ($case =~ /freeze/ ? 1 : 0), sort_keys => ($case =~ /^sorted/ ? 1 : 0)});
my $dec = Sereal::Decoder->new;
my $packet = $enc->encode($data);
my $bytes = length $packet;
my $work = $operation eq 'encode' ? sub {$enc->encode($data)} : sub {$dec->decode($packet)};
$work->() for 1..3;
sub batch {
    my ($n) = @_;
    my $start = clock_gettime(CLOCK_MONOTONIC);
    $work->() for 1..$n;
    return clock_gettime(CLOCK_MONOTONIC)-$start;
}
my $n = 1;
while (1) {
    my $seconds = batch($n);
    last if $seconds >= 0.2;
    $n *= 2;
}
print encode_json({perl => "$^V", encoder => $Sereal::Encoder::VERSION,
    decoder => $Sereal::Decoder::VERSION, encoder_path => $INC{'Sereal/Encoder.pm'},
    bytes => $bytes, batch_size => $n, case => $case, operation => $operation}), "\n";
while (my $command = <STDIN>) {
    last if $command =~ /^quit/;
    die "unexpected command" unless $command =~ /^run/;
    my $seconds = batch($n);
    printf "%.12g\n", $seconds/$n;
}
package Foo;
sub FREEZE { return $_[0]{name} }
sub THAW { return bless {name => $_[2]}, $_[0] }
