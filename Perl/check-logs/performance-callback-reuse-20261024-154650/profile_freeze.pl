use strict;
use warnings;
use Sereal::Encoder;

my ($case, $repetitions) = @ARGV;
die "case and repetitions required\n" unless $case && $repetitions;
my $data = $case eq 'freeze-100'
    ? [ map { bless { name => 'blargh' }, 'Foo' } 1 .. 100 ]
    : $case eq 'sorted-freeze-1000'
    ? { map { ("key-$_" => bless { name => 'blargh' }, 'Foo') } 1 .. 1000 }
    : die "bad case\n";
my $encoder = Sereal::Encoder->new({
    protocol_version => 5,
    freeze_callbacks => 1,
    sort_keys => ($case eq 'sorted-freeze-1000' ? 1 : 0),
});
my $packet = $encoder->encode($data);
print STDERR "$case bytes=", length($packet), " encoder=$Sereal::Encoder::VERSION\n";
$encoder->encode($data) for 1 .. $repetitions;

package Foo;
sub FREEZE { return $_[0]{name} }
