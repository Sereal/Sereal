use strict;
use warnings;
use Dumbbench;
use IPC::Open2 qw(open2);
use JSON::PP qw(encode_json decode_json);
use IO::Handle;
$| = 1;
my ($dir, $case, $operation, $block, $probe) = @ARGV;
my $repo = '/home/demerphq/git_tree/Sereal';
my $perl = '/home/demerphq/perl5/perlbrew/perls/perl-5.44.0/bin/perl';
my @order = $block % 2 ? ('current', $probe) : ($probe, 'current');
my @instances;
for my $variant (@order) {
    local %ENV = %ENV;
    delete @ENV{qw(PERL5LIB PERL5OPT LD_PRELOAD SEREAL_DEFAULT_KEY SEREAL_CHECKSUM_POLICY)};
    $ENV{PERL_HASH_SEED} = 0;
    $ENV{PERL_PERTURB_KEYS} = 0;
    if (1) {
        my $base = $variant eq 'current' ? $repo : qq(/tmp/sereal-cause/$variant);
        $ENV{PERL5LIB} = join ':', map {("$base/Perl/$_/blib/lib", "$base/Perl/$_/blib/arch")} qw(Encoder);
    }
    my ($read, $write);
    my $pid = open2($read, $write, 'taskset', '-c', '0', $perl, "$dir/worker.pl", $case, $operation, "$dir/fixture.pl");
    $write->autoflush(1);
    my $line = <$read>;
    die "worker failed: $variant" unless defined $line;
    my $meta = decode_json($line);
    print "$case $operation block=$block $variant: $line";
    push @instances, SerealTiming->new(name => $variant, read => $read, write => $write, pid => $pid, meta => $meta);
}
die 'output size mismatch' unless $instances[0]{meta}{bytes} == $instances[1]{meta}{bytes};
my $bench = Dumbbench->new(initial_runs => 20, max_iterations => 40,
    target_rel_precision => 0.005, subtract_dry_run => 0, verbosity => 0);
$bench->add_instances(@instances);
$bench->run;
$bench->report;
my @results;
for my $inst (@instances) {
    my $result = $inst->result;
    push @results, {case => $case, operation => $operation, block => 0+$block,
        variant => $inst->name, mean_seconds => $result->raw_number,
        uncertainty_seconds => $result->raw_error->[0], retained => $result->nsamples,
        timings => $inst->timings, metadata => $inst->{meta}};
    my $write = $inst->{write};
    print $write "quit\n";
    close $write;
    waitpid($inst->{pid}, 0);
    die "worker exit: $?" if $?;
}
open my $out, '>', "$dir/$probe-$case-$operation-$block.json" or die $!;
print $out encode_json(\@results), "\n";
close $out;
package SerealTiming;
use parent 'Dumbbench::Instance';
sub single_run {
    my $self = shift;
    my ($read, $write) = @{$self}{qw(read write)};
    print $write "run\n";
    my $result = <$read>;
    die 'worker disappeared' unless defined $result && $result > 0;
    return 0+$result;
}
