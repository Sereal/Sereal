#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Long qw(GetOptions);
use Time::HiRes qw(time);
use Sereal::Encoder;

# Run from Encoder with its local build on the load path, for example:
#   /usr/bin/time -f 'peak_rss_kb=%M' perl -Mblib \
#       author_tools/bench_retention.pl --case records --count 200000
# Compare separate processes built with the same Perl and compiler flags.
# Peak RSS includes construction of the input as well as encoding it.
my ( $case, $count, $iterations ) = ( 'records', 200000, 9 );
GetOptions( 'case=s' => \$case, 'count=i' => \$count, 'iterations=i' => \$iterations )
    or die "Usage: $0 --case records|arrays|scalars|keys --count N --iterations N\n";
die "count and iterations must be positive\n" unless $count > 0 && $iterations > 0;

my $data;
if ( $case eq 'records' ) {
    $data = [ map { +{
        id => $_, name => "name-$_", tags => [ 1, 2, 3 ],
        meta => { a => 1, b => [$_] },
    } } 1 .. $count ];
}
elsif ( $case eq 'arrays' ) {
    $data = [ map { [$_] } 1 .. $count ];
}
elsif ( $case eq 'scalars' ) {
    $data = [ 1 .. $count ];
}
elsif ( $case eq 'keys' ) {
    $data = { map { ( "key-$_" => $_ ) } 1 .. $count };
}
else {
    die "Unknown case '$case'\n";
}

my $encoder = Sereal::Encoder->new( { protocol_version => 6, checksum_v0_key => undef } );
my $bytes = length $encoder->encode($data); # warm up the reusable encoder
my @seconds;
for ( 1 .. $iterations ) {
    my $start = time;
    my $packet = $encoder->encode($data);
    push @seconds, time - $start;
    die "packet size changed between iterations\n" unless length($packet) == $bytes;
}
@seconds = sort { $a <=> $b } @seconds;
printf "case=%s count=%d iterations=%d bytes=%d median_ms=%.3f min_ms=%.3f\n",
    $case, $count, $iterations, $bytes,
    1000 * $seconds[ int( @seconds / 2 ) ], 1000 * $seconds[0];

__END__

=head1 NAME

bench_retention.pl - Compare encoder traversal time and peak memory

=head1 REPRODUCING THE COMPARISON

Build each revision with the same Perl and compiler flags. From its
C<Perl/Encoder> directory, run this script with the local build loaded:

    PERL_HASH_SEED=0 PERL_PERTURB_KEYS=0 /usr/bin/time -f 'peak_rss_kb=%M' \
        perl -Mblib author_tools/bench_retention.pl \
        --case records --count 200000 --iterations 9

Use C<--case arrays --count 300000> for the single-element-array case.
The script also supplies flat C<scalars> and unique hash C<keys> cases.
Run the revisions sequentially to avoid competing benchmark processes.

Sample measurements on Linux x86-64, Perl 5.44.0, GCC 13, C<-O3>, with
nine timed encodes after one warm-up encode:

                         Median milliseconds             Peak RSS (KiB)
    Input             historical  before    scan      historical before  scan
    200k records         40.664   731.681   59.198       315028   501892 262676
    300k arrays           7.460    40.602   10.873        79128   101116  81404
    200k scalars          1.419     1.471    1.711        25056    25092  26636
    300k keys           118.293   145.145  133.217       154080   158704 154152

C<historical> is d6d5378d, before container retention; C<before> is
99927aad, with unconditional retention for nested references; C<scan> is
the complete input scan that selects retention only when Perl callbacks
are possible. Peak RSS includes input construction, encoder state and
the returned packet. Measurements depend on the machine and allocator.

The scan removes most of the nested-container regression, but still adds
work compared with the historical encoder, including for flat scalars.
Inputs with magic, objects or unsupported types keep the retaining path.

=cut
