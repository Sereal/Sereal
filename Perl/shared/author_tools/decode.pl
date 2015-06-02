#!/usr/bin/perl -w

# This script is for testing Sereal decode speeds, with various
# generated test inputs (which are first encoded).  Sample usages:
#
# decode.pl --build --output=data.srl
#
# will (1) build a "graph" (a hash of small strings, really,
# which can be seen as an adjacency list representation of
# a graph, the vertex and its neighbors) of 1e5 vertices
# (2) decode the encoded blob 5 times (the 'graph', 1e5, and 5
# being the defaults).
#
# Other inputs types (--type=T) are
# aoi (array of int) (value == key)
# aoir (array of int) (value == randomly shuffled key)
# aof (array of float) (rand())
# aos (array of string) (value eq key)
# hoi (hash of int)
# hof (hash of float)
# hos (hash of string)
#
# The 'base' number of elements in each case is controlled by --elem=N.
# For the array and hash the number of elements is trivial, for the graph
# the total number of elements (in its hash-of-hashes) is O(N log N).
#
# The number decode repeats is controlled by --repeat_decode=N and --repeat_decode=N.
#
# The encode input needs to be built only once, the --output tells
# where to save the encoded blob.  The encode blob can be read back
# from the save file with --input, much faster, especially in the case
# of the graph input.

use strict;

use Time::HiRes;
use Sereal::Encoder;
use Sereal::Decoder;
use Getopt::Long;
use Fcntl qw[O_RDONLY O_WRONLY O_CREAT O_TRUNC];
use List::Util qw[shuffle];

sub MB () { 2 ** 20 }

my %Opt;
my @Opt = ('input=s', 'output=s', 'type=s', 'elem=f', 'build',
           'repeat_encode=i', 'repeat_decode=i',

           # If non-zero, will drop the minimum and maximum
           # values before computing statistics IF the number
           # of measurements is at least this limit.  So with
           # a value of 5 will leave 3 measurements.  Lowers
           # the stddev, should not affect avg/median (much).
           # Helpful in reducing cache effects.
           'min_max_drop_limit=i',

           'size');
my %OptO = map { my ($n) = /^(\w+)/; $_ => \$Opt{$n} } @Opt;
my @OptU = map { "--$_" } @Opt;

GetOptions(%OptO) or die "GetOptions: @OptU\n";

my $data;
my $blob;
my $size;
my $data_size;
my $blob_size;
my $dt;

if (defined $Opt{size}) {
    eval 'use Devel::Size qw[total_size]';
    if ($@) {
        die "$0: --size but Devel::Size=total_size not found\n";
    }
}

if (defined $Opt{build}) {
    die "$0: --input with --build makes no sense\n" if defined $Opt{input};
    $Opt{elem} //= 1e5;
} else {
    die "$0: --output without --build makes no sense\n" if defined $Opt{output};
    die "$0: --elem without --build makes no sense\n" if defined $Opt{elem};
    die "$0: Must specify either --build or --input\n" unless defined $Opt{input};
}
if (defined ($Opt{output})) {
    die "$0: --input with --output makes no sense\n" if defined $Opt{input};
}

$Opt{type} //= 'graph';
$Opt{repeat_encode} //= 1;
$Opt{repeat_decode} //= 5;
$Opt{min_max_drop_limit} //= 0;

my %TYPE = map { $_ => 1 } qw[aoi aoir aof aos hoi hof hos graph];

die "$0: Unexpected --type=$Opt{type}\n$0: Expected --type=@{[join('|', sort keys %TYPE)]}\n"
    unless exists $TYPE{$Opt{type}};

sub Times::new {
    my $t = Time::HiRes::time();
    my ($u, $s, $cu, $cs) = times();
    bless {
        wall => $t,
        usr  => $u,
        sys  => $s,
        cpu  => $u + $s,
        cusr => $cu,
        csys => $cs,
    }, $_[0];
}
sub Times::diff {
    die "Unexpected diff(@_)\n" unless ref $_[0] eq ref $_[1];
    bless { map { $_ => ($_[0]->{$_} - $_[1]->{$_}) } keys %{$_[0]} }, ref $_[0];
}
sub Times::wall { $_[0]->{wall} }
sub Times::usr  { $_[0]->{usr}  }
sub Times::sys  { $_[0]->{sys}  }
sub Times::cpu  { $_[0]->{cpu}  }
# times() can often sum just a tad higher than wallclock.
sub Times::pct { 100 * ($_[0]->cpu > $_[0]->wall ? 1 : $_[0]->cpu / $_[0]->wall) }

sub timeit {
    my $code = shift;
    my $t0 = Times->new();
    my @res = $code->(@_);
    my $t1 = Times->new();
    my $dt = $t1->diff($t0);
    return $dt;
}

sub __stats {
    # The caller is supposed to have done this sorting
    # already, but let's be wasteful and paranoid.
    my @v = sort { $a <=> $b } @_;
    my $min = $v[0];
    my $max = $v[-1];
    my $med = @v % 2 ? $v[@v/2] : ($v[@v/2-1] + $v[@v/2]) / 2;
    my $sum = 0;
    for my $t (@_) {
        $sum += $t;
    }
    my $avg = $sum / @_;
    my $sqsum = 0;
    for my $t (@_) {
        $sqsum += ($avg - $t) ** 2;
    }
    my $stddev = sqrt($sqsum / @_);
    return ( avg => $avg,
             stddev => $stddev,
             rstddev => $avg ? $stddev / $avg : undef,
             min => $min, med => $med, max => $max );
}

sub stats {
    my %stats;
    for my $k (qw(wall cpu)) {
        my @v = sort { $a <=> $b } map { $_->{$k} } @_;
        if ($Opt{min_max_drop_limit} > 0 &&
            @v >= $Opt{min_max_drop_limit}) {
            print "$k: dropping min and max ($v[0] and $v[-1])\n";
            shift @v;
            pop @v;
        }
        $stats{$k} = { __stats(@v) };
    }
    return %stats;
}

if (defined $Opt{build}) {
    print "building data\n";
    my $E;
    if ($Opt{type} eq 'graph') {
	print "building graph\n";
	my $V = $Opt{elem};
	$E = int($V * log($V)/log(2));
	printf("data of %d (%.1fM) vertices %d (%.1fM) edges\n",
	       $V, $V / MB, $E, $E / MB);
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    my $a = int(rand($V));
		    my $b = int(rand($V));
		    $data->{$a}{$b}++;
		}
	    });
    } elsif ($Opt{type} eq 'aoi') {
	print "building aoi\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    push @$data, $i;
		}
	    });
    } elsif ($Opt{type} eq 'aoir') {
	print "building aoir\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (shuffle 1..$E) {
		    push @$data, $i;
		}
	    });
    } elsif ($Opt{type} eq 'aof') {
	print "building aof\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    push @$data, rand();
		}
	    });
    } elsif ($Opt{type} eq 'aos') {
	print "building aos\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    push @$data, rand() . $$;
		}
	    });
    } elsif ($Opt{type} eq 'hoi') {
	print "building hoi\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    $data->{$i} = $i;
		}
	    });
    } elsif ($Opt{type} eq 'hof') {
	print "building hof\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    $data->{$i} = rand();
		}
	    });
    } elsif ($Opt{type} eq 'hos') {
	print "building hos\n";
	$E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    $data->{$i} = "$i";
		}
	    });
    } else {
	die "$0: Unexpected type '$Opt{type}'\n";
    }
    printf("build %.2f sec %.2f usr %.2f sys %.2f cpu %3d%% (%.1f elements/sec)\n",
           $dt->wall, $dt->usr, $dt->sys, $dt->cpu, $dt->pct, $E / $dt->wall);
    if ($Opt{size}) {
	$dt = timeit(sub { $data_size = total_size($data);});
	printf("data size %d bytes (%.1fMB) %.1f sec\n",
	       $data_size, $data_size / MB, $dt->wall);
    }

    my $encoder = Sereal::Encoder->new;

    {
	print "encoding data\n";
        my @dt;
        for my $i (1..$Opt{repeat_encode}) {
            $dt = timeit(sub { $blob = $encoder->encode($data); });
            $blob_size = length($blob);
            printf("%d/%d: encode to %d bytes (%.1fMB) %.2f sec %.2f usr %.2f sys %.2f cpu %3d%% (%.1f MB/sec)\n",
                   $i, $Opt{repeat_encode}, $blob_size, $blob_size / MB, $dt->wall, $dt->usr, $dt->sys, $dt->cpu, $dt->pct,
                   $blob_size / (MB * $dt->wall));
            push @dt, $dt;
        }
        if (@dt) {
            my %stats = stats(@dt);
            for my $k (qw(wall cpu)) {
                my $avg = $stats{$k}{avg};
                printf("encode %-4s avg %.2f sec (%.1f MB/sec) stddev %.2f sec (%.2f) min %.2f med %.2f max %.2f\n",
                       $k,
                       $avg, $avg ? $blob_size / (MB * $avg) : 0, $stats{$k}{stddev}, $avg ? $stats{$k}{rstddev} : 0,
                       $stats{$k}{min}, $stats{$k}{med}, $stats{$k}{max});
            }
        }
    }

    if (defined $Opt{output}) {
	print "opening output\n";
	my $fh;
	sysopen($fh, $Opt{output}, O_WRONLY|O_CREAT|O_TRUNC)
	    or die qq[sysopen "$Opt{output}": $!\n];
	print "writing blob\n";
	$dt = timeit(
	    sub {
		syswrite($fh, $blob)
		    or die qq[syswrite "$Opt{otput}": $!\n] });
	$blob_size = length($blob);
	printf("wrote %d bytes (%.1f MB) %.2f sec  %.2f usr %.2f sys %.2f cpu %3d%% (%.1f MB/sec)\n",
	       $blob_size, $blob_size / MB, $dt->wall, $dt->usr, $dt->sys, $dt->cpu, $dt->pct,
               $blob_size / (MB * $dt->wall));
    }
} elsif (defined $Opt{input}) {
    print "opening input\n";
    my $fh;
    sysopen($fh, $Opt{input}, O_RDONLY) or die qq[sysopen "$Opt{input}": $!\n];
    print "reading blob\n";
    $dt = timeit(
	sub {
	    sysread($fh, $blob, -s $fh)
		or die qq[sysread "$Opt{input}": $!\n];
	});
    $blob_size = length($blob);
    printf("read %d bytes (%.1f MB) %.2f sec %.2f usr %.2f sys %.2f cpu %3d%% (%.1f MB/sec)\n",
	   $blob_size, $blob_size / MB, $dt->wall,  $dt->usr, $dt->sys, $dt->cpu, $dt->pct,
           $blob_size / (MB * $dt->wall));
}

my $decoder = Sereal::Decoder->new;

{
    print "decoding blob\n";
    $blob_size = length($blob);
    my @dt;
    for my $i (1..$Opt{repeat_decode}) {
	$dt = timeit(sub { $data = $decoder->decode($blob); });
	printf("%d/%d: decode from %d bytes (%.1fM) %.2f sec %.2f usr %.2f sys %.2f cpu %3d%% (%.1f MB/sec)\n",
	       $i, $Opt{repeat_decode}, $blob_size, $blob_size / MB,
	       $dt->wall, $dt->usr, $dt->sys, $dt->cpu, $dt->pct, $blob_size / (MB * $dt->wall));
	push @dt, $dt;
    }
    if (ref $data eq 'HASH') {
        printf("data is hashref of %d elements\n", scalar keys %{$data});
    } elsif (ref $data eq 'ARRAY') {
        printf("data is hashref of %d elements\n", scalar @{$data});
    } elsif (ref $data) {
        printf("data is ref of %s\n", ref $data);
    } else {
        printf("data is of unexpected type\n");
    }
    if (@dt) {
        my %stats = stats(@dt);
        for my $k (qw(wall cpu)) {
            my $avg = $stats{$k}{avg};
            printf("decode %-4s avg %.2f sec (%.1f MB/sec) stddev %.2f sec (%.2f) min %.2f med %.2f max %.2f\n",
                   $k,
                   $avg, $avg ? $blob_size / (MB * $stats{$k}{avg}) : 0, $stats{$k}{stddev}, $avg ? $stats{$k}{rstddev} : 0,
                   $stats{$k}{min}, $stats{$k}{med}, $stats{$k}{max});
        }
    }
    if ($Opt{size}) {
	$dt = timeit(sub { $data_size = total_size($data); });
	printf("data size %d bytes (%.1fMB) %.1f sec\n",
	       $data_size, $data_size / MB, $dt->wall);
    }
}

if ($Opt{size}) {
    if ($blob_size && $data_size) {
        printf("data size / blob size %.2f\n", $data_size / $blob_size);
    }
}

exit(0);
