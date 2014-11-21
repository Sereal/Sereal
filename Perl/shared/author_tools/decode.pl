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
# The number decode repeats is controlled by --repeat=N.
#
# The encode input needs to be built only once, the --output tells
# where to save the encoded blob.  The encode blob can be read back
# from the save file with --input, much faster, especially in the case
# of the graph input.

use strict;

use Time::HiRes qw[time];
use Sereal::Encoder;
use Sereal::Decoder;
use Getopt::Long;
use Devel::Size qw[total_size];
use Fcntl qw[O_RDONLY O_WRONLY O_CREAT O_TRUNC];

sub MB () { 2 ** 20 }

my %Opt;
my @Opt = ('input=s', 'output=s', 'type=s', 'elem=f', 'build', 'repeat=i', 'size');
my %OptO = map { my ($n) = /^(\w+)/; $_ => \$Opt{$n} } @Opt;
my @OptU = map { "--$_" } @Opt;

GetOptions(%OptO) or die "GetOptions: @OptU\n";

my $data;
my $blob;
my $size;
my $data_size;
my $blob_size;
my $dt;

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
$Opt{repeat} //= 5;

my %TYPE = map { $_ => 1 } qw[aoi aof aos hoi hof graph];

die "$0: Unexpected --repeat=$Opt{repeat}\n" if $Opt{repeat} < 1;
die "$0: Unexpected --type=$Opt{type}\n$0: Expected --type=@{[join('|', sort keys %TYPE)]}\n"
    unless exists $TYPE{$Opt{type}};

sub timeit {
    my $code = shift;
    my $t0 = time();
    my @res = $code->(@_);
    my $dt = time() - $t0;
    return $dt;
}

if (defined $Opt{build}) {
    print "building data\n";
    if ($Opt{type} eq 'graph') {
	print "building graph\n";
	my $V = $Opt{elem};
	my $E = int($V * log($V)/log(2));
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
	printf("build %.3f sec (%.1f edges/sec)\n", $dt, $E / $dt);
    } elsif ($Opt{type} eq 'aoi') {
	print "building aoi\n";
	my $E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    push @$data, $i;
		}
	    });
	printf("build %.3f sec (%.1f elements/sec)\n", $dt, $E / $dt);
    } elsif ($Opt{type} eq 'aof') {
	print "building aof\n";
	my $E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    push @$data, rand();
		}
	    });
	printf("build %.3f sec (%.1f elements/sec)\n", $dt, $E / $dt);
    } elsif ($Opt{type} eq 'aos') {
	print "building aos\n";
	my $E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    push @$data, rand() . $$;
		}
	    });
	printf("build %.3f sec (%.1f elements/sec)\n", $dt, $E / $dt);
    } elsif ($Opt{type} eq 'hoi') {
	print "building hoi\n";
	my $E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    $data->{$i} = "$i";
		}
	    });
	printf("build %.3f sec (%.1f elements/sec)\n", $dt, $E / $dt);
    } elsif ($Opt{type} eq 'hof') {
	print "building hof\n";
	my $E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    $data->{$i} = rand();
		}
	    });
	printf("build %.3f sec (%.1f elements/sec)\n", $dt, $E / $dt);
    } elsif ($Opt{type} eq 'hos') {
	print "building hos\n";
	my $E = $Opt{elem};
	$dt = timeit(
	    sub {
		for my $i (1..$E) {
		    $data->{$i} = "$i";
		}
	    });
	printf("build %.3f sec (%.1f elements/sec)\n", $dt, $E / $dt);
    } else {
	die "$0: Unexpected type '$Opt{type}'\n";
    }
    if ($Opt{size}) {
	$dt = timeit(sub { $data_size = total_size($data);});
	printf("data size %d bytes (%.1fMB) %.1f sec\n",
	       $data_size, $data_size / MB, $dt);
    }

    my $encoder = Sereal::Encoder->new;

    {
	print "encoding data\n";
	$dt = timeit(
	    sub {
		$blob = $encoder->encode($data);
	    });
	$blob_size = length($blob);
	printf("encode to %d bytes (%.1fMB) %.3f sec (%.1f MB/sec)\n",
	       $blob_size, $blob_size / MB, $dt, $blob_size / (MB * $dt));
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
	printf("wrote %d bytes (%.1f MB) %.3f sec (%.1f MB/sec)\n",
	       $blob_size, $blob_size / MB, $dt, $blob_size / (MB * $dt));
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
    printf("read %d bytes (%.1f MB) %.3f sec (%.1f MB/sec)\n",
	   $blob_size, $blob_size / MB, $dt, $blob_size / (MB * $dt));
}

my $decoder = Sereal::Decoder->new;

{
    print "decoding blob\n";
    my @dt;
    $blob_size = length($blob);
    for my $i (1..$Opt{repeat}) {
	$dt = timeit(sub { $data = $decoder->decode($blob); });
	printf("%d/%d: decode from %d bytes (%.1fM) %.3f sec (%.f MB/sec)\n",
	       $i, $Opt{repeat}, $blob_size, $blob_size / MB,
	       $dt, $blob_size / (MB * $dt));
	push @dt, $dt;
    }
    if (@dt) {
	my $sum = 0;
	for my $t (@dt) {
	    $sum += $t;
	}
	my $avg = $sum / @dt;
	my $sqsum = 0; 
	for my $t (@dt) {
	    $sqsum += ($avg - $t) ** 2;
	}
	my $stddev = sqrt($sqsum / @dt);
	printf("decode avg %.2f sec (%.1f MB/sec) stddev %.2f sec (%.2f)\n",
	       $avg, $blob_size / (MB * $avg), $stddev, $stddev / $avg); 
    }
    if ($Opt{size}) {
	$dt = timeit(sub { $data_size = total_size($data); });
	printf("data size %d bytes (%.1fMB) %.1f sec\n",
	       $data_size, $data_size / MB, $dt);
    }
}

exit(0);
