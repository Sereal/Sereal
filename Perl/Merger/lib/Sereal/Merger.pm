package Sereal::Merger;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION    = '0.001';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

# not for public consumption, just for testing.
(my $num_version = $VERSION) =~ s/_//;
my $TestCompat = [ map sprintf("%.2f", $_/100), reverse( 300 .. int($num_version * 100) ) ]; # compat with 3.00 to ...
sub _test_compat {return(@$TestCompat, $VERSION)}

XSLoader::load(__PACKAGE__, $Sereal::Merger::VERSION);

1;
