package Sereal::Path::Tie;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;
use Sereal::Path::Iterator;

our $VERSION    = '0.012';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

XSLoader::load(__PACKAGE__, $Sereal::Path::Tie::VERSION);

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Path::Tie - interface to Sereal documents via tied variables

=cut
