package Sereal::Splitter;

=head1 NAME

Sereal::Splitter - splits a Sereal blob in chunks of roughly the same size

=cut

use strict;
use warnings;

our $VERSION     = '0.001';

use XSLoader;

XSLoader::load(__PACKAGE__, $Sereal::Splitter::VERSION);

1;
