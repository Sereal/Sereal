package Sereal::Path::Iterator;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION    = '0.001';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

XSLoader::load(__PACKAGE__, $Sereal::Path::Iterator::VERSION);

1;
