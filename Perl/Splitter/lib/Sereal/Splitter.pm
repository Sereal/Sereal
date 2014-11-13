package Sereal::Splitter;

=head1 NAME

Sereal::Splitter - splits a Sereal blob in chunks of roughly the same size

=head1 SYNOPSIS

  use Sereal::Splitter qw(SRL_ZLIB);

  my $splitter = Sereal::Splitter->new(
    { input => $data, chunk_size => 1, compress => SRL_ZLIB }
  );
  while (defined( my $chunk = $splitter->next_chunk())) {
    # do stuff with $chunk;
  }

=head1 DESCRIPTION

This library implements an efficient way of splitting a Sereal blob into
smaller chunks.

=head1 CONSTRUCTOR

=head2 new

Takes a HashRef with options:

=head3 input

Mandatory, String, the Sereal blob to split

=head3 chunk_size

Mandatory, positive Int, the approximate size of the B<uncompressed> chunk

=head3 compress

Optional, Int, one of SRL_UNCOMPRESSED, SRL_SNAPPY or SRL_ZLIB. These constant
can be exported at use time.

=head1 METHODS

=head2 next_chunk

returns the next chunk as a String, or Undef if it was the last chunk

=cut

use strict;
use warnings;

our $VERSION     = '0.500';

use constant SRL_UNCOMPRESSED => 0;
use constant SRL_SNAPPY       => 1;
use constant SRL_ZLIB         => 2;

use Exporter 'import';
our @EXPORT_OK = qw(
  SRL_UNCOMPRESSED
  SRL_SNAPPY
  SRL_ZLIB
);
our %EXPORT_TAGS = (all => \@EXPORT_OK);


use XSLoader;

XSLoader::load(__PACKAGE__, $Sereal::Splitter::VERSION);

1;
