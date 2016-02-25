package Sereal::Splitter;

=head1 NAME

Sereal::Splitter - splits a Sereal blob in chunks of roughly the same size

=head1 SYNOPSIS

  use Sereal::Splitter qw(SRL_ZLIB create_header_data_template);

  my $splitter = Sereal::Splitter->new(
    { input => $data, chunk_size => 1, compress => SRL_ZLIB,
      header_data_template => create_header_data_template(
        { date => time(), elements_count => '__$CNT__' }
      )
    }
  );
  while (defined( my $chunk = $splitter->next_chunk())) {
    # do stuff with $chunk;
  }

=head1 DESCRIPTION

This library implements an efficient way of splitting a Sereal blob into
smaller chunks. Currently, it only works with ArrayRefs Sereal blobs, like
this:

  [ $element_1, $element_2, ..., $element_n ]

In the future, it may also work with HashRefs.


=head1 CONSTRUCTOR

=head2 new

Takes a HashRef with options:

=head3 input

Mandatory, String, the Sereal blob to split

=head3 chunk_size

Mandatory, positive Int, the approximate size of the B<uncompressed> chunk

=head3 compress

Optional, Int, one of SRL_UNCOMPRESSED, SRL_SNAPPY or SRL_ZLIB. These constant
can be exported at use time. If set, indicates how chunks must be compressed.
Defaults to SRL_UNCOMPRESSED.

=head3 header_data_template

Optional, Str, the header_data to inject in each chunk. This header_data can
contain special scalar values, that will be replaced by values. Special scalar values are:

=over

=item '__$CNT__'

This will be replaced by the number of elements that the chunks contains. It
must be encoded as SHORT_BINARY_08. It'll be replaced by a VARINT.

REMARK: In theory, it should be a string of lentgh 11, because varint max size
are 11 bytes. However, the Sereal decoder code, can't cope with varint bigger
than 8 bytes, because of a bug, and even if the varint is forged like
0x8180808080808080808080.

=back

To make things easier, you can use C<create_header_data_template> (see below)
to create it for you.

=head1 METHODS

=head2 next_chunk

returns the next chunk as a String, or Undef if it was the last chunk

=cut

use strict;
use warnings;
use Carp;

our $VERSION     = '0.840';

use constant SRL_UNCOMPRESSED => 0;
use constant SRL_SNAPPY       => 1;
use constant SRL_ZLIB         => 2;

use IO::File;

use Exporter 'import';
our @EXPORT_OK = qw(
  SRL_UNCOMPRESSED
  SRL_SNAPPY
  SRL_ZLIB
  create_header_data_template
);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

=head1 EXPORTED FUNCTIONS

=head2 create_header_data_template

Given a structure, will return a Sereal *body*, that can be used as value for
the C<header_data_template> constructor option.

This function loads C<Sereal::Encoder> if it's not already loaded.

=cut

sub create_header_data_template {
    require Sereal::Encoder;
    require Sereal::Encoder::Constants;
    my ($struct) = @_;
    my $blob = Sereal::Encoder::encode_sereal_with_header_data(1, $struct);


    my $fh = IO::File->new(\$blob, 'r')
      or croak "failed to open blob";
    $fh->binmode(':raw');

    my $length = 0;

    # magic
    $length += $fh->read(my $magic, Sereal::Encoder::Constants::SRL_MAGIC_STRLEN());
    $magic eq Sereal::Encoder::Constants::SRL_MAGIC_STRING() || $magic eq Sereal::Encoder::Constants::SRL_MAGIC_STRING_HIGHBIT()
      or croak "invalid magic";

    # version-type
    $length += $fh->read(my $version_type, 1);

    $blob = substr $blob, $length, -1;
    
    return $blob;
}

sub new {
    my ($class, $args) = @_;
    
    if ( my $header_data_template = $args->{header_data_template} ) {
        my $str_to_replace = chr(0x68) . '__$CNT__';
        if ( (my $where = index($header_data_template, $str_to_replace)) >= 0) {
            my $l = length $str_to_replace;
            my $copy = $header_data_template;
            substr($copy, $where, $l, chr(0x20) . chr(0) x ($l-1) );
            $args = { %{$args},
                      header_data_template => $copy,
                      header_count_idx => $where + 1,
                    };
        }
    }

    $class->new_xs($args);
}

use XSLoader;

XSLoader::load(__PACKAGE__, $Sereal::Splitter::VERSION);

1;
