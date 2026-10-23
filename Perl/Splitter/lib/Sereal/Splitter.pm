package Sereal::Splitter;

=head1 NAME

Sereal::Splitter - splits a Sereal blob in chunks of roughly the same size

=head1 SYNOPSIS

  use Sereal::Splitter qw(SRL_ZSTD create_header_data_template);

  my $splitter = Sereal::Splitter->new(
    { input => $data, chunk_size => 1, compress => SRL_ZSTD,
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

The input may use any Sereal protocol version up to the current one, and may
be compressed with Snappy, Zlib or Zstd. Each chunk has the protocol version
of the input (at least 3), since chunk bodies are copied from it.


=head1 CONSTRUCTOR

=head2 new

Takes a HashRef with options:

=head3 input

Mandatory, String, the Sereal blob to split

=head3 chunk_size

Mandatory, positive Int, the approximate size of the B<uncompressed> chunk

=head3 compress

Optional, Int, one of SRL_UNCOMPRESSED, SRL_SNAPPY, SRL_ZLIB or SRL_ZSTD. These
constants can be exported at use time. If set, indicates how chunks must be
compressed. Defaults to SRL_UNCOMPRESSED. A chunk that does not get smaller is
left uncompressed. Zstd compression makes chunks protocol version 4 at least.

=head3 compress_level

Optional, Int, the compression level. Defaults to the zlib default for
SRL_ZLIB and to 3 for SRL_ZSTD, like L<Sereal::Encoder>.

=head3 max_recursion_depth

Optional, positive Int, how deeply nested an element may be, counting the
references that the splitter has to follow. Defaults to 10000; deeper input
is rejected.

=head3 checksum_policy, checksum_v0_key, checksum_v1_key, checksum_v2_key

Optional. These control how a checksummed (protocol version 6) input is
authenticated. They take the same values and have the same defaults as the
L<Sereal::Decoder> options of the same names, including
C<$Sereal::DefaultKey> and the C<SEREAL_CHECKSUM_POLICY> environment variable.
Chunks carry no checksum.

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
use constant SRL_ZSTD         => 3;

use IO::File;

use Exporter 'import';
our @EXPORT_OK = qw(
  SRL_UNCOMPRESSED
  SRL_SNAPPY
  SRL_ZLIB
  SRL_ZSTD
  create_header_data_template
);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

=head1 EXPORTED FUNCTIONS

=head2 create_header_data_template

Given a structure, will return a Sereal *body*, that can be used as value for
the C<header_data_template> constructor option.

An optional second argument is the protocol version to encode the header
with. A chunk has the protocol version of the input it was split from, so
the header must not use tags that are newer than that: pass the input's
protocol version if the input is older than the current protocol, and the
header holds values (such as integers below -16, or v-strings) that newer
protocol versions encode differently.

This function loads C<Sereal::Encoder> if it's not already loaded.

=cut

sub create_header_data_template {
    require Sereal::Encoder;
    require Sereal::Encoder::Constants;
    my ($struct, $protocol_version) = @_;
    my $blob = Sereal::Encoder::encode_sereal_with_header_data(1, $struct,
        defined $protocol_version ? { protocol_version => $protocol_version } : ());


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
