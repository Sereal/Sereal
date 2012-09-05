package Sereal::Decoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '0.03';
use Exporter 'import';

our @EXPORT_OK = qw(decode_sereal);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

XSLoader::load('Sereal::Decoder', $VERSION);

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Decoder - Fast, compact, powerful binary deserialization

=head1 SYNOPSIS

  use Sereal::Decoder qw(decode_sereal);
  
  my $decoder = Sereal::Decoder->new({...options...});
  
  my $structure;
  $decoder->decode($blob, $structure); # deserializes into $structure
  
  # or if you don't have references to the top level structure, this works, too:
  $structure = $decoder->decode($blob);
  
  # alternatively functional interface:
  decode_sereal($blob, {... options ...}, $structure);
  $structure = decode_sereal($blob, {... options ...});

=head1 DESCRIPTION

B<This is an experimental module. The interface may change without notice.
Before using it in production, please get in touch with the authors!>

This library implements a deserializer for an efficient, compact-output,
and feature-rich binary protocol called I<Sereal>.
Its sister module L<Sereal::Encoder> implements an encoder for this format.
The two are released separately to allow for independent and safer upgrading.

The Sereal protocol version that is compatible with this decoder implementation
is currently protocol version 1. As it stands, it will refuse to attempt to
decode future versions of the protocol, but there is likely going to be an
option to decode the parts of the input that are compatible with version 1
of the protocol. The protocol was designed to allow for this.

Right now, the protocol specification can be found in the F<srl_protocol.h>
header file within this distribution. The specification might be moved to
documentation at a later date.

=head1 CLASS METHODS

=head2 new

Constructor. Optionally takes a hash reference as first parameter. This hash
reference may contain any number of options that influence the behaviour of the
encoder. Currently, no such options are defined, but will be in a later release.

=head1 INSTANCE METHODS

=head2 decode

Given a byte string of Sereal data, the C<decode> call derializes that data
structure. The result can be obtained in one of two ways: C<decode> accepts
a second parameter, which is a scalar to write the result to, AND C<decode>
will return the resulting data structure.

The two are subtly different in case of data structures that contain
references to the root element. In that case, the return value will be
a (non-recursive) copy of the reference. The pass-in style is more correct.
In other words,

  $decoder->decode($sereal_string, my $out);
  # is almost the same but safer than:
  my $out = $decoder->decode($sereal_string);

This is an unfortunate side-effect of perls standard copy semantics of
assignment. Possibly one day we will have an alternative to this.

=head1 EXPORTABLE FUNCTIONS

=head2 decode_sereal

The functional interface that is equivalent to using C<new> and C<decode>.
Expects a byte string to deserialize as first argument, optionally followed
by a hash reference of options (see documentation for C<new()>). Finally,
C<decode_sereal> supports a third parameter, which is the output scalar
to write to. See the documentation for C<decode> above for details.

The functional interface is marginally slower than the OO interface since
it cannot reuse the decoder object.

=head1 PERFORMANCE

The exact performance in time and space depends heavily on the data structure
to be serialized. For ready-made comparison scripts, see the
F<author_tools/bench.pl> and F<author_tools/dbench.pl> programs that are part
of this distribution. Suffice to say that this library is easily competitive
in both time and space efficiency with the best alternatives.

=head1 AUTHOR

Yves Orton E<lt>demerphq@gmail.comE<gt>

Damian Gryski

Steffen Mueller E<lt>smueller@cpan.orgE<gt>

Rafaël Garcia-Suarez

Ævar Arnfjörð Bjarmason

Some inspiration and code was taken from Marc Lehmann's
excellent JSON::XS module due to obvious overlap in
problem domain.

=head1 ACKNOWLEDGMENT

This module was originally developed for Booking.com.
With approval from Booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 by Steffen Mueller
Copyright (C) 2012 by Yves Orton

Excluding portions taken from Marc Lehmann's code for the JSON::XS
module. The license for JSON::XS is the same as for this module:

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
