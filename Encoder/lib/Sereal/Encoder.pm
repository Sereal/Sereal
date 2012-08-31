package Sereal::Encoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '0.01';
use Exporter 'import';
our @EXPORT_OK = qw(encode_sereal);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

XSLoader::load('Sereal::Encoder', $VERSION);

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Encoder - Fast, compact, powerful binary serialization

=head1 SYNOPSIS

  use Sereal::Encoder qw(encode_sereal);
  
  my $encoder = Sereal::Encoder->new({...options...});
  my $out = $encoder->encode($structure);
  # alternatively:
  $out = encode_sereal($structure, {... options ...});

=head1 DESCRIPTION

B<This is an experimental module. The interface may change without notice.
Before using it in production, please get in touch with the authors!>

This library implements an efficient, compact-output, and feature-rich
serializer using a binary protocol called I<Sereal>.
Its sister module L<Sereal::Decoder> implements a decoder for this format.
The two are released separately to allow for independent and safer upgrading.

The Sereal protocol version emitted by this encoder implementation is currently
protocol version 1.

Right now, the protocol specification can be found in the F<srl_protocol.h>
header file within this distribution. The specification might be moved to
documentation at a later date.

=head1 CLASS METHODS

=head2 new

Constructor. Optionally takes a hash reference as first parameter. This hash
reference may contain any number of options that influence the behaviour of the
encoder. Currently, the following options are recognized:

=over 2

=item no_shared_hashkeys

When the C<no_shared_hashkeys> option is set ot a true value, then
the encoder will disable the detection and elimination of repeated hash
keys. This only has an effect for serializing structures containing hashes.
By skipping the detection of repeated hash keys, performance goes up a bit,
but the size of the output can potentially be much larger.
Do not disable this unless you have a reason to.

=back

The thusly allocated encoder object and its output buffer will be reused
between invocations of C<encode()>, so hold on to it for an efficiency
gain if you plan to serialize multiple similar data structures, but destroy
it if you serialize a single very large data structure just once to free
the memory.

=head1 INSTANCE METHOD

=head2 encode

Given a Perl data structure, serializes that data structure and returns a
binary string that can be turned back into the original data structure by
L<Sereal::Decoder>.

=head1 EXPORTABLE FUNCTIONS

=head2 encode_sereal

The functional interface that is equivalent to using C<new> and C<encode>.
Expects a data structure to serialize as first argument, optionally followed
by a hash reference of options (see documentation for C<new()>).

The functional interface is marginally slower than the OO interface since
it cannot reuse the encoder object.

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

This module was originally developed for booking.com.
With approval from booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 by Steffen Mueller

Except portions taken from Marc Lehmann's code for the JSON::XS
module. The license for JSON::XS is the same as for this module:

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
