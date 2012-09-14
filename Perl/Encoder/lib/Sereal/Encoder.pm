package Sereal::Encoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '0.09';

# not for public consumption, just for testing.
my $TestCompat = [qw( 0.08 0.07 0.06 )];
sub _test_compat {return(@$TestCompat, $VERSION)}

use Exporter 'import';
our @EXPORT_OK = qw(encode_sereal);
our %EXPORT_TAGS = (all => \@EXPORT_OK);
# export by default if run from command line
our @EXPORT = ((caller())[1] eq '-e' ? @EXPORT_OK : ());

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

The protocol specification and many other bits of documentation
can be found in the github repository. Right now, the specification is at
L<https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod>,
there is a discussion of the design objectives in
L<https://github.com/Sereal/Sereal/blob/master/README.pod>, and the output
of our benchmarks can be seen at
L<https://github.com/Sereal/Sereal/wiki/Sereal-Comparison-Graphs>.

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

=item snappy

If set, the main payload of the Sereal document will be compressed using
Google's Snappy algorithm. This can yield anywhere from no effect
to significant savings on output size at rather low run time cost.
If in doubt, test with your data whether this helps or not.

The decoder (version 0.04 and up) will know how to handle Snappy-compressed
Sereal documents transparently.

=item snappy_threshold

The size threshold (in bytes) of the uncompressed output below which
snappy compression is not even attempted even if enabled.
Defaults to one kilobyte (1024 bytes). Set to 0 and C<snappy> to enabled
to always compress.

=item croak_on_bless

If this option is set, then the encoder will refuse to serialize blessed
references and throw an exception instead.

This can be important because blessed references can mean executing
a destructor on a remote system or generally executing code based on
data.

=item undef_unknown

If set, unknown/unsupported data structures will be encoded as C<undef>
instead of throwing an exception.

Mutually exclusive with C<stringify_unknown>.
See also C<warn_unknown> below.

=item stringify_unknown

If set, unknown/unsupported data structures will be stringified and
encoded as that string instead of throwing an exception. The
stringification may cause a warning to be emitted by perl.

Mutually exclusive with C<undef_unknown>.
See also C<warn_unknown> below.

=item warn_unknown

If set, any unknown/unsupported data structure encountered will emit a
warning. Only has an effect if C<undef_unknown> or C<stringify_unknown>
are enabled.

=back

The thusly allocated encoder object and its output buffer will be reused
between invocations of C<encode()>, so hold on to it for an efficiency
gain if you plan to serialize multiple similar data structures, but destroy
it if you serialize a single very large data structure just once to free
the memory.

=head1 INSTANCE METHODS

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

The license for the code in this distribution is the following,
with the exceptions listed below:

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

Except portions taken from Marc Lehmann's code for the JSON::XS
module, which is licensed under the same terms as this module.

Also except the code for Snappy compression library, whose license
is reproduced below and which, to the best of our knowledge,
is compatible with this module's license. The license for the
enclosed Snappy code is:

  Copyright 2011, Google Inc.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

    * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following disclaimer
  in the documentation and/or other materials provided with the
  distribution.
    * Neither the name of Google Inc. nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut
