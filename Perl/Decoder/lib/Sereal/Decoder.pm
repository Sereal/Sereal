package Sereal::Decoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '0.08';
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
encoder. These options are currently valid:

=over 2

=item refuse_snappy

If set, the decoder will refuse Snappy-compressed input data. This can be
desirable for robustness. See the section C<ROBUSTNESS> below.

=back

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

=head2 decode_with_offset

Same as the C<decode> method, except as second parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalar (see C<decode> above)
is relegated to being the third parameter.

=head2 bytes_consumed

After using the C<decode> method, C<bytes_consumed> can return the
number of bytes of the input string that were actually consumed by
the decoder. That is, if you append random garbage to a valid
Sereal document, C<decode> will happily decode the data and ignore the
garbage. If that is an error in your use case, you can use C<bytes_consumed>
to catch it.

  my $out = $decoder->decode($sereal_string);
  if (length($sereal_string) != $decoder->bytes_consumed) {
    die "Not all input data was consumed!";
  }

Chances are that if you do this, you're violating UNIX philosophy
in "be strict in what you emit but lenient in what you accept".

You can also use this to deserialize a list of Sereal documents that
is concatenated into the same string (code not very robust...):

  my @out;
  my $pos = 0;
  eval {
    while (1) {
      push @out, $decoder->decode_with_offset($sereal_string, $pos);
      $pos += $decoder->bytes_consumed;
      last if $pos >= length($sereal_string)
           or not $decoder->bytes_consumed;
    }
  };

=head1 EXPORTABLE FUNCTIONS

=head2 decode_sereal

The functional interface that is equivalent to using C<new> and C<decode>.
Expects a byte string to deserialize as first argument, optionally followed
by a hash reference of options (see documentation for C<new()>). Finally,
C<decode_sereal> supports a third parameter, which is the output scalar
to write to. See the documentation for C<decode> above for details.

The functional interface is marginally slower than the OO interface since
it cannot reuse the decoder object.

=head1 ROBUSTNESS

This implementation of a Sereal decoder tries to be as robust to invalid
input data as reasonably possible. This means that it should never
(though read on) segfault. It may, however, cause a large malloc
to fail. Generally speaking, invalid data should cause a Perl-trappable
exception. The one exception is that for Snappy-compressed Sereal documents,
the Snappy library may cause segmentation faults (invalid reads orwrites).
This should only be a problem if you do not checksum your data (internal
checksum support is a To-Do) or if you accept data from potentially
malicious sources.

It requires a lot of run-time boundary checks to prevent decoder
segmentation faults on invalid data. We implemented them in the
lightest way possible. Adding robustness against running out of memory
would cause an very significant run-time overhead. In most cases of
random garbage (with valid header no less) when a malloc() fails due
to invalid data, the problem was caused by a very large array or
string length. This kind of very large malloc can then fail, being
trappable from Perl. Only when packet causes many repeated allocations
do you risk causing a hard OOM error from the kernel that cannot be
trapped because Perl may require some small allocations to succeed
before the now-invalid memory is released. It is at least not entirely
trivial to craft a Sereal document that causes this behaviour.

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
