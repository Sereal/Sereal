package Sereal::Encoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '0.36'; # Don't forget to update the TestCompat set for testing against installed decoders!

# not for public consumption, just for testing.
my $TestCompat = [ map sprintf("%.2f", $_/100), reverse( 23 .. int($VERSION * 100) ) ]; # compat with 0.23 to ...
sub _test_compat {return(@$TestCompat, $VERSION)}

use Exporter 'import';
our @EXPORT_OK = qw(encode_sereal);
our %EXPORT_TAGS = (all => \@EXPORT_OK);
# export by default if run from command line
our @EXPORT = ((caller())[1] eq '-e' ? @EXPORT_OK : ());

sub CLONE_SKIP {1}

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
encoder.

Currently, the following options are recognized, none of them are on
by default.

=head3 no_shared_hashkeys

When the C<no_shared_hashkeys> option is set ot a true value, then
the encoder will disable the detection and elimination of repeated hash
keys. This only has an effect for serializing structures containing hashes.
By skipping the detection of repeated hash keys, performance goes up a bit,
but the size of the output can potentially be much larger.
Do not disable this unless you have a reason to.

=head3 snappy

If set, the main payload of the Sereal document will be compressed using
Google's Snappy algorithm. This can yield anywhere from no effect
to significant savings on output size at rather low run time cost.
If in doubt, test with your data whether this helps or not.

The decoder (version 0.04 and up) will know how to handle Snappy-compressed
Sereal documents transparently.

B<NOTE 1:> Do not use this if you want to parse multiple Sereal packets
from the same buffer. Instead use C<snappy_incr> instead.

=head3 snappy_incr

Enables a version of the snappy protocol which is suitable for incremental
parsing of packets. See also the C<snappy> option above for more details.

=head3 snappy_threshold

The size threshold (in bytes) of the uncompressed output below which
snappy compression is not even attempted even if enabled.
Defaults to one kilobyte (1024 bytes). Set to 0 and C<snappy> to enabled
to always compress.

=head3 croak_on_bless

If this option is set, then the encoder will refuse to serialize blessed
references and throw an exception instead.

This can be important because blessed references can mean executing
a destructor on a remote system or generally executing code based on
data.

See also C<no_bless_objects> to skip the blessing of objects.
When both flags are set, C<croak_on_bless> has a higher precedence then
C<no_bless_objects>.

=head3 no_bless_objects

If this option is set, then the encoder will serialize blessed references
without the bless information and provide plain data structures instead.

See also the C<croak_on_bless> option above for more details.

=head3 undef_unknown

If set, unknown/unsupported data structures will be encoded as C<undef>
instead of throwing an exception.

Mutually exclusive with C<stringify_unknown>.
See also C<warn_unknown> below.

=head3 stringify_unknown

If set, unknown/unsupported data structures will be stringified and
encoded as that string instead of throwing an exception. The
stringification may cause a warning to be emitted by perl.

Mutually exclusive with C<undef_unknown>.
See also C<warn_unknown> below.

=head3 warn_unknown

Only has an effect if C<undef_unknown> or C<stringify_unknown>
are enabled.

If set to a positive integer,
any unknown/unsupported data structure encountered will emit a
warning. If set to a negative integer, it will warn for unsupported
data structures just the same as for a positive value with one
exception: For blessed, unsupported items that have string overloading,
we silently stringify without warning.

=head3 max_recursion_depth

C<Sereal::Encoder> is recursive. If you pass it a Perl data structure
that is deeply nested, it will eventually exhaust the C stack. Therefore,
there is a limit on the depth of recursion that is accepted. It defaults
to 10000 nested calls. You may choose to override this value with the
C<max_recursion_depth> option. Beware that setting it too high can
cause hard crashes, so only do that if you B<KNOW> that it is safe to
do so.

Do note that the setting is somewhat approximate. Setting it to 10000 may break at
somewhere between 9997 and 10003 nested structures depending on their types.

=head3 sort_keys

Normally C<Sereal::Encoder> will output hashes in whatever order is convenient,
generally that used by perl to actually store the hash, or whatever order
was returned by a tied hash.

If this option is enabled then the Encoder will sort the keys before outputting
them. It uses more memory, and is quite a bit slower than the default.

Generally speaking this should mean that a hash and a copy should produce the
same output. Nevertheless the user is warned that Perl has a way of "morphing"
variables on use, and some of its rules are a little arcane (for instance utf8
keys), and so two hashes that might appear to be the same might still produce
different output as far as Sereal is concerned.

The thusly allocated encoder object and its output buffer will be reused
between invocations of C<encode()>, so hold on to it for an efficiency
gain if you plan to serialize multiple similar data structures, but destroy
it if you serialize a single very large data structure just once to free
the memory.

See L</NON-CANONICAL> for why you might want to use this, and for the
various caveats involved.

=head3 dedupe_strings

If this is option is enabled/true then Sereal will use a hash to encode duplicates
of strings during serialization efficiently using (internal) backreferences. This
has a peformance and memory penalty during encoding so it defaults to off.
On the other hand, data structures with many duplicated strings will see a
significant reduction in the size of the encoded form. Currently only strings
longer than 3 characters will be deduped, however this may change in the future.

Note that Sereal will perform certain types of deduping automatically even
without this option. In particular class names and hash keys are deduped
regardless of this option. Only enable this if you have good reason to
believe that there are many duplicated strings as values in your data
structure.

Use of this option does not require an upgraded decoder. The deduping
is performed in such a way that older decoders should handle it just fine.
In other words, the output of a Sereal B<decoder> should not depend on
whether this option was used during B<encoding>. See also below:
I<aliased_dedupe_strings>.

=head3 aliased_dedupe_strings

This is an advanced option that should be used only after fully understanding
its ramifications.

This option enables a mode of operation that is similar to I<dedupe_strings>
and if both options are set, I<aliased_dedupe_strings> takes precedence.

The behaviour of I<aliased_dedupe_strings> differs from I<dedupe_strings>
in that the duplicate occurrances of strings are emitted as Perl language
level B<aliases> instead of as Sereal-internal backreferences. This means
that using this option actually produces a different output data structure
when decoding. The upshot is that with this option, the application
using (decoding) the data may save a lot of memory in some situations
but at the cost of potential action at a distance due to the aliasing.

Beware: The test suite currently does not cover this option as well as it
probably should. Patches welcome.

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

=head1 THREAD-SAFETY

C<Sereal::Encoder> is thread-safe on Perl's 5.8.7 and higher. This means
"thread-safe" in the sense that if you create a new thread, all
C<Sereal::Encoder> objects will become a reference to undef in the new
thread. This might change in a future release to become a full clone
of the encoder object.

=head1 NON-CANONICAL 

You might want to compare two data structures by comparing their serialized
byte strings.  For that to work reliably the serialization must take extra
steps to ensure that identical data structures are encoded into identical
serialized byte strings (a so-called "canonical representation").

Currently the Sereal encoder I<does not> provide a mode that will reliably
generate a canonical representation of a data structure. The reasons are many
and sometimes subtle.

Sereal does support some use-cases however. In this section we attempt to outline
the issues well enough for you to decide if it is suitable for your needs.

=over 4

=item Sereal doesn't order the hash keys by default.

This can be enabled via C<sort_keys>, see above.

=item There are multiple valid Sereal documents that you can produce for the same Perl data structure.

Just L<sorting hash keys|/sort_keys> is not enough. A trivial example is PAD bytes which
mean nothing and are skipped. They mostly exist for encoder optimizations to
prevent certain nasty backtracking situations from becoming O(n) at the cost of
one byte of output. An explicit canonical mode would have to outlaw them (or
add more of them) and thus require a much more complicated implementation of
refcount/weakref handing in the encoder while at the same time causing some
operations to go from O(1) to a full memcpy of everything after the point of
where we backtracked to. Nasty.

Another example is COPY. The COPY tag indicates that the next element is an
identical copy of a previous element (which is itself forbidden from including
COPY's other than for class names). COPY is purely internal. The Perl/XS
implementation uses it to share hash keys and class names. One could use it for
other strings (theoretically), but doesn't for time-efficiency reasons. We'd
have to outlaw the use of this (significant) optimization of canonicalization.

Sereal represents a reference to an array as a sequence of
tags which, in its simplest form, reads I<REF, ARRAY $array_length TAG1 TAG2 ...>.
The separation of "REF" and "ARRAY" is necessary to properly implement all of
Perl's referencing and aliasing semantics correctly. Quite frequently, however,
your array is only reference once and plainly so. If it's also at most 15 elements
long, Sereal optimizes all of the "REF" and "ARRAY" tags, as well as the length
into a special one byte ARRAYREF tag. This is a very significant optimization
for common cases. This, however, does mean that most arrays up to 15 elements
could be represented in two different, yet perfectly valid forms. ARRAYREF would
have to be outlawed for a properly canonical form. The exact same logic
applies to HASH vs. HASHREF.

Similar to how Sereal can represent arrays and hashes in a full and a compact
form. For small integers (between -16 and +15 inclusive), Sereal emits only
one byte including the encoding of the type of data. For larger integers,
it can use either varints (positive only) or zigzag encoding, which can also
represent negative numbers. For a canonical mode, the space optimizations
would have to be turned off and it would have to be explicitly specified
whether varint or zigzag encoding is to be used for encoding positive
integers.

Perl may choose to retain multiple representations of a scalar. Specifically,
it can convert integers, floating point numbers, and strings on the fly and
will aggressively cache the results. Normally, it remembers which of the
representations can be considered canonical, that means, which can be used
to recreate the others reliably. For example, C<0> and C<"0">
can both be considered canonical since they naturally transform into each
other. Beyond intrinsic ambiguity, there are ways to
trick Perl into allowing a single scalar to have distinct string, integer,
and floating point representations that are all flagged as canonical, but can't
be transformed into each other. These are the so-called dualvars. Sereal
cannot represent dualvars (and that's a good thing).

Floating point values can appear to be the same but serialize to different byte
strings due to insignificant 'noise' in the floating point representation. Sereal
supports different floating point precisions and will generally choose the most
compact that can represent your floating point number correctly.

These issues are especially relevant when considering language interoperability.

=back

Often, people don't actually care about "canonical" in the strict sense
required for real I<identity> checking. They just require a best-effort sort of
thing for caching. But it's a slippery slope!

In a nutshell, the C<sort_keys> option may be sufficient for an application
which is simply serializing a cache key, and thus there's little harm in an
occasional false-negative, but think carefully before applying Sereal in other
use-cases.

=head1 AUTHOR

Yves Orton E<lt>demerphq@gmail.comE<gt>

Damian Gryski

Steffen Mueller E<lt>smueller@cpan.orgE<gt>

Rafaël Garcia-Suarez

Ævar Arnfjörð Bjarmason E<lt>avar@cpan.orgE<gt>

Tim Bunce

Daniel Dragan E<lt>bulkdd@cpan.orgE<gt> (Windows support and bugfixes)

Some inspiration and code was taken from Marc Lehmann's
excellent L<JSON::XS> module due to obvious overlap in
problem domain. Thank you!

=head1 ACKNOWLEDGMENT

This module was originally developed for Booking.com.
With approval from Booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012, 2013 by Steffen Mueller
Copyright (C) 2012, 2013 by Yves Orton

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
