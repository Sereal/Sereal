package Sereal::Merger;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION    = '0.001';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

# not for public consumption, just for testing.
(my $num_version = $VERSION) =~ s/_//;
my $TestCompat = [ map sprintf("%.2f", $_/100), reverse( 300 .. int($num_version * 100) ) ]; # compat with 3.00 to ...
sub _test_compat {return(@$TestCompat, $VERSION)}

# Make sure these constants in sync with C code.
use constant SRL_TOP_LEVEL_SCALAR => 0;
use constant SRL_TOP_LEVEL_ARRAY  => 1;
use constant SRL_TOP_LEVEL_HASH   => 2;

use Exporter 'import';
our @EXPORT_OK = qw(
    SRL_TOP_LEVEL_SCALAR
    SRL_TOP_LEVEL_ARRAY
    SRL_TOP_LEVEL_HASH
);

our %EXPORT_TAGS = (all => \@EXPORT_OK);
# export by default if run from command line
our @EXPORT = ((caller())[1] eq '-e' ? @EXPORT_OK : ());

sub CLONE_SKIP {1}

XSLoader::load(__PACKAGE__, $Sereal::Merger::VERSION);

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Merger - A tool to merge Sereal documents into single one

=head1 SYNOPSIS

    use Sereal::Merger;

    my $merger = Sereal::Merger->new({...options...});

    $merger->append($sereal_document);
    # or
    $merger->append_all(\@sereal_documents);

    my $out = $merger->finish();

=head1 DESCRIPTION

This library provides a way of merging multiple Sereal documents into one.

The Sereal protocol version emitted by merger is currently protocol version 3
by default.

=head1 CLASS METHODS

=head2 new

Constructor. Optionally takes a hash reference as first parameter. This hash
reference may contain any number of options that influence the behaviour of the
encoder.

Currently, the following options are recognized, none of them are on
by default.

=head3 compress

If this option provided and true, compression of the document body is enabled.
As of Sereal version 3, two different compression techniques are supported
and can be enabled by setting C<compress> to the respective named
constants (exportable from the C<Sereal::Merger> module):
Snappy (named constant: C<SRL_SNAPPY>),
and Zlib (C<SRL_ZLIB>).
For your convenience, there is also a C<SRL_UNCOMPRESSED>
constant.

=head3 max_recursion_depth

C<Sereal::Merger> is recursive. If you pass it a Perl data structure
that is deeply nested, it will eventually exhaust the C stack. Therefore,
there is a limit on the depth of recursion that is accepted. It defaults
to 10000 nested calls. You may choose to override this value with the
C<max_recursion_depth> option. Beware that setting it too high can
cause hard crashes, so only do that if you B<KNOW> that it is safe to
do so.

Do note that the setting is somewhat approximate. Setting it to 10000 may break at
somewhere between 9997 and 10003 nested structures depending on their types.

=head3 dedupe_strings

If this is option is enabled/true then Sereal will use a hash to encode
duplicates of strings during merging. This has a performance and memory penalty
during merging so it defaults to off.  On the other hand, data structures with
many duplicated strings will see a significant reduction in the size of the
encoded form. Currently only strings longer than 3 characters will be deduped,
however this may change in the future.

=head3 protocol_version

Specifies the version of the Sereal protocol to emit. Valid are integers
between 1 and the current version. If not specified, the most recent protocol
version will be used.

=head3 top_level_element

This option specifies what objects will be used as top level container for merged documents. There are three available options:

=over 4

SCALAR - not really a coninter. Mostly used for testing purposes.
Exported as SRL_TOP_LEVEL_SCALAR.

=back

=over 4

ARRAY - default option. The result of merging is eqvivalent to:

    my @data = (...documents...);
    my $encoded = encoder_sereal([ @data ]);

Exported as SRL_TOP_LEVEL_ARRAY.

=back

=head1 INSTANCE METHODS

=head2 append

Merge provided Sereal document. If the document is invalid, the method throws
exception. However, it's garanteed that the result of previos merging operation
will not be affected.

=head2 append_all

Same as C<append>, but expects ArrayRef as input. If ArrayRef contains invalid
Sereal document an excetion is thrown. However, it's garanteed that
C<Sereal::Merger> preserves consistent internal state. Merging operation can be
continued. The index where merging failed to be obtained via
C<elements_merged>.

=head2 finish

Finalize merging operation. The output of this function is valid Sereal document.

=head2 elements_merged

Return number of merged documents.

=head1 BUGS, CONTACT AND SUPPORT

For reporting bugs, please use the github bug tracker at
L<http://github.com/Sereal/Sereal/issues>.

For support and discussion of Sereal, there are two Google Groups:

Announcements around Sereal (extremely low volume):
L<https://groups.google.com/forum/?fromgroups#!forum/sereal-announce>

Sereal development list:
L<https://groups.google.com/forum/?fromgroups#!forum/sereal-dev>

=head1 AUTHORS AND CONTRIBUTORS

Ivan Kruglov E<lt>ivan.kruglov@yahoo.comE<gt>

Yves Orton E<lt>demerphq@gmail.comE<gt>

Damian Gryski

Steffen Mueller E<lt>smueller@cpan.orgE<gt>

Rafaël Garcia-Suarez

Damien Krotkine

=head1 ACKNOWLEDGMENT

This module was originally developed for Booking.com.
With approval from Booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2014, 2015 by Ivan Kruglov

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
