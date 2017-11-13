package Sereal::Path::Iterator;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '4.004';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

# TODO autogenerate constants from srl_iterator.h
use constant {
    SRL_INFO_ROOT    => (1  << 8),
    SRL_INFO_REF     => (2  << 8),
    SRL_INFO_HASH    => (4  << 8),
    SRL_INFO_ARRAY   => (8  << 8),
    SRL_INFO_REGEXP  => (16 << 8),
    SRL_INFO_SCALAR  => (32 << 8),
    SRL_INFO_BLESSED => (1  << 16),
    SRL_INFO_REF_TO  => (2  << 16),
};

use Exporter 'import';
our @EXPORT = qw(
    SRL_INFO_ROOT
    SRL_INFO_REF
    SRL_INFO_HASH
    SRL_INFO_ARRAY
    SRL_INFO_REGEXP
    SRL_INFO_SCALAR
    SRL_INFO_BLESSED
    SRL_INFO_REF_TO
);

our %EXPORT_TAGS = (all => \@EXPORT);

sub CLONE_SKIP {1}

XSLoader::load(__PACKAGE__, $XS_VERSION);

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Path::Iterator - iterate and partly deserialize Sereal documents

=head1 SYNOPSIS

  use Sereal::Encoder qw/encode_sereal/;
  use Sereal::Path::Iterator;

  my $data = encode_sereal(
      [
        { foo => 'far' },
        { bar => 'foo' },
      ]
  );

  my $spi = Sereal::Path::Iterator->new($data);
  $spi->step_in();          # step inside array
  $spi->next();             # step over first hashref
  my $r = $spi->decode();   # decode second hashref

=head1 DESCRIPTION

Sereal::Path::Iterator is a way to iterate over serialized Sereal documents and
decode them partly. For example, given a serialized array of 10 elements you
can deserialize only 3rd one. Or, given huge hash, you can decode only key
"foo" without touching others.

Sereal::Path::Iterator has internal state (or pointer/position/offset) which
reflects current position in parsed document. By calling C<step_in>,
C<step_out>, C<next>, C<rewind> or other function describe below you can change
the state and move forward or backward through the document.

Apart of maintaining position, Sereal::Path::Iterator also has a stack where
each new element on the stack represent new level in the document. For
instance, in following example the arrayref is located at depth 0, hashref is
at depth 1 and strings are at depth 2.

  [
    {
        foo => 'bar'
    }
  ]

=head1 METHODS

=head2 new

This method creates new iterator object. It optionally accepts serialized
Sereal document as first argument and uses C<set> to set it.

  my $spi = Sereal::Path::Iterator->new(encode_sereal({}));

=head2 set

As alternative to passing serialized document to C<new> you can call this
function to achieve same result.

  my $spi = Sereal::Path::Iterator->new();
  $spi->set(encode_sereal({}));

The current position is set to be very first top level element of parse
document.

=head2 reset

This method resets iterator's internal state. The result will be equal to
calling C<set> function but without possible overheads on decompressing
document.

  my $spi = Sereal::Path::Iterator->new(encode_sereal({}));
  # some actions here ...
  $spi->reset(); # return iterator to pre-actions state

=head2 eof

C<eof> returns true value if end of a document is reached and false value
otherwise.

=head2 info

C<info> inspects serialized object at current position and returns information
about it. It expects no input arguments and returns arrays. The content of
returned array depends on nature of inspected object. But it contains at least
one element - type.

=over 4

=item 1) Type - an integer with some bits set encoding object's information.
L<Sereal::Path::Iterator> exports following constants to works with the bitset:

=over 8

=item SRL_INFO_REF        object is a reference

=item SRL_INFO_HASH       object is a hash

=item SRL_INFO_ARRAY      object is an array

=item SRL_INFO_SCALAR     object is scalar

=item SRL_INFO_REGEXP     object is regexp

=item SRL_INFO_BLESSED    object is blessed

=item SRL_INFO_REF_TO     object is a reference to something

=back

Examples:

    Serialized object           Type value
    ____________________________________________________________
    'string'                    SRL_INFO_SCALAR
    []                          SRL_INFO_REF_TO | SRL_INFO_ARRAY
    {}                          SRL_INFO_REF_TO | SRL_INFO_HASH
    \'string'                   SRL_INFO_REF_TO | SRL_INFO_SCALAR
    \\'string'                  SRL_INFO_REF_TO | SRL_INFO_REF
    \[]                         SRL_INFO_REF_TO | SRL_INFO_REF
    bless [], "Foo"             SRL_INFO_REF_TO | SRL_INFO_HASH | SRL_INFO_BLESSED

=item 2) Length. If type has SRL_INFO_REF_TO bit set the second element in
array reflects length underlying array or hash. If underlying object is also a
reference the value is 1.

Examples:

    Serialized object           Length
    ____________________________________________________________
    [] or {}                    0
    [ 1, 2, 3 ]                 3
    { foo => 'bar' }            1
    bless [ 'test' ], "Foo"     1

Note that due to the way Sereal encodes hashes actual amount of elements
inside a hash will be twice more than what length value tell you. This is
because each key and value in the hash is represented as two object. One for
key (which is always at even index) and one for value (which is always at odd
index).

=item 3) Class name. If type has SRL_INFO_BLESSED bit set than third element in
returned array contains classname of blesses object.

Examples:

    Serialized object           Class name
    ____________________________________________________________
    bless [ 'test' ], "Foo"     Foo

=back

=head2 step_in

C<step_in> function has two cases:

=over 4

=item 1) a reference object is being parsed (SRL_INFO_REF_TO). In this case the
function step inside the object and let caller inspect its content. You can
think about C<step_in> as dereferencing the reference. The function increment
iterator's stack depth by one.

=item 2) in other cases function acts similarly to C<next>.

=back

C<step_in> has single optional input argument. It's number of steps to do.
Default value is 1.

=head2 step_out

This function is opposite to C<step_in>. Assuming that current stack's depth is
N, the function moves current position forward until next element at level N-1
is reached. Consider following:

    [
        { foo => 'bar' },
        100,
    ]

If current position of iterator is any of values inside hashref, a call to
C<step_out> makes iterator be at value 100.

C<step_out>, similarly to C<step_in>, accepts single optional input argument which
is number of steps to do. Default value is 1.

=head2 next

C<next> does stepping over current element without investigating its content.
Its main use case is to skip complex data structures. For instance, given

    [
        [ array of 1K elements ],
        { bar => 'foo' },
    ]

a sequence of single C<step_in> and a call to C<next> would result into
iterator be at the hashref (but not inside it).

The function guarantees to remain on same stack depth.

C<next> also accepts single optional input argument which is number of steps to
do. Default value is 1.

=head2 rewind

This function rewinds current position to first element at current depth. For example:

    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

if current position is at element 5 of the array, a call to C<rewind> brings it
back to 1.

C<rewind> accepts single optional argument. If a position integer is passed than
C<rewind> also act similarly to C<step_out>. However, it doesn't move current position
forward but rather backward.

=head2 array_goto

C<array_goto> accepts single argument being index of element to go at current
depth. If such element exists, the function moves current position to given
index. C<array_goto> rewinds stack if necessary. The function croaks if given
index is outside of array boundaries.

=head2 array_exists

C<array_exists> returns non-negative value if given index exists and -1
otherwise.

=head2 hash_exists

C<hash_exists> returns non-negative value if given hash key exists and -1
otherwise. If key exists, the functions stops at key's value.

Note that C<hash_exists> does search by linearly scanning entire hash until
either key is found or end of hash is reached. It's an O(n) operation.

=head2 hash_key

C<hash_key> assumes that current iterator's position is a hash key. If so, the
function deserializes and returns the key.

=head2 stack_depth

C<stack_depth> returns current stack's depth.

=head2 stack_index

C<stack_index> returns current index of an element at current depth.

=head2 stack_length

C<stack_length> returns total amount of elements at current depth. Please note
that value returned by this function does not always match with length returned
by C<info>. In particular, length for hashes will be twice bigger.

=head2 decode

C<decode> decodes object at current position. Also check L<KNOWN ISSUES>.

=head2 decode_and_next

C<decode_and_next> is experimental method combining C<decode> and C<next> in
one call. Internal optimizations let's this method avoiding double parsing
which happens if one uses C<decode> followed by C<next>.

=head1 KNOWN ISSUES

=over 4

=item C<decode> do not bless decoded values

=item C<decode> do not weaken decoded values

=item C<decode> do not alias decoded values

=back

=head1 AUTHOR

Ivan Kruglov <ivan.kruglov@yahoo.com>

=head1 CONTRIBUTORS

Roman Studenikin <roman.studenikin@booking.com>

Steven Lee <stevenwh.lee@gmail.com>

Gonzalo Diethem <gonzalo.diethelm@gmail.com>

=head1 COPYRIGHT AND LICENCE

Copyright 2014-2017 Ivan Kruglov.

This module is tri-licensed. It is available under the X11 (a.k.a. MIT)
licence; you can also redistribute it and/or modify it under the same
terms as Perl itself.

=head2 a.k.a. "The MIT Licence"

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE

=cut

