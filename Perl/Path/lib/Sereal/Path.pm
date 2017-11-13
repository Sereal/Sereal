package Sereal::Path;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;
use Sereal::Path::Iterator;

our $VERSION = '4.004';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

XSLoader::load(__PACKAGE__, $XS_VERSION);

sub _normalize {
    my ($self, $x) = @_;
    #$x =~ s/[\['](\??\(.*?\))[\]']/_callback_01($self,$1)/eg;
    $x =~ s/'?\.'?|\['?/;/g;
    $x =~ s/;;;|;;/;..;/g;
    $x =~ s/;\$|'?\]|'$//g;
    #$x =~ s/#([0-9]+)/_callback_02($self,$1)/eg;
    #$self->{'result'} = [];   # result array was temporarily used as a buffer
    return $x;
}

sub traverse {
    my ($self, $query) = @_;
    my $norm = $self->_normalize($query);
    $norm =~ s/^\$;//;
    my @expr = split(/;/, $norm);
    $self->_traverse(\@expr, '$');
    return $self->results;
}

sub value {
    my ($self, $query) = @_;
    my $values = $self->traverse($query);
    return $values->[0];
}

sub values {
    my ($self, $query) = @_;
    my $values = $self->traverse($query);
    return wantarray ? @$values : scalar @$values;
}

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Path - set of tools to work with serialized Sereal documents

=head1 DESCRIPTION

Sereal::Path consists of following tools:

=over 4

=item L<Sereal::Path::Iterator> - a way to iterate over serialized Sereal
documents and decode them partly. This module is foundation of all others.

=item L<Sereal::Path::Tie> - a tied interface for accessing parts of serialized
document.

=item L<Sereal::Path> - a query language for Sereal. It's similar to XPath or
JSONPath.

=back

The result of this documentation describes Sereal::Path module. For others
please use appropriate link above.

=head1 Sereal::Path

=head2 Synopsis

  use Sereal::Path;
  use Sereal::Encoder qw/encode_sereal/;
  
  my $data = [ { foo => 'bar' }, { foo => 'barbar' } ];
  my $sp = Sereal::Path->new(encode_sereal($data));
  
  # $result will contain { foo => 'bar' }
  my $result = sp->traverse('$[0]');

  # $result1 will contain [ 'bar', 'barbar' ]
  my $result1 = $sp->traverse('$[*][foo]');

=head2 Description

Sereal::Path for Sereal is the same as XPath for XML or JSONPath for JSON.
Sereal::Path can directly work with encoded Sereal document. In other words,
there is no need to decode a document before executing queries against it.

As of first public release of Sereal::Path, it's considered to be in beta phase
and is not ready for production usage.

Sereal::Path derived it query syntax from JSONPath. Its documentation can be
found at L<http://goessner.net/articles/JsonPath/>.

However, Sereal::Path supports a subset of JSONPath. Below is the comparasion
between XPath, JSONPath and Sereal::Path:

  XPath           JSONPath            Sereal::Path            Description
  /               $                   $                       The root object/element
  .               @                   not impl                The current object/element
  /               . or []             . or []                 Child operator
  ..              n/a                 n/a                     Parent operator
  //              ..                  not impl                Recursive descent. JSONPath borrows this syntax from E4X.
  *               *                   *                       Wildcard. All objects/elements regardless their names.
  @               n/a                 n/a                     Attribute access. JSON structures don't have attributes.
  []              []                  []                      Subscript operator. XPath uses it to iterate over element
                                                              collections and for predicates. In Javascript and JSON it
                                                              is the native array operator.
  |               [,]                 [,]                     Union operator in XPath results in a combination of node sets.
                                                              JSONPath allows alternate names or array indices as a set.
  n/a             [start:end:step]    [start:end:step]        Array slice operator borrowed from ES4.
  []              ?()                 not impl                Applies a filter (script) expression.
  n/a             ()                  not impl                Script expression, using the underlying script engine.
  ()              n/a                 n/a                     Grouping in Xpath

Items which are marked as 'not impl' will be implemented at later stages of the project.

=head2 Important

Sereal::Path is still under development. It's possible that API will be change at any moment.

=head1 ACKNOWLEDGMENT

This module was inspired by JSON::Path module by Toby Inkster which is a port
of the PHP JSONPath implementation (version 0.8.x) by Stefan Goessner.
See L<http://code.google.com/p/jsonpath/>.

This module was originally developed for Booking.com.  With approval from
Booking.com, this module was generalized and published on CPAN, for which the
authors would like to express their gratitude.

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
THE SOFTWARE.

=cut
