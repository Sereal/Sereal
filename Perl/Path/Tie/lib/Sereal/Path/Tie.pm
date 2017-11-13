package Sereal::Path::Tie;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;
use Sereal::Path::Iterator;

our $VERSION = '4.004';
our $XS_VERSION = $VERSION; $VERSION= eval $VERSION;

XSLoader::load(__PACKAGE__, $XS_VERSION);

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Path::Tie - interface to Sereal documents via tied variables

=head1 SYNOPSIS

  use Sereal::Encoder qw/encode_sereal/;
  use Sereal::Path::Iterator;
  use Sereal::Path::Tie;

  my $data = encode_sereal({ foo => 'bar' });
  my $spi = Sereal::Path::Iterator->new($data);
  my $tie = Sereal::Path::Tie->new($spi);
  my $val = $tie->{foo}; # return bar

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
