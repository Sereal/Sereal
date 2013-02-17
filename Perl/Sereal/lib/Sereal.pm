package Sereal;
use 5.008;
use strict;
use warnings;
our $VERSION;
BEGIN {
    $VERSION = '0.310';
}
use Sereal::Encoder 0.31 qw(encode_sereal);
use Sereal::Decoder 0.31 qw(decode_sereal looks_like_sereal);

use Exporter 'import';
our @EXPORT_OK = qw(encode_sereal decode_sereal looks_like_sereal);
our %EXPORT_TAGS = (all => \@EXPORT_OK);
# export by default if run from command line
our @EXPORT = ((caller())[1] eq '-e' ? @EXPORT_OK : ());

1;

__END__

=encoding utf8

=head1 NAME

Sereal - Fast, compact, powerful binary (de-)serialization

=head1 SYNOPSIS

  use Sereal qw(encode_sereal decode_sereal looks_like_sereal);
  
=head1 DESCRIPTION

B<This is an experimental module.
Before using it in production, please get in touch with the authors!>

I<Sereal> is an efficient, compact-output, binary and feature-rich
serialization protocol. The Perl encoder is implemented as the
L<Sereal::Encoder> module, the Perl decoder correspondingly as 
L<Sereal::Decoder>. They are distributed separately to allow for
safe upgrading without downtime. (Hint: Upgrade the decoder everywhere
first, then the encoder.)

This C<Sereal> module is a very thin wrapper around both C<Sereal::Encoder>
and C<Sereal::Decoder>. It depends on both and loads both. So if you have
a user of both encoder and decoder, it is enough to depend on a particular
version of C<Sereal> and you'll get the most recent released versions
of C<Sereal::Encoder> and C<Sereal::Decoder> whose version is smaller than
or equal to the version of C<Sereal> you depend on.

The protocol specification and many other bits of documentation
can be found in the github repository. Right now, the specification is at
L<https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod>,
there is a discussion of the design objectives in
L<https://github.com/Sereal/Sereal/blob/master/README.pod>, and the output
of our benchmarks can be seen at
L<https://github.com/Sereal/Sereal/wiki/Sereal-Comparison-Graphs>.

=head2 EXPORTED FUNCTIONS

It is recommended to use the object-oriented interface of
C<Sereal::Encoder> and C<Sereal::Decoder> if you care about
performance.

You can optionally import three functions from C<Sereal>.
C<encode_sereal> is the same function as L<Sereal::Encoder>'s
C<encode_sereal> function. C<decode_sereal> and C<looks_like_sereal>
are the same as L<Sereal::Decoder>'s functions of the same names.

After loading the C<Sereal> module, both C<Sereal::Encoder> and
C<Sereal::Decoder> are guaranteed to be loaded, so you can use
their object-oriented interface.

=head1 AUTHOR

Steffen Mueller E<lt>smueller@cpan.orgE<gt>

=head1 ACKNOWLEDGMENT

This module was originally developed for Booking.com.
With approval from Booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012, 2013 by Steffen Mueller

=cut
