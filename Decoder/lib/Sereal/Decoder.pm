package Sereal::Decoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION = '0.01';
use Exporter 'import';

our @EXPORT_OK = qw(decode_sereal);
our %EXPORT_TAGS = (all => \@EXPORT_OK);

XSLoader::load('Sereal::Decoder', $VERSION);

1;

__END__

=head1 NAME

Sereal::Decoder - Module to decode Sereal data structures.

=head1 SYNOPSIS

TODO

=head1 DESCRIPTION

B<This is an experimental module. The interface may change without notice.
Before using it in production, please get in touch with the authors!>

TODO

=head1 AUTHOR

Yves Orton E<lt>demerphq@gmail.comE<gt>

Damian Gryski

Steffen Mueller E<lt>smueller@cpan.orgE<gt>

Some inspiration and code was taken from Marc Lehmann's
excellent JSON::XS module due to obvious overlap in
problem domain.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 by Steffen Mueller

Except portions taken from Marc Lehmann's code for the JSON::XS
module. The license for JSON::XS is the same as for this module:

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
