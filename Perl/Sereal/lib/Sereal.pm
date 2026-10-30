package Sereal;
use 5.014;
use strict;
use warnings;
our $VERSION    = '6.001';
our $XS_VERSION = $VERSION; $VERSION = eval $VERSION;
use Sereal::Encoder 6.001 qw(
    encode_sereal
    sereal_encode_with_object
    SRL_UNCOMPRESSED
    SRL_SNAPPY
    SRL_ZLIB
    SRL_ZSTD
);
use Sereal::Decoder 6.001 qw(
    get_sereal_checksum
    validate_sereal_checksum
    decode_sereal
    looks_like_sereal
    decode_sereal_with_header_data
    scalar_looks_like_sereal
    sereal_decode_with_object
    sereal_decode_with_header_with_object
    sereal_decode_only_header_with_object
    sereal_decode_only_header_with_offset_with_object
    sereal_decode_with_header_and_offset_with_object
    sereal_decode_with_offset_with_object
);

use Exporter 'import';
our @EXPORT_OK = qw(
    get_sereal_checksum
    validate_sereal_checksum
    get_sereal_decoder
    get_sereal_encoder
    clear_sereal_object_cache

    encode_sereal
    decode_sereal

    read_sereal
    read_sereal_file
    write_sereal
    write_sereal_file

    looks_like_sereal
    scalar_looks_like_sereal

    sereal_encode_with_object
    sereal_decode_with_object
    decode_sereal_with_header_data

    sereal_decode_with_header_with_object
    sereal_decode_only_header_with_object
    sereal_decode_only_header_with_offset_with_object
    sereal_decode_with_header_and_offset_with_object
    sereal_decode_with_offset_with_object

    SRL_UNCOMPRESSED
    SRL_SNAPPY
    SRL_ZLIB
    SRL_ZSTD
);
our %EXPORT_TAGS = ( all => \@EXPORT_OK );

# export by default if run from command line
our @EXPORT = ( ( caller() )[1] eq '-e' ? @EXPORT_OK : () );

our %ENCODERS;
our %DECODERS;

sub _key {
    join "\t", map { $_ => $_[0]->{$_} } sort keys %{ $_[0] };
}

sub clear_sereal_object_cache {
    my $count = keys(%DECODERS) + keys(%ENCODERS);
    %ENCODERS = ();
    %DECODERS = ();
    return $count;
}

sub get_sereal_encoder {
    my ($opts) = @_;
    return $ENCODERS{ _key($opts) } ||= Sereal::Encoder->new($opts);
}

sub get_sereal_decoder {
    my ($opts) = @_;
    return $DECODERS{ _key($opts) } ||= Sereal::Decoder->new($opts);
}

sub write_sereal_file {
    my ( $file, $struct, $append, $opts ) = @_;
    get_sereal_encoder($opts)->encode_to_file( $file, $_[1], $append );
}

sub read_sereal_file {
    my ( $file, $opts ) = @_;
    get_sereal_decoder($opts)->decode_from_file( $file, @_ > 2 ? $_[2] : () );
}

*read_sereal  = *read_sereal  = *read_sereal_file;
*write_sereal = *write_sereal = *write_sereal_file;

1;

__END__

=encoding utf8

=head1 NAME

Sereal - Fast, compact, powerful binary (de-)serialization

=head1 SYNOPSIS

    use Sereal qw(
        get_sereal_decoder
        get_sereal_encoder
        clear_sereal_object_cache

        encode_sereal
        decode_sereal

        read_sereal
        read_sereal_file
        write_sereal
        write_sereal_file

        looks_like_sereal
        scalar_looks_like_sereal

        sereal_encode_with_object
        sereal_decode_with_object
        decode_sereal_with_header_data

        sereal_decode_with_header_with_object
        sereal_decode_only_header_with_object
        sereal_decode_only_header_with_offset_with_object
        sereal_decode_with_header_and_offset_with_object
        sereal_decode_with_offset_with_object

        SRL_UNCOMPRESSED
        SRL_SNAPPY
        SRL_ZLIB
        SRL_ZSTD
    );
    # Note: For performance reasons, you should prefer the OO interface,
    #       or sereal_(en|de)code_with_object over the stateless
    #       encode_sereal/decode_sereal functions.
    #       See the Sereal::Performance documentation for details.

=head1 DESCRIPTION

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

This distribution requires Perl 5.14 or newer.

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
performance. For detailed performance considerations,
see L<Sereal::Performance>.

You can optionally import five functions from C<Sereal>.
C<encode_sereal> is the same function as L<Sereal::Encoder>'s
C<encode_sereal> function. C<decode_sereal> and C<looks_like_sereal>
are the same as L<Sereal::Decoder>'s functions of the same names.
Finally, you can import the advanced functional interface
C<sereal_encode_with_object> and C<sereal_decode_with_object>.
Again, see L<Sereal::Performance> for information about those.

After loading the C<Sereal> module, both C<Sereal::Encoder> and
C<Sereal::Decoder> are guaranteed to be loaded, so you can use
their object-oriented interface.

=head2 get_sereal_encoder($OPTSHASH)

Returns a Sereal::Encoder with the given options. This encoder will be shared by other calls
to this function.

=head2 get_sereal_decoder($OPTSHASH)

Returns a Sereal::Decoder with the given options. This encoder will be shared by other calls
to this function.

=head2 clear_sereal_object_cache

Clears cache of objects created via get_sereal_encoder() and get_sereal_decoder(). Returns
the number of objects that were removed from the cache (the sum of both types).

=head2 write_sereal_file($FILENAME,$STRUCT,$APPEND,$OPTS)

Write a sereal packet to $FILENAME. See Sereal::Encoder::encode_to_file().

=head2 write_sereal($FILENAME,$STRUCT,$APPEND,$OPTS)

alias for write_sereal_file()

=head2 read_sereal_file($FILENAME,$OPTS,$ROOT)

Read a sereal packet from a file. See Sereal::Decoder::decode_from_file().

=head2 read_sereal($FILENAME,$OPTS,$ROOT)

alias for read_sereal_file()

=head1 CHECKSUM CONFIGURATION

Encoder and Decoder accept C<checksum_key> and C<checksum_mode>. The aliases
C<checksum_key_v1>, C<checksum_key_v2>, and C<checksum_key_v3> each set the
same key and mode together:

  checksum_key_v2 => $key
  # equivalent to:
  checksum_key => $key, checksum_mode => 2

Duplicate key assignments warn when the values match and die when they
differ. On Encoder, duplicate mode assignments warn, including identical
assignments, and select the highest numeric mode. Constructor arguments
take precedence over package defaults.

Decoder uses the key with the algorithm specified in each document. It
accepts C<checksum_mode> for shared configurations, but that option does not
restrict or disable verification. Set C<checksum_policy> to C<'require_v1'>,
C<'require_v2'>, or C<'require_v3'> to require authentication at that level or
higher. Existing C<'require_key'> and C<'require'> policies mean C<'require_v1'>.
Encoder rejects a key/mode configuration that cannot satisfy the policy.

The shared package defaults C<$Sereal::ChecksumMode> and
C<$Sereal::DefaultKey> configure Encoder's mode and both modules' key. They can also
be initialized at module startup with C<SEREAL_CHECKSUM_MODE> and
C<SEREAL_DEFAULT_KEY>. The mode accepts C<'none'> or C<'v1'>, C<'v2'>, and
C<'v3'> (also numeric C<0> through C<3>); C<'v1'> through C<'v3'> select wire
checksum types 0 through 2. The shared key is raw key bytes; the environment
value is decoded from 32 hexadecimal characters or normalized as a key.

Checksum configuration resolves in this order: constructor options,
module-specific package defaults, then the shared package defaults. The
Encoder and Decoder have separate C<DefaultKey> and C<ChecksumPolicy> package
variables and environment variables for module-specific defaults. Encoder
also has its own C<ChecksumMode> default, which is C<'v3'> when unspecified.
Decoder's algorithm always comes from the document. See
L<Sereal::Encoder/checksum_key>, L<Sereal::Encoder/checksum_mode>,
L<Sereal::Decoder/checksum_key>, and L<Sereal::Decoder/checksum_mode> for the
complete option and environment variable details.

=head1 SECURITY

Restrict decoding to trusted sources and protect serialized packets against
tampering. See L<Sereal::Decoder/ROBUSTNESS> for packet authentication,
resource limits, and object handling controls, and L<Sereal::Encoder/SECURITY>
for encoding and checksum guidance.

The project-wide security policy and private vulnerability reporting
instructions are available at
L<https://github.com/Sereal/Sereal/security/policy>. Report suspected
vulnerabilities privately at
L<https://github.com/Sereal/Sereal/security/advisories/new>.

=head1 BUGS, CONTACT AND SUPPORT

For reporting bugs, please use the github bug tracker at
L<http://github.com/Sereal/Sereal/issues>.

For support and discussion of Sereal, there are two Google Groups:

Announcements around Sereal (extremely low volume):
L<https://groups.google.com/forum/?fromgroups#!forum/sereal-announce>

Sereal development list:
L<https://groups.google.com/forum/?fromgroups#!forum/sereal-dev>

=head1 AUTHOR

Steffen Mueller E<lt>cpan@steffen-mueller.netE<gt>

=head1 ACKNOWLEDGMENT

This module was originally developed for Booking.com.
With approval from Booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012, 2013, 2014 by Steffen Mueller

=cut
