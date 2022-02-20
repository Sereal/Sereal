package Sereal::Decoder;
use 5.008;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION= '4.023'; # Don't forget to update the TestCompat set for testing against installed encoders!
our $XS_VERSION= $VERSION; $VERSION= eval $VERSION;

# not for public consumption, just for testing.
( my $num_version= $VERSION ) =~ s/_//;
my $TestCompat= [ map sprintf( "%.2f", $_ / 100 ), reverse( 400 .. int( $num_version * 100 ) ) ]; # compat with 4.00 to ...
sub _test_compat { return ( @$TestCompat, $VERSION ) }

use Exporter 'import';
our @EXPORT_OK= qw(
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
our %EXPORT_TAGS= ( all => \@EXPORT_OK );

# export by default if run from command line
our @EXPORT= ( ( caller() )[1] eq '-e' ? @EXPORT_OK : () );

sub CLONE_SKIP { 1 }
XSLoader::load( 'Sereal::Decoder', $XS_VERSION );
#start-no-tidy
use constant #begin generated
{
  'SRL_F_DECODER_ALIAS_CHECK_FLAGS' => 28672,
  'SRL_F_DECODER_ALIAS_SMALLINT' => 4096,
  'SRL_F_DECODER_ALIAS_VARINT' => 8192,
  'SRL_F_DECODER_DECOMPRESS_SNAPPY' => 8,
  'SRL_F_DECODER_DECOMPRESS_ZLIB' => 16,
  'SRL_F_DECODER_DECOMPRESS_ZSTD' => 131072,
  'SRL_F_DECODER_DESTRUCTIVE_INCREMENTAL' => 1024,
  'SRL_F_DECODER_DIRTY' => 2,
  'SRL_F_DECODER_NEEDS_FINALIZE' => 4,
  'SRL_F_DECODER_NO_BLESS_OBJECTS' => 512,
  'SRL_F_DECODER_PROTOCOL_V1' => 2048,
  'SRL_F_DECODER_READONLY_FLAGS' => 98304,
  'SRL_F_DECODER_REFUSE_OBJECTS' => 128,
  'SRL_F_DECODER_REFUSE_SNAPPY' => 32,
  'SRL_F_DECODER_REFUSE_ZLIB' => 64,
  'SRL_F_DECODER_REFUSE_ZSTD' => 262144,
  'SRL_F_DECODER_REUSE' => 1,
  'SRL_F_DECODER_SET_READONLY' => 32768,
  'SRL_F_DECODER_SET_READONLY_SCALARS' => 65536,
  'SRL_F_DECODER_USE_UNDEF' => 16384,
  'SRL_F_DECODER_VALIDATE_UTF8' => 256,
  'SRL_F_DECODER_VOLATILE_FLAGS' => 133150,
  '_FLAG_NAME' => [
                    'REUSE',
                    'DIRTY',
                    'NEEDS_FINALIZE',
                    'DECOMPRESS_SNAPPY',
                    'DECOMPRESS_ZLIB',
                    'REFUSE_SNAPPY',
                    'REFUSE_ZLIB',
                    'REFUSE_OBJECTS',
                    'VALIDATE_UTF8',
                    'NO_BLESS_OBJECTS',
                    'DESTRUCTIVE_INCREMENTAL',
                    'PROTOCOL_V1',
                    'ALIAS_SMALLINT',
                    'ALIAS_VARINT',
                    'USE_UNDEF',
                    'SET_READONLY',
                    'SET_READONLY_SCALARS',
                    'DECOMPRESS_ZSTD',
                    'REFUSE_ZSTD'
                  ],
  '_FLAG_NAME_STATIC' => [
                           'REUSE',
                           undef,
                           undef,
                           undef,
                           undef,
                           'REFUSE_SNAPPY',
                           'REFUSE_ZLIB',
                           'REFUSE_OBJECTS',
                           'VALIDATE_UTF8',
                           'NO_BLESS_OBJECTS',
                           'DESTRUCTIVE_INCREMENTAL',
                           undef,
                           'ALIAS_SMALLINT',
                           'ALIAS_VARINT',
                           'USE_UNDEF',
                           'SET_READONLY',
                           'SET_READONLY_SCALARS',
                           undef,
                           'REFUSE_ZSTD'
                         ],
  '_FLAG_NAME_VOLATILE' => [
                             undef,
                             'DIRTY',
                             'NEEDS_FINALIZE',
                             'DECOMPRESS_SNAPPY',
                             'DECOMPRESS_ZLIB',
                             undef,
                             undef,
                             undef,
                             undef,
                             undef,
                             undef,
                             'PROTOCOL_V1',
                             undef,
                             undef,
                             undef,
                             undef,
                             undef,
                             'DECOMPRESS_ZSTD',
                             undef
                           ]
}; #end generated
#end-no-tidy

sub decode_from_file {
    my ( $self, $file, )= @_;    # pos 3 is "target var" if one is provided
    $self= $self->new() unless ref $self;
    open my $fh, "<", $file
        or die "Failed to open '$file' for read: $!";
    my $buf= do { local $/; <$fh> };
    close $fh
        or die "Failed to close '$file': $!";
    if ( wantarray && ( $self->flags & SRL_F_DECODER_DESTRUCTIVE_INCREMENTAL ) ) {
        my @ret;
        while ( length $buf ) {
            push @ret, $self->decode($buf);
        }
        return @ret;
    }
    return $self->decode( $buf, @_ > 2 ? $_[2] : () );
}

my $flags= sub {
    my ( $int, $ary )= @_;
    return map { ( $ary->[$_] and $int & ( 1 << $_ ) ) ? $ary->[$_] : () } ( 0 .. $#$ary );
};

sub flag_names {
    my ( $self, $val )= @_;
    return $flags->( defined $val ? $val : $self->flags, _FLAG_NAME );
}

sub flag_names_volatile {
    my ( $self, $val )= @_;
    return $flags->( $val // $self->flags, _FLAG_NAME_VOLATILE );
}

sub flag_names_static {
    my ( $self, $val )= @_;
    return $flags->( $val // $self->flags, _FLAG_NAME_STATIC );
}

1;

__END__

=encoding utf8

=head1 NAME

Sereal::Decoder - Fast, compact, powerful binary deserialization

=head1 SYNOPSIS

  use Sereal::Decoder
    qw(decode_sereal sereal_decode_with_object scalar_looks_like_sereal);

  my $decoder = Sereal::Decoder->new({...options...});

  my $structure;
  $decoder->decode($blob, $structure); # deserializes into $structure

  # or if you don't have references to the top level structure, this works, too:
  $structure = $decoder->decode($blob);

  # alternatively functional interface: (See Sereal::Performance)
  sereal_decode_with_object($decoder, $blob, $structure);
  $structure = sereal_decode_with_object($decoder, $blob);

  # much slower functional interface with no persistent objects:
  decode_sereal($blob, {... options ...}, $structure);
  $structure = decode_sereal($blob, {... options ...});

  # Not a full validation, but just a quick check for a reasonable header:
  my $is_likely_sereal = scalar_looks_like_sereal($some_string);
  # or:
  $is_likely_sereal = $decoder->looks_like_sereal($some_string);

=head1 DESCRIPTION

This library implements a deserializer for an efficient, compact-output,
and feature-rich binary protocol called I<Sereal>.
Its sister module L<Sereal::Encoder> implements an encoder for this format.
The two are released separately to allow for independent and safer upgrading.

The Sereal protocol versions that are compatible with this decoder
implementation are currently protocol versions 1, 2, 3 and 4. As it stands, it
will refuse to attempt to decode future versions of the protocol, but if
necessary there is likely going to be an option to decode the parts of the
input that are compatible with version 4 of the protocol. The protocol was
designed to allow for this.

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

=head3 refuse_snappy

If set, the decoder will refuse Snappy-compressed input data. This can be
desirable for robustness. See the section C<ROBUSTNESS> below.

=head3 refuse_objects

If set, the decoder will refuse deserializing any objects in the input stream and
instead throw an exception. Defaults to off. See the section C<ROBUSTNESS> below.

=head3 no_bless_objects

If set, the decoder will deserialize any objects in the input stream but without
blessing them. Defaults to off. See the section C<ROBUSTNESS> below.

=head3 validate_utf8

If set, the decoder will refuse invalid UTF-8 byte sequences. This is off
by default, but it's strongly encouraged to be turned on if you're dealing
with any data that has been encoded by an external source (e.g. http cookies).

=head3 max_recursion_depth

C<Sereal::Decoder> is recursive. If you pass it a Sereal document that is deeply
nested, it will eventually exhaust the C stack. Therefore, there is a limit on
the depth of recursion that is accepted. It defaults to 10000 nested calls. You
may choose to override this value with the C<max_recursion_depth> option.
Beware that setting it too high can cause hard crashes.

Do note that the setting is somewhat approximate. Setting it to 10000 may break at
somewhere between 9997 and 10003 nested structures depending on their types.

=head3 max_num_hash_entries

If set to a non-zero value (default: 0), then C<Sereal::Decoder> will refuse
to deserialize any hash/dictionary (or hash-based object) with more than
that number of entries. This is to be able to respond quickly to any future
hash-collision attacks on Perl's hash function, and also the memory exhaustion
attacks on Sereal itself. For a gentle introduction to the topic from the
cryptographic point of view, see L<http://en.wikipedia.org/wiki/Collision_attack>.

=head3 max_num_array_entries

If set to a non-zero value (default: 0), then C<Sereal::Decoder> will refuse
to deserialize any array with more than that number of entries.
This is to be able to respond quickly to any future memory exhaustion attacks on
Sereal.

=head3 max_string_length

If set to a non-zero value (default: 0), then C<Sereal::Decoder> will refuse
to deserialize any string with more than that number of characters.
This is to be able to respond quickly to any future memory exhaustion attacks on
Sereal.

=head3 max_uncompressed_size

If set to a non-zero value (default: 0), then C<Sereal::Decoder> will refuse
to deserialize any blob with a size that exceds the value when uncompressed.
This is to be able to respond quickly to any future memory exhaustion attacks on
Sereal.

=head3 incremental

If set to a non-zero value (default: 0), then C<Sereal::Decoder> will
destructively parse Sereal documents out of a variable. Every time a Sereal
document is successfully parsed it is removed from the front of the string
it is parsed from.

This means you can do this:

    while (length $buffer) {
        my $data= decode_sereal($buffer,{incremental=>1});
    }

=head3 alias_smallint

If set to a true value then C<Sereal::Decoder> will share integers from
-16 to 15 (encoded as either SRL_HDR_NEG and SRL_HDR_POS) as read-only
aliases to a common SV.

The result of this may be significant space savings in data structures with
many integers in the specified range. The cost is more memory used by the
decoder and a very modest speed penalty when deserializing.

Note this option changes the structure of the dumped data. Use with caution.

See also the "alias_varint_under" option.

=head3 alias_varint_under

If set to a true positive integer smaller than 16 then this option is
similar to setting "alias_smallint" and causes all integers from -16 to 15
to be shared as read-only aliases to the same SV, except that this treatment
ALSO applies to SRL_HDR_VARINT. If set to a value larger than 16 then this
applies to all varints varints under the value set. (In general SRL_HDR_VARINT
is used only for integers larger than 15, and SRL_HDR_NEG and SRL_HDR_POS are
used for -16 to -1  and 0 to 15 respectively.)

In simple terms if you want to share values larger than 16 then you should use
this option, if you want to share only values in the -16 to 15 range then you
should use the "alias_smallint" option instead.

The result of this may be significant space savings in data structures with
many integers in the desire range. The cost is more memory used by the decoder
and a very modest speed penalty when deserializing.

Note this option changes the structure of the dumped data. Use with caution.

=head3 use_undef

If set to a true value then this any undef value to be deserialized as
PL_sv_undef. This may change the structure of the data structure being
dumped, do not enable this unless you know what you are doing.

=head3 set_readonly

If set to a true value then the output will be completely readonly (deeply).

=head3 set_readonly_scalars

If set to a true value then scalars in the output will be readonly (deeply).
References won't be readonly.

=head1 INSTANCE METHODS

=head2 decode

Given a byte string of Sereal data, the C<decode> call deserializes that data
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

=head2 decode_with_header

Given a byte string of Sereal data, the C<decode_with_header> call deserializes
that data structure as C<decode> would do, however it also decodes the optional
user data structure that can be embedded into a Sereal document, inside the
header  (see L<Sereal::Encoder::encode>).

It accepts an optional second parameter, which is a scalar to write the body
to, and an optional third parameter, which is a scalar to write the header to.

Regardless of the number of parameters received, C<decode_with_header> returns
an ArrayRef containing the deserialized header, and the deserialized body, in
this order.

See C<decode> for the subtle difference between the one, two and three
parameters versions.

If there is no header in a Sereal document, corresponding variable or return
value will be set to undef.

=head2 decode_only_header

Given a byte string of Sereal data, the C<decode_only_header> deserializes
only the optional user data structure that can be embedded into a Sereal
document, inside the header (see L<Sereal::Encoder::encode>).

It accepts an optional second parameter, which is a scalar
to write the header to.

Regardless of the number of parameters received, C<decode_only_header> returns
the resulting data structure.

See C<decode> for the subtle difference between the one and two parameters
versions.

If there is no header in a Sereal document, corresponding variable or return
value will be set to undef.

=head2 decode_with_offset

Same as the C<decode> method, except as second parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalar (see C<decode> above)
is relegated to being the third parameter.

=head2 decode_only_header_with_offset

Same as the C<decode_only_header> method, except as second parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalar (see C<decode_only_header> above)
is relegated to being the third parameter.

=head2 decode_with_header_and_offset

Same as the C<decode_with_header> method, except as second parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalars (see C<decode_with_header> above)
are relegated to being the third and fourth parameters.

=head2 bytes_consumed

After using the various C<decode> methods documented previously,
C<bytes_consumed> can return the number of bytes B<from the body> of the input
string that were actually consumed by the decoder. That is, if you append
random garbage to a valid Sereal document, C<decode> will happily decode the
data and ignore the garbage. If that is an error in your use case, you can use
C<bytes_consumed> to catch it.

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

As mentioned, only the bytes consumed from the body are considered. So the
following example is correct, as only the header is deserialized:

  my $header = $decoder->decode_only_header($sereal_string);
  my $count = $decoder->bytes_consumed;
  # $count is 0

=head2 decode_from_file

    Sereal::Decoder->decode_from_file($file);
    $decoder->decode_from_file($file);

Read and decode the file specified. If called in list context
and incremental mode is enabled then decodes all packets
contained in the file and returns a list, otherwise decodes
the first (or only) packet in the file. Accepts an optinal
"target" variable as a second argument.

=head2 looks_like_sereal

Performs some rudimentary check to determine if the argument
appears to be a valid Sereal packet or not. These tests are not
comprehensive and a true result does not mean that the document
is valid, merely that it appears to be valid. On the other hand
a false result is always reliable.

The return of this method may be treated as a simple boolean but
is in fact a more complex return. When the argument does not
look anything like a Sereal document then the return is perl's FALSE,
which has the property of being string equivalent to "" and
numerically equivalent to 0. However when the argument appears to
be a UTF-8 encoded protocol 3 Sereal document (by noticing that
the \xF3 in the magic string has been replaced by \xC3\xB3) then
it returns 0 (the number, which is string equivalent to "0"), and
otherwise returns the protocol version of the document. This means
you can write something like this:

    $type= Sereal::Decoder->looks_like_sereal($thing);
    if ($type eq '') {
        say "Not a Sereal document";
    } elsif ($type eq '0') {
        say "Possibly utf8 encoded Sereal document";
    } else {
        say "Sereal document version $type";
    }

For reference, Sereal's magic value is a four byte string which is
either C<=srl> for protocol version 1 and 2 or C<=\xF3rl> for protocol
version 3 and later. This function checks that the magic string
corresponds with the reported version number, as well as other
checks, which may be enhanced in the future.

Note that looks_like_sereal() may be called as a class or object method,
and may also be called as a single argument function. See the related
scalar_looks_like_sereal() for a version which may ONLY be called as a
function, not as a method (and which is typically much faster).

=head1 EXPORTABLE FUNCTIONS

=head2 sereal_decode_with_object

The functional interface that is equivalent to using C<decode>. Takes a
decoder object reference as first parameter, followed by a byte string
to deserialize.  Optionally takes a third parameter, which is the output
scalar to write to. See the documentation for C<decode> above for details.

This functional interface is marginally faster than the OO interface
since it avoids method resolution overhead and, on sufficiently modern
Perl versions, can usually avoid subroutine call overhead. See
L<Sereal::Performance> for a discussion on how to tune Sereal for maximum
performance if you need to.

=head2 sereal_decode_with_header_with_object

The functional interface that is equivalent to using C<decode_with_header>.
Takes a decoder object reference as first parameter, followed by a byte string
to deserialize. Optionally takes third and fourth parameters, which are
the output scalars to write to. See the documentation for C<decode_with_header>
above for details.

This functional interface is marginally faster than the OO interface
since it avoids method resolution overhead and, on sufficiently modern
Perl versions, can usually avoid subroutine call overhead. See
L<Sereal::Performance> for a discussion on how to tune Sereal for maximum
performance if you need to.

=head2 sereal_decode_only_header_with_object

The functional interface that is equivalent to using C<decode_only_header>.
Takes a decoder object reference as first parameter, followed by a byte string
to deserialize. Optionally takes a third parameters, which outputs scalars to write to.
See the documentation for C<decode_with_header> above for details.

This functional interface is marginally faster than the OO interface
since it avoids method resolution overhead and, on sufficiently modern
Perl versions, can usually avoid subroutine call overhead. See
L<Sereal::Performance> for a discussion on how to tune Sereal for maximum
performance if you need to.

=head2 sereal_decode_only_header_with_offset_with_object

The functional interface that is equivalent to using C<decode_only_header_with_offset>.
Same as the C<sereal_decode_only_header_with_object> function,
except as the third parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalar (see C<sereal_decode_only_header_with_object> above)
is relegated to being the fourth parameter.

This functional interface is marginally faster than the OO interface
since it avoids method resolution overhead and, on sufficiently modern
Perl versions, can usually avoid subroutine call overhead. See
L<Sereal::Performance> for a discussion on how to tune Sereal for maximum
performance if you need to.

=head2 sereal_decode_with_header_and_offset_with_object

The functional interface that is equivalent to using C<decode_with_header_and_offset>.
Same as the C<sereal_decode_with_header_with_object> function, except as the third parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalars (see C<sereal_decode_with_header_with_object> above)
are relegated to being the fourth and fifth parameters.

This functional interface is marginally faster than the OO interface
since it avoids method resolution overhead and, on sufficiently modern
Perl versions, can usually avoid subroutine call overhead. See
L<Sereal::Performance> for a discussion on how to tune Sereal for maximum
performance if you need to.

=head2 sereal_decode_with_offset_with_object

The functional interface that is equivalent to using C<decode_with_offset>.
Same as the C<sereal_decode_with_object> function, except as the third parameter, you must
pass an integer offset into the input string, at which the decoding is
to start. The optional "pass-in" style scalar (see C<sereal_decode_with_object> above)
is relegated to being the third parameter.

This functional interface is marginally faster than the OO interface
since it avoids method resolution overhead and, on sufficiently modern
Perl versions, can usually avoid subroutine call overhead. See
L<Sereal::Performance> for a discussion on how to tune Sereal for maximum
performance if you need to.

=head2 decode_sereal

The functional interface that is equivalent to using C<new> and C<decode>.
Expects a byte string to deserialize as first argument, optionally followed
by a hash reference of options (see documentation for C<new()>). Finally,
C<decode_sereal> supports a third parameter, which is the output scalar
to write to. See the documentation for C<decode> above for details.

This functional interface is significantly slower than the OO interface since
it cannot reuse the decoder object.

=head2 decode_sereal_with_header_data

The functional interface that is equivalent to using C<new> and C<decode_with_header>.
Expects a byte string to deserialize as first argument, optionally followed
by a hash reference of options (see documentation for C<new()>). Finally,
C<decode_sereal> supports third and fourth parameters, which are the output scalars
to write to. See the documentation for C<decode_with_header> above for details.

This functional interface is significantly slower than the OO interface since
it cannot reuse the decoder object.

=head2 scalar_looks_like_sereal

The functional interface that is equivalent to using C<looks_like_sereal>.

Note that this version cannot be called as a method. It is normally executed
as a custom opcode, as such errors about its usage may be caught at compile
time, and it should be much faster than looks_like_sereal.

=head1 ROBUSTNESS

This implementation of a Sereal decoder tries to be as robust to invalid
input data as reasonably possible. This means that it should never
(though read on) segfault. It may, however, cause a large malloc
to fail. Generally speaking, invalid data should cause a Perl-trappable
exception. The one exception is that for Snappy-compressed Sereal documents,
the Snappy library may cause segmentation faults (invalid reads or writes).
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

Finally, deserializing proper objects is potentially a problem because
classes can define a destructor. Thus, the data fed to the decoder can
cause the (deferred) execution of any destructor in your application.
That's why the C<refuse_objects> option exists and what the C<no_bless_objects>
can be used for as well. Later on, we may or may not provide a facility to
whitelist classes. Furthermore, if the encoder emitted any objects using
C<FREEZE> callbacks, the C<THAW> class method may be invoked on the
respective classes. If you can't trust the source of your Sereal documents,
you may want to use the C<refuse_objects> option. For more details on
the C<FREEZE/THAW> mechanism, please refer to L<Sereal::Encoder>.

=head1 PERFORMANCE

Please refer to the L<Sereal::Performance> document
that has more detailed information about Sereal performance and
tuning thereof.

=head1 THREAD-SAFETY

C<Sereal::Decoder> is thread-safe on Perl's 5.8.7 and higher. This means
"thread-safe" in the sense that if you create a new thread, all
C<Sereal::Decoder> objects will become a reference to undef in the new
thread. This might change in a future release to become a full clone
of the decoder object.

=head1 BUGS, CONTACT AND SUPPORT

For reporting bugs, please use the github bug tracker at
L<http://github.com/Sereal/Sereal/issues>.

For support and discussion of Sereal, there are two Google Groups:

Announcements around Sereal (extremely low volume):
L<https://groups.google.com/forum/?fromgroups#!forum/sereal-announce>

Sereal development list:
L<https://groups.google.com/forum/?fromgroups#!forum/sereal-dev>

=head1 AUTHORS AND CONTRIBUTORS

Yves Orton E<lt>demerphq@gmail.comE<gt>

Damian Gryski

Steffen Mueller E<lt>smueller@cpan.orgE<gt>

Rafaël Garcia-Suarez

Ævar Arnfjörð Bjarmason E<lt>avar@cpan.orgE<gt>

Tim Bunce

Daniel Dragan E<lt>bulkdd@cpan.orgE<gt> (Windows support and bugfixes)

Zefram

Borislav Nikolov

Ivan Kruglov E<lt>ivan.kruglov@yahoo.comE<gt>

Eric Herman E<lt>eric@freesa.orgE<gt>

Some inspiration and code was taken from Marc Lehmann's
excellent JSON::XS module due to obvious overlap in
problem domain.

=head1 ACKNOWLEDGMENT

This module was originally developed for Booking.com.
With approval from Booking.com, this module was generalized
and published on CPAN, for which the authors would like to express
their gratitude.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012, 2013, 2014 by Steffen Mueller
Copyright (C) 2012, 2013, 2014 by Yves Orton

The license for the code in this distribution is the following,
with the exceptions listed below:

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

Except portions taken from Marc Lehmann's code for the JSON::XS
module, which is licensed under the same terms as this module.
(Many thanks to Marc for inspiration, and code.)

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
