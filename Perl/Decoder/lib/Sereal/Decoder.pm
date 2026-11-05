package Sereal::Decoder;
use 5.014;
use strict;
use warnings;
use Carp qw/croak/;
use XSLoader;

our $VERSION    = '7.001';
our $XS_VERSION = $VERSION; $VERSION = eval $VERSION;

our $ChecksumPolicy;
our $UnknownOptionPolicy;
our $DefaultKey;
$Sereal::ChecksumPolicy = $ENV{SEREAL_CHECKSUM_POLICY}
    if !defined($Sereal::ChecksumPolicy) && defined($ENV{SEREAL_CHECKSUM_POLICY}) && length($ENV{SEREAL_CHECKSUM_POLICY});
$ChecksumPolicy = $ENV{SEREAL_DECODER_CHECKSUM_POLICY}
    if !defined($ChecksumPolicy) && defined($ENV{SEREAL_DECODER_CHECKSUM_POLICY}) && length($ENV{SEREAL_DECODER_CHECKSUM_POLICY});
$Sereal::UnknownOptionPolicy = $ENV{SEREAL_UNKNOWN_OPTION_POLICY}
    if !defined($Sereal::UnknownOptionPolicy) && defined($ENV{SEREAL_UNKNOWN_OPTION_POLICY}) && length($ENV{SEREAL_UNKNOWN_OPTION_POLICY});
$UnknownOptionPolicy = $ENV{SEREAL_DECODER_UNKNOWN_OPTION_POLICY}
    if !defined($UnknownOptionPolicy) && defined($ENV{SEREAL_DECODER_UNKNOWN_OPTION_POLICY}) && length($ENV{SEREAL_DECODER_UNKNOWN_OPTION_POLICY});

use Exporter 'import';
our @EXPORT_OK = qw(
    prepare_checksum_key
    siphash24_128
    get_sereal_checksum
    validate_sereal_checksum
    siphash24
    siphash36_128
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
our %EXPORT_TAGS = ( all => \@EXPORT_OK );

# export by default if run from command line
our @EXPORT = ( ( caller() )[1] eq '-e' ? @EXPORT_OK : () );

sub CLONE_SKIP { 1 }
XSLoader::load( 'Sereal::Decoder', $XS_VERSION );
if ( !defined($DefaultKey) && defined( $ENV{SEREAL_DECODER_DEFAULT_KEY} ) ) {
    my $key = $ENV{SEREAL_DECODER_DEFAULT_KEY};
    if ( $key =~ /\A[0-9a-fA-F]{32}\z/ ) {
        $DefaultKey = pack( 'H*', $key );
    }
    else {
        warn "SEREAL_DECODER_DEFAULT_KEY is not exactly 32 hexadecimal characters; normalizing with Sereal::Decoder::prepare_checksum_key()\n";
        $DefaultKey = prepare_checksum_key($key);
    }
}
unless ($Sereal::_DefaultKeyInitialized) {
    if ( !defined($Sereal::DefaultKey) && defined( $ENV{SEREAL_DEFAULT_KEY} ) ) {
        my $key = $ENV{SEREAL_DEFAULT_KEY};
        if ( $key =~ /\A[0-9a-fA-F]{32}\z/ ) {
            $Sereal::DefaultKey = pack( 'H*', $key );
        }
        else {
            warn
                "SEREAL_DEFAULT_KEY is not exactly 32 hexadecimal characters; normalizing with Sereal::Decoder::prepare_checksum_key()\n";
            $Sereal::DefaultKey = prepare_checksum_key($key);
        }
    }
    $Sereal::_DefaultKeyInitialized = 1;
}
#start-no-tidy
use constant #begin generated
{
  'SRL_F_DECODER_ALIAS_CHECK_FLAGS' => 28672,
  'SRL_F_DECODER_ALIAS_SMALLINT' => 4096,
  'SRL_F_DECODER_ALIAS_VARINT' => 8192,
  'SRL_F_DECODER_ALLOW_UNKNOWN_LONG_DOUBLE' => 16777216,
  'SRL_F_DECODER_BIG_ENDIAN_FLOATS' => 8388608,
  'SRL_F_DECODER_DECOMPRESS_SNAPPY' => 8,
  'SRL_F_DECODER_DECOMPRESS_ZLIB' => 16,
  'SRL_F_DECODER_DECOMPRESS_ZSTD' => 131072,
  'SRL_F_DECODER_DESTRUCTIVE_INCREMENTAL' => 1024,
  'SRL_F_DECODER_DIRTY' => 2,
  'SRL_F_DECODER_FLOAT_BYTEORDER_SET' => 4194304,
  'SRL_F_DECODER_NEEDS_FINALIZE' => 4,
  'SRL_F_DECODER_NO_BLESS_OBJECTS' => 512,
  'SRL_F_DECODER_NO_THAW_OBJECTS' => 524288,
  'SRL_F_DECODER_PROTOCOL_V1' => 2048,
  'SRL_F_DECODER_READONLY_FLAGS' => 98304,
  'SRL_F_DECODER_REFUSE_OBJECTS' => 128,
  'SRL_F_DECODER_REFUSE_SNAPPY' => 32,
  'SRL_F_DECODER_REFUSE_ZLIB' => 64,
  'SRL_F_DECODER_REFUSE_ZSTD' => 262144,
  'SRL_F_DECODER_REUSE' => 1,
  'SRL_F_DECODER_SET_READONLY' => 32768,
  'SRL_F_DECODER_SET_READONLY_SCALARS' => 65536,
  'SRL_F_DECODER_THAW_DENY_RAW' => 1048576,
  'SRL_F_DECODER_USE_NATIVE_FLOATS' => 2097152,
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
                    'REFUSE_ZSTD',
                    'NO_THAW_OBJECTS',
                    'THAW_DENY_RAW',
                    'USE_NATIVE_FLOATS',
                    'FLOAT_BYTEORDER_SET',
                    'BIG_ENDIAN_FLOATS',
                    'ALLOW_UNKNOWN_LONG_DOUBLE'
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
                           'REFUSE_ZSTD',
                           'NO_THAW_OBJECTS',
                           'THAW_DENY_RAW',
                           'USE_NATIVE_FLOATS',
                           'FLOAT_BYTEORDER_SET',
                           'BIG_ENDIAN_FLOATS',
                           'ALLOW_UNKNOWN_LONG_DOUBLE'
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
                             undef,
                             undef,
                             undef,
                             undef,
                             undef,
                             undef,
                             undef
                           ]
}; #end generated
#end-no-tidy

sub decode_from_file {
    my ( $self, $file, ) = @_;    # pos 3 is "target var" if one is provided
    $self = $self->new() unless ref $self;
    open my $fh, "<", $file
        or die "Failed to open '$file' for read: $!";
    my $buf = do { local $/; <$fh> };
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

my $flags = sub {
    my ( $int, $ary ) = @_;
    return map { ( $ary->[$_] and $int & ( 1 << $_ ) ) ? $ary->[$_] : () } ( 0 .. $#$ary );
};

sub flag_names {
    my ( $self, $val ) = @_;
    return $flags->( defined $val ? $val : $self->flags, _FLAG_NAME );
}

sub flag_names_volatile {
    my ( $self, $val ) = @_;
    return $flags->( $val // $self->flags, _FLAG_NAME_VOLATILE );
}

sub flag_names_static {
    my ( $self, $val ) = @_;
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

This distribution requires Perl 5.14 or newer.

This decoder supports Sereal protocol versions 1 through 7 and rejects
unsupported protocol versions.
Tags and compression formats that require a newer protocol version than the
version declared in a document are rejected.

The protocol specification and many other bits of documentation
can be found in the github repository. Right now, the specification is at
L<https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod>,
there is a discussion of the design objectives in
L<https://github.com/Sereal/Sereal/blob/master/README.pod>, and the output
of our benchmarks can be seen at
L<https://github.com/Sereal/Sereal/wiki/Sereal-Comparison-Graphs>.

=head1 CLASS METHODS

=head2 get_sereal_checksum

  my $tag = Sereal::Decoder::get_sereal_checksum($packet);
  my $hex = defined($tag) ? unpack('H*', $tag) : undef;
  my $tag = Sereal::Decoder::get_sereal_checksum($stream, $offset);

Returns the raw checksum bytes stored in the envelope, or C<undef> for an
unsigned packet. It checks framing and declared lengths, including the presence
of the declared payload, but does not authenticate, decompress, decode, or run
callbacks. Malformed framing raises an exception. It can inspect an unknown
checksum type, whose tag length need not be 8 or 16 bytes. Retrieving a tag does
not establish its validity. Bytes following the declared packet are ignored.

=head2 validate_sereal_checksum

  my $valid = Sereal::Decoder::validate_sereal_checksum($packet);
  my $valid = Sereal::Decoder::validate_sereal_checksum($packet, $key16);
  my $valid = Sereal::Decoder::validate_sereal_checksum($stream, $key16, $offset);

Verifies packet authentication without decompressing or decoding its contents.
Returns true for a valid tag and false for a mismatch or an unsigned packet.
Malformed framing, unknown checksum types, missing keys for checksummed input,
and keys of a size other than 16 bytes raise exceptions. An omitted key uses
C<$Sereal::DefaultKey>; an explicitly undefined key does not supply a key.
The packet selects the SipHash variant. This function always verifies,
regardless of C<checksum_policy> or its environment default.

A valid checksum authenticates the protected bytes, not their validity as a
Sereal value or their suitability for your application. Validation performs
one MAC pass over the protected bytes. Decoding the packet afterward with
verification enabled repeats that work. Both checksum functions support custom
opcodes for direct calls and the same behavior through XSUB calls via code
references. They can also be imported from the Sereal wrapper.

=head2 prepare_checksum_key

  my $key16 = Sereal::Decoder::prepare_checksum_key($key);
  my $key16 = Sereal::Decoder::prepare_checksum_key($key, $opt_key);
  my $empty_key16 = Sereal::Decoder::prepare_checksum_key();

Returns a deterministic 16-byte normalization of a byte string using
SipHash-2-4-128. It first computes SipHash-2-4-128 of C<Out damned spot!>
followed by the optional C<$opt_key>, keyed by C<Call me Ishmael.>, then uses
that result as the key to SipHash-2-4-128 of C<Luke, I am your father.>
followed by C<$key>. Both arguments default to the empty string. This only
normalizes the length; it does not add entropy or replace a password KDF.

=head2 siphash24, siphash24_128 and siphash36_128

  my $tag8  = Sereal::Decoder::siphash24($bytes, $key16);
  my $tag16 = Sereal::Decoder::siphash24_128($bytes, $key16);
  my $tag16 = Sereal::Decoder::siphash36_128($bytes, $key16);
  my $tag16_default = Sereal::Decoder::siphash36_128($bytes);

Compute a raw SipHash tag for a byte string and an exactly 16-byte key.
C<siphash24()> returns the eight-byte SipHash-2-4 result,
C<siphash24_128()> returns the 16-byte SipHash-2-4-128 result, and C<siphash36_128()>
returns the 16-byte SipHash-3-6-128 result. Each helper accepts an omitted key,
which defaults to sixteen null bytes. Explicit keys must be exactly 16 bytes.
These low-level helpers are
provided for tooling that needs to inspect or verify Sereal checksum
envelopes. The returned values are binary strings.

In SipHash-c-d, c is the compression round count per message block and d is
the round count in each finalization stage. The 128 suffix specifies the tag
size. See the authors' L<SipHash variant documentation|https://github.com/veorq/SipHash#variants>.

=head2 new

Constructs a decoder object. Optionally takes a hash reference of
options. Defaults are given below.

=head3 General Options

=head4 legacy_float_byteorder

Selects the source byte order for floating-point values in protocol
versions 1 through 6. Accepted values are C<'native'> (the default),
C<'little'>, and C<'big'>. Set this when reading protocol v1 through v6
data from a machine with a different, known byte order. It has no effect
on protocol v7.

This option does not convert between different native extended formats.
See L</FLOATING-POINT INTEROPERABILITY> for details.

=head4 allow_unknown_long_double

Allows protocol v7 native extended values with an unidentified layout to
be read using the local C<long double> representation. This option is
off by default. Enable it only for data from a matching Perl build and
ABI. Storage sizes must still match, and known format or byte-order
mismatches remain errors.

  my $decoder = Sereal::Decoder->new({ allow_unknown_long_double => 1 });

See L</FLOATING-POINT INTEROPERABILITY> for the limits of this mode.

=head4 incremental

If set to a non-zero value (default: 0), then C<Sereal::Decoder> will
destructively parse Sereal documents out of a variable. Every time a
Sereal document is successfully parsed it is removed from the front of
the string it is parsed from. Input and output must be different scalars
in this mode. When used with an offset, removal includes the prefix
before that offset.

This means you can do this:

    while (length $buffer) {
        my $data= decode_sereal($buffer,{incremental=>1});
    }

=head4 alias_smallint

If set to a true value then C<Sereal::Decoder> will share integers from
-16 to 15 (encoded as either SRL_HDR_NEG and SRL_HDR_POS) as read-only
aliases to a common SV.

The result of this may be significant space savings in data structures
with many integers in the specified range. The cost is more memory used
by the decoder and a very modest speed penalty when deserializing.

Note this option changes the structure of the dumped data. Use with
caution.

See also the "alias_varint_under" option.

=head4 alias_varint_under

If set to a true positive integer smaller than 16 then this option is
similar to setting "alias_smallint" and causes all integers from -16 to
15 to be shared as read-only aliases to the same SV, except that this
treatment ALSO applies to SRL_HDR_VARINT. If set to a value larger than
16 then this applies to all varints varints under the value set. (In
general SRL_HDR_VARINT is used only for integers larger than 15, and
SRL_HDR_NEG and SRL_HDR_POS are used for -16 to -1 and 0 to 15
respectively.)

In simple terms if you want to share values larger than 16 then you
should use this option, if you want to share only values in the -16 to
15 range then you should use the "alias_smallint" option instead.

The result of this may be significant space savings in data structures
with many integers in the desire range. The cost is more memory used by
the decoder and a very modest speed penalty when deserializing.

Note this option changes the structure of the dumped data. Use with
caution.

=head4 use_undef

If set to a true value then this any undef value to be deserialized as
PL_sv_undef. This may change the structure of the data structure being
dumped, do not enable this unless you know what you are doing.

=head4 set_readonly

If set to a true value then the output will be completely readonly
(deeply).

=head4 set_readonly_scalars

If set to a true value then scalars in the output will be readonly
(deeply). References won't be readonly.

=head3 Security and Policy Related Options

These options control authentication, object handling, input validation,
and resource limits. See L</ROBUSTNESS> for guidance on untrusted input.

=head4 checksum_key

A 16-byte key used to verify checksummed input. If omitted, the
configured default key is used; explicit C<undef> suppresses that
default. See L</CHECKSUM CONFIGURATION> for package and environment
defaults.

The packet selects the checksum algorithm. C<checksum_policy> controls
verification and key-length validation.

Supplying multiple key options warns if their values are identical and
throws an exception if they differ.

=head4 checksum_mode

Accepted for configurations shared with Encoder. Values are C<'none'> or
C<0>, C<'v1'> or C<1>, C<'v2'> or C<2>, and C<'v3'> or C<3>. The packet
determines the algorithm; use C<checksum_policy> to control
verification. This option and mode defaults do not change Decoder
behavior.

=head4 checksum_policy

Controls checksum verification and whether unsigned input is accepted.
The default is C<'verify-if-available'>. Accepted values are:

=over 4

=item * C<'verify-if-available'> - verify when the checksum type is
supported and a key is configured; reject a failed verification.
Otherwise warn and decode without authentication.

=item * C<'die'> - reject checksummed packets that cannot be verified,
including unsupported checksum types, missing keys and invalid tags.
Unsigned packets are accepted.

=item * C<'require_key'> - require a key at construction and reject
every packet that is unsigned or cannot be verified. C<'require'> and
C<'require_v1'> are aliases.

=item * C<'require_v2'> - require a key and accept only verified packets
using checksum mode v2 or v3.

=item * C<'require_v3'> - require a key and accept only verified packets
using checksum mode v3.

=item * C<'ignore'> - skip verification silently, even when a key is
configured.

=item * C<'ignore-warn'> - skip verification and warn for each
checksummed packet.

=back

The v1, v2 and v3 names refer to checksum modes. Policies other than the
C<'require'> policies accept unsigned packets. C<'die'> and the
C<'require'> policies require exactly 16 key bytes; other policies pad
or truncate keys with a warning. Required checksum modes are checked
before decompression or body decoding. See L</PERFORMANCE> for
verification costs.

=head4 checksum_key_v1

Compatibility alias for C<checksum_key>. It can verify every supported
checksum type; the suffix does not restrict accepted input.

=head4 checksum_key_v2

Compatibility alias for C<checksum_key>. It can verify every supported
checksum type; the suffix does not restrict accepted input.

=head4 checksum_key_v3

Compatibility alias for C<checksum_key>. It can verify every supported
checksum type; the suffix does not restrict accepted input.

=head4 refuse_objects

Rejects objects with an exception. This option is off by default. See
L</ROBUSTNESS> for object-handling considerations.

=head4 no_bless_objects

Decodes ordinary serialized objects without blessing them. This option
is off by default. See C<no_thaw_objects> to prevent C<THAW> callbacks.

=head4 no_thaw_objects

Returns frozen objects as argument arrays blessed into
C<Sereal::Decoder::THAW_args> instead of calling C<THAW>. This option is
off by default. See L</FREEZE/THAW CALLBACK MECHANISM> for the array
layout.

=head4 thaw_allow_classes

Restricts which classes may invoke C<THAW>. Accepts an array reference
of class names, a hash reference with true values for allowed names, or
a predicate C<< $cb->($classname) >> called once per frozen object and
returning true to allow the call. Without this option, all classes are
allowed.

For a denied class, see L</thaw_deny_action>. This option has no effect
when C<refuse_objects> or C<no_thaw_objects> prevents thawing. See
L</FREEZE/THAW CALLBACK MECHANISM> for callback behavior.

=head4 thaw_deny_action

Action when C<thaw_allow_classes> denies a C<THAW> call: C<'croak'>
throws an exception (the default); C<'raw'> returns the argument array
blessed into C<Sereal::Decoder::THAW_args>, as C<no_thaw_objects> does.

=head4 refuse_snappy

Rejects Snappy-compressed documents. This option is off by default.

=head4 refuse_zstd

Rejects Zstd-compressed documents. This option is off by default.

=head4 validate_utf8

Rejects invalid UTF-8 byte sequences in text strings. This option is off
by default. Enable it when input must contain valid UTF-8.

=head4 max_recursion_depth

Maximum recursion depth, default 10000. The limit is approximate and
depends on the structures encountered. Deep recursion can exhaust the C
stack; raising this limit can cause a crash. See L</ROBUSTNESS>.

=head4 max_num_hash_entries

Maximum entries in a hash or hash-based object. Zero means unlimited
(the default). Input exceeding the limit is rejected. See L</ROBUSTNESS>
for resource limits.

=head4 max_num_array_entries

Maximum entries in an array. Zero means unlimited (the default). Input
exceeding the limit is rejected. See L</ROBUSTNESS> for resource limits.

=head4 max_string_length

Maximum encoded string length in bytes. Zero means unlimited (the
default). Input exceeding the limit is rejected. See L</ROBUSTNESS> for
resource limits.

=head4 max_uncompressed_size

Maximum uncompressed document size in bytes. Zero means unlimited (the
default). Input exceeding the limit is rejected. See L</ROBUSTNESS> for
resource limits.

=head4 undef_deadly_policy

Controls the diagnostic C<UNDEF_DEADLY> value introduced in protocol
version 6. C<'undef'> decodes it silently as ordinary undef (the
default); C<'warn'> warns and decodes it; C<'die'> throws an exception.
An omitted or undefined option selects C<'undef'>; other values are
rejected at construction. C<use_undef> and reference tracking apply as
for ordinary undef.

=head4 unknown_option_policy

Controls unrecognized constructor keys: C<'warn'> warns and continues
(the default), C<'die'> throws an exception, and C<'ignore'> ignores
them. Invalid policy values throw an exception.

C<SEREAL_UNKNOWN_OPTION_POLICY> and
C<SEREAL_DECODER_UNKNOWN_OPTION_POLICY> initialize
C<$Sereal::UnknownOptionPolicy> and
C<$Sereal::Decoder::UnknownOptionPolicy> at module load if undefined. At
construction, the first defined value wins: the option, the
Decoder-specific package variable, then the shared package variable.
Otherwise C<'warn'> is used.

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

Caller-supplied outputs are cleared before parsing begins. This releases their
previous values and may run C<DESTROY>; the input and output scalars remain
retained during the reset.

Once parsing begins, application callbacks, including THAW methods, must not
modify the input scalar. The decoder reads directly from its buffer.
Input and output may otherwise use the same scalar; the decoder copies the
input for this case. Incremental mode requires separate input and output.

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

=head1 REGULAR EXPRESSIONS

The Decoder restores the regexp pattern and its matching modifiers, including
the character set modes C</d>, C</l>, C</u>, C</a> and C</aa>, C</n> (no unnamed
captures), and C</xx> (extended whitespace handling inside character classes).
It also preserves C</m>, C</s>, C</i>, C</x> and C</p>.

C</p> first shipped in Perl 5.10; the character set modifiers in Perl 5.14;
C</n> in Perl 5.22; and C</xx> in Perl 5.26. A modifier unavailable on the
receiving Perl causes an exception naming the modifier and the required
stable Perl version. Unknown modifier bytes and conflicting character set
modes are rejected. These checks apply to every Sereal protocol version.

Preserving modifiers does not freeze the regexp engine, Unicode tables, or
the execution locale. Those can still change matching behavior between hosts
or Perl versions.

Perl 5.14 and 5.16 have a core regexp-copy lifetime bug: a scalar copy of a
regexp can become invalid if the original regexp scalar is overwritten. Keep the
original alive and unchanged while its scalar copies are in use, including
when reusing an explicit decoder output scalar. This also affects regexps
created by Perl's own C<eval> and C<qr//>; Perl 5.18 and later avoid it.

Perl 5.20 also has a core cleanup issue for read-only bare regexp values in
lexicals: scope cleanup can raise C<Modification of a read-only value attempted>.
This occurs with regexps created by Perl itself as well. Prefer regexp
references on that Perl when using read-only values.

=head1 FLOATING-POINT INTEROPERABILITY

=head2 Perl builds and numeric formats

Most Perl builds store floating-point numbers as C<double>, normally IEEE
754 binary64. Long-double and quadmath builds support greater precision or
range. Quadmath uses IEEE binary128; C<long double> varies by platform.
Perl's C<Config> module reports its internal floating-point type (NV) in
C<$Config{nvtype}>. Platforms can also differ in byte order.

Encoding and decoding make separate decisions. The encoder chooses a
stored representation from the producer's NV type. The decoder decides
whether to accept that representation, then converts accepted values to
its local NV type. Input options control interpretation and acceptance;
they do not change the local NV type or increase its precision or range.
A successful read can therefore involve rounding or loss of range.

=head2 Reading protocol v7

Protocol v7 uses little-endian IEEE binary32, binary64, and binary128 by
default. The decoder handles byte order automatically and can read binary128
without quadmath support. No special option is needed.

Values are rounded to the local NV precision when necessary, using
nearest/even rounding. Overflow produces signed infinity; underflow can
produce a subnormal value or signed zero. NaN payload bits are not preserved.
Use a Perl build with sufficient precision and range if these losses are
unacceptable.

=head2 Reading native long doubles

A long-double producer can request native storage instead of portable
storage. Protocol v7 identifies the native format, byte order, and storage
size. The decoder rejects known layouts that do not match its own.

Unknown layouts are rejected by default. Set
C<allow_unknown_long_double =E<gt> 1> only when producer and reader use
matching Perl builds and ABIs (native memory layouts). This trusts the
stored bytes rather than converting them. The storage size must still
match, and known format or byte-order mismatches remain errors. Only
same-build value round trips are guaranteed for unknown layouts.

=head2 Reading protocol versions 1 through 6

These versions used the producer's native byte order without identifying
it in the document. Extended values could also depend on the producer's
native format and storage size. The decoder preserves the historical
behavior by assuming its own native representation.

When reading data from a different platform, obtain the producer's byte
order and set C<legacy_float_byteorder =E<gt> 'little'> or C<'big'> as needed.
This corrects byte order only; it cannot convert between different native
extended formats or storage sizes. Do not infer byte order from apparently
reasonable decoded numbers: the wrong order can also produce plausible
values. This option has no effect on protocol v7.

=head1 CHECKSUM CONFIGURATION

For per-object settings, see L</checksum_key>, L</checksum_mode>, and
L</checksum_policy>. Package and environment defaults are described
below.

=head2 Shared default key

C<$Sereal::DefaultKey> supplies a shared default when no checksum key
option is present. C<$Sereal::Decoder::DefaultKey> can override the
Decoder's default. Both contain raw key bytes, not hexadecimal text. A
default Decoder key is used for all three supported checksum types.
Explicit key options replace the default configuration; supplying an
explicit C<undef> key also suppresses the default.

The first load of Sereal::Encoder or Sereal::Decoder initializes this
shared variable from C<SEREAL_DEFAULT_KEY>, unless application code has
already set it. The Decoder-specific C<SEREAL_DECODER_DEFAULT_KEY>
initializes C<$Sereal::Decoder::DefaultKey> the same way. Constructor
key options take precedence over the Decoder-specific default, which
takes precedence over the shared default. Exactly 32 hexadecimal
characters are decoded to 16 bytes. Other lengths or non-hexadecimal
values are passed unchanged to C<prepare_checksum_key()>, with a warning
that does not include the secret. Normalization does not add entropy or
turn a weak password into a strong key. If the environment variable is
absent, no default key is installed. Loading the other module does not
reset an initialized default, and subsequent environment changes do not
affect it.

Code can set or localize the shared variable:

  local $Sereal::DefaultKey = $key16;
  my $decoder = Sereal::Decoder->new({ checksum_policy => 'require_key' });

New encoders and decoders copy its current value. Existing objects
retain their keys. Localizing it to C<undef> disables the default for
new objects in that scope. Cached objects, including objects returned by
the Sereal wrapper's cache helpers, retain the configuration they were
created with.

=head2 Default policy

C<SEREAL_CHECKSUM_POLICY> sets the shared default;
C<SEREAL_DECODER_CHECKSUM_POLICY> sets the Decoder-specific default. At
module startup they initialize C<$Sereal::ChecksumPolicy> and
C<$Sereal::Decoder::ChecksumPolicy> when those variables are undefined.

  SEREAL_CHECKSUM_POLICY=require_key perl my_application.pl

The constructor option overrides the Decoder-specific package variable,
which overrides the shared package variable. If none is defined, the
policy is C<'verify-if-available'>. Invalid values cause construction to
fail. Existing objects and recursive clones retain their configured
policy.

=head1 ROBUSTNESS

The project-wide security policy, supported-version guidance, and private
reporting instructions are at
L<https://github.com/Sereal/Sereal/security/policy>. Report suspected
vulnerabilities through
L<https://github.com/Sereal/Sereal/security/advisories/new>.

The decoder validates packet structure and bounds before using packet data,
and malformed input should normally produce a Perl exception. This is a
security goal, not a guarantee that every malformed packet is safe or that the
implementation has no vulnerabilities. Treat packets from untrusted or
attacker-influenced sources as hostile input, keep Sereal::Decoder updated, and
do not use successful decoding as a substitute for validating the resulting
values in the application.

Decoding can consume substantial CPU and memory even when a packet is valid.
The resource limits below default to unlimited (zero):
C<max_num_hash_entries>, C<max_num_array_entries>, C<max_string_length>, and
C<max_uncompressed_size>. Set limits appropriate to the application when
decoding data whose size or producer is not controlled. The recursion limit
defaults to 10000, which can still be too high for some Perl builds; lower it
when appropriate. These decoder limits do not replace process-level CPU and
memory limits. In particular, a checksum does not limit the cost of decoding
a packet from someone who knows the key.

Protocol version 6 checksums authenticate only packets that use a checksum
envelope. The default policy verifies when the matching key is configured;
without one it warns and decodes without authentication. Use
C<checksum_policy =E<gt> 'require_key'> with a matching key when every packet
must be authenticated; it rejects unsigned packets and failed verification.
C<'die'> rejects checksummed packets that cannot be verified but still accepts
unsigned packets. Verification runs before decompression. Checksums are not
encryption, do not prevent replay, and do not make data safe to deserialize
when the key holder is malicious or compromised. Packets without a checksum
have no cryptographic integrity check provided by Sereal.

Object handling can execute application code or cause later destructor code
to run. Use C<refuse_objects> when objects are not expected. For frozen objects,
C<no_thaw_objects> prevents C<THAW> calls, while C<thaw_allow_classes> can
restrict which classes may run them. Do not assume that disabling module
autoloading makes callbacks safe: already loaded classes can still have their
C<THAW> methods invoked unless these controls are used. See
L</FREEZE/THAW CALLBACK MECHANISM> for details.

=head1 FREEZE/THAW CALLBACK MECHANISM

Some objects do not lend themselves naturally to naive perl
datastructure level serialization. For instance XS code might use a
hidden structure that would not get serialized, or an object may contain
volatile data like a filehandle that would not be reconstituted
properly. To support cases like this C<Sereal> supports a FREEZE and
THAW api. When objects are serialized their FREEZE method is asked for a
replacement representation, and when objects are deserialized their THAW
method is asked to convert that replacement back to something useful.

For security reasons decoding an object will NOT autoload any modules
to support THAW, however if the classes and methods are preloaded it
will invoke THAW as required and an exception will be thrown if the class
has not been loaded. It is possible to disable THAW in the decoder by using
the C<no_thaw_objects> option, which when true causes frozen objects to
be blessed into a special utility class "Sereal::Decoder::THAW_args"
for debugging. The objects themselves are an array, whose last member
will contain the class name the arguments are for.

Prior to v4.024 the decoder had issues with frozen representations that
contained other objects and did not define a specific order that items
would be thawed and blessed, making it impractical to put an object
inside of the frozen representation of another object.

As of v4.024 frozen representations may contain other objects, and the
order in which they are thawed is defined to be LIFO, thus the arguments
to a THAW method will themselves be thawed (if need be) before the call
to the containing objects THAW method. Thawing occurs only after all
simple objects have been blessed into their appropriate object form.

Each frozen argument array must belong to a single C<OBJECT_FREEZE> or
C<OBJECTV_FREEZE> declaration. Conflicting declarations are rejected rather
than replacing the class associated with that array. Further references to
the same frozen object may use C<REFP> normally.

The FREEZE/THAW mechanism is inspired by the equivalent mechanism in
L<CBOR::XS>. The general mechanism is documented in the
I<A GENERIC OBJECT SERIALIATION PROTOCOL> section of L<Types::Serialiser>.
Similar to CBOR using C<CBOR>, Sereal uses the string C<Sereal> as a
serializer identifier for the callbacks.

=head1 PERFORMANCE

See L<Sereal::Performance> for general performance and tuning
information.

Checksum verification makes an additional pass over the protected header
and encoded payload. Its CPU cost grows with their size and depends on
the SipHash variant. Compressed payloads are verified before
decompression. With a configured key, all verifying policies perform the
same verification work for a given packet. Ignore policies skip
authentication but still validate the envelope framing.

Reusing a decoder avoids repeated configuration work; each packet still
requires verification. Warnings and their logging can also add
per-packet cost.

=head1 THREAD-SAFETY

C<Sereal::Decoder> is thread-safe on Perl 5.14 and higher. This means
"thread-safe" in the sense that if you create a new thread, all
C<Sereal::Decoder> objects will become a reference to undef in the new
thread. This might change in a future release to become a full clone
of the decoder object.

=head1 DEVELOPMENT TESTING

The Git repository includes an ASan and libFuzzer harness for Decoder
in F<Perl/author_tools/fuzz_decoder>. It builds Decoder with sanitizer
and coverage instrumentation and uses L<Sereal::Encoder> to generate seed
packets. It is a repository development tool, distributed separately from
the CPAN modules, and is not run by C<make test>.

See the L<harness README|https://github.com/Sereal/Sereal/tree/main/Perl/author_tools/fuzz_decoder>
for requirements and build and run instructions.

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

Steffen Mueller E<lt>cpan@steffen-mueller.netE<gt>

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
