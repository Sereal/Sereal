#!/usr/bin/env perl
use strict;
use warnings;
use Config;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

use Sereal::Encoder;
use Sereal::Encoder::Constants qw(SRL_HDR_FLOAT_128);
use Sereal::Decoder;
use Scalar::Util qw(weaken);
use Sereal::TestSet qw(regexp_modifier_test_cases);
use Sereal::TestSet::Corpus qw(write_packet_set prune_unreferenced unpack_corpus pack_corpus);

# Regenerate only with an Encoder and Perl supporting the full modifier matrix.
die "Regexp modifier corpus generation requires Perl 5.26 or later\n" if $] < 5.026;
Sereal::Encoder->VERSION('6.000004');

my %packets;
my $root = File::Spec->catdir( 't', 'data', 'packet_corpus' );
unpack_corpus($root);
for my $version ( 3, 6 ) {
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    $packets{"v$version/regexp_object"} = $encoder->encode(
        { re => qr/ab+c/i, obj => bless( qr/x/, 'Foo' ) } );
    my $shared = qr/q/;
    $packets{"v$version/regexp_shared"} = $encoder->encode( [ $shared, $shared ] );
    my $copy = ${qr/abc/i};
    $packets{"v$version/regexp_copy"} = $encoder->encode( [ $copy, \$copy ] );

    my $alias_encoder = Sereal::Encoder->new( {
        protocol_version       => $version,
        aliased_dedupe_strings => 1,
    } );
    $packets{"v$version/aliased_strings"} = $alias_encoder->encode( [ ('abcdef') x 4 ] );
}
$packets{'v6/undef'} = Sereal::Encoder->new( { protocol_version => 6 } )->encode(undef);

my $warn_packet;
{
    local $SIG{__WARN__} = sub { };
    $warn_packet = Sereal::Encoder->new( {
        protocol_version  => 6,
        snappy_incr       => 1,
        stringify_unknown => 1,
        warn_unknown      => 1,
    } )->encode( [ sub { 1 }, 'abcd' x 1024 ] );
}
my %feature_packets = ( 'snappy_and_warn_unknown/v6' => $warn_packet );
my %distinct_hash = map { ( "key$_", $_ ) } 1 .. 26;

for my $case (
    [ 'special/ref_undef',     \undef() ],
    [ 'special/ref_undef_var', \do { my $value = undef } ],
    [ 'special/ref_false',     \!1 ],
    [ 'special/ref_false_var', \do { my $value = !1 } ],
    [ 'special/ref_true',      \!0 ],
    [ 'special/ref_true_var',  \do { my $value = !0 } ],
    )
{
    $feature_packets{ $case->[0] } = Sereal::Encoder->new->encode( $case->[1] );
}

for my $case (
    [ 'alpha',      { map { $_ => $_ } 'a' .. 'z' } ],
    [ 'upper',      { map { $_ => $_ } 'A' .. 'Z' } ],
    [ 'small_nums', { map { $_ => $_ } 0 .. 100 } ],
    [ 'large_nums', { map { $_ => $_ } 10000 .. 10512 } ],
    [ 'distinct',   \%distinct_hash ],
    )
{
    $feature_packets{ "each/$case->[0]" } = Sereal::Encoder->new->encode( $case->[1] );
}

for my $up_to ( 15, 127 ) {
    my @alias_values = -16 .. $up_to;
    my %alias_map = map { $_ => $_ } @alias_values;
    my $alias_struct = {
        array  => [@alias_values],
        array2 => [ reverse @alias_values ],
        %alias_map,
    };
    $feature_packets{"alias_options/$up_to"} = Sereal::Encoder->new->encode($alias_struct);
}

my $reserialize_values = [ 0, 1, 'foo', 1, 2, 3, 3, 4, 3, 3, 2, 1, 27, 1 ];
my $reserialize_decoder = Sereal::Decoder->new( { alias_varint_under => 100000 } );
my $reserialize_packet = Sereal::Encoder->new->encode($reserialize_values);
my $reserialized_value = $reserialize_decoder->decode($reserialize_packet);
$feature_packets{'alias_reserialize/initial'} = $reserialize_packet;
$feature_packets{'alias_reserialize/reencoded'} =
    Sereal::Encoder->new->encode($reserialized_value);

my $readonly_foo = bless( [ 1, 2, 3 ], 'foo' );
my $readonly_weak = bless( { blah => 'bat', hash => { t => 1 } }, 'SomeClass' );
weaken( $readonly_weak->{foo} = $readonly_weak );
my $readonly_struct = {
    hashref               => { a => [ 'b', 5, bless( { foo => 'bar' }, 'SomeClass' ) ] },
    blessed_ref_with_refs => bless( {
        foo => { bar => 'test' }, bar => ['baz'], empty_href => {}, empty_aref => [],
    }, 'Blah' ),
    string            => 'foobar',
    arrayref          => ['foobar'],
    blessed_arrayref  => $readonly_foo,
    weak_blessed_href => $readonly_weak,
};
$feature_packets{'set_readonly'} = Sereal::Encoder->new->encode($readonly_struct);

$feature_packets{'nobless'} = Sereal::Encoder->new->encode( bless( { x => 1 }, 'MyFoo' ) );

my $utf8_snappy_input = {
    foo => 'bar', f111 => 'bar', f1111 => 'bar', f11111 => 'bar',
    f111111 => 'bar', f1111111 => 'bar', f11111111 => 'bar',
    f111111111 => 'bar', f1111111111 => 'bar', f11111111111 => 'bar',
    f111111111111 => 'bar', f1111111111111 => 'bar',
    f11111111111111 => 'bar', f111111111111111 => 'bar',
    f1111111111111111 => 'bar', f11111111111111111 => 'bar',
};
$feature_packets{'utf8decoding/snappy'} = Sereal::Encoder->new( {
    snappy           => 1,
    snappy_threshold => 0,
} )->encode($utf8_snappy_input);
$feature_packets{'utf8decoding/plain'} = Sereal::Encoder->new->encode(
    { therefore => "\x{2234}" } );

for my $case (
    [ raw => {} ],
    [ snappy_incr => { snappy_incr => 1 } ],
    [ zlib => { compress => Sereal::Encoder::SRL_ZLIB() } ],
    )
{
    my ( $name, $options ) = @$case;
    my $encoder = Sereal::Encoder->new($options);
    $feature_packets{"incremental/$name"} = join '', map { $encoder->encode($_) } 1 .. 30;
}

{
    package Rot13;
    sub new { bless \$_[1], $_[0] }
    sub FREEZE {
        my $value = ${ $_[0] };
        $value =~ tr/a-zA-Z/nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM/;
        return $value;
    }
    package ScalarReturn;
    sub FREEZE { return $_[0]->[0] }
}
my $freeze_encoder = Sereal::Encoder->new( { freeze_callbacks => 1 } );
$feature_packets{'thaw_scalar/rot13'} = do {
    my $secret = 'SECRET';
    $freeze_encoder->encode(
        [ bless( \$secret, 'Rot13' ), { secret => bless( \$secret, 'Rot13' ) } ] );
};
$feature_packets{'thaw_scalar/returns'} = $freeze_encoder->encode(
    [ map { bless [$_], 'ScalarReturn' } ( undef, 0, '', 42, 'text' ) ] );

my $unknown_stringifier = Sereal::Encoder->new( {
    stringify_unknown => 1,
    warn_unknown      => 1,
} );
$feature_packets{'overload/unknown_object'} = $unknown_stringifier->encode(
    bless( { foo => '123' }, 'Str' ) );
my $overloaded_object = bless( { str => 'asd' }, 'Foo' );
$feature_packets{'overload/repeated_object'} = $unknown_stringifier->encode(
    [ $overloaded_object, $overloaded_object ] );

{
    package Matcher;
    sub new { bless { pattern => $_[1] }, $_[0] }
    sub FREEZE { return [ $_[0]->{pattern} ], '22' }
}
my $plain_encoder = Sereal::Encoder->new;
my $matcher_string = 'string';
my $matcher_blessed = bless {}, 'Bespoke';
my $matcher_qr = qr/foo/;
my @thaw_090 = (
    $matcher_string,
    $matcher_blessed,
    Matcher->new($matcher_qr),
    Matcher->new($matcher_string),
    Matcher->new($matcher_blessed),
);
for my $index ( 0 .. $#thaw_090 ) {
    $feature_packets{"thaw/090/plain/" . ( $index + 1 )} = $plain_encoder->encode($thaw_090[$index]);
    $feature_packets{"thaw/090/freeze/" . ( $index + 1 )} = $freeze_encoder->encode($thaw_090[$index]);
}

{
    package TA::Allowed;
    sub new { bless { n => $_[1] }, $_[0] }
    sub FREEZE { $_[0]->{n} }
    package TA::Denied;
    sub new { bless { n => $_[1] }, $_[0] }
    sub FREEZE { $_[0]->{n} }
    package TA::Inner;
    sub new { bless { v => $_[1] }, $_[0] }
    sub FREEZE { $_[0]->{v} }
    package TA::Outer;
    our $INNER_BLOB;
    sub new { bless {}, $_[0] }
    sub FREEZE { $INNER_BLOB }
}
my $allow_encoder = Sereal::Encoder->new( { freeze_callbacks => 1 } );
$feature_packets{'thaw_allow/allowed'} = $allow_encoder->encode( TA::Allowed->new(11) );
$feature_packets{'thaw_allow/denied'} = $allow_encoder->encode( TA::Denied->new(22) );
$feature_packets{'thaw_allow/mixed'} = $allow_encoder->encode(
    [ TA::Allowed->new(1), TA::Denied->new(2) ] );
my $allowed_repeated = TA::Allowed->new(42);
$feature_packets{'thaw_allow/refint'} = $allow_encoder->encode(
    [ $allowed_repeated, $allowed_repeated ] );
$TA::Outer::INNER_BLOB = $allow_encoder->encode( TA::Inner->new(99) );
$feature_packets{'thaw_allow/inner'} = $TA::Outer::INNER_BLOB;
$feature_packets{'thaw_allow/reentrant'} = $allow_encoder->encode( TA::Outer->new );

{
    package FooInside;
    sub new {
        my ( $class, $attrs, @inside ) = @_;
        return bless {
            attrs  => { %{ $attrs || {} } },
            inside => { map { $_ => $inside[$_] } 0 .. $#inside },
        }, $class;
    }
    sub FREEZE {
        my $self = shift;
        return {
            attrs  => $self->{attrs},
            inside => [ map { $self->{inside}{$_} } sort { $a <=> $b } keys %{ $self->{inside} } ],
        };
    }
    package FooOutside;
    sub new {
        my ( $class, $attrs, @inside ) = @_;
        return bless {
            attrs  => { %{ $attrs || {} } },
            inside => { map { $_ => $inside[$_] } 0 .. $#inside },
        }, $class;
    }
    sub FREEZE {
        my $self = shift;
        return {
            attrs  => $self->{attrs},
            inside => [ map { $self->{inside}{$_} } sort { $a <=> $b } keys %{ $self->{inside} } ],
        };
    }
}
my $order_struct = FooOutside->new(
    { attr1 => 'foobar', bar => FooInside->new( {}, 'should_be_first' ) },
    FooInside->new( {}, 'second/not_frozen', 'third/not_frozen' ),
    FooOutside->new( {}, FooInside->new( {}, 'fourth/inner' ) ),
);
$feature_packets{'thaw/order'} = Sereal::Encoder::encode_sereal(
    $order_struct, { freeze_callbacks => 1, canonical => 1 } );

{
    package Mutator;
    sub FREEZE { 'f' }
}
my $callback_body = { map { ( "key$_" => "value$_" x 20 ) } 1 .. 200 };
$feature_packets{'header_callback_input_mutation'} =
    Sereal::Encoder->new( { freeze_callbacks => 1 } )->encode(
    $callback_body, [ bless( {}, 'Mutator' ) ] );

my $large_size_data = { a => [ ( "x" x 100 ) x 10 ] };
$feature_packets{'huge_decompressed_size/normal_zlib'} = Sereal::Encoder->new( {
    compress           => Sereal::Encoder::SRL_ZLIB(),
    compress_threshold => 0,
} )->encode($large_size_data);

my $zipbomb_data = "\0" x ( 10 * 1024 * 1024 + 1 );
for my $case (
    [ snappy => Sereal::Encoder::SRL_SNAPPY() ],
    [ zlib   => Sereal::Encoder::SRL_ZLIB() ],
    [ zstd   => eval { Sereal::Encoder::SRL_ZSTD() } ],
    )
{
    my ( $name, $compress ) = @$case;
    next unless defined $compress;
    $feature_packets{"zipbomb/$name"} = Sereal::Encoder->new( {
        compress           => $compress,
        compress_threshold => 0,
    } )->encode($zipbomb_data);
}
$feature_packets{'zipbomb/zlib_short'} = Sereal::Encoder->new( {
    compress           => Sereal::Encoder::SRL_ZLIB(),
    compress_threshold => 0,
} )->encode( 'a' x 10_000 );

for my $profile (
    [ plain                  => {} ],
    [ no_shared_hk           => { no_shared_hashkeys     => 1 } ],
    [ dedupe_strings         => { dedupe_strings         => 1 } ],
    [ aliased_dedupe_strings => { aliased_dedupe_strings => 1 } ],
    )
{
    my ( $profile_name, $options ) = @$profile;
    my %values;
    for my $string ( "0", "1", "100", "10.01", ".01" ) {
        for my $prefix ( map { ( $_, " $_" ) } "", "0", "-", "-0" ) {
            for my $suffix ( map { ( $_, "$_ " ) } "", "0" ) {
                $values{"str num '$prefix$string$suffix'"} = "$prefix$string$suffix";
            }
        }
    }
    $values{"num: $_"} = $_ for ( map { ( " $_", "$_ ", " $_ " ) } ( qw(0e0 3e3), "0 but true" ) );
    $values{false} = !1;
    $values{true}  = !0;
    for my $value ( values %values ) {
        no warnings;
        my $integer = int $value;
        my $float = $value + 0.5;
        my $stringified = "" . $value;
    }
    for my $key ( keys %values ) {
        no warnings;
        $values{"$key (PN)"} = "" . $values{$key};
        my $float = $values{"$key (PN)"} + 0.5;
        $values{"$key (PI)"} = "" . $values{$key};
        my $integer = int $values{"$key (PI)"};
    }
    $values{'raw false'} = !1;
    $values{'raw true'} = !0;
    my $encoder = Sereal::Encoder->new( { protocol_version => 3, %$options } );
    for my $name ( sort keys %values ) {
        $feature_packets{"flags/$profile_name/$name"} = $encoder->encode( $values{$name} );
    }
}

my $issue15_encoder = Sereal::Encoder->new( { snappy_incr => 1, snappy_threshold => 1 } );
$feature_packets{'issue15/single'} = $issue15_encoder->encode('foo');
$feature_packets{'issue15/concatenated'} =
    $issue15_encoder->encode('foo') . $issue15_encoder->encode('foo');

{
    package Foo;
    sub FREEZE { Sereal::Encoder->new->encode( $_[0]->{a} ) }
}
my $reentrant_value = [ bless( { a => 42 }, 'Foo' ) ];
push @$reentrant_value, $reentrant_value;
$feature_packets{'reentrancy/freeze_thaw'} =
    Sereal::Encoder->new( { freeze_callbacks => 1 } )->encode($reentrant_value);
my $plain_value = 'checksummed Sereal payload';
my $key0 = 'Out damned spot!';
my $key1 = 'Call me Ishmael.';
$feature_packets{'checksum/v0'} = Sereal::Encoder->new( {
    protocol_version => 6,
    checksum_key_v1  => $key0,
} )->encode($plain_value);
$feature_packets{'checksum/v1-compressed'} = Sereal::Encoder->new( {
    protocol_version   => 6,
    checksum_key_v2    => $key1,
    compress           => Sereal::Encoder::SRL_ZLIB(),
    compress_threshold => 1,
} )->encode( $plain_value x 100 );
$feature_packets{'checksum/plain'} = Sereal::Encoder->new( { protocol_version => 6 } )
    ->encode($plain_value);
for my $version ( 1, 2 ) {
    $feature_packets{"checksum/v$version"} = Sereal::Encoder->new( {
        protocol_version => 6,
        ( "checksum_key_v" . ( $version + 1 ) ) => $key1,
    } )->encode($plain_value);
}

my $manifest_hash = write_packet_set(
    $root,
    'decoder-regressions',
    Sereal::Encoder->VERSION,
    \%packets,
);
print "Wrote decoder-regressions manifest $manifest_hash using Sereal::Encoder ",
    Sereal::Encoder->VERSION, "\n";
my $feature_manifest = write_packet_set(
    $root,
    'decoder-features',
    Sereal::Encoder->VERSION,
    \%feature_packets,
);
print "Wrote decoder-features manifest $feature_manifest\n";
if ( ( $Config{usequadmath} || '' ) eq 'define' ) {
    my $value = 1.000000000000000000000000000001;
    my $packet = Sereal::Encoder->new( { protocol_version => 5 } )->encode($value);
    die "Encoder did not emit FLOAT_128 for the quadmath fixture\n"
        unless substr( $packet, 6, 1 ) eq chr(SRL_HDR_FLOAT_128);
    my $quadmath_manifest = write_packet_set(
        $root,
        'quadmath-encoder',
        Sereal::Encoder->VERSION,
        { 'float128/precision' => $packet },
    );
    print "Wrote quadmath-encoder manifest $quadmath_manifest\n";
}
my %regexp_packets;
for my $version (1 .. 6) {
    my $encoder = Sereal::Encoder->new({ protocol_version => $version });
    for my $case (regexp_modifier_test_cases()) {
        my ($modifiers, $pattern) = @$case;
        my $regexp = eval 'qr/' . $pattern . '/' . $modifiers;
        die "Cannot compile corpus regexp /$pattern/$modifiers: $@" if $@;
        my $name = "v$version/" . unpack('H*', $modifiers) . '/' . unpack('H*', $pattern);
        $regexp_packets{$name} = $encoder->encode($regexp);
    }
}
write_packet_set($root, 'regexp-modifiers', Sereal::Encoder->VERSION, \%regexp_packets);
print "Wrote regexp-modifiers corpus\n";

prune_unreferenced($root);
pack_corpus($root);
