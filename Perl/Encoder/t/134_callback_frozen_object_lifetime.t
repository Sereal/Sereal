#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

# FREEZE and stringification callbacks run in the middle of serializing the
# container that holds them. t/133 covers callbacks that mutate the array in
# place (grow, clear, shrink, shift). This file covers callbacks that free
# the array's storage (undef) or force it to be reallocated, and a FREEZE
# that drops the last reference to the very object being frozen, which used
# to leave the encoder reading a freed object.

our @parent;
our %cache;
our ( $predicate_freezes, $predicate_destroys );
{

    package ClearParentFreeze;
    sub FREEZE { $main::clear->(); return $_[0]{name} }
    sub THAW   { my ( $class, $serializer, $name ) = @_; bless { name => $name }, $class }

    package ClearParentString;
    use overload '""' => sub { $_[0]->() }, fallback => 1;

    package DropSelfFreeze;
    sub FREEZE { delete $main::cache{obj}; return 'dropped' }
    sub THAW   { my ( $class, $serializer, $name ) = @_; bless { name => $name }, $class }

    package PredicateVictim;
    sub FREEZE  { $main::predicate_freezes++;             return $_[0]{name} }
    sub THAW    { my ( $class, $serializer, $name ) = @_; bless { name => $name }, $class }
    sub DESTROY { $main::predicate_destroys++ }

    package AllowCbFreeze;
    sub FREEZE { return 'allowed' }
    sub THAW   { my ( $class, $serializer, $name ) = @_; bless { name => $name }, $class }
}

my @filler = map { "element $_ " x 8 } 1 .. 50;
my %clear = (
    undef   => sub { undef @parent },
    realloc => sub { @parent = ( ("y" x 30) x 400 ) },
);

for my $version ( 2 .. 6 ) {
    for my $how ( sort keys %clear ) {
        our $clear = $clear{$how};
        for my $option (qw(freeze_callbacks stringify_unknown)) {
            my $expected;
            if ( $option eq 'freeze_callbacks' ) {
                @parent   = ( bless( { name => 'first' }, 'ClearParentFreeze' ), @filler );
                $expected = [ bless( { name => 'first' }, 'ClearParentFreeze' ), @filler ];
            }
            else {
                @parent   = ( bless( sub { $clear->(); 'first value' }, 'ClearParentString' ), @filler );
                $expected = [ 'first value', @filler ];
            }
            my $decoded = eval {
                my $encoder = Sereal::Encoder->new( { protocol_version => $version, $option => 1 } );
                Sereal::Decoder->new->decode( $encoder->encode( \@parent ) );
            };
            my $error = $@;
            my $label = "v$version $how $option";
            ok( !$error || $error =~ /input (?:array|hash|container) was modified during encoding/, "$label: freeing the array from a callback fails safely or encodes" ) or diag($error);
            SKIP: {
                skip 'encode/decode failed', 1 if $error;
                is_deeply( $decoded, $expected, "$label: captured elements survive the callback" );
            }
        }
    }

    # The object being frozen may lose its last reference inside FREEZE.
    %cache = ( obj => bless( {}, 'DropSelfFreeze' ) );
    my $decoded = eval {
        my $encoder = Sereal::Encoder->new( { protocol_version => $version, freeze_callbacks => 1 } );
        Sereal::Decoder->new->decode( $encoder->encode( $cache{obj} ) );
    };
    my $error = $@;
    ok( !$error, "v$version: FREEZE dropping the last reference to its object still encodes" ) or diag($error);
    SKIP: {
        skip 'encode/decode failed', 2 if $error;
        is( ref($decoded),      'DropSelfFreeze', "v$version: the dropped object keeps its class" );
        is( $decoded->{name},   'dropped',        "v$version: and its frozen payload" );
    }

    # The freeze_allow_classes callback is user code too, and runs before
    # FREEZE; it may also drop the last reference to the object.
    {
        my @warnings;
        local $SIG{__WARN__} = sub { push @warnings, @_ };
        %cache = ( obj => bless( {}, 'AllowCbFreeze' ) );
        my $decoded = eval {
            my $encoder = Sereal::Encoder->new( {
                protocol_version     => $version,
                freeze_callbacks     => 1,
                freeze_allow_classes => sub { delete $main::cache{obj}; 1 },
            } );
            Sereal::Decoder->new->decode( $encoder->encode( $cache{obj} ) );
        };
        my $error = $@;
        ok( !$error, "v$version: allow-classes callback dropping the object still encodes" ) or diag($error);
        SKIP: {
            skip 'encode/decode failed', 2 if $error;
            is( ref($decoded),    'AllowCbFreeze', "v$version: the dropped object keeps its class" );
            is( $decoded->{name}, 'allowed',       "v$version: and its frozen payload" );
        }
        is_deeply( \@warnings, [], "v$version: no warnings from the allow-classes callback case" );
    }
}

# The allow-list predicate runs before FREEZE, and may remove the same owning
# reference. Retention must cover acceptance, ordinary serialization and croak.
for my $version ( 2 .. 6 ) {
    for my $action (qw(allow serialize croak)) {
        ( $predicate_freezes, $predicate_destroys ) = ( 0, 0 );
        %cache = ( obj => bless( { name => 'retained' }, 'PredicateVictim' ) );
        my $encoder = Sereal::Encoder->new( {
            protocol_version     => $version,
            freeze_allow_classes => sub { delete $cache{obj}; return $action eq 'allow' },
            freeze_deny_action   => $action eq 'serialize' ? 'serialize' : 'croak',
        } );
        my $packet = eval { $encoder->encode( $cache{obj} ) };
        my $error  = $@;
        my $label  = "v$version predicate=$action";
        is_deeply( \%cache, {}, "$label: predicate removed the owning reference" );
        is( $predicate_destroys, 1, "$label: source object is released exactly once" );
        is(
            $predicate_freezes, $action eq 'allow' ? 1 : 0,
            "$label: FREEZE follows the predicate decision"
        );
        if ( $action eq 'croak' ) {
            like(
                $error, qr/FREEZE not allowed for class 'PredicateVictim'/,
                "$label: denial throws the intended exception"
            );
        }
        else {
            ok( !$error || $error =~ /input (?:array|hash|container) was modified during encoding/, "$label: encoding survives the predicate" ) or diag($error);
            my $decoded      = eval { Sereal::Decoder->new->decode($packet) };
            my $decode_error = $@;
            ok( !$decode_error, "$label: packet decodes" ) or diag($decode_error);
            SKIP: {
                skip 'encode/decode failed', 2 if $error || $decode_error;
                isa_ok( $decoded, 'PredicateVictim', "$label: object class survives" );
                is( $decoded->{name}, 'retained', "$label: object contents survive" );
            }
        }
    }
}
# Method lookup happens before the predicate. Removing or replacing the glob
# must not free the CV selected for this invocation.
{
    package MethodLifetime;
    sub THAW { my ($class, $format, $value) = @_; bless {value => $value}, $class }
}
my $freeze_name = 'MethodLifetime::FREEZE';
for my $version (2 .. 6) {
    for my $change (qw(delete replace)) {
        my $replacement_calls = 0;
        {
            no strict 'refs';
            no warnings 'redefine';
            *{$freeze_name} = sub { return 'selected' };
        }
        my $object = bless {}, 'MethodLifetime';
        my $encoder = Sereal::Encoder->new({
            protocol_version => $version,
            freeze_allow_classes => sub {
                delete $MethodLifetime::{FREEZE};
                if ($change eq 'replace') {
                    no strict 'refs';
                    no warnings 'redefine';
                    *{$freeze_name} = sub { $replacement_calls++; return 'replacement' };
                }
                return 1;
            },
        });
        my $decoded = eval { Sereal::Decoder->new->decode($encoder->encode($object)) };
        my $error = $@;
        ok(!$error, "v$version $change: method survives the predicate") or diag($error);
        SKIP: {
            skip 'encode/decode failed', 2 if $error;
            is($decoded->{value}, 'selected', "v$version $change: selected FREEZE is invoked");
            is($replacement_calls, 0, "v$version $change: replacement applies to future calls");
        }
    }
}
done_testing();
