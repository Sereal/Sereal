#!perl
use strict;
use warnings;

use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Test::More;
use Sereal::TestSet qw(:all);

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of encoder';
}

# Two classes with FREEZE/THAW so we can exercise the allow-list.
{

    package TA::Allowed;
    sub new  { bless { n => $_[1] }, $_[0] }
    sub FREEZE { my $self = shift; return $self->{n} }
    sub THAW   { my ( $class, $ser, $n ) = @_; return $class->new($n) }

    package TA::Denied;
    sub new  { bless { n => $_[1] }, $_[0] }
    sub FREEZE { my $self = shift; return $self->{n} }
    sub THAW   { my ( $class, $ser, $n ) = @_; return $class->new($n) }
}

my $enc = Sereal::Encoder->new( { freeze_callbacks => 1 } );
my $allowed_blob = $enc->encode( TA::Allowed->new(11) );
my $denied_blob  = $enc->encode( TA::Denied->new(22) );

# 1. No allow-list: everything thaws as before (backwards compatible).
{
    my $dec = Sereal::Decoder->new();
    my $o = $dec->decode($denied_blob);
    isa_ok( $o, 'TA::Denied', 'no allow-list thaws normally' );
    is( $o->{n}, 22, '  ... with correct data' );
}

# 2. Allow-list as an array ref: allowed class thaws, disallowed croaks.
{
    my $dec = Sereal::Decoder->new( { thaw_allow_classes => ['TA::Allowed'] } );
    my $o = $dec->decode($allowed_blob);
    isa_ok( $o, 'TA::Allowed', 'array-ref allow: allowed class thaws' );
    is( $o->{n}, 11, '  ... with correct data' );

    ok( !eval { $dec->decode($denied_blob); 1 },
        'array-ref allow: disallowed class croaks (default action)' );
    like( $@, qr/THAW not allowed for class 'TA::Denied'/,
        '  ... with a helpful message' );
}

# 3. Allow-list as a hash ref.
{
    my $dec = Sereal::Decoder->new(
        { thaw_allow_classes => { 'TA::Allowed' => 1, 'TA::Denied' => 0 } } );
    isa_ok( $dec->decode($allowed_blob), 'TA::Allowed', 'hash-ref allow: true value allows' );
    ok( !eval { $dec->decode($denied_blob); 1 },
        'hash-ref allow: false value disallows' );
}

# 4. Allow-list as a code ref predicate.
{
    my @seen;
    my $dec = Sereal::Decoder->new(
        {   thaw_allow_classes => sub { push @seen, $_[0]; $_[0] eq 'TA::Allowed' }
        }
    );
    isa_ok( $dec->decode($allowed_blob), 'TA::Allowed', 'code-ref allow: predicate true allows' );
    is_deeply( \@seen, ['TA::Allowed'], '  ... predicate received the class name' );
    ok( !eval { $dec->decode($denied_blob); 1 },
        'code-ref allow: predicate false disallows' );
}

# 5. thaw_deny_action => 'raw' yields the raw args instead of croaking.
{
    my $dec = Sereal::Decoder->new(
        {   thaw_allow_classes => ['TA::Allowed'],
            thaw_deny_action   => 'raw',
        }
    );
    isa_ok( $dec->decode($allowed_blob), 'TA::Allowed', 'raw action: allowed class still thaws' );
    my $raw = $dec->decode($denied_blob);
    isa_ok( $raw, 'Sereal::Decoder::THAW_args',
        'raw action: disallowed class yields raw THAW args' );
}

# 6. An explicit 'croak' action behaves like the default.
{
    my $dec = Sereal::Decoder->new(
        {   thaw_allow_classes => ['TA::Allowed'],
            thaw_deny_action   => 'croak',
        }
    );
    ok( !eval { $dec->decode($denied_blob); 1 },
        "explicit 'croak' action croaks on disallowed class" );
}

# 7. A bogus deny action and a bogus allow-list type are rejected at construction.
{
    ok( !eval { Sereal::Decoder->new( { thaw_deny_action => 'nope' } ); 1 },
        'invalid thaw_deny_action croaks' );
    ok( !eval { Sereal::Decoder->new( { thaw_allow_classes => 'scalar' } ); 1 },
        'invalid thaw_allow_classes type croaks' );
}

# 8. A structure mixing allowed and disallowed objects.
{
    my $mixed = $enc->encode( [ TA::Allowed->new(1), TA::Denied->new(2) ] );

    my $dec = Sereal::Decoder->new( { thaw_allow_classes => ['TA::Allowed'] } );
    ok( !eval { $dec->decode($mixed); 1 },
        'mixed structure: a single disallowed member croaks the whole decode' );

    my $raw = Sereal::Decoder->new(
        { thaw_allow_classes => ['TA::Allowed'], thaw_deny_action => 'raw' } );
    my $out = $raw->decode($mixed);
    isa_ok( $out->[0], 'TA::Allowed', 'mixed raw: allowed member is thawed' );
    isa_ok( $out->[1], 'Sereal::Decoder::THAW_args', 'mixed raw: disallowed member is kept raw' );
}

# 9. Referential integrity: several references to one allowed frozen object.
{
    my $a    = TA::Allowed->new(42);
    my $blob = $enc->encode( [ $a, $a ] );
    my $dec  = Sereal::Decoder->new( { thaw_allow_classes => ['TA::Allowed'] } );
    my $out  = $dec->decode($blob);
    isa_ok( $out->[0], 'TA::Allowed', 'refint: allowed object is thawed' );
    is( $out->[0]{n}, 42, '  ... with correct data' );
    is( $out->[0], $out->[1], 'refint: both references are the same thawed object' );
}

# 10. A global no_thaw_objects makes the allow-list moot (never consulted, never croaks).
{
    my $dec = Sereal::Decoder->new(
        { thaw_allow_classes => ['TA::Allowed'], no_thaw_objects => 1 } );
    my $out = eval { $dec->decode($denied_blob) };
    isa_ok( $out, 'Sereal::Decoder::THAW_args',
        'no_thaw_objects: disallowed class is not consulted and does not croak' );
}

# 11. refuse_objects still refuses everything, regardless of the allow-list.
{
    my $dec = Sereal::Decoder->new(
        { thaw_allow_classes => ['TA::Allowed'], refuse_objects => 1 } );
    ok( !eval { $dec->decode($allowed_blob); 1 },
        'refuse_objects overrides the allow-list (still refuses allowed classes)' );
}

# 12. Re-entrancy: a THAW hook that re-invokes the same decoder must see the
#     allow-list on the cloned decoder struct.
{

    package TA::Inner;
    sub new    { bless { v => $_[1] }, $_[0] }
    sub FREEZE { my $s = shift; return $s->{v} }
    sub THAW   { my ( $c, $ser, $v ) = @_; return $c->new($v) }

    package TA::Outer;
    our $DEC;
    our $INNER_BLOB;
    sub new    { bless {}, $_[0] }
    sub FREEZE { return $INNER_BLOB }    # carries a nested Sereal document
    sub THAW   { my ( $c, $ser, $blob ) = @_; return bless { inner => $DEC->decode($blob) }, $c }
}
{
    $TA::Outer::INNER_BLOB = $enc->encode( TA::Inner->new(99) );
    my $outer_blob = $enc->encode( TA::Outer->new );

    # TA::Inner is NOT allowed: the nested (cloned) decode must enforce it.
    $TA::Outer::DEC = Sereal::Decoder->new( { thaw_allow_classes => ['TA::Outer'] } );
    ok( !eval { $TA::Outer::DEC->decode($outer_blob); 1 },
        're-entrant clone enforces the allow-list' );
    like( $@, qr/THAW not allowed for class 'TA::Inner'/,
        '  ... naming the disallowed nested class' );

    # Now allow the nested class too: the whole thing round-trips.
    $TA::Outer::DEC = Sereal::Decoder->new( { thaw_allow_classes => [ 'TA::Outer', 'TA::Inner' ] } );
    my $o = $TA::Outer::DEC->decode($outer_blob);
    isa_ok( $o, 'TA::Outer', 're-entrant clone allows a listed nested class' );
    isa_ok( $o->{inner}, 'TA::Inner', '  ... nested object thawed' );
    is( $o->{inner}{v}, 99, '  ... with correct data' );
}

done_testing();
