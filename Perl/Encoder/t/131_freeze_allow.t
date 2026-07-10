#!perl
use strict;
use warnings;

use File::Spec;
use Test::More;
use Data::Dumper;

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of decoder';
}

our ( %FREEZE_CALLED, %THAW_CALLED );

# Two classes with FREEZE/THAW plus one plain blessed class (no FREEZE).
{

    package FA::Allowed;
    sub new    { bless { n => $_[1] }, $_[0] }
    sub FREEZE { $FREEZE_CALLED{'FA::Allowed'}++; return $_[0]->{n} }
    sub THAW   { $THAW_CALLED{'FA::Allowed'}++; my ( $c, $s, $n ) = @_; return $c->new($n) }

    package FA::Denied;
    sub new    { bless { n => $_[1] }, $_[0] }
    sub FREEZE { $FREEZE_CALLED{'FA::Denied'}++; return $_[0]->{n} }
    sub THAW   { $THAW_CALLED{'FA::Denied'}++; my ( $c, $s, $n ) = @_; return $c->new($n) }

    package FA::Plain;
    sub new { bless { n => $_[1] }, $_[0] }    # no FREEZE method
}

my $dec = Sereal::Decoder->new();

# 1. freeze_allow_classes implies freeze support: allowed class is FROZEN and
#    round-trips through THAW.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $enc = Sereal::Encoder->new( { freeze_allow_classes => ['FA::Allowed'] } );
    my $blob = $enc->encode( FA::Allowed->new(11) );
    ok( $FREEZE_CALLED{'FA::Allowed'}, 'allowed class: FREEZE was invoked (freeze support auto-enabled)' );
    my $o = $dec->decode($blob);
    isa_ok( $o, 'FA::Allowed', 'allowed class round-trips' );
    ok( $THAW_CALLED{'FA::Allowed'}, '  ... via THAW' );
    is( $o->{n}, 11, '  ... with correct data' );
}

# 2. Disallowed class that has a FREEZE method croaks by default.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $enc = Sereal::Encoder->new( { freeze_allow_classes => ['FA::Allowed'] } );
    ok( !eval { $enc->encode( FA::Denied->new(22) ); 1 },
        'disallowed class croaks (default action)' );
    like( $@, qr/FREEZE not allowed for class 'FA::Denied'/, '  ... with a helpful message' );
    ok( !$FREEZE_CALLED{'FA::Denied'}, '  ... and FREEZE was never invoked' );
}

# 3. freeze_deny_action => 'serialize' serializes the disallowed object normally.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $enc = Sereal::Encoder->new(
        {   freeze_allow_classes => ['FA::Allowed'],
            freeze_deny_action   => 'serialize',
        }
    );
    my $blob = $enc->encode( FA::Denied->new(22) );
    ok( !$FREEZE_CALLED{'FA::Denied'}, 'serialize action: FREEZE not invoked for disallowed class' );
    my $o = $dec->decode($blob);
    isa_ok( $o, 'FA::Denied', 'serialize action: object serialized as a plain blessed ref' );
    ok( !$THAW_CALLED{'FA::Denied'}, '  ... and THAW was not called' );
    is( $o->{n}, 22, '  ... structure preserved' );
}

# 4. Classes without a FREEZE method are unaffected by the allow-list.
{
    my $enc = Sereal::Encoder->new( { freeze_allow_classes => ['FA::Allowed'] } );
    my $blob = $enc->encode( FA::Plain->new(33) );
    my $o    = $dec->decode($blob);
    isa_ok( $o, 'FA::Plain', 'class without FREEZE is unaffected' );
    is( $o->{n}, 33, '  ... structure preserved' );
}

# 5. Hash-ref and code-ref allow-list forms.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $enc = Sereal::Encoder->new( { freeze_allow_classes => { 'FA::Allowed' => 1 } } );
    isa_ok( $dec->decode( $enc->encode( FA::Allowed->new(1) ) ), 'FA::Allowed', 'hash-ref allow works' );
    ok( !eval { $enc->encode( FA::Denied->new(2) ); 1 }, 'hash-ref allow: false/absent disallows' );
}
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my @seen;
    my $enc = Sereal::Encoder->new(
        { freeze_allow_classes => sub { push @seen, [ $_[0], ref $_[1] ]; $_[0] eq 'FA::Allowed' } } );
    isa_ok( $dec->decode( $enc->encode( FA::Allowed->new(1) ) ), 'FA::Allowed', 'code-ref allow works' );
    is_deeply( $seen[0], [ 'FA::Allowed', 'FA::Allowed' ],
        '  ... predicate received class name and the object' );
    ok( !eval { $enc->encode( FA::Denied->new(2) ); 1 }, 'code-ref allow: predicate false disallows' );
}

# 6. Bad option values are rejected at construction.
{
    ok( !eval { Sereal::Encoder->new( { freeze_allow_classes => 'scalar' } ); 1 },
        'invalid freeze_allow_classes type croaks' );
    ok( !eval {
            Sereal::Encoder->new(
                { freeze_allow_classes => ['FA::Allowed'], freeze_deny_action => 'nope' } );
            1;
        },
        'invalid freeze_deny_action croaks'
    );
}

# 7. A structure mixing an allowed and a disallowed object.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $enc = Sereal::Encoder->new( { freeze_allow_classes => ['FA::Allowed'] } );
    ok( !eval { $enc->encode( [ FA::Allowed->new(1), FA::Denied->new(2) ] ); 1 },
        'mixed structure: a single disallowed member croaks the whole encode' );

    my $ser = Sereal::Encoder->new(
        { freeze_allow_classes => ['FA::Allowed'], freeze_deny_action => 'serialize' } );
    my $out = $dec->decode( $ser->encode( [ FA::Allowed->new(1), FA::Denied->new(2) ] ) );
    isa_ok( $out->[0], 'FA::Allowed', 'mixed serialize: allowed member frozen+thawed' );
    isa_ok( $out->[1], 'FA::Denied',  'mixed serialize: disallowed member serialized normally' );
    ok( !$THAW_CALLED{'FA::Denied'}, '  ... and the disallowed member never THAWed' );
}

# 8. Referential integrity: an allowed frozen object referenced twice is FROZEN
#    once and round-trips to a single shared instance.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $enc = Sereal::Encoder->new( { freeze_allow_classes => ['FA::Allowed'] } );
    my $a   = FA::Allowed->new(42);
    my $out = $dec->decode( $enc->encode( [ $a, $a ] ) );
    isa_ok( $out->[0], 'FA::Allowed', 'refint: allowed frozen object thawed' );
    is( $out->[0], $out->[1], 'refint: both references are the same thawed object' );
    is( $FREEZE_CALLED{'FA::Allowed'}, 1, 'refint: FREEZE was invoked only once' );
}

# 9. The functional interface honors the allow-list too.
{
    local ( %FREEZE_CALLED, %THAW_CALLED );
    my $blob = Sereal::Encoder::encode_sereal( FA::Allowed->new(7),
        { freeze_allow_classes => ['FA::Allowed'] } );
    isa_ok( $dec->decode($blob), 'FA::Allowed', 'encode_sereal honors freeze_allow_classes' );
    ok( !eval {
            Sereal::Encoder::encode_sereal( FA::Denied->new(8),
                { freeze_allow_classes => ['FA::Allowed'] } );
            1;
        },
        'encode_sereal enforces the deny action (croak)'
    );
}

# 10. Re-entrancy: a FREEZE hook that re-invokes the same encoder must see the
#     allow-list on the cloned encoder struct.
{

    package FA::ReInner;
    sub new    { bless { v => $_[1] }, $_[0] }
    sub FREEZE { $FREEZE_CALLED{'FA::ReInner'}++; return $_[0]->{v} }
    sub THAW   { my ( $c, $ser, $v ) = @_; return $c->new($v) }

    package FA::ReOuter;
    our $ENC;
    sub new    { bless {}, $_[0] }
    sub FREEZE { return $ENC->encode( FA::ReInner->new(7) ) }    # re-entrant encode
    sub THAW   { my ( $c, $ser, $blob ) = @_; return bless { inner_blob => $blob }, $c }
}
{
    local ( %FREEZE_CALLED, %THAW_CALLED );

    # FA::ReInner is NOT allowed: the nested (cloned) encode must enforce it.
    $FA::ReOuter::ENC = Sereal::Encoder->new( { freeze_allow_classes => ['FA::ReOuter'] } );
    ok( !eval { $FA::ReOuter::ENC->encode( FA::ReOuter->new ); 1 },
        're-entrant clone enforces the allow-list' );
    like( $@, qr/FREEZE not allowed for class 'FA::ReInner'/,
        '  ... naming the disallowed nested class' );

    # Now allow the nested class too: the nested encode succeeds.
    $FA::ReOuter::ENC =
        Sereal::Encoder->new( { freeze_allow_classes => [ 'FA::ReOuter', 'FA::ReInner' ] } );
    my $blob = eval { $FA::ReOuter::ENC->encode( FA::ReOuter->new ) };
    ok( defined $blob && length $blob, 're-entrant clone allows a listed nested class' );
    ok( $FREEZE_CALLED{'FA::ReInner'}, '  ... and the nested FREEZE actually ran' );
}

done_testing();
