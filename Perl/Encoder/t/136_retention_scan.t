#!perl
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw(refaddr weaken);
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;

plan skip_all => 'decoder is unavailable' unless have_encoder_and_decoder();

our ( $parent, $fetches );
{
    package DeepRetentionFetch;
    sub TIESCALAR { bless {}, $_[0] }
    sub FETCH { ++$main::fetches; @$main::parent = (); return 'fetched' }
}

for my $version ( 1 .. 6 ) {
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    my $plain = [ { a => [ 1, 2 ], b => { c => 3 } }, [ 4, 5 ] ];
    my $before = $encoder->encode($plain);
    is_deeply( Sereal::Decoder->new->decode($before), $plain,
        "v$version plain nested input round trips" );

    # A fixed-depth check would miss this FETCH. The scan must also avoid
    # running it early: it clears the outer array before encoding can begin.
    for my $depth ( 1, 16, 256 ) {
        my @leaf = (undef);
        tie $leaf[0], 'DeepRetentionFetch';
        my $nested = \@leaf;
        $nested = [ $nested ] for 1 .. $depth;
        $parent = [ $nested, 'second', 'third' ];
        $fetches = 0;
        my $decoded = Sereal::Decoder->new->decode( $encoder->encode($parent) );
        is( scalar @$decoded, 3, "v$version depth=$depth retains outer siblings" );
        is_deeply( [ @$decoded[ 1, 2 ] ], [ 'second', 'third' ],
            "v$version depth=$depth preserves sibling values" );
        my $value = $decoded->[0];
        $value = $value->[0] for 1 .. $depth;
        is( $value->[0], 'fetched', "v$version depth=$depth encodes the magical leaf" );
        ok( $fetches > 0, "v$version depth=$depth invokes FETCH during encoding" );
        is_deeply( $parent, [], "v$version depth=$depth callback changed its input" );
    }

    is( $encoder->encode($plain), $before,
        "v$version reuse returns to the plain-input path after callbacks" );

    my $shared = [ 7, 8 ];
    my $cycle = [ $shared, $shared ];
    push @$cycle, $cycle;
    my $decoded = Sereal::Decoder->new->decode( $encoder->encode($cycle) );
    is( refaddr($decoded->[0]), refaddr($decoded->[1]), "v$version keeps shared references" );
    is( refaddr($decoded->[2]), refaddr($decoded), "v$version handles cycles" );
    pop @$cycle;
    pop @$decoded;

    # A weak edge may close a cycle without increasing the referent count.
    my $weak_cycle = [];
    push @$weak_cycle, $weak_cycle;
    weaken($weak_cycle->[0]);
    $decoded = Sereal::Decoder->new->decode( $encoder->encode($weak_cycle) );
    is( refaddr($decoded->[0]), refaddr($decoded), "v$version handles weak cycles" );
}

SKIP: {
    # Refaliasing can make a shared RV point at a singly referenced AV.
    # Looking only at the AV's reference count would miss this cycle.
    my $alias_cycle = eval q{
        use feature 'refaliasing';
        no warnings 'experimental::refaliasing';
        sub {
            my $root = [];
            \($root->[0]) = \$root;
            my $packet = Sereal::Encoder->new->encode($root);
            delete $root->[0];
            return Sereal::Decoder->new->decode($packet);
        }
    };
    skip 'refaliasing is unavailable on this Perl', 1 unless $alias_cycle;
    my $decoded = $alias_cycle->();
    is( refaddr($decoded->[0]), refaddr($decoded), 'handles a cycle through an aliased RV' );
    pop @$decoded;
}

{
    # Header callbacks can modify the body before it is encoded. The scan
    # must inspect both roots before choosing the retention mode.
    package HeaderRetentionFetch;
    sub TIESCALAR { bless {}, $_[0] }
    sub FETCH { push @$main::parent, 'from header'; return 'header' }
}
for my $version ( 2 .. 6 ) {
    $parent = ['body'];
    my $header;
    tie $header, 'HeaderRetentionFetch';
    my $encoder = Sereal::Encoder->new( { protocol_version => $version } );
    my $packet = $encoder->encode( $parent, \$header );
    my $decoded = Sereal::Decoder->new->decode($packet);
    is_deeply( $decoded, $parent, "v$version header callbacks run before body encoding" );
    ok( @$decoded > 1, "v$version header FETCH actually changed the body" );
}

done_testing();
