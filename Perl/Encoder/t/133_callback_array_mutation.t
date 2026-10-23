#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;

plan skip_all => 'Did not find right version of decoder' if !have_encoder_and_decoder();
our ( @parent, $action );

sub mutate {
    if    ( $action eq 'grow' )   { push @parent, ('extra') x 10000 }
    elsif ( $action eq 'clear' )  { @parent = () }
    elsif ( $action eq 'shrink' ) { $#parent = 0 }
    elsif ( $action eq 'shift' )  { shift @parent }
    return 'frozen';
}
{

    package ArrayFreeze;
    sub FREEZE { main::mutate() }
    sub THAW   { bless {}, $_[0] }
}
{

    package ArrayString;
    use overload '""' => sub { main::mutate() }, fallback => 1;
}
{

    package ArrayFetch;
    sub TIESCALAR { bless {}, $_[0] }
    sub FETCH     { main::mutate() }
}

for my $version ( 1 .. 6 ) {
    for $action (qw(grow clear shrink shift)) {
        for my $kind (qw(freeze stringify tied nested)) {
            next if $kind eq 'freeze' && $version == 1;
            my %options = ( protocol_version => $version );
            my $expected;
            @parent = ( undef, 'second', 'third' );
            if ( $kind eq 'freeze' ) {
                $parent[0]                 = bless {}, 'ArrayFreeze';
                $options{freeze_callbacks} = 1;
                $expected                  = [ bless( {}, 'ArrayFreeze' ), 'second', 'third' ];
            }
            elsif ( $kind eq 'stringify' ) {
                $parent[0]                  = bless sub { }, 'ArrayString';
                $options{stringify_unknown} = 1;
                $expected                   = [ 'frozen', 'second', 'third' ];
            }
            elsif ( $kind eq 'tied' ) {
                tie $parent[0], 'ArrayFetch';
                $expected = [ 'frozen', 'second', 'third' ];
            }
            else {
                my %inner;
                tie $inner{value}, 'ArrayFetch';
                $parent[0] = \%inner;
                $expected = [ { value => 'frozen' }, 'second', 'third' ];
            }
            my $label   = "v$version $kind $action";
            my $decoded = eval {
                Sereal::Decoder->new->decode(
                    Sereal::Encoder->new( \%options )->encode( \@parent ) );
            };
            my $error = $@;
            ok( !$error, "$label: array mutation does not invalidate traversal" ) or diag($error);
            SKIP: {
                skip 'encode/decode failed', 1 if $error;
                is_deeply( $decoded, $expected, "$label: captured elements survive" );
            }
        }
    }
}

# Retaining elements must preserve aliases and holes as well as values.
my $value  = 'shared';
my @sparse = ( \$value, undef, \$value );
delete $sparse[1];
my $out = Sereal::Decoder->new->decode( Sereal::Encoder->new->encode( \@sparse ) );
is_deeply( $out, [ \$value, undef, \$value ], 'snapshot retains sparse array values' );
${ $out->[0] } = 'changed';
is( ${ $out->[2] }, 'changed', 'snapshot preserves repeated scalar references' );
done_testing();
