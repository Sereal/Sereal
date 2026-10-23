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
our %parent;
{

    package RemoveSiblingFreeze;
    sub FREEZE { delete $main::parent{ $_[0]{victim} };    return $_[0]{victim} }
    sub THAW   { my ( $class, $serializer, $victim ) = @_; bless { victim => $victim }, $class }
}
{

    package RemoveSiblingString;
    use overload '""' => sub { $_[0]->() }, fallback => 1;
}
{

    package RemoveSiblingFetch;
    sub TIESCALAR { bless [ $_[1] ], $_[0] }
    sub FETCH     { delete $main::parent{ $_[0][0] }; return 'value' }
}

my @sorts = @ARGV ? @ARGV : ( 0 .. 3 );
for my $version ( 2 .. 6 ) {
    for my $sort (@sorts) {
        for my $option (qw(freeze_callbacks stringify_unknown)) {
            my $expected;
            if ( $option eq 'freeze_callbacks' ) {
                %parent = (
                    a => bless( { victim => 'b' }, 'RemoveSiblingFreeze' ),
                    b => bless( { victim => 'a' }, 'RemoveSiblingFreeze' ) );
                $expected = {
                    a => bless( { victim => 'b' }, 'RemoveSiblingFreeze' ),
                    b => bless( { victim => 'a' }, 'RemoveSiblingFreeze' ) };
            }
            else {
                %parent = (
                    a => bless( sub { delete $parent{b}; 'a value' }, 'RemoveSiblingString' ),
                    b => bless( sub { delete $parent{a}; 'b value' }, 'RemoveSiblingString' ) );
                $expected = { a => 'a value', b => 'b value' };
            }
            my $decoded = eval {
                my $encoder = Sereal::Encoder->new(
                    { protocol_version => $version, sort_keys => $sort, $option => 1 } );
                Sereal::Decoder->new->decode( $encoder->encode( \%parent ) );
            };
            my $error = $@;
            my $label = "v$version sort=$sort $option";
            ok( !$error, "$label: sibling deletion produces a complete hash" ) or diag($error);
            SKIP: {
                skip 'encode/decode failed', 2 if $error;
                is_deeply( $decoded, $expected, "$label: captured entries survive callbacks" );
                is_deeply( \%parent, {},        "$label: both callbacks ran" );
            }
        }
    }
}

# A non-magical hash can invoke Perl through tied values or their descendants,
# without freeze_callbacks or stringify_unknown being enabled.
for my $version ( 1 .. 6 ) {
    for my $sort (@sorts) {
        for my $nested ( 0, 1 ) {
            %parent = ( a => undef, b => undef );
            if ($nested) {
                for my $key (qw(a b)) {
                    my @inner = (undef);
                    tie $inner[0], 'RemoveSiblingFetch', $key eq 'a' ? 'b' : 'a';
                    $parent{$key} = \@inner;
                }
            }
            else {
                tie $parent{a}, 'RemoveSiblingFetch', 'b';
                tie $parent{b}, 'RemoveSiblingFetch', 'a';
            }
            my $decoded = eval {
                Sereal::Decoder->new->decode(
                    Sereal::Encoder->new( {
                            protocol_version => $version, sort_keys => $sort,
                        }
                    )->encode( \%parent ) );
            };
            my $error = $@;
            my $label = "v$version sort=$sort nested=$nested";
            ok( !$error, "$label: tied values cannot truncate a hash" ) or diag($error);
            SKIP: {
                skip 'encode/decode failed', 2 if $error;
                my $expected =
                    $nested ? { a => ['value'], b => ['value'] } : { a => 'value', b => 'value' };
                is_deeply( $decoded, $expected, "$label: captured entries survive FETCH" );
                is_deeply( \%parent, {},        "$label: both callbacks ran" );
            }
        }
    }
}
done_testing();
