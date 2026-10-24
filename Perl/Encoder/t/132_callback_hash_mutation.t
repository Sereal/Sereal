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

{

    package ReplaceEarlierKey;
    sub TIESCALAR { bless $_[1], $_[0] }
    sub FETCH     { $_[0]->() }
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
            ok(
                !$error || $error =~ /input (?:array|hash|container) was modified during encoding/,
                "$label: sibling deletion fails safely or encodes"
            ) or diag($error);
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
            ok(
                !$error || $error =~ /input (?:array|hash|container) was modified during encoding/,
                "$label: tied-value mutation fails safely or encodes"
            ) or diag($error);
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

# Runtime-created keys avoid retaining them as compiled constants.
sub changing_key {
    my ( $text, $mode ) = @_;
    my $key = pack( 'a32', $text );
    if    ( $mode eq 'wide' )  { $key .= "\x{100}" }
    elsif ( $mode eq 'latin' ) { $key .= "\x{e9}"; utf8::upgrade($key) }
    return $key;
}

# A deleted key's allocation must remain alive while its address identifies
# a COPY target, so a new key cannot reuse the cached pointer.
for my $version ( 1 .. 6 ) {
    for my $sort (@sorts) {
        for my $mode (qw(binary wide latin)) {
            for my $no_shared ( 0, 1 ) {
                my $encoder = Sereal::Encoder->new( {
                    protocol_version   => $version, sort_keys => $sort,
                    no_shared_hashkeys => $no_shared,
                } );
                for my $iteration ( 1 .. 8 ) {
                    my %first = ( changing_key( "old-$iteration", $mode ) => 111 );
                    my %second;
                    my @data = ( \%first, undef, \%second );
                    my $ran  = 0;
                    tie $data[1], 'ReplaceEarlierKey', sub {
                        if ( !$ran++ ) {
                            %first  = ();
                            %second = ( changing_key( "new-$iteration", $mode ) => 222 );
                        }
                        return 0;
                    };
                    my $decoded = Sereal::Decoder->new->decode( $encoder->encode( \@data ) );
                    is_deeply(
                        $decoded, [
                            { changing_key( "old-$iteration", $mode ) => 111 },
                            0,
                            { changing_key( "new-$iteration", $mode ) => 222 },
                        ],
                        "v$version sort=$sort $mode no_shared=$no_shared pass=$iteration: deleted key cannot be reused as another COPY target"
                    );
                }
            }
        }
    }
}
done_testing();
