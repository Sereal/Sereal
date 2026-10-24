#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;
plan skip_all => 'decoder unavailable' unless have_encoder_and_decoder();

our $callback;
{

    package SerealSortedCursorFreeze;
    sub FREEZE { $main::callback->( $_[0]{key} ); return $_[0]{key} }
    sub THAW   { bless { key => $_[2] }, $_[0] }
}

for my $version ( 2 .. 6 ) {
    for my $sort ( 1 .. 3 ) {
        my $encoder = Sereal::Encoder->new( {
            protocol_version => $version, sort_keys => $sort, freeze_callbacks => 1,
        } );
        for my $mutation (qw(none replace_value delete_future replace_future delete_current)) {
            my %input = map {
                my $key = sprintf 'key-%02d', $_;
                ( $key => bless( { key => $key }, 'SerealSortedCursorFreeze' ) )
            } 1 .. 20;
            my $ran = 0;
            my $victim;
            local $callback = sub {
                my ($current) = @_;
                return if $ran++;
                ($victim) = grep { $_ ne $current } sort keys %input;
                if    ( $mutation eq 'replace_value' ) { $input{$victim} = 'new value' }
                elsif ( $mutation eq 'delete_future' ) { delete $input{$victim} }
                elsif ( $mutation eq 'replace_future' ) {
                    delete $input{$victim};
                    $input{'replacement'} = 'replacement';
                }
                elsif ( $mutation eq 'delete_current' ) {
                    delete $input{$current};
                    $input{'replacement'} = 'replacement';
                }
            };
            my $packet = eval { $encoder->encode( \%input ) };
            my $error  = $@;
            my $label  = "v$version sort=$sort $mutation";
            if ( $mutation eq 'none' || $mutation eq 'replace_value' ) {
                is( $error, '', "$label encodes" );
                my $decoded = eval { Sereal::Decoder->new->decode($packet) };
                is_deeply( $decoded, \%input, "$label reads values at use without snapshots" );
            }
            else {
                like(
                    $error, qr/input hash was modified during encoding/,
                    "$label rejects stale iteration state"
                );
            }
            is_deeply(
                Sereal::Decoder->new->decode( $encoder->encode( [7] ) ), [7],
                "$label encoder remains reusable"
            );
        }
    }
}
done_testing;
