#!perl
use strict;
use warnings;
use Test::More;
use Sereal::Decoder;
use lib 't/lib';
use Sereal::TestSet qw(Header short_string varint);

{

    package ThrowThaw;
    our @steps;
    sub THAW { push @steps, 'throw'; die "THAW failed deliberately\n" }

    package GoodThaw;
    our $destroyed = 0;
    sub THAW { push @ThrowThaw::steps, 'good'; return bless [], 'GoodResult' }

    package GoodResult;
    sub DESTROY { ++$GoodThaw::destroyed }

    package ArgMarker;
    our $destroyed = 0;
    sub DESTROY { ++$destroyed }

    package MissingThaw;
}

for my $version ( 2 .. 6 ) {
    my $header = Header($version);
    for my $weak_first ( 0, 1 ) {
        my $body = "\x46\x32" . short_string('ThrowThaw') . ( $weak_first ? "\x30" : '' ) . "\x28";
        my $bad_offset = length($body) + 1;
        $body .= "\xab\x01\x2c" . short_string('ArgMarker') . "\x40";
        $body .= "\x29" . chr($bad_offset) . "\x30\x29" . chr($bad_offset);
        $body .= "\x32" . short_string('GoodThaw') . ( $weak_first ? "\x30" : '' ) . "\x28";
        my $good_offset = length($body) + 1;
        $body .= "\xab\0\x29" . chr($good_offset) . "\x30\x29" . chr($good_offset);
        my $packet = $header . $body;

        for my $mode ( 'thaw', 'predicate', 'deny' ) {
            my %options;
            if ( $mode ne 'thaw' ) {
                $options{thaw_allow_classes} = sub {
                    return 1                              if $_[0] eq 'GoodThaw';
                    die "predicate failed deliberately\n" if $mode eq 'predicate';
                    return 0;
                };
            }
            my $dec   = Sereal::Decoder->new( \%options );
            my $label = "v$version weak_first=$weak_first $mode";
            for my $attempt ( 1 .. 3 ) {
                $ArgMarker::destroyed = $GoodThaw::destroyed = 0;
                @ThrowThaw::steps     = ();
                eval { $dec->decode($packet); 1 };
                my $error = $@;
                like(
                    $error, $mode eq 'deny' ? qr/THAW not allowed/ : qr/failed deliberately/,
                    "$label attempt $attempt reports callback failure"
                );
                is_deeply(
                    \@ThrowThaw::steps, $mode eq 'thaw' ? [ 'good', 'throw' ] : ['good'],
                    "$label first THAW completed before failure"
                );
                is( $ArgMarker::destroyed, 1, "$label releases failed object's arguments" );
                is( $GoodThaw::destroyed,  1, "$label releases already-thawed result once" );
                is( $dec->decode( $header . "\x07" ), 7, "$label decoder reusable after failure" );
                my $good = $dec->decode( $header . "\x32" . short_string('GoodThaw') . "\x40" );
                is( ref($good), 'GoodResult', "$label decoder can thaw again" );
                undef $good;
                is( $GoodThaw::destroyed, 2, "$label subsequent result released once" );
            }
        }
    }

    # Without REFP duplicates the class record is a bare stash, and no
    # fixup array can incidentally keep the popped primary reference alive.
    # An earlier queued object must also be cleaned up if a later one fails.
    for my $options ( {}, { set_readonly => 1 }, { set_readonly_scalars => 1 } ) {
        for my $mode ( 'thaw', 'predicate', 'deny', 'missing method' ) {
            my %options = %$options;
            if ( $mode eq 'predicate' || $mode eq 'deny' ) {
                $options{thaw_allow_classes} = sub {
                    die "predicate failed deliberately\n" if $mode eq 'predicate';
                    return 0;
                };
            }
            my $dec   = Sereal::Decoder->new( \%options );
            my $class = $mode eq 'missing method' ? 'MissingThaw' : 'ThrowThaw';
            my $frozen =
                "\x32" . short_string($class) . "\x41\x2c" . short_string('ArgMarker') . "\x40";
            my $error_pattern =
                  $mode eq 'missing method' ? qr/No THAW method defined/
                : $mode eq 'deny'           ? qr/THAW not allowed/
                :                             qr/failed deliberately/;
            for my $pending ( 0, 1 ) {
                my $packet = $header . ( $pending ? "\x42" . $frozen : '' ) . $frozen;
                my $label  = "v$version $mode pending=$pending " . join( ',', sort keys %options );
                for my $attempt ( 1 .. 3 ) {
                    $ArgMarker::destroyed = 0;
                    @ThrowThaw::steps     = ();
                    eval { $dec->decode($packet); 1 };
                    like( $@, $error_pattern, "$label attempt $attempt reports failure" );
                    is(
                        $ArgMarker::destroyed, 1 + $pending,
                        "$label releases primary and still-pending arguments"
                    );
                    is_deeply(
                        \@ThrowThaw::steps, $mode eq 'thaw' ? ['throw'] : [],
                        "$label invokes only the expected failing callback"
                    );
                    is(
                        $dec->decode( $header . "\x07" ), 7,
                        "$label decoder reusable after failure"
                    );
                }
            }
        }
    }
}
done_testing;
