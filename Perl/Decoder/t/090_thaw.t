#!/usr/bin/env perl
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
use Sereal::Decoder qw(decode_sereal);
use Sereal::TestSet::Corpus qw(read_packet);
use Data::Dumper;

my $last_thaw_arg;
{

    package Matcher;

    sub new {
        my ( $class, $pattern ) = @_;
        bless { pattern => $pattern }, $class;
    }

    sub FREEZE {
        my ( $self, $serializer ) = @_;
        return [ $self->{pattern} ], "22";
    }

    sub THAW {
        my ( $class, $serializer, @data ) = @_;
        $last_thaw_arg = Data::Dumper::Dumper( \@data );
        return $class->new( $data[0][0] );
    }
}

my $corpus = File::Spec->catdir(qw(t data packet_corpus));

sub strip_white {
    my $s = $_[0];
    $s =~ s/\s+/ /g;
    $s =~ s/\A\s+//;
    $s =~ s/\s+\z//;
    return $s;
}

sub no_warnings(&) {
    my @warn;
    local $SIG{__WARN__} = sub { push @warn, $_[0] };
    $_[0]->();
    return @warn == 0;
}

my $string  = 'string';
my $blessed = bless {}, 'Bespoke';
my $qr      = qr/foo/;

my @test_data = ( {
        data        => $string,
        description => 'Str'
    },
    {
        data        => $blessed,
        description => 'Object without FREEZE/THAW'
    },
    {
        data        => Matcher->new($qr),
        description => 'Object without FREEZE/THAW (regexp)'
    },
    {
        data        => Matcher->new($string),
        description => '(Object with FREEZE/THAW) containing a Str'
    },
    {
        data        => Matcher->new($blessed),
        description => '(Object with FREEZE/THAW) containing an (Object without FREEZE/THAW)'
    },
);

for my $index ( 0 .. $#test_data ) {
    my $data = $test_data[$index];
    my $plain = read_packet( $corpus, 'decoder-features', 'thaw/090/plain/' . ( $index + 1 ) );
    my $freeze = read_packet( $corpus, 'decoder-features', 'thaw/090/freeze/' . ( $index + 1 ) );
    ok no_warnings {
        decode_sereal($plain);
    }, "Plain round-trip on $data->{description}";

    ok no_warnings {
        decode_sereal($freeze);
    }, "FREEZE/THAW round-trip on $data->{description}";
}
my $want = strip_white(<<'EOF_THAW_ARG');
$VAR1 = [
           [
             bless( {}, 'Bespoke' )
           ],
           '22'
         ];
EOF_THAW_ARG
is( strip_white($last_thaw_arg), $want, "Last Thaw Args look as expected" );

$want = strip_white(<<'EOF_STRUCT');
$VAR1 = bless(
     [  [ bless( {}, 'Bespoke' ) ], '22', 'Matcher' ],
    'Sereal::Decoder::THAW_args'
);
EOF_STRUCT
my $decoded = decode_sereal(
    read_packet( $corpus, 'decoder-features', 'thaw/090/freeze/5' ),
    { no_thaw_objects => 1 } );
is(
    strip_white( Dumper($decoded) ),
    $want, "no_thaw_objects=1 worked as expected"
);

$want = strip_white(<<'EOF_STRUCT');
$VAR1 = bless( { 'pattern' => {} }, 'Matcher' );
EOF_STRUCT
$decoded = decode_sereal(
    read_packet( $corpus, 'decoder-features', 'thaw/090/freeze/5' ),
    { no_thaw_objects => 0, no_bless_objects => 1 } );
is(
    strip_white( Dumper($decoded) ),
    $want, "no_thaw_objects=0 no_bless_objects worked as expected"
);

$want = strip_white(<<'EOF_STRUCT');
$VAR1 = bless( [ [ {} ], '22', 'Matcher' ], 'Sereal::Decoder::THAW_args' );
EOF_STRUCT
$decoded = decode_sereal(
    read_packet( $corpus, 'decoder-features', 'thaw/090/freeze/5' ),
    { no_bless_objects => 1 } );
is(
    strip_white( Dumper($decoded) ),
    $want, "no_bless_objects=1 worked as expected"
);
done_testing;
__END__
