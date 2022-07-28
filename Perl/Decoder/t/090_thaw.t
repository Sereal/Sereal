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
use Data::Dumper;

if ( !have_encoder_and_decoder() ) {
    plan skip_all => 'Did not find right version of encoder';
}

my $last_thaw_arg;
package Matcher {
	sub new {
		my ($class, $pattern) = @_;
		bless { pattern => $pattern }, $class;
	}

	sub FREEZE {
		my ($self, $serializer) = @_;
		return [$self->{pattern}],"22";
	}

	sub THAW {
		my ($class, $serializer, @data) = @_;
                $last_thaw_arg= Data::Dumper::Dumper(\@data);
		return $class->new( $data[0][0] );
	}
}


my $encoder_plain  = Sereal::Encoder->new();
my $encoder_freeze = Sereal::Encoder->new({ freeze_callbacks => 1 });

sub strip_white {
    my $s= $_[0];
    $s=~s/\s+/ /g;
    $s=~s/\A\s+//;
    $s=~s/\s+\z//;
    return $s
}
sub no_warnings(&) {
    my @warn;
    local $SIG{__WARN__}= sub { push @warn, $_[0] };
    $_[0]->();
    return @warn == 0
}


my $string = 'string';
my $blessed = bless {}, 'Bespoke';
my $qr= qr/foo/;

my @test_data = (
	{ data => $string,
		description => 'Str' },
	{ data => $blessed,
		description => 'Object without FREEZE/THAW' },
        { data => Matcher->new( $qr ),
                description => 'Object without FREEZE/THAW (regexp)' },
	{ data => Matcher->new( $string ),
		description => '(Object with FREEZE/THAW) containing a Str' },
	{ data => Matcher->new( $blessed ),
		description => '(Object with FREEZE/THAW) containing an (Object without FREEZE/THAW)' },
);

for my $data (@test_data) {
	ok no_warnings {
		decode_sereal(  $encoder_plain->encode($data->{data})  );
	}, "Plain round-trip on $data->{description}";

	ok no_warnings {
                my $encoded= $encoder_freeze->encode($data->{data});
		decode_sereal($encoded);
	}, "FREEZE/THAW round-trip on $data->{description}";
}
my $want= strip_white(<<'EOF_THAW_ARG');
$VAR1 = [
           [
             bless( {}, 'Bespoke' )
           ],
           '22'
         ];
EOF_THAW_ARG
is(strip_white($last_thaw_arg),$want, "Last Thaw Args look as expected");

$want= strip_white(<<'EOF_STRUCT');
$VAR1 = bless(
     [  [ bless( {}, 'Bespoke' ) ], '22', 'Matcher' ],
    'Sereal::Decoder::THAW_args'
);
EOF_STRUCT
my $decoded= decode_sereal($encoder_freeze->encode($test_data[-1]{data}),{no_thaw_objects=>1});
is(strip_white(Dumper($decoded)), $want, "no_thaw_objects=1 worked as expected");

$want= strip_white(<<'EOF_STRUCT');
$VAR1 = bless( { 'pattern' => {} }, 'Matcher' );
EOF_STRUCT
$decoded= decode_sereal($encoder_freeze->encode($test_data[-1]{data}),{no_thaw_objects=>0,no_bless_objects=>1});
is(strip_white(Dumper($decoded)), $want, "no_thaw_objects=0 no_bless_objects worked as expected");

$want= strip_white(<<'EOF_STRUCT');
$VAR1 = bless( [ [ {} ], '22', 'Matcher' ], 'Sereal::Decoder::THAW_args' );
EOF_STRUCT
$decoded= decode_sereal($encoder_freeze->encode($test_data[-1]{data}),{no_bless_objects=>1});
is(strip_white(Dumper($decoded)), $want, "no_bless_objects=1 worked as expected");
done_testing;
__END__
