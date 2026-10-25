#!perl
use strict;
use warnings;
use File::Spec;
use Test::Warn;

use lib File::Spec->catdir(qw(t lib));

BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

use Sereal::Encoder;
use Sereal::Encoder::Constants qw(:all);

BEGIN {
    if ( not have_encoder_and_decoder() ) {
        plan skip_all => 'Did not find right version of decoder';
        exit 0;
    }
}
use Sereal::Decoder;

my ( $ok, $err, $out );

# croak_on_bless test
SCOPE: {
    my $e = Sereal::Encoder->new( {
        croak_on_bless => 1,
    } );

    is( $e->encode(1), Header() . integer(1), "Encoder works before exception" );
    $ok  = eval { $out = $e->encode( bless( {}, "Foo" ) ); 1 };
    $err = $@ || 'Zombie error';

    ok( !$ok,              "Object throws exception" );
    ok( $err =~ /object/i, 'Exception refers to object' );

    is( $e->encode(1), Header() . integer(1), "Encoder works after exception" );

    $ok = eval { $out = $e->encode( {} ); 1 };
    ok( $ok, "Non-blessed hash does not throw exception" );

    # test that code refs throw exception
    $ok = eval {
        $out = $e->encode( sub { } ); 1;
    };
    ok( !$ok, "Code ref throws exception" );
}

# test that code refs with undef_unknown don't throw exceptions
SCOPE: {
    my $e = Sereal::Encoder->new( { undef_unknown => 1 } );
    $ok = eval {
        $out = $e->encode( sub { } ); 1;
    };
    $err = $@ || 'Zombie error';
    ok( $ok, "undef_unknown makes CODE encoding not fail" );
    is( $out, Header() . chr(SRL_HDR_UNDEF), "output is undef" )
        or do {
        hobodecode($out) if $ENV{DEBUG_SEREAL};
    }
}

# test that code refs with stringify_unknown don't throw exceptions
SCOPE: {
    my $e   = Sereal::Encoder->new( { stringify_unknown => 1 } );
    my $sub = sub { };
    $ok  = eval { $out = $e->encode($sub); 1 };
    $err = $@ || 'Zombie error';
    ok( $ok, "stringify_unknown makes CODE encoding not fail" );

    my $str = $e->encode("$sub");
    is( $out, $str, "output is stringified ref" )
        or do {
        hobodecode($out), hobodecode($str) if $ENV{DEBUG_SEREAL};
    }
}

# test that code refs with warn_unknown do warn
SCOPE: {
    my $e   = Sereal::Encoder->new( { stringify_unknown => 1, warn_unknown => 1 } );
    my $sub = sub { };
    warning_like {
        $ok = eval { $out = $e->encode($sub); 1 };
    }
    qr/Sereal/,
        "warn_unknown warns about stringified sub";
}

# test that blessed code refs with stringify_unknown don't throw exceptions
SCOPE: {
    my $e   = Sereal::Encoder->new( { stringify_unknown => 1 } );
    my $sub = bless( sub { }, "Foo" );
    $ok  = eval { $out = $e->encode($sub); 1 };
    $err = $@ || 'Zombie error';
    ok( $ok, "stringify_unknown makes CODE encoding not fail" );

    my $str = $e->encode("$sub");
    is( $out, $str, "output is stringified ref" )
        or do {
        hobodecode($out), hobodecode($str) if $ENV{DEBUG_SEREAL};
    }
}

# dito for string overloading
SCOPE: {
    SCOPE2: {

        package BlessedCodeRefOverload;
        use overload '""' => sub { $_[0]->() };

        sub new {
            my ( $class, $data ) = @_;
            bless sub { return $data } => __PACKAGE__;
        }
    }
    SCOPE3: {

        package BlessedCodeRef;

        sub new {
            my ( $class, $data ) = @_;
            bless sub { return $data } => __PACKAGE__;
        }
    }
    my $e   = Sereal::Encoder->new( { stringify_unknown => 1 } );
    my $sub = BlessedCodeRefOverload->new("hello");
    is( "$sub", "hello", "BlessedCodeRefOverload stringification actually works as designed" );

    $ok  = eval { $out = $e->encode($sub); 1 };
    $err = $@ || 'Zombie error';
    ok( $ok, "stringify_unknown makes CODE encoding not fail" );

    my $str = $e->encode("$sub");
    is( $out, $str, "output is stringified ref" )
        or do {
        hobodecode($out), hobodecode($str) if $ENV{DEBUG_SEREAL};
        };

    # test that we get a warning with warn_unknown
    $e = Sereal::Encoder->new( { stringify_unknown => 1, warn_unknown => 1 } );
    warning_like {
        $ok = eval { $out = $e->encode($sub); 1 };
    }
    qr/Sereal/,
        "warn_unknown warns about stringified sub despite overloading";

    # Test that we do NOT get a warning with warn_unknown set to -1
    # FIXME Test::Warn doesn't have a "no_warnings" function, so let's just
    #       run this for now and hope the user will be spooked by the warning
    #       if there is one. Duh.
    $e   = Sereal::Encoder->new( { stringify_unknown => 1, warn_unknown => -1 } );
    $out = $e->encode($sub);
    ok( defined $out && $out !~ /CODE/ && $out !~ "Blessed", "RV of encode makes some sense" );

    # Test that we DO get a warning for non-overloaded unsupported stuff
    my $sub2 = BlessedCodeRef->new("hello");
    warning_like {
        $ok = eval { $out = $e->encode($sub2); 1 };
    }
    qr/Sereal/,
        "warn_unknown == -1 warns about stringified sub without overloading";
}

{
    package UnsupportedDiagnosticOverload;
    our $calls = 0;
    use overload '""' => sub { ++$calls; 'overloaded-code' }, fallback => 1;
}

my @diagnostic_cases = (
    [ 'plain CODE', sub {}, 'CODE', 0 ],
    [ 'blessed CODE', bless(sub {}, 'UnsupportedDiagnostic'), 'UnsupportedDiagnostic=CODE', 0 ],
    [ 'overloaded CODE', bless(sub {}, 'UnsupportedDiagnosticOverload'),
        'UnsupportedDiagnosticOverload=CODE', 1 ],
);
if ($] >= 5.016) {
    my $class = "UnsupportedDiagnostic::\x{100}";
    push @diagnostic_cases, [ 'Unicode class', bless(sub {}, $class), "$class=CODE", 0 ];
}
for my $case (@diagnostic_cases) {
    my ($label, $value, $type, $overloaded) = @$case;
    for my $mode ('exception', 'undef_unknown', 'stringify_unknown') {
        my %options = $mode eq 'exception' ? () : ($mode => 1, warn_unknown => 1);
        my $encoder = Sereal::Encoder->new(\%options);
        my @warnings;
        local $SIG{__WARN__} = sub { push @warnings, @_ };
        local $UnsupportedDiagnosticOverload::calls = 0;
        my $success = eval { $encoder->encode($value); 1 };
        my $error = $@;
        my $diagnostic = $mode eq 'exception' ? $error : join('', @warnings);
        like($diagnostic,
            qr/^Found type \d+ \Q$type\E\(0x[0-9a-f]+\), but it is not representable /i,
            "$label $mode reports class and underlying type");
        if ($mode eq 'exception') {
            ok(!$success, "$label unsupported value still throws");
            is(scalar @warnings, 0, "$label exception does not also warn");
        }
        else {
            is($error, '', "$label $mode still encodes");
            is(scalar @warnings, 1, "$label $mode warns once");
        }
        is($UnsupportedDiagnosticOverload::calls,
            $mode eq 'stringify_unknown' && $overloaded ? 1 : 0,
            "$label $mode diagnostic does not invoke stringification");
        is(Sereal::Decoder->new->decode($encoder->encode(7)), 7,
            "$label $mode leaves encoder reusable");
    }
}

done_testing;
