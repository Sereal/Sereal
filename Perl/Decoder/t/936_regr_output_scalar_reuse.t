#!perl
use strict;
use warnings;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));

BEGIN { lib->import('lib') if !-d 't' }

use Test::More;
use Sereal::TestSet qw(Header);
use Sereal::Decoder;

# decode($blob, $out) and friends write into a caller-supplied scalar that
# may already hold a value. The value readers assume a fresh scalar, so the
# decoder must reset $out the way an assignment would: release a previous
# referent (running DESTROY), drop a string buffer (including copy-on-write
# buffers) and refuse read-only destinations.

my $dec = Sereal::Decoder->new;

my $hash_packet   = Header(6) . "\x51\x61k\x01";    # { k => 1 }
my $string_packet = Header(6) . "\x63abc";          # "abc"
my $int_packet    = Header(6) . "\x05";             # 5

# These cases used to crash perl at scope exit or when the result was copied,
# so they run in a child process whose exit status is the assertion.
sub child_survives {
    my ( $code, $name ) = @_;
    my $status =
        system( $^X, '-Mblib', '-It/lib', '-MSereal::Decoder', '-MSereal::TestSet=Header', '-e',
        $code
        );
    is( $status, 0, "$name: child exits cleanly" );
}

my $prelude = q{
    my $dec = Sereal::Decoder->new;
    my $hash_packet   = Header(6) . "\x51\x61k\x01";
    my $string_packet = Header(6) . "\x63abc";
};

child_survives(
    $prelude . q{
        { my $out = ''; $dec->decode( $hash_packet, $out ); my @copy = ($out); die unless ref $copy[0] eq 'HASH'; }
    },
    'empty string literal as output scalar'
);
child_survives(
    $prelude . q{
        { my $out = "pre-existing value"; $dec->decode( $hash_packet, $out ); my @copy = ($out); die unless $copy[0]{k} == 1; }
    },
    'string as output scalar'
);
child_survives(
    $prelude . q{
        my $out = "x" x 10;
        $dec->decode( $string_packet, $out ); $dec->decode( $hash_packet, $out ); $dec->decode( $string_packet, $out );
        my ( $h, $b ) = ( "head", "body" );
        $dec->decode_with_header( $hash_packet, $b, $h );
        die unless $out eq 'abc' && ref $b eq 'HASH';
    },
    'alternating value types in reused output scalars'
);

# Resetting an output that holds an object runs its DESTROY, which may
# replace the input scalar. The reset must happen before the decoder
# captures the input buffer, so the new input is decoded rather than the
# freed old buffer being read.
child_survives(
    q{
        our $blob = Header(6) . "\x51\x61k\x01";
        package Clobber { sub DESTROY { $main::blob = main::Header(6) . "\x26\x80\x20" . ( 'x' x 4096 ) } }
        my $dec = Sereal::Decoder->new;
        my $out = bless {}, 'Clobber';
        $dec->decode( $blob, $out );
        die "unexpected result" unless !ref $out && $out eq 'x' x 4096;
    },
    'output DESTROY replacing the input scalar'
);
child_survives(
    q{
        our $blob = Header(6, "\x63hdr") . "\x51\x61k\x01";
        package Clobber { sub DESTROY { $main::blob = main::Header(6, "\x26\x80\x20" . ( 'y' x 4096 )) . "\x01" } }
        my $dec = Sereal::Decoder->new;
        my $hdr = bless {}, 'Clobber';
        $dec->decode_only_header( $blob, $hdr );
        die "unexpected header" unless !ref $hdr && $hdr eq 'y' x 4096;
    },
    'header output DESTROY replacing the input scalar'
);

# A previous referent must be released, not leaked.
{

    package Counted;
    my $live = 0;
    sub new     { $live++; bless {}, shift }
    sub DESTROY { $live-- }
    sub live    { $live }
}
{
    my $out = Counted->new;
    is( Counted::live(), 1, 'one live object before decoding over it' );
    $dec->decode( $hash_packet, $out );
    is( Counted::live(), 0, 'decoding into a scalar releases the previous referent' );
    is( $out->{k},       1, 'and stores the decoded value' );
}

# Read-only destinations are refused instead of being written through.
{
    my $ok = eval { $dec->decode( $int_packet, undef ); 1 };
    ok( !$ok, 'a literal undef output argument is refused' );
    like( $@, qr/read-only/, 'with the usual read-only diagnostic' );
    my $ok2 = eval { $dec->decode( $int_packet, 'constant' ); 1 };
    ok( !$ok2, 'a literal string output argument is refused' );
}

# The input SV itself may disappear during the output reset, independently
# of its PV being changed. Each entry point must retain it before DESTROY.
for my $api (qw(decode decode_only_header decode_with_header)) {
    child_survives(
        q{
            our %cache = (packet => Header(6, "\x63hdr") . "\x51\x61k\x01");
            package DeleteInput { sub DESTROY { delete $main::cache{packet} } }
            my $dec = Sereal::Decoder->new;
            my $out = bless {}, 'DeleteInput';
        } . (
            $api eq 'decode'
            ? q{
                $dec->decode($cache{packet}, $out);
                die 'wrong body' unless $out->{k} == 1;
            }
            : $api eq 'decode_only_header' ? q{
                $dec->decode_only_header($cache{packet}, $out);
                die 'wrong header' unless $out eq 'hdr';
            }
            : q{
                my $hdr;
                $dec->decode_with_header($cache{packet}, $out, $hdr);
                die 'wrong header/body' unless $hdr eq 'hdr' && $out->{k} == 1;
            }
        ),
        "$api output DESTROY deleting the input's owning entry"
    );
}
child_survives(
    q{
        our %outputs;
        package DeleteOtherOutput { sub DESTROY { delete $main::outputs{body} } }
        my $dec = Sereal::Decoder->new;
        $outputs{header} = bless {}, 'DeleteOtherOutput';
        $outputs{body} = 'old body';
        my $blob = Header(6, "\x63hdr") . "\x51\x61k\x01";
        $dec->decode_with_header($blob, $outputs{body}, $outputs{header});
        die 'wrong header' unless $outputs{header} eq 'hdr';
    },
    'header output DESTROY deleting the body output owning entry'
);
done_testing;
