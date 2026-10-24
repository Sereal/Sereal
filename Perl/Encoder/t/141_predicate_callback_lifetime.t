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

our ($freezes, $destroys, $truths, @input);
{
    package SerealPredicateObject;
    sub FREEZE { ++$main::freezes; return 'kept alive' }
    sub THAW { bless {value => $_[2]}, $_[0] }
    sub DESTROY { ++$main::destroys }

    package SerealPredicateTruth;
    use overload bool => sub { ++$main::truths; $_[0]->() }, fallback => 1;

}

for my $version (2 .. 6) {
    for my $action (qw(allow serialize croak)) {
        ($freezes, $destroys) = (0, 0);
        my $object = bless {value => 'kept alive'}, 'SerealPredicateObject';
        my $encoder = Sereal::Encoder->new({
            protocol_version => $version,
            freeze_allow_classes => sub { undef $object; undef $_[1]; return $action eq 'allow' },
            freeze_deny_action => $action eq 'serialize' ? 'serialize' : 'croak',
        });
        my $packet = eval { $encoder->encode($object) };
        my $error = $@;
        my $label = "v$version predicate overwrites argument $action";
        ok(!defined $object, "$label changes the original reference");
        is($destroys, 1, "$label releases the original referent once");
        is($freezes, $action eq 'allow' ? 1 : 0, "$label follows the decision");
        if ($action eq 'croak') {
            like($error, qr/FREEZE not allowed for class 'SerealPredicateObject'/, "$label throws");
        } else {
            is($error, '', "$label encodes");
            my $decoded = Sereal::Decoder->new->decode($packet);
            is($decoded->{value}, 'kept alive', "$label retains the referent through use");
        }
        is_deeply(Sereal::Decoder->new->decode($encoder->encode([7])), [7], "$label encoder remains reusable");
    }

    for my $action (qw(clear throw)) {
        ($freezes, $truths) = (0, 0);
        @input = (bless({value => 'first'}, 'SerealPredicateObject'), 'second', 'third');
        my $encoder = Sereal::Encoder->new({
            protocol_version => $version,
            freeze_allow_classes => sub {
                return bless sub {
                    die "predicate truth failure\n" if $action eq 'throw';
                    undef @input;
                    return 1;
                }, 'SerealPredicateTruth';
            },
        });
        eval { $encoder->encode(\@input) };
        my $error = $@;
        my $label = "v$version overloaded predicate truth $action";
        like($error, $action eq 'throw' ? qr/predicate truth failure/ : qr/input array was modified during encoding/,
            "$label throws before stale state is used");
        is($truths, 1, "$label converts the result once");
        is($freezes, 0, "$label never calls FREEZE");
        is_deeply(Sereal::Decoder->new->decode($encoder->encode([7])), [7], "$label encoder remains reusable");
        @input = ();
    }
}
done_testing;
