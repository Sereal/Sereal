#!perl
use strict;
use warnings;
use Data::Dumper;
use File::Spec;
use Scalar::Util qw(blessed);

# These tests use an installed Decoder to do testing on horrific
# Perl data structures such as overloaded and tied structures.

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Test::More;

if (not have_encoder_and_decoder()) {
    plan skip_all => 'Did not find right version of decoder';
    exit 0;
}

Sereal::Encoder->import(":all");
Sereal::Decoder->import(":all");

# First, test tied hashes. Expected behaviour: We don't segfault, we don't
# throw exceptions (unless the tied hash is not iterable repeatedly),
# we serialize the tied hash as if it was a normal hash - so no trace of
# tiedness in the output.
{
    SCOPE: {
        package TiedHash;
        require Tie::Hash;
        our @ISA = qw(Tie::StdHash);
    }

    my %testhash = (
        foo => [qw(a b c)],
        baz => 123,
        dfvgbnhmjk => "345ty6ujh",
        a => undef,
    );

    my %tied_hash;
    tie %tied_hash => 'TiedHash';
    %{tied(%tied_hash)} = %testhash;
    is_deeply(\%tied_hash, \%testhash);

    my ($out, $ok, $err, $data);
    $ok = eval {$out = encode_sereal(\%tied_hash); 1};
    $err = $@ || 'Zombie error';
    ok($ok, "serializing tied hash did not die")
        or note("Error was '$err'");
    ok(defined $out, "serializing tied hash returns string");

    $ok = eval {$data = decode_sereal($out); 1;};
    $err = $@ || 'Zombie error';
    ok($ok, "deserializing tied hash did not die")
        or note("Error was '$err', data was:\n"), hobodecode($out);
    ok(defined $data, "deserializing tied hash yields defined output");
    is_deeply($data, \%testhash, "deserializing tied hash yields expected output");
}


# Now tied arrays.
{
    SCOPE: {
        package TiedArray;
        require Tie::Array;
        our @ISA = qw(Tie::StdArray);
    }

    my @testarray = (1, 2, "foo", "bar", []);
    my @tied_array;
    tie @tied_array => 'TiedArray';
    @{tied(@tied_array)} = @testarray;
    is_deeply(\@tied_array, \@testarray);

    my ($out, $ok, $err, $data);
    $ok = eval {$out = encode_sereal(\@tied_array); 1};
    $err = $@ || 'Zombie error';
    ok($ok, "serializing tied array did not die")
        or note("Error was '$err'");
    ok(defined $out, "serializing tied array returns string");

    $ok = eval {$data = decode_sereal($out); 1;};
    $err = $@ || 'Zombie error';
    ok($ok, "deserializing tied array did not die")
        or note("Error was '$err', data was:\n"), hobodecode($out);
    ok(defined $data, "deserializing tied array yields defined output");
    is_deeply($data, \@testarray, "deserializing tied array yields expected output");
}

# Now tied scalars.
{

    SCOPE: {
        package TiedScalar;
        require Tie::Scalar;
        our @ISA = qw(Tie::StdScalar);
    }

    my $testscalar = [qw(foo bar baz)];
    my $tied_scalar;
    tie $tied_scalar => 'TiedScalar';
    ${tied($tied_scalar)} = $testscalar;
    is_deeply($tied_scalar, $testscalar);

    my ($out, $ok, $err, $data);
    $ok = eval {$out = encode_sereal(\$tied_scalar); 1};
    $err = $@ || 'Zombie error';
    ok($ok, "serializing tied scalar did not die")
      or note("Error was '$err'");
    ok(defined $out, "serializing tied scalar returns string");

    $ok = eval {$data = decode_sereal($out); 1;};
    $err = $@ || 'Zombie error';
    ok($ok, "deserializing tied scalar did not die")
      or note("Error was '$err', data was:\n"), hobodecode($out);
    ok(defined $data, "deserializing tied scalar yields defined output");
    is_deeply($data, \$testscalar, "deserializing tied scalar yields expected output");
}

# Now test re-entrancy. DO NOT DO THIS AT HOME!
SCOPE: {
    my $enc = Sereal::Encoder->new;
    my $die_run = 0;
    eval {
        local $SIG{__DIE__} = sub {
            $die_run++;
            ok(defined($enc->encode("foo")), "encode does not segfault");
            $die_run++;
        };
        $enc->encode(["foo", sub{}]);
    };
    ok($die_run == 2, "__DIE__ called, encode 2 did not die ($die_run)");
}

# github Sereal/Sereal issue 7 regression test:
SCOPE: {
    {
        package # hide from PAUSE
            Blessed::Sub::With::Overload;
        use overload '""' => sub { shift->() };
        sub new { bless $_[1] => $_[0] }
    }
    {
        package # hide from PAUSE
            Blessed::Sub::With::Lazy::Overload;
        use overload '""' => sub {
            my ($self) = @_;
            return $self->[1] if defined $self->[1];
            return "OH NOES WE DON'T HAVE A SUB" unless ref $self->[0] eq 'CODE';
            return ($self->[1] = $self->[0]->());
        };
        sub new {
            bless [
                # The callback
                $_[1],
                # Cached value
                undef
            ] => $_[0]
        }
    }
    my $data;
    $data->[0] = sub {};
    $data->[1] = $data->[0];
    $data->[2] = Blessed::Sub::With::Overload->new(sub { "hello there" });
    $data->[3] = $data->[2];
    $data->[4] = Blessed::Sub::With::Overload->new(sub { \"hello there" });
    $data->[5] = $data->[4];
    my $called;
    $data->[6] = Blessed::Sub::With::Overload->new(sub { $called++; "hello there" });
    $data->[7] = $data->[6];
    $data->[8] = $data->[6];
    $data->[9] = $data->[6];
    $data->[10] = Blessed::Sub::With::Lazy::Overload->new(sub { "hello there" });
    $data->[11] = $data->[10];

    my $encode = encode_sereal($data, {stringify_unknown => 1});
    # Before 48d5cdc3dc07fd29ac7be05678a0b614244fec4f, we'd
    # die here because $data->[1] is a ref to something that doesn't exist anymore
    my $decode = decode_sereal($encode);

    is($decode->[0], $decode->[1]);
    is($decode->[2], $decode->[3]);
    is($decode->[4], $decode->[5]);
    is($decode->[6], $decode->[$_]) for 7..9;
    is($called, 4, "We'll call the sub every time, and won't re-use the initial return value");
    ok(blessed($decode->[10]), "We won't be stringifying objects");
    like($decode->[10]->[0], qr/^CODE\(.*?\)$/, "And the subroutine we have will just be stringified as usual in Perl");
    is("$decode->[10]", "OH NOES WE DON'T HAVE A SUB", "So our subroutine won't survive the roundtrip, our object is broken");
    is_deeply($decode->[10], $decode->[11], "Both the original and the reference to it are equally screwed");
}

pass("Alive at end");
done_testing();

