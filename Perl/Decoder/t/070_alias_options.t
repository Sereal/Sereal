use strict;
use warnings;

use Sereal::Decoder;
use Test::More;

my @tests= (
    [ 15,  alias_smallint => 1  ],
    [ 127, alias_varint_under => 128  ],
);

if (eval "use Sereal::Encoder; 1") {
    my $num_tests= 0;
    $num_tests += ((16 + $_->[0] + 2) * 2) for @tests;
    plan tests => $num_tests;
} else {
    plan skip_all => "Requires Sereal::Encoder to be installed";
}
foreach my $test (@tests) {
    my ($up_to, $opt, $opt_val)= @$test;
    #diag "$up_to: $opt $opt_val";

    my $enc = Sereal::Encoder->new;
    my $dec = Sereal::Decoder->new( { $opt => $opt_val } );

    my $struct= {
        array => [-16 .. $up_to],
        array2 => [reverse -16 .. $up_to],
        map { $_ => $_ } -16 .. $up_to,
    };
    my $got= $dec->decode($enc->encode($struct));

    # undef the decoder to make sure if it blows up on DESTROY it does it before we test.
    undef $dec;
    undef $enc;

    # Make sure we get the expected aliases
    for (-16..$up_to) {
        ok(\$got->{array}[$_+16] == \$got->{array2}[- 1 - ($_+16)],"$opt: array alias: $_");
        ok(\$got->{$_} == \$got->{array}[$_+16],"$opt: array alias: $_");
    }

    # Make sure the aliases are readonly.
    my $eval_ok= eval {
        $got->{$up_to}= 123;
    };
    my $error= $eval_ok ? "" : ("$@" || "Zombie error");
    ok(!$eval_ok,"$opt: expect modification of \$got->{$up_to} to die");
    like($error,qr/read-only/,"$opt: expect an error about read-only values");
}

