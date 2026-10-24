#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Splitter qw(SRL_ZLIB);

use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal);

# split, decode every chunk and check that the chunks add up to the input
sub check_split {
    my ($name, $data, $encoder_opt, %splitter_opt) = @_;
    my $o = Sereal::Splitter->new({ input => encode_sereal($data, $encoder_opt), %splitter_opt });
    my @elements;
    while (defined( my $chunk = $o->next_chunk())) {
        push @elements, @{ decode_sereal($chunk) };
    }
    is_deeply(\@elements, $data, $name);
}

check_split("no refp / copy tag",
    [ {foo => 1 }, {bar => 2} ], {}, chunk_size => 1);

check_split("with a copy tag inside the chunk",
    [ {foobarbaz => 1 }, {foobarbaz => 2 } ], { dedupe_strings => 1 }, chunk_size => 200);

check_split("with a copy tag pointing to outside of the chunk",
    [ {foobarbaz => 1 }, {foobarbaz => 2} ], { dedupe_strings => 1 }, chunk_size => 1);

check_split("with an objectv",
    [ bless({aaa => 1 }, 'My::Foo'), bless({bbb => 1 }, 'My::Foo') ], { dedupe_strings => 1 }, chunk_size => 200);

check_split("with an objectv pointing to outside of the chunk",
    [ bless({aaa => 1 }, 'My::Foo'), bless({bbb => 1 }, 'My::Foo') ], { dedupe_strings => 1 }, chunk_size => 1);

{
    my $t = { foo => 1 };
    check_split("with a refp", [ $t, $t ], { dedupe_strings => 1 }, chunk_size => 200, compress => SRL_ZLIB);

    my $o = Sereal::Splitter->new({ input => encode_sereal([ $t, $t ]), chunk_size => 200 });
    my $struct = decode_sereal($o->next_chunk());
    is($struct->[0], $struct->[1], "a refp inside the chunk is kept as a reference");

    check_split("with a refp pointing to outside of the chunk", [ $t, $t ], {}, chunk_size => 1);
}

{
    my $s = "aliased string value";
    check_split("with aliases", [ $s, $s, [ $s ] ],
        { dedupe_strings => 1, aliased_dedupe_strings => 1 }, chunk_size => 1);
}

done_testing;
