#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Splitter qw(SRL_ZLIB);

use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal);

if (1) {
    # no refp / copy tag
    my $data = encode_sereal([ {foo => 1 }, {bar => 2} ]);
    my $o = Sereal::Splitter->new({chunk_size => 1, input => $data});

    while (defined( my $chunk = $o->next_chunk())) {
        my $struct = decode_sereal($chunk);
        print Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with a copy tag inside the chunk
    my $data = encode_sereal([ {foobarbaz => 1 },
                               {foobarbaz => 2 } ],
                             { dedupe_strings => 1} );
    my $o = Sereal::Splitter->new({chunk_size => 200, input => $data});

    while (defined( my $chunk = $o->next_chunk())) {
        my $struct = decode_sereal($chunk);
        print Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with a copy tag pointing to outside of the chunk
    my $data = encode_sereal([ {foobarbaz => 1 },
                               {foobarbaz => 2} ],
                             { dedupe_strings => 1} );
    my $o = Sereal::Splitter->new({chunk_size => 1, input => $data});

    while (defined( my $chunk = $o->next_chunk())) {
        my $struct = decode_sereal($chunk);
        print Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with an objectv
    my $data = encode_sereal([ bless({aaa => 1 }, 'My::Foo'),
                               bless({bbb => 1 }, 'My::Foo') ],
                             { dedupe_strings => 1} );
    my $o = Sereal::Splitter->new({chunk_size => 200, input => $data});

    while (defined( my $chunk = $o->next_chunk())) {
        my $struct = decode_sereal($chunk);
        print Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with a refp
    my $t = { foo => 1 };
    my $data = encode_sereal([ $t, $t ],
                             { dedupe_strings => 1} );
    my $o = Sereal::Splitter->new({chunk_size => 200, input => $data, compress => 2 });

    while (defined( my $chunk = $o->next_chunk())) {
        my $struct = decode_sereal($chunk);
        print Dumper($struct); use Data::Dumper;
    }

    pass;

}

pass;

done_testing;
