#!perl
use strict;
use warnings;
use Test::More;
use 5.10.1;

use Sereal::Splitter;

use Data::HexDump;

use blib '../Encoder';
use blib '../Decoder';
use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal);

if (1) {
    # no refp / copy tag
    my $data = encode_sereal([ {foo => 1 }, {bar => 2} ]);
    print HexDump $data;
    my $o = Sereal::Splitter->new({chunk_size => 1, input => $data});
    

    while (defined( my $chunk = $o->next_chunk())) {
        print HexDump $chunk;
        my $struct = decode_sereal($chunk);
        say Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with a copy tag inside the chunk
    my $data = encode_sereal([ {foobarbaz => 1 },
                               {foobarbaz => 2 } ],
                             { dedupe_strings => 1} );
    print HexDump $data;
    my $o = Sereal::Splitter->new({chunk_size => 200, input => $data});
    

    while (defined( my $chunk = $o->next_chunk())) {
        print HexDump $chunk;
        my $struct = decode_sereal($chunk);
        say Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with a copy tag pointing to outside of the chunk
    my $data = encode_sereal([ {foobarbaz => 1 },
                               {foobarbaz => 2} ],
                             { dedupe_strings => 1} );
    print HexDump $data;
    my $o = Sereal::Splitter->new({chunk_size => 1, input => $data});
    

    while (defined( my $chunk = $o->next_chunk())) {
        print HexDump $chunk;
        my $struct = decode_sereal($chunk);
        say Dumper($struct); use Data::Dumper;
    }

    pass;

}

if (1) {
    # with an objectv
    my $data = encode_sereal([ bless({aaa => 1 }, 'My::Foo'),
                               bless({bbb => 1 }, 'My::Foo') ],
                             { dedupe_strings => 1} );
    print HexDump $data;
    my $o = Sereal::Splitter->new({chunk_size => 200, input => $data});
    

    while (defined( my $chunk = $o->next_chunk())) {
        print HexDump $chunk;
        my $struct = decode_sereal($chunk);
        say Dumper($struct); use Data::Dumper;
    }

    pass;

}

pass;

done_testing;


  # 4 5 6 7    8 9    10   11   12
  # O [ A REFP 6 REFP 4 ] COPY 5

  # 4 5 6 7    8 9    10   11 12 13   14 15   16
  # O [ A REFP 6 REFP 4  ] [  A  REFP 6  REFP 4  ]



