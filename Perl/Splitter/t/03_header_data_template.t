#!perl
use strict;
use warnings;
use Test::More;

use Sereal::Splitter qw(create_header_data_template);

use Data::Dumper;

use Sereal::Encoder qw(encode_sereal);
use Sereal::Decoder qw(decode_sereal decode_sereal_with_header_data);


if (1) {

    my $data = encode_sereal([ {foo => 1 }, {bar => 2} ]);
    my $o = Sereal::Splitter->new({ chunk_size => 1000, input => $data,
                                    header_data_template => create_header_data_template({count => '__$CNT__'}),
                                  });
    

    while (defined( my $chunk = $o->next_chunk())) {
        my $struct = decode_sereal($chunk);
        is_deeply($struct, [ {foo => 1 }, {bar => 2} ]);
        (my $header) = @{decode_sereal_with_header_data($chunk)};
        is($header->{count}, 2);
    }

    pass;

}

# create_header_data_template({ count => '__$COUNT$__' });



pass;

done_testing;
