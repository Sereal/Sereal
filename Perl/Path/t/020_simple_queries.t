#!perl
use strict;
use warnings;

use JSON::Path;
use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Test::More tests => 25;
use Data::Dumper;

sub hobodecode {
    return unless defined $_[0];
    open my $fh, "| $^X -Mblib=../Encoder -Mblib=../Decoder ../shared/author_tools/hobodecoder.pl -e" or die $!;
    print $fh $_[0];
    close $fh;
}

my %plain_data = (
    'plain array' => {
        dataset => [ 1, 2, 3, '4', 5, 6, 7, 8, 9, '10' ],
        queries => [ '$.0', '$[0]', '$.3', '$[9]', '$.-1', '$.-5', '$[-10]',
                     '$[0,1]', '$[8,9]', '$.9,8', '$[-1,-2]', '$[0,-1,1,-2]',
                     '$.0,1,2,3,4,5,6,7,8,9', '$.*' ],
    },
    'plain hash' => {
        dataset => { a => '1', b => '2', c => '3' },
        queries => [ '$[a]', '$.b', '$.c', '$[a,b,c]', '$.c.b.a', '$.a,c', '$[*]' ]
    },
    'walk over array of hashes' => {
        dataset => [ { a => '1', b => '2' }, { a => '3', b => '4', c => 5 } ],
        queries => [ '$[*][a]', '$.*.b', '$[*][a,b]', '$[*][c]' ]
    },
);

foreach my $name (keys %plain_data) {
    my $ds = $plain_data{$name}{dataset};
    my $sp = Sereal::Path->new(encode_sereal($ds));

    foreach my $q (@{ $plain_data{$name}{queries} }) {
        my @got = sort $sp->values($q);
        my @expected = sort JSON::Path->new($q)->values($ds);
        is_deeply(\@got, \@expected, "dataset '$name' query $q")
            or warn "got:\n" . Dumper(\@got) . "expected:\n" . Dumper (\@expected);
    }
}

done_testing();
