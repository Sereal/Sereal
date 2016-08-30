#  Tests for self referential tree save and reload where most refs to the root are weakened.
use strict;
use warnings;
use File::Spec;
use Scalar::Util qw /weaken/;

local $| = 1;

use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}

use Sereal::TestSet qw(:all);
use Sereal::BulkTest qw(:all);
use Test::More;
use Sereal::Encoder;


my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of decoder';
}
else {
    run_weakref_tests();
}


sub run_weakref_tests {
    #  Child to parent refs are weak, root node is stored once in the hash
    #  Was failing on x64 Strawberry perls 5.16.3, 5.18.4, 5.20.1
    test_save_and_reload ();
    
    #  Child to parent refs are weak, but we store the root node twice in the hash
    #  (second time is in the "TREE_BY_NAME" subhash)
    #  Was failing on x64 Strawberry perls 5.16.3, passing on 5.18.4, 5.20.1
    test_save_and_reload (store_root_by_name => 1);
    
    #  child to parent refs are strong
    #  Should pass
    test_save_and_reload (no_weaken_refs => 1);
}

pass();
done_testing();

exit;


sub get_data {
    my %args = @_;

    my @children;

    my $root = {
        name     => 'root',
        children => \@children,
    };

    my %hash = (
        TREE => $root,
        TREE_BY_NAME => {},
    );

    if ($args{store_root_by_name}) {
        $hash{TREE_BY_NAME}{root} = $root;
    }

    foreach my $i (0 .. 1) {
        my $child = {
            PARENT => $root,
            NAME => $i,
        };

        if (!$args{no_weaken_refs}) {
            weaken $child->{PARENT};
        }

        push @children, $child;
        #  store it in the by-name cache
        $hash{TREE_BY_NAME}{$i} = $child;
    }

    return \%hash;
}


sub test_save_and_reload {
    my %args = @_;
    my $data = get_data (%args);

    #diag '=== ARGS ARE:  ' . join ' ', %args;

    my $context_text;
    $context_text .= $args{no_weaken} ? 'not weakened' : 'weakened';
    $context_text .= $args{store_root_by_name}
        ? ', extra root ref stored'
        : ', extra root ref not stored';

    my $encoder = Sereal::Encoder->new;
    my $decoder = Sereal::Decoder->new;
    my ($encoded_data, $decoded_data);

    $encoded_data = eval {$encoder->encode($data)};
    my $e = $@;
    ok (!$e, "Encoded without exception, $context_text");

    #  no point testing if serialisation failed
    if ($encoded_data) {
        eval {$decoder->decode ($encoded_data, $decoded_data)};
        my $e = $@;
        ok (!$e, "Decoded using Sereal, $context_text");

        is_deeply (
            $decoded_data,
            $data,
            "Data structures match, $context_text",
        );
    }

}


1;
