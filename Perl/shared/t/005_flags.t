use strict;
use warnings;

use Test::More;
use File::Spec;
use lib File::Spec->catdir(qw(t lib));
BEGIN {
    lib->import('lib')
        if !-d 't';
}
use Sereal::TestSet qw(:all);

if (have_encoder_and_decoder(3.005003)) {
    run_tests("plain",                   {                             } );
    run_tests( "no_shared_hk",           { no_shared_hashkeys     => 1 } );
    run_tests( "dedupe_strings",         { dedupe_strings         => 1 } );
    run_tests( "aliased_dedupe_strings", { aliased_dedupe_strings => 1 } );
    done_testing();
} else {
    plan skip_all => "Did not find right version of encoder/decoder";
}

sub run_tests {
    my ( $extra_name, $opt_hash ) = @_;

    my $encoder= Sereal::Encoder->new($opt_hash);
    my $decoder= Sereal::Decoder->new($opt_hash);

    # We must recreate these tests each time we run_tests()
    # as the tests we execute actually change the state
    # of the vars we are testing (in terms of SV flags),
    # which can break the tests.
    my %tests= ();
    foreach my $str ("0","1","100","10.01",".01") {
        foreach my $pfx (map { ($_, " $_") } "","0","-","-0") { 
            foreach my $sfx (map { ($_, "$_ ") } "","0") {
                my $n="str num '$pfx$str$sfx'";
                $tests{$n}= "$pfx$str$sfx";
            }
        }
    }
    $tests{"num: $_"}= $_
        for (map { (" $_", "$_ ", " $_ ") } (qw(0e0 3e3), "0 but true"));
    $tests{"false"}= !1;
    $tests{"true"}= !0;
    foreach my $v (values %tests) {
        no warnings;
        my $i= int $v;
        my $f= $v + 0.5;
        my $s= "" . $v;
    }
    foreach my $k (keys %tests) {
        no warnings;
        $tests{"$k (PN)"}= "" . $tests{$k};
        my $f= $tests{"$k (PN)"} + 0.5;
        $tests{"$k (PI)"}= "" . $tests{$k};
        my $i= int  $tests{"$k (PI)"};
    }

    $tests{"raw false"}= !1;
    $tests{"raw true"}= !0;


    foreach my $test ( sort keys %tests ) {
        my $test_name= "$test - $extra_name";
        my $encoded= $encoder->encode($tests{$test});
        my $decoded= $decoder->decode($encoded);

        TODO: {
            # we must do this test before we test numeric equivalence
            no warnings 'numeric';
            my $have= ($decoded ^ '1');
            my $want= ($tests{$test} ^ '1');
            local $TODO = $have ne $want ? "Cannot reliably round trip NIOK flag(s)" : undef;
            is($have, $want, "$test_name - Xor string (\$var ^ '1')");
        }
        {
            no warnings 'numeric';
            ok( $decoded eq $tests{$test}, "$test_name - string equivalence");
            # this test MUST be last.
            ok( $decoded == $tests{$test}, "$test_name - numeric equivalence");
        }
        # hobodecode($expect);
        # hobodecode($out);
    }
}

