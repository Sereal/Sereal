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
my $problem= check_for_dependency_issues();
if ( !$problem or $problem !~ /available to test/ and
     $Sereal::Decoder::VERSION and
     $Sereal::Encoder::VERSION
 ) {
    plan tests => 2;
}
else {
    plan skip_all => 'Must have both encoder and decoder to run this test.';
}
diag "Testing with both encoder and decoder.";
diag "Sereal::Decoder v$Sereal::Decoder::VERSION";
diag "Sereal::Encoder v$Sereal::Encoder::VERSION";
my $show_diag;
cmp_ok($Sereal::Encoder::VERSION,'<=',$Sereal::Decoder::VERSION,
    "Check that Decoder version is newer or the same as Encoder")
    or $show_diag++;
is($problem,"","There should be no Encoder/Decoder dependency problems.")
    or $show_diag++;
diag "If this test fails it means you need to upgrade Sereal::Decoder first!\n"
            . "You are strongly advised to follow this guidance, upgrading the Encoder\n"
            . "before you upgrade the Decoder may lead to serious problems\n\n"
            . "YOU HAVE BEEN WARNED.\n\n"
    if $show_diag;
