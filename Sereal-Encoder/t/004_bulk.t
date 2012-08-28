use strict;
use warnings;
use Benchmark qw(cmpthese);
use Data::Dumper;
use File::Spec;
use Test::More;
use Test::LongString;
use Sereal::Encoder qw(encode_sereal);

use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't'; }
use Sereal::TestSet qw(:all);

my $ok = have_encoder_and_decoder();
if (not $ok) {
    plan skip_all => 'Did not find right version of decoder';
}
else {
    require Sereal::Decoder;
    Sereal::Decoder->import(qw(decode_sereal));
}

our $HAVE_JSON_XS;
BEGIN {
    $HAVE_JSON_XS= eval "use JSON::XS; 1";
}
our $CORPUS;
BEGIN {
    $CORPUS||= $ENV{CORPUS} || File::Spec->catfile(qw(t data corpus));
}
my @corpus;
my @js_corpus;
sub read_files {
    my ($sub, $json)= @_;
    if (!@corpus) {
        print "Reading\n";
        open my $fh, "<", $CORPUS
            or die "Failed to read '$CORPUS': $!";
        local $/="\n---\n";
        while (<$fh>) {
            chomp;
            my $VAR1;
            my $res= eval $_;
            if ($@) {
                die $@;
            }
            push @corpus, $res;
        }
        close $fh;
    }
    my $count= 0;
    foreach (@corpus) {
        $count++ if $sub->($_);
    }
    return $count;
}

if (!@ARGV) {
    my $total= read_files(sub { return 1 });
    plan( tests => $total + 1 );
    my $read= 0;
    my $eval_ok= read_files(sub {
        print STDERR "# read $read\n" unless ++$read % 1000;
        my ($dump,$undump);
        my $ok= eval {
            $dump = encode_sereal($_[0]);
            $undump= decode_sereal($dump);
            1;
        };
        ok($ok,"Error return is empty: '$@'")
            or return $ok;

        my $eval_dump= Data::Dumper->new([ $_[0] ])->Sortkeys(1)->Dump();
        my $undump_dump= Data::Dumper->new([ $undump ])->Sortkeys(1)->Dump();
        $ok= is_string($undump_dump, $eval_dump)
            or diag $_[0];
        return $ok;
    });
    is($total,$eval_ok);
}
