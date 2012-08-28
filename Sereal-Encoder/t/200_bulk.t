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
our $CORPUS;
BEGIN {
    $HAVE_JSON_XS= eval "use JSON::XS; 1";
    $CORPUS||= $ENV{CORPUS} || File::Spec->catfile(qw(t data corpus));
}

my @corpus;
my @js_corpus;
my @sereal_corpus;
my @raw_corpus;
sub read_files {
    my ($sub, $what)= @_;
    if (!@corpus) {
        note("Reading");
        open my $fh, "<", $CORPUS
            or die "Failed to read '$CORPUS': $!";
        local $/="\n---\n";
        while (<$fh>) {
            chomp;
            my $VAR1;
            push @raw_corpus, $_;
            my $res= eval $_;
            die $@ if $@;
            warn "SRC=$_\n\nRES=$res\n\n" if not ref $res;

            push @corpus, $res;
            push @sereal_corpus, Sereal::Encoder::encode_sereal($res);
            if ($HAVE_JSON_XS) {
                push @js_corpus, JSON::XS::encode_json($res);
            }
        }
        close $fh;
    }
    my $corpus;
    $what = '' if not defined $what;
    if ($what =~ /json/i) {
        $corpus = \@js_corpus;
    }
    elsif ($what =~ /sereal/i) {
        $corpus = \@sereal_corpus;
    }
    elsif ($what =~ /raw/i) {
        $corpus = \@raw_corpus;
    }
    else {
        $corpus = \@corpus;
    }

    my $count= 0;
    foreach (@$corpus) {
        $count++ if $sub->($_);
    }
    return $count;
}

if (!@ARGV) {
    my $total= read_files(sub { return 1 });
    plan( tests => $total * 2 + 1 );
    my $read= 0;
    my $eval_ok= read_files(sub {
        diag("read $read\n") unless ++$read % 1000;
        my ($dump,$undump);
        my $ok= eval {
            $dump = encode_sereal($_[0]);
            $undump= decode_sereal($dump);
            1;
        };
        my $err = $@ || 'Zombie error';
        ok($ok,"Error return is empty")
            or diag("Error was: '$err'"), return $ok;

        my $eval_dump= Data::Dumper->new([ $_[0] ])->Sortkeys(1)->Dump();
        my $undump_dump= Data::Dumper->new([ $undump ])->Sortkeys(1)->Dump();
        $ok= is_string($undump_dump, $eval_dump)
            or diag $_[0];
        return $ok;
    });
    is($total,$eval_ok);
}


if (grep /^--bench$/, @ARGV) {
    my $result= cmpthese(
        -3,
        {
            'noop' => sub {
                read_files(sub{return 1})
            },
            'decode_sereal' => sub{
                read_files(sub { return( decode_sereal($_[0]) ); }, 'sereal')
            },
            'eval' => sub{
                read_files(sub { return( eval $_[0] ); }, 'raw')
            },
            do {eval "require Data::Undump"} ? (
                'undump' => sub{
                    read_files(sub { return( Data::Undump::undump($_[0]) ); }, 'raw')
                },
            ): (),
            $HAVE_JSON_XS ? (
                'decode_json' => sub {
                    read_files(sub { return decode_json($_[0]) }, 'json'),
                }
            ) : (),
        }
    );
    note join "\n","", map {sprintf"%-20s" . (" %20s" x (@$_-1)), @$_ } @$result;
}

