package # hide from PAUSE
    Sereal::BulkTest;

use strict;
use warnings;
use Data::Dumper;
use File::Spec;
use Test::More;
use Test::LongString;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(run_bulk_tests);
our %EXPORT_TAGS = ('all' => \@EXPORT_OK);

use Sereal::TestSet qw(:all);

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
    foreach my $test (@$corpus) {
        $count++ if $sub->($test);
    }
    return $count;
}
#use Devel::Peek;
sub run_bulk_tests {
    my %opt = @_;

    if (not $opt{bench}) {
        my $total= read_files(sub { return 1 });
        my $read= 0;
        my $eval_ok= read_files(sub {
            my $struct= $_[0];
            diag("read $read\n") unless ++$read % 1000;
            my ($dump, $undump);
            my $ok= eval {
                $dump = Sereal::Encoder::encode_sereal($_[0]);
                $undump= Sereal::Decoder::decode_sereal($dump, $opt{decoder_options} || {});
                1;
            };
            my $err = $@ || 'Zombie error';
            ok($ok,"Error return is empty")
                or diag("Error was: '$err'"), return $ok;
            if ($ok and ref($struct) eq "HASH") {
                my $each_count= 0;

                $each_count++ while my($k,$v)= each %$undump;

                my $keys_count= 0 + keys %$struct;
                is($each_count,$keys_count,"Number of keys match");
            }

            my $struct_dd= Data::Dumper->new([ $struct ])->Sortkeys(1)->Dump();
            my $undump_dd= Data::Dumper->new([ $undump ])->Sortkeys(1)->Dump();
            $ok= is_string($undump_dd, $struct_dd)
                or diag $struct_dd;
            return $ok;
        });
        is($total,$eval_ok);
    }

    if ($opt{bench}) {
        require Benchmark;
        require Time::HiRes;
        Benchmark->import(qw(:hireswallclock));
        my $result= cmpthese(
            -3,
            {
                'noop' => sub {
                    read_files(sub{return 1})
                },
                'decode_sereal' => sub{
                    read_files(sub { return( decode_sereal($_[0], $opt{decoder_options} || {} ) ); }, 'sereal')
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
}
1;
