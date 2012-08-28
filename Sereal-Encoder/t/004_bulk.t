use strict;
use warnings;
use Benchmark qw(cmpthese);
use Data::Dumper;
use Test::More;
use Test::LongString;
use Data::Undump qw(undump);
our $HAVE_JSON_XS;
BEGIN {
    $HAVE_JSON_XS= eval "use JSON::XS; 1";
}
our $CORPUS;
BEGIN {
    $CORPUS||= $ENV{CORPUS} || "corpus";
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
            push @corpus, $_;
            if ($HAVE_JSON_XS) {
                if (my $res= undump($_)) {
                    if ($@) {
                        die Dumper($@,$_);
                    } elsif (!ref $res) {
                        $res= [$res];
                    }
                    push @js_corpus, encode_json($res);
                }
            }
        }
        close $fh;
    }
    my $count= 0;
    if ($json) {
        foreach (@js_corpus) {
            $count++ if $sub->($_);
        }
    } else {
        foreach (@corpus) {
            $count++ if $sub->($_);
        }
    }
    return $count;
}

if (!@ARGV) {
    my $total= read_files(sub { return 1 });
    plan( tests => $total + 1 );
    my $read= 0;
    my $eval_ok= read_files(sub {
        print STDERR "# read $read\n" unless ++$read % 1000;
        my $undump = undump($_[0]);
        if ($@) {
            my $ok= is($@,"Encountered variable in input. This is not eval - can not undump code\n")
                or diag("\nUndump died with error:\n$@\n$_[0]\n"); 
            return $ok;
        };
        my $VAR1;
        my $eval= eval $_[0];
        my $eval_dump= Data::Dumper->new([$eval])->Sortkeys(1)->Dump();
        my $undump_dump= Data::Dumper->new([$undump])->Sortkeys(1)->Dump();
        my $ok= is_string($undump_dump, $eval_dump)
            or diag $_[0];
        return $ok;
    });
    is($total,$eval_ok);
}
my $time= $CORPUS=~/big/ ? 5 : -5;
my $result= cmpthese $time, {
    ((0) ? ( 'read' => sub {
        read_files(sub { return 1 });
    }) : ()),
    'eval'   => sub{
        read_files(sub { my $VAR1; return eval($_[0]); })
    },
    'undump' => sub{
        read_files(sub { return undump($_[0]); })
    },
    'undump_eval' => sub{
        read_files(sub { my $VAR1; return( undump($_[0])||eval($_[0])); })
    },
    $HAVE_JSON_XS ? (
        'undump_json' => sub {
            read_files(sub { return decode_json($_[0]) },1),
        }
    ) : (),
};
diag join "\n","", map {sprintf"%-20s" . (" %20s" x (@$_-1)), @$_ } @$result;
