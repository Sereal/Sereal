use strict;
use warnings;
use blib;
use Benchmark qw(cmpthese :hireswallclock);
use Sereal::Decoder qw(decode_sereal);
use Sereal::Encoder qw(encode_sereal);
use JSON::XS qw(decode_json encode_json);
use Storable qw(nfreeze thaw);
use Data::Undump qw(undump);
use Data::Dumper qw(Dumper);
use Data::Dumper::Limited qw(DumpLimited);
use Data::MessagePack;
use Getopt::Long qw(GetOptions);

my (
    $duration,
    $encoder,
    $decoder,
    $dump,
    $tiny_data,
    $small_data,
    $medium_data,
);
BEGIN {
    my $sereal_only = 0;
    GetOptions(
        'duration=i' => \($duration=-3),
        'encoder'   => \$encoder,
        'decoder'   => \$decoder,
        'dump|d'    => \$dump,
        'tiny'      => \$tiny_data,
        'small'     => \$small_data,
        'medium'    => \$medium_data,
        'sereal_only|sereal-only|serealonly' => \$sereal_only,
    );
    eval "sub SEREAL_ONLY () { $sereal_only }";
}

my $fail = do {no warnings; $tiny_data + $small_data + $medium_data - 1};
if ($fail and $fail > 0) {
    die "Only one of --tiny, --small, --medium allowed!";
}
$encoder = 1 if not $encoder and not $decoder;

our %opt = @ARGV;

our $mpo = Data::MessagePack->new();

srand(0);
my @str;
push @str, join("", map chr(65+int(rand(57))), 1..10) for 1..1000;
my @rand = map rand,1..1000;
our %data;

$data{$_}= make_data() for qw(sereal sereal_func dd1 dd2 ddl mp json_xs storable);

our $enc = Sereal::Encoder->new(\%opt);
our $dec = Sereal::Decoder->new(\%opt);

our ($json_xs, $dd1, $dd2, $ddl, $sereal, $storable, $mp);
# do this first before any of the other dumpers "contaminate" the iv/pv issue
$sereal   = $enc->encode($data{sereal});
if (!SEREAL_ONLY) {
    $json_xs  = encode_json($data{json_xs});
    $dd1      = Data::Dumper->new([$data{dd1}])->Indent(0)->Dump();
    $dd2      = Dumper($data{dd2});
    $ddl      = DumpLimited($data{ddl});
    $mp       = $mpo->pack($data{mp});
    $storable = nfreeze($data{storable}); # must be last
}
print($sereal), exit if $dump;

my $sereal_len= bytes::length($sereal);
require bytes;
if (!SEREAL_ONLY) {
    for my $tuple (
        ["JSON::XS",  bytes::length($json_xs)],
        ["Data::Dumper::Limited", bytes::length($ddl)],
        ["Data::Dumper (1)", bytes::length($dd1)],
        ["Data::Dumper (2)", bytes::length($dd2)],
        ["Storable" , bytes::length($storable)],
        ["Data::MessagePack" ,bytes::length($mp)],
        ["Sereal::Encoder",  bytes::length($sereal)],
    ) {
        my ($name, $size) = @$tuple;
        printf "%-40s %12d bytes %.2f%% of sereal\n", $name, $size, $size/$sereal_len *100;
    }
}

our $x;
if ($encoder) {
    cmpthese(
        $duration,
        {
            (!SEREAL_ONLY
                ? (
                    json_xs => '$::x = decode_json($::json_xs);',
                    undump_ddl => '$::x = Data::Undump::undump($::ddl);',
                    eval_ddl => '$::x = eval $::ddl;',
                    storable => '$::x = thaw($::storable);',
                    mp => '$::x = $::mpo->unpack($::mp);',
                ) : ()),
            sereal_func => '$::x = decode_sereal($::sereal, \%::opt);',
            sereal => '$::x = $::dec->decode($::sereal);',
        }
    );
}

if ($decoder) {
    cmpthese(
        $duration,
        {
            (!SEREAL_ONLY
                ? (
                    json_xs => '$::x = encode_json($::data{json_xs});',
                    ddl => '$::x = DumpLimited($::data{ddl});',
                    dd1 => '$::x = Data::Dumper->new([$::data{dd1}])->Indent(0)->Dump();',
                    dd2 => '$::x = Dumper($::data{dd2});',
                    storable => '$::x = nfreeze($::data{storable});',
                    mp => '$::x = $::mpo->pack($::data{mp});',
                ) : ()),
            sereal_func => '$::x = encode_sereal($::data{sereal_func}, \%::opt);',
            sereal => '$::x = $::enc->encode($::data{sereal});',
        }
    );
}

sub make_data {
    if ($tiny_data) {
        return {};
    }
    elsif ($small_data) {
        return { foo=> 1, bar => [100,101,102], str => "this is a \x{df} string which has to be serialized" };
    }
    elsif ($medium_data) {
        my @obj = (
            bless({ foo => 1, bar => [100,101,102], str => "this is a \x{df} string which has to be serialized" } => "Baz"),
            bless({ foo => 2, bar => [103,103,106,999], str2 => "this is a \x{df} aaaaaastring which has to be serialized" } => "Baz"),
            bless({ foozle => 3, bar => [100], str3 => "this is a \x{df} string which haaaaadsadas to be serialized" } => "Baz3"),
            bless({ foozle => 3, bar => [], st4r => "this is a \x{df} string which has to be sdassdaerialized" } => "Baz2"),
            bless({ foo => 1, bar => [100,101,102], s5tr => "this is a \x{df} string which has to be serialized" } => "Baz"),
            bless({ foo => 2, bar => [103,103,106,999], str => "this is a \x{df} aaaaaastring which has to be serialized" } => "Baz"),
            bless({ foozle => 3, bar => [100], str => "this is a \x{df} string which haaaaadsadas to be serialized" } => "Baz3"),
            bless({ foozle => 3, bar => [], str2 => "this is a \x{df} string which has to be sdassdaerialized" } => "Baz2"),
            bless({ foo2 => -99999, bar => [100,101,102], str2 => "this is a \x{df} string which has to be serialized" } => "Baz"),
            bless({ foo2 => 213, bar => [103,103,106,999], str => "this is a \x{df} aaaaaastring which has to be serialized" } => "Baz"),
            bless({ foozle2 => undef, bar => [100], str => "this is a \x{df} string which haaaaadsadas to be serialized" } => "Baz3"),
            bless({ foozle2 => undef, bar => [1..20], str => "this is a \x{df} string which has to be sdassdaerialized" } => "Baz2"),
        );
        foreach my $i (1..$#obj) {
            $obj[$i]->{parent} = $obj[$i-1];
        }
        return \@obj;
    }
    else {
        return [
            [1..10000], {@str}, {@str}, [1..10000],
            {@str}, [@rand], {@str}, {@str},
        ]
    }
}
