#!/usr/bin/env perl
use 5.014;
use strict;
use warnings;
use Config;
use Cwd qw(abs_path);
use File::Basename qw(dirname);
use File::Spec;
use FindBin;
use Getopt::Long qw(GetOptions);
use JSON::PP;
use POSIX qw(strftime);
use lib File::Spec->catdir( $FindBin::Bin, '..', 't', 'lib' );
use Sereal::Encoder 7;
use Sereal::Decoder 7;
use Sereal::TestSet::Corpus qw(write_packet_set pack_corpus);

# Keep platform captures separate so collecting a report cannot replace
# the historical inputs used by compatibility tests.
my ( $output, $label, $help, $include_native );
GetOptions( 'output=s' => \$output, 'label=s' => \$label, 'help' => \$help, 'include-native' => \$include_native )
    or die "Invalid options; use --help\n";
if ( $help || !defined($output) ) {
    print <<'USAGE';
From Perl/Decoder, after building both modules with the target Perl:
  perl -Mblib=../Encoder -Mblib author_tools/collect_float_corpus.pl --output /tmp/sereal-floats

The output directory must not exist. It receives corpus.pack, producer.json,
and perl-config.txt. Archive it with the test log when sharing a report.

--include-native  Include native profiles; unknown padding may expose process memory.
--label NAME      Identify the producing build.

Profile errors are recorded in producer.json; collection is not a test-suite run.
See README.corpus.md for collection and regeneration instructions.
USAGE
    exit( $help ? 0 : 2 );
}
$output = File::Spec->rel2abs($output);
my $identity = join '-', $Config{archname}, $Config{nvtype}, $Config{nvsize};
$identity =~ s/[^A-Za-z0-9_.-]+/-/g;
$label = $identity unless defined $label;
die "Label must contain only letters, digits, dots, underscores or hyphens\n"
    unless $label =~ /\A[A-Za-z0-9_.-]+\z/;
mkdir $output or die "Cannot create new output directory '$output': $!\n";

my %report = (
    schema => 'sereal-float-capture-v1',
    created_utc => strftime( '%Y-%m-%dT%H:%M:%SZ', gmtime ),
    label => $label,
    perl_version => "$^V",
    perl_executable => $^X,
    encoder_version => "$Sereal::Encoder::VERSION",
    decoder_version => "$Sereal::Decoder::VERSION",
    encoder_module => $INC{'Sereal/Encoder.pm'},
    decoder_module => $INC{'Sereal/Decoder.pm'},
    config => { map { $_ => $Config{$_} } qw(
        archname osname osvers byteorder ptrsize ivsize uvsize nvtype nvsize
        nv_preserves_uv_bits doublesize longdblsize longdblkind uselongdouble
        usequadmath cc ccflags
    ) },
    profiles => {}, skipped_values => [],
);

# The collector and loaded modules can come from different checkouts when
# blib or PERL5LIB selects an unintended build. Record each checkout rather
# than attributing the collector's revision to all of the producer code.
sub checkout_info {
    my ($path) = @_;
    my $dir = dirname( abs_path($path) );
    while ( !-e File::Spec->catfile( $dir, '.git' ) ) {
        my $parent = dirname($dir);
        return undef if $parent eq $dir;
        $dir = $parent;
    }
    my %info = ( directory => $dir );
    if ( open my $git, '-|', 'git', '-C', $dir, 'rev-parse', 'HEAD' ) {
        my $revision = <$git>;
        if ( close($git) && defined($revision) ) {
            chomp $revision;
            $info{revision} = $revision;
            my $status = system( 'git', '-C', $dir, 'diff', '--quiet', 'HEAD', '--' );
            $info{dirty} = $status == 0 ? JSON::PP::false
                : $status == 256 ? JSON::PP::true : undef;
        }
    }
    return \%info;
}
$report{checkouts} = {
    collector => checkout_info($0),
    encoder => checkout_info( $INC{'Sereal/Encoder.pm'} ),
    decoder => checkout_info( $INC{'Sereal/Decoder.pm'} ),
};

# A checkout revision cannot identify a stale XS binary by itself. File
# fingerprints retain that distinction for review, while a missing SHA
# module does not prevent the rest of a failing platform's capture.
if ( eval { require Digest::SHA; 1 } ) {
    for my $path ( $0, $INC{'Sereal/Encoder.pm'}, $INC{'Sereal/Decoder.pm'},
        grep { m{[/\\]auto[/\\]Sereal[/\\](?:Encoder|Decoder)[/\\]} }
            @DynaLoader::dl_shared_objects )
    {
        my $ok = eval {
            open my $fh, '<:raw', $path or die "Cannot read '$path': $!";
            $report{file_sha256}{$path} = Digest::SHA->new(256)->addfile($fh)->hexdigest;
            close $fh or die "Cannot close '$path': $!";
            1;
        };
        $report{fingerprint_errors}{$path} = "$@" unless $ok;
    }
}
else {
    $report{fingerprint_error} = "$@";
}

my @values = (
    [ 'half', '0.5', 0.5 ],
    [ 'negative-one-and-half', '-1.5', -1.5 ],
    [ 'decimal-tenth', '0.1', 0.1 ],
    [ 'third', '1/3', 1/3 ],
    [ 'negative-zero', '-(0.0)', -(0.0) ],
    [ 'double-minimum-subnormal', '2**-1074', 2**-1074 ],
);
for my $bits ( 53, 60, 100, 112 ) {
    my $value = 1 + 2**-$bits;
    if ( $value == 1 ) {
        push @{ $report{skipped_values} }, {
            name => "one-plus-2-minus-$bits", recipe => "1 + 2**-$bits",
            reason => 'The source NV rounds this expression to 1',
        };
        next;
    }
    push @values, [ "one-plus-2-minus-$bits", "1 + 2**-$bits", $value ];
}
for my $exponent ( -16445, -16494 ) {
    my $value = 2**$exponent;
    if ( $value == 0 ) {
        push @{ $report{skipped_values} }, {
            name => "tiny-$exponent", recipe => "2**$exponent",
            reason => 'The source NV underflows this expression to zero',
        };
        next;
    }
    push @values, [ "tiny-$exponent", "2**$exponent", $value ];
}

my @profiles = (
    [ 'legacy-v6', { protocol_version => 6 } ],
    [ 'portable-v7', { protocol_version => 7 } ],
    [ 'native-v7', { protocol_version => 7, float_type => 'native' } ],
);
my $total = 0;
for my $profile (@profiles) {
    my ( $name, $options ) = @$profile;
    my %options = ( %$options, compress => 0, checksum_mode => 'none', checksum_policy => 'ignore' );
    my $entry = $report{profiles}{$name} = { options => \%options, values => {} };
    if ( !$include_native && ( $name eq 'native-v7'
        || ( $name eq 'legacy-v6' && ( $Config{nvtype} || '' ) eq 'long double' ) ) ) {
        $entry->{capture_skipped} = 'Native storage requires --include-native; unknown padding may contain process memory';
        next;
    }
    my $encoder = eval { Sereal::Encoder->new( \%options ) };
    if ( !$encoder ) {
        $entry->{constructor_error} = "$@";
        next;
    }
    # Selecting native output only activates it when this Perl uses a native
    # long-double NV. Recording the effective state distinguishes a no-op
    # request on double/quadmath builds from an actual native producer.
    my $native_enabled = $options{protocol_version} >= 7
        && ($encoder->flags & Sereal::Encoder::SRL_F_USE_NATIVE_LONG_DOUBLE());
    $entry->{native_mode_enabled} = $native_enabled ? JSON::PP::true : JSON::PP::false;
    $entry->{decoder_options} = { checksum_policy => 'ignore' };
    $entry->{decoder_options}{allow_unknown_long_double} = 1 if $native_enabled;
    my $reader = Sereal::Decoder->new( $entry->{decoder_options} );
    my %packets;
    for my $case (@values) {
        my ( $case_name, $recipe, $value ) = @$case;
        my $result = $entry->{values}{$case_name} = { recipe => $recipe };
        my $packet = eval { $encoder->encode($value) };
        if ( $@ || !defined($packet) ) {
            $result->{encode_error} = $@ ? "$@" : 'Encoder returned undef without an exception';
            next;
        }
        $packets{$case_name} = $packet;
        my $decoded = eval { $reader->decode($packet) };
        if ( $@ ) {
            $result->{decode_error} = "$@";
        }
        else {
            # Formatting is evidence for humans, not the numeric oracle.
            # Recipes and producer bytes preserve the interpretation even
            # when another NV cannot represent the original value exactly.
            $result->{source_decimal} = sprintf '%.40g', $value;
            $result->{decoded_decimal} = sprintf '%.40g', $decoded;
            $result->{same_numeric_value} = $decoded == $value ? JSON::PP::true : JSON::PP::false;
            $result->{same_zero_sign} = pack('d', $decoded) eq pack('d', $value)
                ? JSON::PP::true : JSON::PP::false if $value == 0 && $decoded == 0;
        }
    }
    $entry->{source} = "capture-$label-$name";
    $entry->{packet_count} = scalar keys %packets;
    $total += scalar keys %packets;
    write_packet_set( $output, $entry->{source}, Sereal::Encoder->VERSION, \%packets );
}
# Even a producer that cannot construct any profile can supply configuration
# and errors. An empty source keeps its artifact readable by the corpus tools.
write_packet_set( $output, "capture-$label-empty", Sereal::Encoder->VERSION, {} ) unless $total;
pack_corpus($output);
for my $file (
    [ 'producer.json', JSON::PP->new->canonical->pretty->encode( \%report ) ],
    [ 'perl-config.txt', Config::myconfig() . "\n" . Config::config_sh() ],
) {
    my $path = File::Spec->catfile( $output, $file->[0] );
    open my $fh, '>:raw', $path or die "Cannot write '$path': $!\n";
    print {$fh} $file->[1] or die "Cannot write '$path': $!\n";
    close $fh or die "Cannot close '$path': $!\n";
}
print "Captured $total packets in $output\n";
print "producer.json records profile errors, source precision and local decode comparisons.\n";
