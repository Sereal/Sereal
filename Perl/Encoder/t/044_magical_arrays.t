#!perl
use strict;
use warnings;
use File::Spec;
use Test::More;
use lib File::Spec->catdir(qw(t lib));
BEGIN { lib->import('lib') if !-d 't' }
use Sereal::TestSet qw(have_encoder_and_decoder);
use Sereal::Encoder;

plan skip_all => 'Did not find right version of decoder' if !have_encoder_and_decoder();

# Non-tied magical arrays such as @- and @+ (regdata magic) report their
# length through magic while AvARRAY() holds no elements. Encoding them used
# to read past the array and crash, so run each case in a child perl.
sub run_child {
    my ($code) = @_;
    my $pid = open( my $fh, '-|' );
    die "fork failed: $!" if !defined $pid;
    if ( !$pid ) {
        open( STDERR, '>&', \*STDOUT ) or die "dup STDERR failed: $!";
        exec( $^X, '-Mblib', '-Mblib=../Decoder/blib', '-e', $code )
            or die "exec failed: $!";
    }
    my $output = do { local $/; <$fh> };
    close($fh);
    return ( $?, defined $output ? $output : '' );
}

my $prelude = <<'EOP';
use strict;
use warnings;
use Sereal::Encoder;
use Sereal::Decoder qw(decode_sereal);
sub flat { join ",", map { ref($_) eq "ARRAY" ? "[" . flat(@$_) . "]" : defined($_) ? $_ : "undef" } @_ }
EOP

for my $version ( 1 .. 6 ) {
    my $code = $prelude . <<"EOC";
my \$enc = Sereal::Encoder->new( { protocol_version => $version } );
"abcdef" =~ /(b)(c)(d)/;
my \$payload = \$enc->encode( [ \\\@-, \\\@+ ] );
print "decoded=", flat( decode_sereal(\$payload) ), "\\n";
EOC
    my ( $status, $output ) = run_child($code);
    is( $status, 0, "v$version: encoding \\\@- and \\\@+ exits cleanly" )
        or diag($output);
    like( $output, qr/^decoded=\[\[1,1,2,3\],\[4,2,3,4\]\]$/m,
        "v$version: \\\@- and \\\@+ round-trip" );
}

SKIP: {
    skip 'perl lacks @{^CAPTURE}', 2 if $] < 5.025007;
    my $code = $prelude . <<'EOC';
"abcdef" =~ /(?<first>b)(?<second>c)(d)/;
my $payload = Sereal::Encoder->new->encode( [ \@{^CAPTURE} ] );
print "decoded=", flat( decode_sereal($payload) ), "\n";
EOC
    my ( $status, $output ) = run_child($code);
    is( $status, 0, 'encoding \@{^CAPTURE} exits cleanly' ) or diag($output);
    like( $output, qr/^decoded=\[\[b,c,d\]\]$/m, '\@{^CAPTURE} round-trips' );
}

{
    package MyTiedArray;
    sub TIEARRAY  { my ( $class, @values ) = @_; return bless [@values], $class }
    sub FETCH     { my ( $self, $i ) = @_; return "fetched:" . $self->[$i] }
    sub FETCHSIZE { my ($self) = @_; return scalar @$self }
}

{
    package MyTiedScalar;
    sub TIESCALAR { my ( $class, $value ) = @_; return bless \$value, $class }
    sub FETCH     { my ($self) = @_; return "scalar:" . $$self }
}

for my $version ( 1 .. 6 ) {
    my $enc = Sereal::Encoder->new( { protocol_version => $version } );

    tie my @tied, 'MyTiedArray', qw(x y z);
    my $decoded = Sereal::Decoder->new->decode( $enc->encode( [ \@tied ] ) );
    is_deeply( $decoded, [ [qw(fetched:x fetched:y fetched:z)] ],
        "v$version: tied array round-trips its FETCHed values" );

    my @plain = ( 1, undef, 3 );
    tie $plain[1], 'MyTiedScalar', 'two';
    $decoded = Sereal::Decoder->new->decode( $enc->encode( [ \@plain ] ) );
    is_deeply( $decoded, [ [ 1, 'scalar:two', 3 ] ],
        "v$version: plain array with a tied scalar element round-trips" );
}

done_testing();
