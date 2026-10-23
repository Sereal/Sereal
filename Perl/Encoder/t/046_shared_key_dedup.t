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

# A hash key seen again in the same packet must be emitted as a COPY of its
# first occurrence. Keys supplied as UTF-8 strings whose characters all fit
# in Latin-1 are stored downgraded with the WASUTF8 flag; they used to lose
# this deduplication entirely.
my $latin1 = "caf\x{e9}";
utf8::upgrade($latin1);
my @kinds = (
    [ ascii  => "cafe",        "cafe" ],
    [ latin1 => $latin1,       "caf\xc3\xa9" ],
    [ wide   => "caf\x{100}",  "caf\xc4\x80" ],
);

for my $version ( 2 .. 6 ) {
    for my $sort ( 0, 1 ) {
        for my $kind (@kinds) {
            my ( $name, $key, $bytes ) = @$kind;
            my $data = [ map { +{ $key => $_ } } 1 .. 3 ];
            my $label = "v$version sort_keys=$sort $name";

            my $packet = Sereal::Encoder->new(
                { protocol_version => $version, sort_keys => $sort } )->encode($data);
            my $count = () = $packet =~ /\Q$bytes\E/g;
            is( $count, 1, "$label: key bytes appear once" );
            is_deeply( Sereal::Decoder->new->decode($packet),
                $data, "$label: round trip" );

            my $plain = Sereal::Encoder->new(
                {
                    protocol_version   => $version,
                    sort_keys          => $sort,
                    no_shared_hashkeys => 1,
                } )->encode($data);
            $count = () = $plain =~ /\Q$bytes\E/g;
            is( $count, 3, "$label no_shared_hashkeys: key bytes appear three times" );
            is_deeply( Sereal::Decoder->new->decode($plain),
                $data, "$label no_shared_hashkeys: round trip" );
        }
    }
}

done_testing();
