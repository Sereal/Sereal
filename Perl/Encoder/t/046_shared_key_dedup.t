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
    [ ascii  => "cafe",       "cafe" ],
    [ latin1 => $latin1,      "caf\xc3\xa9" ],
    [ wide   => "caf\x{100}", "caf\xc4\x80" ],
);

{

    package Sereal::Test::SharedKeyMagic;
    sub TIESCALAR { bless [], shift }
    sub FETCH     { 1 }
}
my $magic;
tie $magic, 'Sereal::Test::SharedKeyMagic';

{

    package SerealSharedCacheFreeze;

    sub FREEZE {
        my $latin = "frozen_latin_\x{e9}";
        utf8::upgrade($latin);
        return { frozen_common_key_abcdef => $_[0]{id}, $latin => 1, "frozen_wide_\x{100}" => 2 };
    }
    sub THAW { bless { id => $_[2]{frozen_common_key_abcdef} }, $_[0] }
}

for my $version ( 2 .. 6 ) {
    for my $sort ( 0, 1 ) {
        for my $with_magic ( 0, 1 ) {
            for my $kind (@kinds) {
                my ( $name, $key, $bytes ) = @$kind;
                my $data = [ map { +{ $key => $_ } } 1 .. 3 ];

                # A magical sibling visited later must not change how these
                # ordinary hashes share their keys.
                push @$data, \$magic if $with_magic;
                my $label = "v$version sort_keys=$sort magic=$with_magic $name";

                my $packet =
                    Sereal::Encoder->new( { protocol_version => $version, sort_keys => $sort } )
                    ->encode($data);
                my $count = () = $packet =~ /\Q$bytes\E/g;
                is( $count, 1, "$label: key bytes appear once" );
                is_deeply(
                    Sereal::Decoder->new->decode($packet),
                    $data, "$label: round trip"
                );

                my $plain = Sereal::Encoder->new( {
                        protocol_version   => $version,
                        sort_keys          => $sort,
                        no_shared_hashkeys => 1,
                    } )->encode($data);
                $count = () = $plain =~ /\Q$bytes\E/g;
                is( $count, 3, "$label no_shared_hashkeys: key bytes appear three times" );
                is_deeply(
                    Sereal::Decoder->new->decode($plain),
                    $data, "$label no_shared_hashkeys: round trip"
                );
            }
        }
    }
}

for my $version ( 2 .. 6 ) {
    my $data   = [ map { bless { id => $_ }, 'SerealSharedCacheFreeze' } 1 .. 20 ];
    my $packet = Sereal::Encoder->new( { protocol_version => $version, freeze_callbacks => 1 } )
        ->encode($data);
    for my $bytes (
        'SerealSharedCacheFreeze', 'frozen_common_key_abcdef', "frozen_latin_\xc3\xa9",
        "frozen_wide_\xc4\x80"
        )
    {
        my $count = () = $packet =~ /\Q$bytes\E/g;
        is( $count, 1, "v$version non-mutating FREEZE keeps class/key deduplication" );
    }
    is_deeply(
        Sereal::Decoder->new->decode($packet), $data,
        'callback cache ownership preserves values'
    );
}

done_testing();
