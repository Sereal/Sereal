#!perl
use strict; use warnings;
use lib '.', 'inc';
use Test::More;

BEGIN {
    plan skip_all => 'Build helpers are not shipped with this distribution'
        unless -f 'inc/Sereal/BuildTools.pm' && -f 'inc/Devel/CheckLib.pm';
}
use inc::Sereal::BuildTools;
use Devel::CheckLib;
for my $supported ( 0, 1 ) {
    no warnings 'redefine';
    local *inc::Sereal::BuildTools::Config = { %Config::Config, gccversion => 'Clang 10.0.0' };
    my $calls = 0;
    local *Devel::CheckLib::check_lib = sub {
        my %args = @_;
        like(
            $args{ccflags}, qr/-Werror=unknown-warning-option/,
            'unknown warning is fatal in probe'
        );
        ok( $args{not_execute}, 'probe does not run a target executable' );
        ++$calls;
        return $supported;
    };
    my $flags = inc::Sereal::BuildTools::build_optimize();
    is( $calls, 1, 'Clang warning support checked once' );
    is(
        !!( $flags =~ /-Wno-compound-token-split-by-macro/ ), !!$supported,
        'suppression added only if supported'
    );
}
done_testing;
