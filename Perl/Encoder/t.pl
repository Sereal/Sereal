use strict;
use warnings;
use blib;
use Sereal::Encoder;

my $enc;
$SIG{__DIE__} = sub {
  warn $enc->encode("foo");
  #warn "DIE: foo";
};

$enc = Sereal::Encoder->new();
$enc->encode({foo => "bar", baz => sub{}});

