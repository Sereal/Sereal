#!perl
use strict;
use warnings;
use Sereal::Decoder;
use Sereal::Decoder::Constants;

use Test::More tests => 1;
diag "Perl Regexp Internals Type = ", Sereal::Decoder::regexp_internals_type();
pass();
