#!/bin/sh -e

# This emits all possible types of Sereal docs, for testing the patch
# to file(1) to detect the various types

rm -v /tmp/sereal-*.srl

perl -MSereal::Encoder=encode_sereal -we 'print encode_sereal("foo" x 8, {use_protocol_v1 => 1})' >/tmp/sereal-v1-plain.srl
hexdump -C /tmp/sereal-v1-plain.srl
sha1sum /tmp/sereal-v1-plain.srl
echo

perl -MSereal::Encoder=encode_sereal -we 'print encode_sereal("foo" x 8, {})' >/tmp/sereal-v2-plain.srl
hexdump -C /tmp/sereal-v2-plain.srl
sha1sum /tmp/sereal-v2-plain.srl
echo

perl -MSereal::Encoder=encode_sereal -we 'print encode_sereal("foo" x 8, {use_protocol_v1 => 1, snappy => 1, snappy_incr => 0, snappy_threshold => 0})' >/tmp/sereal-v1-snappy-nonincr.srl
hexdump -C /tmp/sereal-v1-snappy-nonincr.srl
sha1sum /tmp/sereal-v1-snappy-nonincr.srl
echo

perl -MSereal::Encoder=encode_sereal -we 'print encode_sereal("foo" x 8, {use_protocol_v1 => 1, snappy => 0, snappy_incr => 1, snappy_threshold => 0})' >/tmp/sereal-v1-snappy-incr.srl
hexdump -C /tmp/sereal-v1-snappy-incr.srl
sha1sum /tmp/sereal-v1-snappy-incr.srl
echo

# The non-incremental option doesn't do anything under v2
perl -MSereal::Encoder=encode_sereal -we 'print encode_sereal("foo" x 8, {use_protocol_v1 => 0, snappy => 1, snappy_incr => 0, snappy_threshold => 0})' >/tmp/sereal-v2-snappy-nonincr.srl
hexdump -C /tmp/sereal-v2-snappy-nonincr.srl
sha1sum /tmp/sereal-v2-snappy-nonincr.srl
echo

perl -MSereal::Encoder=encode_sereal -we 'print encode_sereal("foo" x 8, {use_protocol_v1 => 0, snappy => 0, snappy_incr => 1, snappy_threshold => 0})' >/tmp/sereal-v2-snappy-incr.srl
hexdump -C /tmp/sereal-v2-snappy-incr.srl
sha1sum /tmp/sereal-v2-snappy-incr.srl
echo

