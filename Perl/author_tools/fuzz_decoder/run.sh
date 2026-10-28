#!/bin/bash
# Convenience runner: generates a seed corpus (if missing) and starts the fuzzer.
# Any extra args are passed through to libFuzzer (e.g. -max_total_time=300).
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
STAGE="$HERE/stage_asan"
ARCH=$(perl -MConfig -e 'print $Config{archname}')
RTDIR=$(dirname "$(${CLANG:-clang} -print-file-name=libclang_rt.asan.so)")

export PERL5LIB="$STAGE/lib/perl5/$ARCH:$STAGE/lib/perl5${PERL5LIB:+:$PERL5LIB}"
export LD_LIBRARY_PATH="$RTDIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
# allocator_may_return_null=1: a document may legitimately DECLARE a huge
# uncompressed/preallocation size; with max_uncompressed_size unset the decoder
# calls Newx under PL_nomemok and croaks on NULL. Letting the allocator return
# NULL keeps that a clean croak instead of an ASan abort, so fuzzing explores the
# real parse paths. Runaway is still bounded by -rss_limit_mb / -malloc_limit_mb.
export ASAN_OPTIONS="${ASAN_OPTIONS:-detect_leaks=0:allocator_may_return_null=1:abort_on_error=1}"

if [ ! -d "$HERE/corpus" ] || [ -z "$(ls -A "$HERE/corpus" 2>/dev/null)" ]; then
    echo ">> Generating seed corpus (needs Sereal::Encoder installed or in PERL5LIB)"
    perl "$HERE/gen_seeds.pl" "$HERE/corpus"
fi
mkdir -p "$HERE/corpus_run"
exec "$HERE/fuzz_decode" -detect_leaks=0 -rss_limit_mb=2560 -malloc_limit_mb=2048 \
    "$@" "$HERE/corpus_run" "$HERE/corpus"
