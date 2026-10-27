#!/bin/bash
#
# Build the ASan + libFuzzer harness for Sereal::Decoder.
#
# Produces, under this directory:
#   stage_asan/   - an ASan + SanitizerCoverage instrumented Decoder.so
#   fuzz_decode   - the libFuzzer target (embeds perl, loads the above)
#
# Requirements:
#   - clang with compiler-rt (libFuzzer + ASan). Tested with clang 18+.
#   - A perl built usemymalloc=n  (check: perl -V:usemymalloc). Needed so the
#     document's PV lives in a system-malloc allocation ASan can instrument.
#
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
DEC="$HERE/../.."                 # Perl/Decoder
STAGE="$HERE/stage_asan"
CLANG=${CLANG:-clang}

um=$(perl -V:usemymalloc 2>/dev/null || true)
case "$um" in
    *"'n'"*) : ;;
    *) echo "WARNING: this perl looks like usemymalloc=y ($um)." >&2
       echo "         ASan may not see reads into the input buffer. See README.md." >&2 ;;
esac

echo ">> Building ASan+coverage Decoder.so into $STAGE"
rm -rf "$STAGE"; mkdir -p "$STAGE"
SAN="-fsanitize=address -fsanitize=fuzzer-no-link -fno-omit-frame-pointer"
CORE=$(perl -MConfig -e 'print "$Config{archlibexp}/CORE"')
( cd "$DEC"
  make distclean >/dev/null 2>&1 || true
  perl Makefile.PL INSTALL_BASE="$STAGE" \
       CC="$CLANG" LD="$CLANG" \
       OPTIMIZE="-g -O1 $SAN" \
       LDDLFLAGS="-shared -shared-libasan $SAN -L$CORE" >/dev/null
  make -j"$(nproc)" >/dev/null
  make install >/dev/null
  make distclean >/dev/null 2>&1 || true )

echo ">> Building fuzz_decode"
CCOPTS=$(perl -MExtUtils::Embed -e ccopts)
LDOPTS=$(perl -MExtUtils::Embed -e ldopts | sed 's/-flto=auto//g')
# shellcheck disable=SC2086
"$CLANG" -g -O1 -fsanitize=address,fuzzer -shared-libasan -fno-omit-frame-pointer \
    $CCOPTS "$HERE/fuzz_decode.c" -o "$HERE/fuzz_decode" $LDOPTS

echo ">> Done. See README.md for how to run."
