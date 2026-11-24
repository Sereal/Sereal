#!/usr/bin/env bash
# Build the current Perl sources with ASan, independently of normal blib trees.
set -euo pipefail

if [[ ${1:-} == --help || $# -gt 1 ]]; then
    echo 'Usage: PERL=/path/to/perl CC=gcc build.sh [new-work-directory]'
    exit 0
fi

here=$(cd -- "$(dirname -- "$0")" && pwd)
repo=$(git -C "$here" rev-parse --show-toplevel)
perl_bin=$(command -v "${PERL:-perl}")
cc=$(command -v "${CC:-gcc}")
make_bin=$(command -v "${MAKE:-make}")
jobs=${JOBS:-2}
[[ $jobs =~ ^[1-9][0-9]*$ ]] || { echo 'JOBS must be positive' >&2; exit 1; }
[[ $("$perl_bin" -MConfig -e 'print $Config{usemymalloc}') == n ]] || {
    echo 'ASan requires a Perl built with usemymalloc=n' >&2
    exit 1
}
runtime=$("$cc" -print-file-name=libasan.so)
[[ -f $runtime && $runtime == /* ]] || {
    echo 'A GCC compiler with its shared libasan.so is required' >&2
    exit 1
}

if [[ $# == 1 ]]; then
    mkdir -- "$1" # Refuse to overwrite an existing build.
    work=$(cd -- "$1" && pwd)
else
    work=$(mktemp -d "${TMPDIR:-/tmp}/sereal-sanitizers.XXXXXXXX")
fi
echo "Sanitizer workspace: $work"
mkdir -p "$work/source" "$work/logs" "$work/ccache"

# Read tracked files from the working tree, including staged and unstaged
# edits. No git clean, rebuild, or symlink in the original checkout is needed.
git -C "$repo" ls-files -z -- Perl > "$work/source-files.list"
tar -C "$repo" --null -T "$work/source-files.list" -cf - |
    tar -C "$work/source" -xf -
mkdir "$work/source/.git" # Makefile.PL's source-repository marker.

san_flags='-O1 -g -fsanitize=address -fno-omit-frame-pointer'
ld_flags=$("$perl_bin" -MConfig -e 'print $Config{lddlflags}')
{
    printf 'perl_bin=%q\n' "$perl_bin"
    printf 'make_bin=%q\n' "$make_bin"
    printf 'runtime=%q\n' "$runtime"
} > "$work/config.sh"
{
    "$perl_bin" -V
    "$cc" --version
    printf 'XS and bundled zstd flags: %s\nPERL_DESTRUCT_LEVEL=2\n' "$san_flags"
    git -C "$repo" rev-parse HEAD
    git -C "$repo" diff --binary HEAD -- Perl
} > "$work/logs/build-environment.txt"

unset PERL5LIB PERL5OPT
export PATH="$(dirname -- "$perl_bin"):$PATH"
export SEREAL_USE_BUNDLED_LIBS=1 PERL_MM_USE_DEFAULT=1 PERL_DESTRUCT_LEVEL=2
export CCACHE_DIR="$work/ccache"
# zstd's separate Makefile does not inherit MakeMaker's OPTIMIZE setting.
export MOREFLAGS="$san_flags"

for dist in Decoder Encoder Sereal; do
    echo "Building $dist"
    (
        cd "$work/source/Perl/$dist"
        # The wrapper loads Encoder and Decoder during configuration.
        # Preload only into Perl here; build tools do not need the runtime.
        ASAN_OPTIONS=detect_leaks=0:halt_on_error=1 \
        LD_PRELOAD="$runtime" \
        "$perl_bin" Makefile.PL CC="$cc" LD="$cc" \
            OPTIMIZE="$san_flags" LDDLFLAGS="$ld_flags -fsanitize=address"
        # MakeMaker reaches zstd through both its subdirs target and its
        # explicit library prerequisite. Finish it first to avoid concurrent
        # recursive builds writing the same object files with -j.
        if [[ $dist != Sereal ]]; then
            "$make_bin" -C zstd -j"$jobs" CC="$cc"
        fi
        "$make_bin" -j"$jobs"
    ) 2>&1 | tee "$work/logs/build-$dist.log"
done

touch "$work/build-complete"
echo "Build complete. Run: $here/test.sh '$work' asan"
echo "Then run: $here/test.sh '$work' lsan"
