# Source this file from Bash, then run:
#   sereal_check ubsan
#   sereal_check asan
#   sereal_check valgrind
#
# Linux/GCC; uses the current Perl and rebuilds both XS modules and their
# bundled compression libraries. Each invocation runs in a subshell.

SEREAL_CHECK_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

sereal_check() (
    set -e
    root="$SEREAL_CHECK_ROOT"
    mode="${1:-}"
    san=""

    case "$mode" in
        ubsan) san="-fsanitize=undefined -fno-sanitize-recover=all" ;;
        asan) san="-fsanitize=address" ;;
        valgrind) ;;
        *) echo "Usage: sereal_check {ubsan|asan|valgrind}" >&2; exit 2 ;;
    esac

    cd -- "$root"
    flags="-O1 -g -fno-omit-frame-pointer $san"
    ldflags="$(perl -MConfig -e 'print $Config{lddlflags}') $san"
    perl_bin="$(perl -e 'print $^X')"
    export SEREAL_USE_BUNDLED_LIBS=1

    # Clean both before rebuilding either.
    for dist in Decoder Encoder; do
        if [[ -f "$dist/Makefile" ]]; then
            make -C "$dist" clean
        fi
    done

    for dist in Decoder Encoder; do
        (
            cd -- "$root/$dist"
            perl Makefile.PL \
                "OPTIMIZE=$flags" \
                "LDDLFLAGS=$ldflags"
            # MOREFLAGS also instruments bundled Zstd.
            make "MOREFLAGS=$flags"
        )
    done

    export PERL5LIB="$root/Decoder/blib/lib:$root/Decoder/blib/arch:$root/Encoder/blib/lib:$root/Encoder/blib/arch${PERL5LIB:+:$PERL5LIB}"

    case "$mode" in
        ubsan)
            export UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1
            ;;
        asan)
            export LD_PRELOAD="$(gcc -print-file-name=libasan.so)${LD_PRELOAD:+:$LD_PRELOAD}"
            # Oversized-allocation tests need malloc to return NULL.
            # Use Valgrind for leak checking with an ordinary Perl build.
            export ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:allocator_may_return_null=1
            ;;
    esac

    for dist in Decoder Encoder; do
        if [[ "$mode" == valgrind ]]; then
            make -C "$root/$dist" test \
                "FULLPERLRUN=valgrind --tool=memcheck --trace-children=yes --track-origins=yes --leak-check=full --show-leak-kinds=definite,indirect --errors-for-leak-kinds=definite,indirect --error-exitcode=99 --suppressions=$root/shared/author_tools/valgrind.supp $perl_bin"
        else
            make -C "$root/$dist" test
        fi
    done
)
