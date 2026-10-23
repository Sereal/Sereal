# Source this file from Bash, then run:
#   sereal_check ubsan
#   sereal_check asan
#   sereal_check valgrind
#   sereal_check valgrind /tmp/sereal-logs
#
# Linux/GCC; uses the current Perl and rebuilds both XS modules and their
# bundled compression libraries. Each invocation runs in a subshell.
# Each run creates a unique directory under Perl/check-logs (or the optional
# log directory). run.log contains stdout and stderr, followed by Valgrind's
# separate per-process logs when applicable. Console output is shown live.

SEREAL_CHECK_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"

_sereal_check_run() (
    set -e
    root="$SEREAL_CHECK_ROOT"
    mode="${1:-}"
    log_dir="$2"
    san=""

    case "$mode" in
        ubsan) san="-fsanitize=undefined -fno-sanitize-recover=all" ;;
        asan) san="-fsanitize=address" ;;
        valgrind) ;;
        *) echo "Usage: sereal_check {ubsan|asan|valgrind}" >&2; exit 2 ;;
    esac

    cd -- "$root" || exit
    flags="-O1 -g -fno-omit-frame-pointer $san"
    ldflags="$(perl -MConfig -e 'print $Config{lddlflags}') $san" || exit
    perl_bin="$(perl -e 'print $^X')" || exit
    export SEREAL_USE_BUNDLED_LIBS=1
    # Exercise complete interpreter cleanup and reduce leak-report noise.
    export PERL_DESTRUCT_LEVEL=2

    # Clean both before rebuilding either.
    for dist in Decoder Encoder; do
        if [[ -f "$dist/Makefile" ]]; then
            make -C "$dist" clean || exit
        fi
    done

    for dist in Decoder Encoder; do
        (
            cd -- "$root/$dist" || exit
            perl Makefile.PL \
                "OPTIMIZE=$flags" \
                "LDDLFLAGS=$ldflags" || exit
            # MOREFLAGS also instruments bundled Zstd.
            make "MOREFLAGS=$flags" || exit
        ) || exit
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
                "FULLPERLRUN=valgrind --tool=memcheck --trace-children=yes --track-origins=yes --leak-check=full --show-leak-kinds=definite,indirect --errors-for-leak-kinds=definite,indirect --error-exitcode=99 --log-file=\"$log_dir/valgrind-$dist.%p.log\" --suppressions=\"$root/shared/author_tools/valgrind.supp\" \"$perl_bin\"" || exit
        else
            make -C "$root/$dist" test || exit
        fi
    done
)

sereal_check() (
    # Inspect PIPESTATUS ourselves, including when the caller uses errexit.
    set +e
    mode="${1:-}"
    case "$mode" in
        ubsan|asan|valgrind) ;;
        *) echo "Usage: sereal_check {ubsan|asan|valgrind} [log-directory]" >&2; exit 2 ;;
    esac
    if (( $# > 2 )); then
        echo "Usage: sereal_check {ubsan|asan|valgrind} [log-directory]" >&2
        exit 2
    fi

    log_parent="${2:-$SEREAL_CHECK_ROOT/check-logs}"
    mkdir -p -- "$log_parent" || exit
    log_parent="$(cd -- "$log_parent" && pwd -P)" || exit
    log_dir="$(mktemp -d "$log_parent/$mode-$(date -u +%Y%m%dT%H%M%SZ)-XXXXXX")" || exit
    printf 'Logs: %s\n' "$log_dir"

    _sereal_check_run "$mode" "$log_dir" 2>&1 | tee "$log_dir/run.log"
    statuses=( "${PIPESTATUS[@]}" )
    status="${statuses[0]}"
    if (( status == 0 )); then
        status="${statuses[1]}"
    fi

    # Tests can redirect child stderr. Valgrind writes directly to its own
    # files instead; append them to run.log even when the tests failed.
    shopt -s nullglob
    for diagnostic_log in "$log_dir"/valgrind-*.log; do
        {
            printf '\n===== %s =====\n' "${diagnostic_log##*/}"
            cat -- "$diagnostic_log"
        } >> "$log_dir/run.log"
        if (( $? != 0 && status == 0 )); then
            status=1
        fi
    done

    printf '\nExit status: %s\n' "$status" | tee -a "$log_dir/run.log"
    statuses=( "${PIPESTATUS[@]}" )
    if (( statuses[1] != 0 && status == 0 )); then
        status="${statuses[1]}"
    fi
    printf 'Log: %s/run.log\n' "$log_dir"
    exit "$status"
)
