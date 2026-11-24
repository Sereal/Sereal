# Perl sanitizer builds

These Bash scripts build the working tree's tracked Perl sources in an
isolated directory, then run the Decoder, Encoder, and Sereal test suites.
They leave the checkout's normal build alone and are repository-only tools;
do not add them to the CPAN distribution manifests.

Requirements: Linux, Bash, GCC with shared libasan, GNU tar, make, and a Perl
built with `usemymalloc=n`, plus the normal test prerequisites. The Perl
interpreter itself is not rebuilt or instrumented. Its system allocations
are intercepted by ASan. XS, bundled miniz, bundled Snappy, and bundled zstd
are instrumented, including zstd's separate compilation rules. Assembly is
built normally; ASan does not instrument its memory accesses.

Run in a dedicated window in the **same tmux session as the current work**.
Check the session and pane directly after restarting Codex: inherited
`TMUX_PANE` values can refer to an earlier session. Use an explicit tmux
session ID when creating the window.

```bash
cd /path/to/Sereal/Perl
PERL="$HOME/perl5/perlbrew/perls/perl-5.36.3/bin/perl" \
    author_tools/sanitizers/run.sh /tmp/sereal-sanitizers-review
```

The workspace must not exist yet. `PERL` defaults to the `perl` on PATH;
`CC` defaults to `gcc`, `MAKE` to `make`, and `JOBS` to 2. For separate steps:

```bash
PERL=/path/to/perl author_tools/sanitizers/build.sh /tmp/sereal-asan
author_tools/sanitizers/test.sh /tmp/sereal-asan asan
author_tools/sanitizers/test.sh /tmp/sereal-asan lsan
```

Both test modes set `PERL_DESTRUCT_LEVEL=2` for full interpreter teardown.
They also set a finite 64 MiB stack so that ASan's larger C frames can run
the existing 9999-level nesting regression. Set `SEREAL_SANITIZER_STACK_KB`
to choose another size; a hard limit that prevents setting it is an error.
The `asan` run disables leak checks; the `lsan` run enables them in the same
ASan runtime. Neither mode suppresses findings. Plain-Perl runtime preflight
logs distinguish sandbox/ptrace failures from test failures. A failed LSan
preflight is a failed validation, never a successful leak check.
The test runner invokes MakeMaker's Perl test harness with the same file
patterns as `make test`, avoiding loading libasan into make itself and
mistaking make's process-exit allocations for Sereal leaks.

Build commands, compiler flags, Perl configuration, the tracked Perl diff,
and per-distribution test output are retained under `logs/`. Successful
runs create `asan-passed` and `lsan-passed` markers. `run.sh` attempts both
test modes and exits unsuccessfully if either fails. Existing markers for
a mode are removed before that mode is rerun.

The existing `../fuzz_decoder/` harness supplies a separate coverage-guided
decoder test. Its default runner disables leak checks, and its embedded
interpreter sets its own destruction level; it is not a replacement for
the full-teardown LSan suite here.
