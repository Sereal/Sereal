# ASan + libFuzzer harness for Sereal::Decoder

A coverage-guided, in-process fuzzer for the decode path (protocol v1–v6,
including the v6 keyed-checksum envelope and all four compressors). Intended as
an independent memory-safety signal to run before a release.

This tool lives in `Perl/author_tools/fuzz_decoder` in the Git repository.
It is not included in the Encoder or Decoder CPAN distributions and is not
run by `make test`. The target is Decoder; Encoder generates seed packets.

## How it works

`fuzz_decode.c` embeds a Perl interpreter and loads an **ASan + SanitizerCoverage
instrumented** build of `Sereal::Decoder`. Each fuzz input is decoded via
`$dec->decode($bytes)` inside `eval`, so malformed input (a `croak`) is recovered
exactly as in real use and the loop continues.

Why it catches real reads: the perl must be built `usemymalloc=n`, so the input
document's PV is a system-malloc allocation ASan instruments — a read before or
after the buffer is caught. Coverage comes from the dlopened instrumented
`Decoder.so`; libFuzzer resolves the SanitizerCoverage callbacks from the main
binary (built `-fsanitize=fuzzer`, which exports them via `-Wl,-E`), and both
sides share one ASan runtime via `-shared-libasan`. The `NEW_FUNC` /
`-print_coverage=1` output names real decoder functions, so you can confirm the
offset, checksum and decompression paths are being exercised.

## Requirements

- `clang` with compiler-rt (libFuzzer + ASan). Tested with clang 18+.
- A perl built `usemymalloc=n` (`perl -V:usemymalloc`). Most distro perls are.
  On a `usemymalloc=y` perl the reads are still out of bounds but ASan may not
  intercept perl's own arena; build a `usemymalloc=n` perl to fuzz.
- `Sereal::Encoder` available (installed or in `PERL5LIB`) for seed generation.

## Use

```sh
cd Perl/author_tools/fuzz_decoder    # from the repository root
./build.sh          # builds stage_asan/Decoder.so (ASan+cov) and ./fuzz_decode
./run.sh -max_total_time=300    # generates seeds if needed, then fuzzes
```

`build.sh` cleans and rebuilds `Perl/Decoder` with ASan and coverage
instrumentation, installs it under this tool's `stage_asan/`, then cleans
the Decoder build again. Rebuild Decoder normally before returning to
ordinary development. Build Encoder separately or provide an installed
Encoder for seed generation.

`run.sh` passes any extra args through to libFuzzer and sets the `ASAN_OPTIONS`
described below. To inspect which decoder functions a corpus reaches:

```sh
./run.sh -runs=0 -print_coverage=1 corpus 2>&1 | grep COVERED_FUNC | grep /Perl/Decoder/
```

## ASAN_OPTIONS rationale

`run.sh` sets `allocator_may_return_null=1`. A document may legitimately *declare*
a large uncompressed or preallocation size; with `max_uncompressed_size` (etc.)
unset, the decoder calls `Newx` under `PL_nomemok` and croaks on NULL. Letting the
allocator return NULL keeps that a clean croak instead of an ASan abort, so the
fuzzer explores the real parse paths. A genuine runaway is still bounded by
`-rss_limit_mb` / `-malloc_limit_mb`.

To instead treat an over-large declared size as a hard finding, set
`max_uncompressed_size` / `max_string_length` / `max_num_*` on the decoder object
in the `BOOT` string of `fuzz_decode.c` and drop `allocator_may_return_null`.

## Knobs (in `fuzz_decode.c`)

- The harness decoder sets `max_recursion_depth => 1000` to bound stack use.
- It configures a `checksum_key` so the checksum **verification** path runs; drop
  it to fuzz the unauthenticated envelope-parsing path instead.
