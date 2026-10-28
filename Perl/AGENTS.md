# AGENTS.md

## Purpose

This repository contains the Perl implementation of the Sereal serialization protocol.
Sereal is optimized first for efficient, correct round-tripping of Perl data structures.
The Perl implementation is the reference-quality implementation in practice: it is the most complete, receives the most maintenance, and drives most protocol and compatibility work.

The three core Perl distributions are:

- `Sereal::Decoder` in [`Decoder/`](./Decoder)
- `Sereal::Encoder` in [`Encoder/`](./Encoder)
- `Sereal` in [`Sereal/`](./Sereal), a wrapper/meta distribution that depends on both

The wrapper exists for convenience, but encoder and decoder are intentionally releasable as separate distributions. That separation is important policy, not an accident: decoders must remain safe to upgrade independently so mixed-version fleets can read older data during rollout.

This repo also contains adjacent projects such as `Path/`, `Merger/`, and `Splitter/`. Unless the task explicitly concerns those subprojects, treat `Encoder/`, `Decoder/`, `Sereal/`, and `shared/` as the main maintenance surface.

## Core design and compatibility policy

- The decoder must preserve backwards readability. A current decoder should be safe to deploy before encoder upgrades.
- Changes that affect wire compatibility, decode safety, thaw behavior, malformed-input handling, or recursion behavior are high-risk and should be treated conservatively.
- Prefer fixing compatibility in the decoder first when sequencing matters.
- `Sereal` is only a dependency wrapper. Real feature work normally belongs in `Encoder` and/or `Decoder`, with `Sereal/Changes` updated to summarize the same release.
- `shared/` contains code and test infrastructure used by multiple distributions. Changes there often affect both encoder and decoder behavior.

## Repository-specific build behavior

- The file [`this_is_the_Sereal_repo.txt`](./this_is_the_Sereal_repo.txt) is intentionally present and used to detect source-repo builds. Do not remove it.
- Source-repo builds use shared files from `shared/` and local `blib` wiring when building `Sereal`.
- `Encoder/Makefile.PL` and `Decoder/Makefile.PL` may prompt for build options outside source-repo mode. `CPAN_COMPAT=1` forces CPAN-style prompting behavior during testing of packaging/build changes.
- `SEREAL_USE_BUNDLED_LIBS=1` is used by the repo’s main build script.

## Main commands

- Full clean rebuild and distribution check:
  - `./clean_make_all`
- Normal repo build/test/dist flow:
  - `./make_all`
- Regenerate shared headers/constants and perltidy Perl files:
  - `./clean_regen_and_tidy`
- Repository-only Decoder fuzz testing:
  - `author_tools/fuzz_decoder/build.sh`
  - `author_tools/fuzz_decoder/run.sh -max_total_time=300`
  - See `author_tools/fuzz_decoder/README.md` for prerequisites. This builds
    an instrumented Decoder and cleans its normal build. Keep this tool
    outside the CPAN distribution manifests.

`./make_all` currently does this:

1. `git clean -dfX`
2. Build `Decoder`
3. Build `Encoder`
4. Test, `make manifest`, and `make dist` for `Decoder`
5. Test, `make manifest`, and `make dist` for `Encoder`
6. Test, `make manifest`, and `make dist` for `Sereal`

When working only in one distribution, targeted `perl Makefile.PL && make test` in that subdirectory is fine during iteration, but final verification for release-oriented or cross-cutting work should use the repo scripts.

## Change expectations

- Library behavior changes should come with tests.
- User-visible behavior changes should come with documentation updates.
- Every new protocol tag or EXTEND subtype must also be implemented in
  [`shared/author_tools/hobodecoder.pl`](./shared/author_tools/hobodecoder.pl),
  including consuming its complete payload and displaying its meaning.
- Protocol, freeze/thaw, security, malformed-input, and version-compatibility changes should usually add or extend regression tests.
- Prefer small, self-contained, incremental patches that build toward the goal.
- If the task requires a larger cross-cutting patch, call that out explicitly in the commit message and changelog text.

## Release policy

- `Sereal`, `Sereal::Encoder`, and `Sereal::Decoder` should always be released together.
- Every release should update:
  - `Encoder/Changes`
  - `Decoder/Changes`
  - `Sereal/Changes`
- Version bumps are normally done with `fixver.pl`, then changelog entries are updated, then the emitted shell commands are used to test, commit, tag, push, and upload.
- `fixver.pl` also shows the canonical release commit message and tag format.

Current release flow:

1. Run `perl fixver.pl VERSION REASON`
2. Update all three `Changes` files
3. Run the emitted `./clean_make_all`
4. If clean, use the emitted `git commit`, `git tag`, `git push`, and upload commands

Release commit format is strict:

- `Release v5.009 - Fix build ZSTD build issue on some platforms`
- `Release v5.004_001 - Update ZStd, other changes`

Tag format is also strict:

- `Sereal-Decoder-<version>`
- `Sereal-Encoder-<version>`
- `Sereal-<version>`

## Dev releases vs production releases

- Significant build-system or packaging changes are hard to validate locally across the CPAN matrix.
- For those changes, prefer a developer release with an underscore version, then review CPAN Testers results before doing the production release.
- Minor build changes can go out directly if the local test evidence is strong enough.
- Library changes are usually safer to validate with the test suite and can often go straight to a production release if well covered.
- Re-releasing is acceptable when necessary; optimize for safety, not ritual.

Examples from existing history:

- Dev releases for build/test validation: `5.000_001`, `5.001_001`, `5.001_002`, `5.002_001`, `5.002_002`, `5.004_001`
- Production releases consolidating validated dev work: `5.002`, `5.003`

## Changelog conventions

- Changelog entries are concise and operational.
- Lead with the user-visible outcome or maintenance reason.
- Credit contributors when appropriate.
- `Sereal/Changes` is a summary/meta changelog for the wrapper distribution, not the primary place for deep technical detail.
- If decoder-first upgrade safety matters for a release, say so explicitly. Existing changelogs already use warnings like “Upgrade *Sereal::Decoder* FIRST.”

## Commit message conventions

Based on Yves’s history, use short subject lines with a clear area first when not doing a release commit.

Common patterns:

- `Decoder/t/195_backcompat.t - test against older versions output`
- `Makefile.PL - add CPAN_COMPAT env var to simulate CPAN style build`
- `Perl: add FREEZE/THAW class allow-lists to Encoder/Decoder`
- `THAW ordering fixes - use LIFO order for nested objects`
- `shared/zstd: update zstd to the latest verion at time of building`

Guidelines:

- For release commits, use the exact `Release v<version> - <reason>` format from `fixver.pl`.
- For normal commits, start with the most relevant subsystem, file, or topic.
- Keep the summary short and concrete.
- Prefer one logical change per commit.
- If the change is a test-only or build-only change, say so directly.

## Testing guidance

- Reuse the packet-building and inspection helpers in
  `shared/t/lib/Sereal/TestSet.pm` (for example `varint`, `Header`,
  `short_string`, and `read_varint_at`) rather than defining equivalent
  helpers in individual test files. Check the shared helpers before adding
  a test-local utility; add generally useful missing helpers to the shared
  module instead.
- For core encoder/decoder behavior changes, test both directions when relevant:
  - new encoder output can be decoded correctly
  - current decoder still handles older serialized data
- Pay special attention to:
  - freeze/thaw behavior
  - recursion and aliasing
  - malformed or hostile input
  - compression backends (`snappy`, `zlib`, `zstd`)
  - older Perl compatibility, especially 5.8-era constraints still reflected in the codebase
- Existing tests such as `Decoder/t/195_backcompat.t` and files under `shared/t/700_roundtrip/` are important indicators of compatibility expectations.

### Mixed-version tests before release

- Before releasing, test the generated distribution archives in isolated
  directories outside this checkout. Source-repo `blib` wiring can hide
  dependencies on the matching development Encoder or Decoder.
- Test Decoder with no Encoder installed, with the previous stable Encoder,
  and with the matching release Encoder. Test Encoder with its minimum
  supported Decoder and with the matching release Decoder. Respect declared
  prerequisites; do not require an older Decoder to read newer protocols.
- Keep Decoder coverage running when Encoder is absent or too old for a
  feature or protocol. Use checked-in packet corpus input from a capable
  Encoder, plus hand-built wire packets where appropriate. Gate only the
  optional live Encoder round trips on the version that implements the
  behavior under test; loading successfully is not a capability check.
- When adding Encoder-dependent tests, add or regenerate the corresponding
  corpus fixtures and verify the old/absent Encoder paths before release.
  Test every protocol the suite requests without asking an older Encoder
  to emit an unsupported protocol. Record the module pairings and results
  alongside the normal release validation.

## Editing guidance

- Keep changes narrow and local when possible.
- Avoid gratuitous style churn in XS/C/shared vendored code.
- Do not remove old-version compatibility logic casually; much of it exists for a real CPAN/platform reason.
- Treat bundled third-party code updates such as `shared/zstd/`, `shared/miniz.*`, and `shared/snappy/` as vendor updates: minimize unrelated edits and document the version bump clearly.
- If changing shared build logic, expect consequences in both `Encoder` and `Decoder`, and verify accordingly.

## Agent checklist

Before finishing work, verify which category the change falls into:

- Core library behavior
- Decoder safety/backcompat
- Build/packaging/tooling
- Release engineering
- Adjacent subproject work (`Path`, `Merger`, `Splitter`)

Then make sure the patch matches the policy:

- tests added or updated where behavior changed
- docs updated where user-visible behavior changed
- all three `Changes` files updated for releases
- dev release used for significant build-system risk
- commit message follows local conventions
