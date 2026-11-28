# Perl release checks

Commit fixes and their regression tests before preparing a release. Ensure
new files are tracked. A large passing test count on one Perl configuration
does not establish portability across other versions, numeric formats, or
operating systems.

## Review the preceding release

From `Perl/`, install the collector dependencies if necessary and run:

```sh
cpanm --installdeps cpantesters
perl cpantesters/fetch-reports.pl --version PREVIOUS_VERSION --check
```

This refreshes both distribution indexes and collects full FAIL, UNKNOWN,
and NA reports, with persistent caching and paced requests. PASS counts come
from the indexes; passing reports are not downloaded. Report archives default
to `cpantesters-reports/` beside the checkout, where release cleanups preserve
them. An incomplete fetch is not a successful review.

Investigate each non-pass report. Record its disposition, reason, and evidence
in `cpantesters/triage.json`. Accept `fixed` only when the patch and relevant
validation address the reported failure. Accept `external` only for a
documented support exclusion or environment issue. Leave unexplained failures
`open`; a failed reproduction alone does not establish an external cause.
New report GUIDs require review even when the text resembles an earlier fix.

Use an explicit published version. The CPAN Testers `/release` aggregate has
returned stale release lists during investigation; it is not a reliable
source for automatically choosing the latest published Sereal version.
Version-specific `/summary/DIST/VERSION` indexes and full reports were checked
against the matrix and saved web reports. See `cpantesters/README.md`.

## Required CI coverage

The stable **Perl release checks** job requires all of these to succeed:

- The existing native and threaded Perl matrix, including 5.14, 5.16, and 5.18.
- Long-double and quadmath builds whose Config values confirm wider NVs.
- Windows, including Strawberry Perl 5.24.4, with a failing `rm.exe` ahead of
  Unix utilities on PATH so the runner cannot hide that dependency.
- Existing 32-bit and macOS builds.
- Offline collector/release-tool tests and the parallel Zstd build regression.

Keep all these jobs enabled. Configure branch protection to require the
`Perl release checks` status after the workflow is deployed. The release
script independently checks that this exact job actually ran successfully.
CPAN Testers is contacted during explicit release reviews, rather than on
every push and pull request.

## Prepare and publish

Run `perl fixver.pl VERSION REASON`, review all three changelogs, and inspect
the command chain it prints. It captures the preceding stable changelog
version explicitly, including when preparing a developer release.

Every release, including developer releases, must run
`./clean_regen_and_tidy` after the report review and before the clean
build/test/dist flow. This regenerates shared headers and constants and
formats the Perl sources, including maintainer tools and top-level scripts.
It requires `perltidy` from Perl::Tidy to be installed.
Review the resulting changes. Ensure new source files are tracked before
running it: its clean steps remove untracked and ignored files.

The generated chain performs the report review, regeneration and formatting,
clean build/test/dist flow, and mixed-Encoder compatibility matrix before
committing the release. Generation or formatting failures stop the chain. It
then pushes the release commit and uses `author_tools/check_release_ci.pl`
to wait for that exact commit's `ci.yml` push run and successful `Perl release
checks` job. It refuses uncommitted tracked changes, a different revision,
missing or skipped checks, failed CI, and a timeout. Tags and CPAN uploads
come after this gate. The helper requires an authenticated `gh` CLI.

Do not bypass a red gate to publish. If a gate stops the chain, resolve its
cause and continue from the appropriate step; do not create duplicate release
commits. For significant packaging changes, use a developer release and review
its CPAN Testers results before production publication, as described in
`AGENTS.md`.

After publication, review the new version's non-pass reports with the same
collector. Keep the local platform matrix and report-review records together:
neither one substitutes for the other.
