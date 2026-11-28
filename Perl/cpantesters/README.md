# CPAN Testers report collection

This repository-only Perl tool collects CPAN Testers reports for the Encoder
and Decoder. It uses the official [v3 API](https://api.cpantesters.org/docs/?url=/v3)
instead of crawling the matrix pages. It does not belong in the distribution
MANIFESTs.

Run from `Perl/`, with Perl 5.14 or newer:

```sh
cpanm --installdeps cpantesters
perl cpantesters/fetch-reports.pl --version 7.008
```

The default selection is FAIL, UNKNOWN, and NA reports for both distributions.
UNKNOWN includes build failures that never reached the test suite.
`--version` is required. The API's release aggregates can be stale and are not
used to discover the published version. `--dist NAME` is repeatable.

Passing reports are counted from the indexes and never downloaded. To limit
collection to FAIL and UNKNOWN while investigating a particular problem:

```sh
perl cpantesters/fetch-reports.pl --version 7.008 \
    --grades fail,unknown
```

Downloads default to `cpantesters-reports/` **beside the repository checkout**.
Keeping the archive outside the checkout preserves it across the release
scripts' `git clean -dfx`. Use `--output DIR` to choose another persistent
location. Contents include:

- `manifest.json`: report metadata, selection time, per-distribution counts
  for every grade (including PASS), download status, failure
  excerpts, and links to the web reports. Earlier selections remain in the
  report ledger.
- `summary.tsv`: the current selection, suitable for a spreadsheet or `rg`.
- `reports/GUID.json`: the full structured API report, including its environment.
- `reports/GUID.txt`: all output sections, decoded as UTF-8.
- `cache/`: validated JSON responses and HTTP cache validators.
- `rate.json`: the next allowed request time, including server cooldowns.
- `challenges/`: unexpected robot-check pages, if encountered.

## Polite operation and resuming

Requests are sequential, with **10 seconds of idle time after each response**.
The minimum permitted `--delay` is 5 seconds. Redirects and retries also use
this pacing. A lock prevents simultaneous collectors sharing an output
directory. Use the same output directory for successive runs so they share
the request schedule and cache.

Indexes are cached for six hours, with a minimum configurable lifetime of one
hour. `--refresh-index` revalidates them, using ETag or Last-Modified when
available. Full reports are reused permanently. A missing report is retried
after one hour. Each run attempts at most 50 new non-pass reports by default.
Repeating the same command continues pending downloads.
`--max-reports 0` fetches only the indexes. `--offline` prohibits all network
access and uses cached indexes even if they are old.

Temporary errors use bounded exponential backoff. Both forms of `Retry-After`
(seconds and HTTP dates) are honored. Long waits or exhausted retries stop
the run and persist the cooldown, so restarting does not immediately retry.
Other HTTP failures stop collection. Successful earlier downloads survive
an interruption.

The tool identifies itself, verifies TLS, and restricts redirects to the API's
HTTPS origin. A robot challenge is saved and stops the run immediately,
including challenges returned with HTTP 200. It never executes challenge
JavaScript or stores a challenge as a valid index. If a check is required,
complete it in a browser and export cookies in Netscape format:

```sh
perl cpantesters/fetch-reports.pl --version 7.008 --cookies /path/to/cookies.txt
```

Some browser checks bind cookies to the browser or User-Agent, so cookie import
may not be sufficient. `--user-agent` accepts an explicit identification string.
The API normally does not require the matrix site's challenge.

## Pre-release review

Before releasing, refresh reports for the **preceding published stable release**:

```sh
perl cpantesters/fetch-reports.pl --version 7.008 --check
```

This implies `--refresh-index`; offline mode is deliberately incompatible with
the check. It exits 2 if a selected non-pass report is unreviewed or unavailable,
or a download cap leaves it pending. HTTP, schema, and other collection errors
exit 1. Exit 0 means every selected non-pass report was downloaded and has a
documented review. An empty API index cannot produce a successful check.
This check requires FAIL, UNKNOWN, and NA, including support exclusions;
`--grades` cannot narrow a pre-release check.

Record decisions in the checked-in `triage.json`, keyed by report GUID:

```json
{
  "611ef0ee-eaae-11f1-a28f-6209cf400910": {
    "status": "fixed",
    "reason": "Use non-mutating array-slot inspection on older Perls",
    "evidence": ["953 and 955 regression tests pass on Perl 5.16.3"]
  }
}
```

Allowed resolved statuses are `fixed` and `external`; each requires a reason
and a nonempty evidence list. Use `open` for unresolved reports. `external`
means a documented environment or support issue that requires no release
patch. A bare status flag is insufficient. Reviews are explicit per GUID;
the tool does not automatically dismiss new reports because their failure
text resembles a previously fixed bug. The pre-release check validates review
records; the maintainer remains responsible for their evidence and judgment.

For an older release, pass its version explicitly, for example
`--version 7.008 --check`. Rerun until downloads complete, investigate new
reports, update review evidence, and rerun the check before releasing. This
complements the local Perl/platform and mixed-module-version test matrices.

`fixver.pl` determines the preceding stable version from the three changelogs,
requires them to agree, and includes this check with that explicit version at
the start of the release command chain, followed by mandatory regeneration
and formatting before rebuilding. It also waits for successful CI on the
pushed release commit before tagging or uploading. See `../RELEASING.md`.

## Verification

```sh
prove -v cpantesters/t/collector.t
```

The tests use a fake transport and clock, with no network traffic. They check
pacing, caching, resuming, filtering, download limits, challenge handling,
server cooldowns, schema mismatches, UTF-8 output, and review gating.

The endpoint schemas are documented in the API's
[OpenAPI specification](https://github.com/cpan-testers/cpantesters-api/blob/master/share/v3.json).
The CPAN Testers report website itself retrieves structured report data from
the API or its storage frontend and renders `result.output`. The local
collector retains every output section, including `uncategorized` reports.
