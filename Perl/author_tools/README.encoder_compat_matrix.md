# Encoder compatibility matrix

`encoder_compat_matrix.pl` checks the current Decoder against the latest
stable Encoder release in each supported historical major (2 and up), and
against the current checkout for the latest major. It generates named packet
corpora with each selected Encoder in its own Perl process, then decodes those
packets with the current Decoder in a separate process. It also runs the
current Decoder distribution test suite with the selected Encoder first in
`PERL5LIB`.

Run from any directory:

```sh
perl Perl/author_tools/encoder_compat_matrix.pl --plan
perl Perl/author_tools/encoder_compat_matrix.pl
```

By default, a unique persistent workspace is created under the system temp
directory. It contains build snapshots, build and test logs, generated corpora,
and `results.json`. The named historical branches and worktrees remain in the
Git repository and are reused across runs, including when a new temporary
workspace is created. Keep the workspace to investigate failures or replay the
corpus. Use `--work-dir PATH` to choose a stable location for repeat runs.
`--fetch` explicitly fetches tags and `main`; otherwise only local refs are
used. `--corpus-only` skips the full Decoder suite. `--self-test` runs focused
version-selection checks without creating worktrees.

Stable selection compares numeric version components and excludes underscore
development releases. `main` supplies the current module major; stable majors
below 2 are intentionally omitted. Persistent historical branches are named `maint_perl/v2` through
`maint_perl/v5`. Existing local branch commits are reused without resetting;
new checkouts track `origin/maint_perl/vN` when available, then fall back to
the selected release tag. The invoking main checkout supplies the current
major and the current candidate build.
Historical source is copied from its worktree into an isolated source/build
snapshot because old Makefile.PL files expect a real `.git` directory. The
current major uses the invoking main checkout so an unreleased candidate is
tested. Corpus generation asserts that the selected Encoder was loaded from
its own `blib`; current Decoder validation is a separate process and asserts
that the current Decoder came from this checkout. Fixtures cover integer and
string-length boundaries, bytes and Unicode, containers, scalar references,
blessing, shared references, cycles, regexps where the Encoder supports them,
and Snappy compression. Compression is confirmed from the wire header.

The release recipe runs `perl author_tools/encoder_compat_matrix.pl`
immediately after `./clean_make_all`. The runner rebuilds the current Decoder
once, and uses the Perl interpreter that launched the script for all builds and
test harnesses. Keep that interpreter the same one used by `clean_make_all`.

Build failures and compatibility failures are recorded separately in each
entry's status and logs. Any failed matrix entry returns a nonzero exit code.
The runner does not fetch by default or clean/reset the invoking checkout. It
rebuilds the current Decoder and Encoder, generating build artifacts there.
