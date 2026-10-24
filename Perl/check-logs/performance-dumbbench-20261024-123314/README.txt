Long Encoder comparison using Dumbbench 0.505

Run: /home/demerphq/perl5/perlbrew/perls/perl-5.36.3/bin/perl controller.pl LOGDIR CASE encode BLOCK
Analyze: python3 summarize.py LOGDIR
The run.py orchestrator creates a new timestamped log directory and runs all blocks.

Both measured workers use Perl 5.44.0. The 5.36.3 interpreter runs only the
Dumbbench statistical controller. Each worker constructs its own identical input,
creates a reusable encoder with protocol_version=5 and no compression/checksum,
warms up, and calibrates a power-of-two batch to at least 0.2 seconds. Timings use
CLOCK_MONOTONIC inside the worker, cover only repeated calls and returned-value
cleanup, and are divided by the batch count. Worker startup, construction, IPC,
calibration and statistical analysis are outside the measured interval.

The medium input comes directly from bundled bench.pl make_data(), preserving
blessed hashes and parent relations. The four large retention cases match
bench_retention.pl. FREEZE cases follow freeze_thaw_timing.pl with 1 or 100 Foo
objects and callbacks enabled. Both versions must produce equal packet lengths.
This checks size equality, not byte-for-byte output equality or correctness.

Workers run sequentially on CPU 0 with PERL_HASH_SEED=0 and PERL_PERTURB_KEYS=0.
Each case has three fresh-process comparisons; block 2 reverses version order.
There are at least 30 retained samples per version/block, a 60-sample cap, and
0.5% target precision using stock Dumbbench MAD filtering. All raw timings,
including rejected outliers, are saved in JSON. No dry-run subtraction is used.

Dumbbench reports its robust uncertainty estimate. This is NOT a 95% confidence
interval and does not include between-process/environment variation. Use the
paired-results.csv process comparisons and summary.csv ranges to judge repeatability.
Positive change percentages mean the current Encoder is slower.

The CPAN 5.010 build Makefile and local Makefile both specify GCC, -O3 and
matching CCFLAGS. Metadata records the current commit and both binary hashes.
No library source changes are made by this benchmark.
