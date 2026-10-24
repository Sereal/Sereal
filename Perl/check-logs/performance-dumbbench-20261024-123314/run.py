import datetime
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

repo = Path('/home/demerphq/git_tree/Sereal')
work = Path('/tmp/sereal-dumbbench')
logdir = repo / 'Perl/check-logs' / ('performance-dumbbench-' + datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
logdir.mkdir(parents=True)
(work/'logdir').write_text(str(logdir))
for name in ['worker.pl', 'controller.pl', 'run.py']:
    shutil.copy2(work/name, logdir/name)
source = (repo/'Perl/shared/author_tools/bench.pl').read_text()
function = source[source.index('sub make_data {'):source.index('\nif ($diagrams)')]
(logdir/'fixture.pl').write_text('''use strict;
use warnings;
my ($tiny_data, $small_data, $medium_data, $large_data, $very_large_data) = (0,0,1,0,0);
my $nobless = $ENV{SEREAL_FIXTURE_PLAIN};
my ($data_set_name, @str, @rand);
''' + function + '\nmake_data();\n')
metadata = dict(started=datetime.datetime.now().isoformat(),
    commit=subprocess.check_output(['git','rev-parse','HEAD'], cwd=repo, text=True).strip(),
    cpu=next(s for s in Path('/proc/cpuinfo').read_text().splitlines() if s.startswith('model name')),
    cpu_affinity=0, protocol=5, initial_samples=30, max_samples=60, target_precision=0.005,
    process_blocks=3, timed_batch_min_seconds=0.2,
    methodology='Dumbbench 0.505 controls persistent Perl 5.44 workers; single_run returns internally measured CLOCK_MONOTONIC batch time divided by encode count. Construction, warmup, IPC, and startup excluded. No dry-run subtraction. Stock MAD outlier filtering. Three new-process blocks with variant order reversed in block 2. Fixed Perl hash seeds. Same compiler and -O3 in installed/current Makefiles.',
    binaries={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in [
        repo/'Perl/Encoder/blib/arch/auto/Sereal/Encoder/Encoder.so',
        Path('/home/demerphq/perl5/perlbrew/perls/perl-5.44.0/lib/site_perl/5.44.0/x86_64-linux/auto/Sereal/Encoder/Encoder.so')]})
(logdir/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
env = dict(os.environ)
for key in ['PERL5LIB','PERL5OPT','LD_PRELOAD']:
    env.pop(key, None)
perl = '/home/demerphq/perl5/perlbrew/perls/perl-5.36.3/bin/perl'
cases = ['records','arrays','scalars','keys','medium-objects','medium-plain','freeze-1','freeze-100']
print(f'Logs: {logdir}', flush=True)
for block in range(1,4):
    for case in cases:
        name = f'{case}-encode-{block}'
        print(f'START {name} {datetime.datetime.now().isoformat()}', flush=True)
        with (logdir/(name+'.log')).open('w') as out:
            status = subprocess.run([perl,str(logdir/'controller.pl'),str(logdir),case,'encode',str(block)],
                env=env, stdout=out, stderr=subprocess.STDOUT).returncode
        print((logdir/(name+'.log')).read_text(), end='', flush=True)
        if status:
            sys.exit(status)
        print(f'DONE {name} {datetime.datetime.now().isoformat()}', flush=True)
metadata['finished'] = datetime.datetime.now().isoformat()
(logdir/'metadata.json').write_text(json.dumps(metadata, indent=2)+'\n')
print('All benchmark blocks complete.', flush=True)
