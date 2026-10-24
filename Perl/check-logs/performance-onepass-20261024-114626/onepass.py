import csv
import datetime
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess

repo = Path('/home/demerphq/git_tree/Sereal')
work = Path('/tmp/sereal-perf-9wexqb1r')
logdir = repo / 'Perl/check-logs' / ('performance-onepass-' + datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
logdir.mkdir(parents=True)
(work / 'onepass-logdir').write_text(str(logdir))
perl = '/home/demerphq/perl5/perlbrew/perls/perl-5.44.0/bin/perl'
variants = {'installed-5.010': None, 'onepass': repo}
baseenv = dict(os.environ)
for key in ['PERL5LIB', 'PERL5OPT', 'LD_PRELOAD', 'SEREAL_DEFAULT_KEY', 'SEREAL_CHECKSUM_POLICY', 'SEREAL_BENCH_INSTALLED']:
    baseenv.pop(key, None)
baseenv.update(PERL_HASH_SEED='0', PERL_PERTURB_KEYS='0')
rows = []

def run(phase, case, variant, repeat, args, cwd=None):
    root = variants[variant]
    env = dict(baseenv)
    if root:
        dirs = ['Decoder', 'Encoder', 'Merger', 'Splitter', 'Path', 'Path/Iterator', 'Path/Tie']
        env['PERL5LIB'] = ':'.join(str(root / 'Perl' / d / 'blib' / sub) for d in dirs for sub in ['lib', 'arch'])
    else:
        env['SEREAL_BENCH_INSTALLED'] = '1'
    name = f'{phase}-{case}-{variant}-{repeat}'
    command = ['/usr/bin/time', '-f', 'peak_rss_kb=%M', 'taskset', '-c', '0', perl] + args
    cwd = cwd or (root / 'Perl/Encoder' if root else work)
    with (logdir / (name + '.log')).open('w') as output:
        status = subprocess.run(command, cwd=cwd, env=env, stdout=output, stderr=subprocess.STDOUT).returncode
    text = (logdir / (name + '.log')).read_text()
    if status or 'too few iterations' in text:
        raise RuntimeError(f'{name}: status={status}\n{text}')
    rss = int(re.search(r'peak_rss_kb=(\d+)', text)[1])
    if phase == 'bundled':
        size = int(re.search(r'Sereal OO \(srl\)\s+(\d+) bytes', text)[1])
        parts = re.split(r'\* Timing (encoders|decoders)\n', text)
        for i in range(1, len(parts), 2):
            rate = float(re.search(r'^srl\s+([0-9.]+)/s', parts[i+1], re.M)[1])
            rows.append(dict(phase=phase, case=case, variant=variant, repeat=repeat,
                operation=parts[i], ms=1000 / rate, bytes=size, peak_rss_kb=rss))
    else:
        ms = float(re.search(r'median_ms=([0-9.]+)', text)[1])
        size = int(re.search(r'bytes=(\d+)', text)[1])
        rows.append(dict(phase=phase, case=case, variant=variant, repeat=repeat,
            operation='encode' if phase == 'retention' else case, ms=ms, bytes=size, peak_rss_kb=rss))
    print(f'{name}: done', flush=True)
    with (logdir / 'samples.csv').open('w') as output:
        writer = csv.DictWriter(output, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)

metadata = {'started': datetime.datetime.now().isoformat(), 'perl': perl,
    'cpu': next(line.strip() for line in Path('/proc/cpuinfo').read_text().splitlines() if line.startswith('model name')),
    'cpu_affinity': 0, 'protocol_core': 5, 'protocol_tools': 4,
    'base_commit': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=repo, text=True).strip(),
    'notes': 'Installed 5.010 build flags unknown; One-pass encoder built with GCC -O3; installed build flags unknown. Pinning is on actual callbacks, with no whole-graph pre-scan. Three process runs per case. No concurrent benchmark/build jobs.'}
(logdir / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')
for name in ['onepass.py', 'bench_tools.pl']:
    shutil.copy2(work/name, logdir/name)
for name in ['bench.pl', 'bench_retention.pl']:
    shutil.copy2(repo/'Perl/shared/author_tools'/name, logdir/name)
print(f'Logs: {logdir}', flush=True)
cases = [('tiny', ['--tiny']), ('small', ['--small']), ('medium-objects', ['--medium']),
    ('medium-plain', ['--medium', '--no-bless']), ('large', ['--large']), ('very-large', ['--very-large'])]
for repeat in range(1, 4):
    order = list(variants)
    order = order[repeat-1:] + order[:repeat-1]
    for case, options in cases:
        for variant in order:
            run('bundled', case, variant, repeat,
                [str(repo/'Perl/shared/author_tools/bench.pl'), '--encoder', '--decoder', '--only=^srl$',
                 '--duration=-1', '--protocol-version=5'] + options)
for repeat in range(1, 4):
    for case, count in [('records', 200000), ('arrays', 300000), ('scalars', 200000), ('keys', 300000)]:
        for variant in variants:
            run('retention', case, variant, repeat,
                [str(repo/'Perl/shared/author_tools/bench_retention.pl'), '--case', case, '--count', str(count),
                 '--iterations', '9', '--protocol-version', '5'])

summary = []
for phase, case, operation in sorted(set((r['phase'],r['case'],r['operation']) for r in rows)):
    for variant in variants:
        group = [r for r in rows if (r['phase'],r['case'],r['operation'],r['variant']) == (phase,case,operation,variant)]
        if not group: continue
        times = [r['ms'] for r in group]
        summary.append(dict(phase=phase, case=case, operation=operation, variant=variant,
            median_ms=statistics.median(times), min_ms=min(times), max_ms=max(times),
            bytes=group[0]['bytes'], peak_rss_kb=max(r['peak_rss_kb'] for r in group)))
with (logdir/'summary.csv').open('w') as output:
    writer = csv.DictWriter(output, fieldnames=list(summary[0]))
    writer.writeheader(); writer.writerows(summary)
print(json.dumps(summary, indent=2), flush=True)
