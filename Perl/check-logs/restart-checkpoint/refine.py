import csv
import os
from pathlib import Path
import re
import statistics
import subprocess

repo = Path('/home/demerphq/git_tree/Sereal')
work = Path('/tmp/sereal-perf-9wexqb1r')
logdir = Path((work/'logdir').read_text().strip())
perl = '/home/demerphq/perl5/perlbrew/perls/perl-5.44.0/bin/perl'
variants = {'installed-5.010': None, 'before332': work/'before332', 'merged332': repo}
rows = []
for repeat in range(1,4):
    order = list(variants)
    order = order[repeat-1:] + order[:repeat-1]
    for variant in order:
        env = dict(os.environ)
        for key in ['PERL5LIB','PERL5OPT','LD_PRELOAD','SEREAL_DEFAULT_KEY','SEREAL_CHECKSUM_POLICY','SEREAL_BENCH_INSTALLED']:
            env.pop(key, None)
        env.update(PERL_HASH_SEED='0',PERL_PERTURB_KEYS='0')
        root=variants[variant]
        if root:
            env['PERL5LIB']=':'.join(str(root/'Perl'/d/'blib'/s) for d in ['Decoder','Encoder'] for s in ['lib','arch'])
        else:
            env['SEREAL_BENCH_INSTALLED']='1'
        cwd=root/'Perl/Encoder' if root else work
        args=['/usr/bin/time','-f','peak_rss_kb=%M','taskset','-c','0',perl,
              str(repo/'Perl/shared/author_tools/bench.pl'),'--only=^srl$','--decoder',
              '--very-large','--duration=-4','--protocol-version=5']
        name=f'refine-very-large-decoder-{variant}-{repeat}'
        with (logdir/(name+'.log')).open('w') as output:
            subprocess.run(args,cwd=cwd,env=env,stdout=output,stderr=subprocess.STDOUT,check=True)
        text=(logdir/(name+'.log')).read_text()
        rate=float(re.search(r'^srl\s+([0-9.]+)/s',text,re.M)[1])
        rows.append(dict(variant=variant,repeat=repeat,ms=1000/rate))
        print(name,rows[-1],flush=True)
with (logdir/'refinement.csv').open('w') as output:
    writer=csv.DictWriter(output,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
for variant in variants:
    values=[r['ms'] for r in rows if r['variant']==variant]
    print(variant,'median_ms',statistics.median(values),'min',min(values),'max',max(values),flush=True)
