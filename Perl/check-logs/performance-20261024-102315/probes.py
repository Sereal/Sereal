import csv
import os
from pathlib import Path
import re
import statistics
import subprocess

repo=Path('/home/demerphq/git_tree/Sereal')
work=Path('/tmp/sereal-perf-9wexqb1r')
logs=Path((work/'logdir').read_text().strip())
perl='/home/demerphq/perl5/perlbrew/perls/perl-5.44.0/bin/perl'
roots={'control':repo,'object-check':work/'probe-object-check','no-scan':work/'probe-no-scan'}
rows=[]
for repeat in range(1,4):
    for case in ['medium-objects','records','arrays','scalars']:
        for name,root in roots.items():
            env=dict(os.environ)
            for key in ['PERL5LIB','PERL5OPT','LD_PRELOAD','SEREAL_DEFAULT_KEY','SEREAL_CHECKSUM_POLICY','SEREAL_BENCH_INSTALLED']:
                env.pop(key,None)
            env.update(PERL_HASH_SEED='0',PERL_PERTURB_KEYS='0')
            env['PERL5LIB']=':'.join(str(root/'Perl'/d/'blib'/s) for d in ['Encoder','Decoder'] for s in ['lib','arch'])
            if case=='medium-objects':
                args=[str(repo/'Perl/shared/author_tools/bench.pl'),'--encoder','--medium','--only=^srl$','--duration=-2','--protocol-version=5']
            else:
                count=300000 if case=='arrays' else 200000
                args=[str(repo/'Perl/shared/author_tools/bench_retention.pl'),'--case',case,'--count',str(count),'--iterations','9','--protocol-version','5']
            path=logs/f'probe-{name}-{case}-{repeat}.log'
            command=['/usr/bin/time','-f','peak_rss_kb=%M','taskset','-c','0',perl]+args
            with path.open('w') as output:
                subprocess.run(command,cwd=root/'Perl/Encoder',env=env,stdout=output,stderr=subprocess.STDOUT,check=True)
            text=path.read_text()
            if case=='medium-objects':
                rate=float(re.search(r'^srl\s+([0-9.]+)/s',text,re.M)[1]); ms=1000/rate
                size=int(re.search(r'Sereal OO \(srl\)\s+(\d+) bytes',text)[1])
            else:
                ms=float(re.search(r'median_ms=([0-9.]+)',text)[1]);size=int(re.search(r'bytes=(\d+)',text)[1])
            rows.append(dict(variant=name,case=case,repeat=repeat,ms=ms,bytes=size))
            print(rows[-1],flush=True)
with (logs/'probes.csv').open('w') as output:
    writer=csv.DictWriter(output,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
for case in ['medium-objects','records','arrays','scalars']:
    print(case,{name:statistics.median(r['ms'] for r in rows if r['case']==case and r['variant']==name) for name in roots},flush=True)
