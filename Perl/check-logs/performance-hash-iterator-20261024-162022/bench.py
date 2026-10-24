from pathlib import Path
import datetime,json,os,shutil,subprocess,sys
repo=Path('/home/demerphq/git_tree/Sereal')
baseline=Path(sys.argv[1]); label=sys.argv[2]; cases=sys.argv[3:]
old=repo/'Perl/check-logs/performance-guard-fixes-20261024-132117'
p=repo/'Perl/check-logs'/('performance-'+label+'-'+datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
p.mkdir()
Path('/tmp/sereal-iteration/logdir').write_text(str(p))
worker=(old/'worker.pl').read_text().replace('/^freeze-(1|100)$/','/^freeze-(?:allow-)?(1|100)$/')
worker=worker.replace("freeze_callbacks => ($case =~ /freeze/ ? 1 : 0)","($case =~ /allow/ ? (freeze_allow_classes => sub {1}) : ()), freeze_callbacks => ($case =~ /freeze/ ? 1 : 0)")
(p/'worker.pl').write_text(worker)
controller=(old/'controller.pl').read_text().replace('qq(/tmp/sereal-cause/$variant)', '$ENV{SEREAL_BENCH_BASE}')
(p/'controller.pl').write_text(controller)
shutil.copy2(old/'fixture.pl',p/'fixture.pl');shutil.copy2('/tmp/sereal-iteration/bench.py',p/'bench.py')
(p/'working-tree.patch').write_text(subprocess.check_output(['git','diff','HEAD','--','Perl/Encoder'],cwd=repo,text=True))
(p/'metadata.json').write_text(json.dumps(dict(baseline=str(baseline),label=label,cases=cases,
    notes='Dumbbench 0.505, persistent Perl5.44 workers, protocol5, CPU0, fixed hash seeds, internally measured batches >=0.2s. 30 retained samples minimum, 0.5% target, max60. Two fresh-process blocks with reversed order. No concurrent tests/builds.'),indent=2))
env=dict(os.environ,SEREAL_BENCH_BASE=str(baseline))
for k in ['PERL5LIB','PERL5OPT','LD_PRELOAD']:env.pop(k,None)
print(f'Logs: {p}',flush=True)
for block in [1,2]:
    for case in cases:
        name=f'{case}-{block}';print('START '+name,flush=True)
        with (p/(name+'.log')).open('w') as out:
            subprocess.run(['/home/demerphq/perl5/perlbrew/perls/perl-5.36.3/bin/perl',str(p/'controller.pl'),str(p),case,'encode',str(block),'previous'],env=env,stdout=out,stderr=subprocess.STDOUT,check=True)
        print((p/(name+'.log')).read_text(),end='',flush=True)
rows=[]
for f in sorted(p.glob('previous-*.json')):
    pair={r['variant']:r for r in json.loads(f.read_text())};a,b=pair['previous'],pair['current']
    r=dict(case=a['case'],block=a['block'],before_us=a['mean_seconds']*1e6,after_us=b['mean_seconds']*1e6,
        change_percent=100*(b['mean_seconds']/a['mean_seconds']-1),before_error_percent=100*a['uncertainty_seconds']/a['mean_seconds'],after_error_percent=100*b['uncertainty_seconds']/b['mean_seconds'])
    rows.append(r);print(r,flush=True)
(p/'summary.json').write_text(json.dumps(rows,indent=2)+'\n')
print('Completed.',flush=True)
