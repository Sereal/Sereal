from pathlib import Path
import datetime,json,os,shutil,subprocess
repo=Path('/home/demerphq/git_tree/Sereal')
old=repo/'Perl/check-logs/performance-causes-20261024-124955'
p=repo/'Perl/check-logs'/('performance-guard-fixes-'+datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
p.mkdir()
Path('/tmp/sereal-cause/postfix-logdir').write_text(str(p))
worker=(old/'worker.pl').read_text().replace("} elsif ($case =~ /^freeze-(1|100)$/) {", """} elsif ($case =~ /^sorted-freeze-(1000)$/) {
    $data = {map {("key-$_" => bless({name=>'blargh'}, 'Foo'))} 1..$1};
} elsif ($case =~ /^freeze-(1|100)$/) {""").replace("freeze_callbacks => ($case =~ /^freeze/ ? 1 : 0)","freeze_callbacks => ($case =~ /freeze/ ? 1 : 0), sort_keys => ($case =~ /^sorted/ ? 1 : 0)")
(p/'worker.pl').write_text(worker)
controller=(old/'controller.pl').read_text().replace('initial_runs => 20, max_iterations => 40','initial_runs => 30, max_iterations => 60')
(p/'controller.pl').write_text(controller)
for name in ['fixture.pl','sorted-scaling.pl']:shutil.copy2(old/name,p/name)
shutil.copy2('/tmp/sereal-cause/finalfix.py',p/'finalfix.py')
(p/'working-tree.patch').write_text(subprocess.check_output(['git','diff','HEAD','--','Perl/Encoder'],cwd=repo,text=True))
cases=['freeze-1','sorted-freeze-1000','medium-objects','medium-plain']
(p/'metadata.json').write_text(json.dumps(dict(base='f17b6149',baseline='/tmp/sereal-cause/prefix',current=str(repo),cases=cases,blocks=2,notes='Same persistent Perl5.44 workers, protocol5, fixed hash seed, CPU0, internally timed calibrated batches >=0.2s. 30 retained samples minimum, 0.5% Dumbbench target, max60. Prefix is unmodified HEAD, current contains small callback table and sorted-entry-at-use validation; sorted_keyed occupies existing frame padding and is initialized only in the sorted path. Reversed order in second block. No tests or builds run during timing.'),indent=2))
env=dict(os.environ)
for k in ['PERL5LIB','PERL5OPT','LD_PRELOAD']:env.pop(k,None)
print(f'Logs: {p}',flush=True)
for block in [1,2]:
    for case in cases:
        name=f'{case}-{block}'
        print(f'START {name}',flush=True)
        with (p/(name+'.log')).open('w') as out:
            subprocess.run(['/home/demerphq/perl5/perlbrew/perls/perl-5.36.3/bin/perl',str(p/'controller.pl'),str(p),case,'encode',str(block),'prefix'],env=env,stdout=out,stderr=subprocess.STDOUT,check=True)
        print((p/(name+'.log')).read_text(),end='',flush=True)
print('All fix comparisons complete.',flush=True)
