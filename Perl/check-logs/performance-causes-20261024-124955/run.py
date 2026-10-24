from pathlib import Path
import datetime, json, os, shutil, subprocess

repo=Path('/home/demerphq/git_tree/Sereal')
work=Path('/tmp/sereal-cause')
original=repo/'Perl/check-logs/performance-dumbbench-20261024-123314'
logdir=repo/'Perl/check-logs'/('performance-causes-'+datetime.datetime.now().strftime('%Y%m%d-%H%M%S'))
logdir.mkdir()
(work/'logdir').write_text(str(logdir))
for name in ['worker.pl','fixture.pl']:
    shutil.copy2(original/name,logdir/name)
controller=(original/'controller.pl').read_text()
controller=controller.replace('my ($dir, $case, $operation, $block) = @ARGV;', 'my ($dir, $case, $operation, $block, $probe) = @ARGV;')
controller=controller.replace("('installed', 'current') : ('current', 'installed')", "('current', $probe) : ($probe, 'current')")
controller=controller.replace("if ($variant eq 'current') {", "if (1) {\n        my $base = $variant eq 'current' ? $repo : qq(/tmp/sereal-cause/$variant);")
controller=controller.replace('map {("$repo/Perl/$_/blib/lib", "$repo/Perl/$_/blib/arch")} qw(Encoder Decoder)', 'map {("$base/Perl/$_/blib/lib", "$base/Perl/$_/blib/arch")} qw(Encoder)')
controller=controller.replace('initial_runs => 30, max_iterations => 60','initial_runs => 20, max_iterations => 40')
controller=controller.replace('$case-$operation-$block.json','$probe-$case-$operation-$block.json')
(logdir/'controller.pl').write_text(controller)
for name in ['build.py','run.py']:
    shutil.copy2(work/name,logdir/name)
plan=[('smalltable','freeze-1',2),('smalltable','freeze-100',2),
      ('noguards','freeze-1',1),('noguards','freeze-100',1),
      ('noowner','medium-objects',1),('noowner','medium-plain',1),('noowner','freeze-1',1),
      ('noframes','medium-objects',2),('noframes','medium-plain',2),('notag','medium-objects',2)]
(logdir/'metadata.json').write_text(json.dumps(dict(commit='f17b6149',plan=plan,
    notes='Isolated diagnostic controls. Only smalltable is a candidate safe change. Other builds intentionally omit safety mechanisms and must never be released. Same worker, timing method and flags as the longer benchmark. 20 retained samples, 0.5% target, max40. Variant order reverses for second block.'),indent=2))
env=dict(os.environ)
for k in ['PERL5LIB','PERL5OPT','LD_PRELOAD']: env.pop(k,None)
print(f'Logs: {logdir}',flush=True)
for probe,case,blocks in plan:
    for block in range(1,blocks+1):
        name=f'{probe}-{case}-{block}'
        print(f'START {name}',flush=True)
        with (logdir/(name+'.log')).open('w') as out:
            status=subprocess.run(['/home/demerphq/perl5/perlbrew/perls/perl-5.36.3/bin/perl',str(logdir/'controller.pl'),str(logdir),case,'encode',str(block),probe],env=env,stdout=out,stderr=subprocess.STDOUT).returncode
        print((logdir/(name+'.log')).read_text(),end='',flush=True)
        if status: raise RuntimeError(f'{name}: failed {status}')
print('Completed diagnostic comparisons.',flush=True)
