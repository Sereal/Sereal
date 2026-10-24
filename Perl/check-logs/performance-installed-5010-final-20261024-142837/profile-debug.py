from pathlib import Path
import os,re,shutil,subprocess
repo=Path('/home/demerphq/git_tree/Sereal')
root=Path('/tmp/sereal-iteration')
env=dict(os.environ,CCACHE_DIR='/tmp/sereal-fix-ccache',PERL_HASH_SEED='0',PERL_PERTURB_KEYS='0')
for key in ['PERL5LIB','PERL5OPT','LD_PRELOAD']:env.pop(key,None)
sources={'current':repo/'Perl/Encoder','installed':Path('/home/demerphq/.cpanm/work/1792580624.1365740/Sereal-Encoder-5.010')}
for label,source in sources.items():
    target=root/('debug-'+label)/'Perl/Encoder'
    target.parent.mkdir(parents=True,exist_ok=True)
    if label=='current':
        (target.parent/'shared').symlink_to(repo/'Perl/shared',target_is_directory=True)
        (target.parent/'this_is_the_Sereal_repo.txt').touch()
    shutil.copytree(source,target,symlinks=True)
    p=target/'Makefile';s=p.read_text();s=re.sub(r'^(CCFLAGS = .*)$',r'\1 -g',s,flags=re.M);p.write_text(s)
    os.utime(target/'srl_encoder.c',None)
    with (root/('debug-'+label+'-build.log')).open('w') as out:
        subprocess.run(['make','-C',str(target),'-j2'],env=env,stdout=out,stderr=subprocess.STDOUT,check=True)
    print(label+' debug build done',flush=True)
    e=dict(env,PERL5LIB=str(target/'blib/lib')+':'+str(target/'blib/arch'))
    with (root/(label+'-debug-callgrind.log')).open('w') as out:
        subprocess.run(['valgrind','--tool=callgrind','--callgrind-out-file='+str(root/(label+'-debug.callgrind')),
            '/home/demerphq/perl5/perlbrew/perls/perl-5.44.0/bin/perl','/tmp/sereal-cause/profile.pl'],env=e,stdout=out,stderr=subprocess.STDOUT,check=True)
    with (root/(label+'-annotation.txt')).open('w') as out:
        subprocess.run(['callgrind_annotate','--auto=yes','--threshold=99',str(root/(label+'-debug.callgrind')),str(target/'srl_encoder.c')],stdout=out,check=True)
    print(label+' profile done',flush=True)
