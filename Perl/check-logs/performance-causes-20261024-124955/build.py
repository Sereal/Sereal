from pathlib import Path
import os, shutil, subprocess

repo=Path('/home/demerphq/git_tree/Sereal')
root=Path('/tmp/sereal-cause')
source=(repo/'Perl/Encoder/srl_encoder.c').read_text()
variants={
 'smalltable':source.replace('enc->callback_seen = PTABLE_new();','enc->callback_seen = PTABLE_new_size(3);'),
 'notag':source.replace('#define SRL_CLASS_ID(value) INT2PTR(void *, PTR2UV(value) | (UV)1)','#define SRL_CLASS_ID(value) ((void *)(value))'),
 'noowner':source,
 'noframes':source,
 'noguards':source,
}
def empty_function(s,name):
    start=s.index('\n'+name+'(')
    a=s.index('{',start)
    depth=1; b=a+1
    while depth:
        depth+=(s[b]=='{')-(s[b]=='}'); b+=1
    return s[:a]+'{ /* UNSAFE diagnostic control; no hostile callbacks allowed. */ }'+s[b:]
variants['noguards']=empty_function(empty_function(source,'srl_callback_begin'),'srl_callback_end')
variants['noframes']=source.replace('srl_frame_push(enc, &frame, (SV *)src);','/* UNSAFE diagnostic: no traversal frame */').replace('enc->frames = frame.parent;','/* no frame to pop */').replace('enc->frames->cursor = &he;','/* no cursor frame */')
# The non-magical sorted path also initializes frame fields; unused by the measured unsorted workloads.
env=dict(os.environ,CCACHE_DIR='/tmp/sereal-fix-ccache')
for variant,s in variants.items():
    target=root/variant/'Perl/Encoder'
    target.parent.mkdir(parents=True,exist_ok=True)
    (target.parent/'shared').symlink_to(repo/'Perl/shared',target_is_directory=True)
    (target.parent/'this_is_the_Sereal_repo.txt').touch()
    shutil.copytree(repo/'Perl/Encoder',target,symlinks=True)
    (target/'srl_encoder.c').write_text(s)
    if variant=='noowner':
        p=target/'Encoder.xs'
        p.write_text(p.read_text().replace('SAVEFREESV(SvREFCNT_inc(encoder_sv));','/* UNSAFE diagnostic: encoder owner unguarded */'))
    with (root/(variant+'-build.log')).open('w') as out:
        status=subprocess.run(['make','-C',str(target),'-j2'],env=env,stdout=out,stderr=subprocess.STDOUT).returncode
    if status: raise RuntimeError(f'{variant}: build failed')
    print(f'{variant}: built',flush=True)
