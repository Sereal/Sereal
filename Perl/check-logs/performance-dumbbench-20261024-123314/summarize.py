import csv
import json
import math
from pathlib import Path
import statistics
import sys

logdir = Path(sys.argv[1]) if len(sys.argv)>1 else Path('/tmp/sereal-dumbbench/logdir').read_text().strip()
logdir = Path(logdir)
data = [row for file in sorted(logdir.glob('*-encode-*.json')) for row in json.loads(file.read_text())]
fields = ['case','block','installed_us','current_us','change_percent','propagated_dumbbench_error_pp',
          'installed_retained','current_retained','installed_samples','current_samples','bytes']
pairs = []
for case in dict.fromkeys(row['case'] for row in data):
    for block in sorted(set(row['block'] for row in data if row['case']==case)):
        pair = {row['variant']:row for row in data if row['case']==case and row['block']==block}
        old,new = pair['installed'],pair['current']
        a,b = old['mean_seconds'],new['mean_seconds']
        ratio = b/a
        error = ratio*math.hypot(old['uncertainty_seconds']/a,new['uncertainty_seconds']/b)*100
        pairs.append(dict(case=case,block=block,installed_us=a*1e6,current_us=b*1e6,
            change_percent=(ratio-1)*100,propagated_dumbbench_error_pp=error,
            installed_retained=old['retained'],current_retained=new['retained'],
            installed_samples=len(old['timings']),current_samples=len(new['timings']),bytes=new['metadata']['bytes']))
with (logdir/'paired-results.csv').open('w') as out:
    writer=csv.DictWriter(out,fieldnames=fields)
    writer.writeheader()
    writer.writerows(pairs)
summary=[]
for case in dict.fromkeys(row['case'] for row in pairs):
    group=[row for row in pairs if row['case']==case]
    changes=[row['change_percent'] for row in group]
    row=dict(case=case,blocks=len(group),installed_us=statistics.median(r['installed_us'] for r in group),
        current_us=statistics.median(r['current_us'] for r in group),
        median_change_percent=statistics.median(changes),min_change_percent=min(changes),max_change_percent=max(changes),
        samples=sum(r['installed_samples']+r['current_samples'] for r in group),bytes=group[0]['bytes'])
    summary.append(row)
with (logdir/'summary.csv').open('w') as out:
    writer=csv.DictWriter(out,fieldnames=list(summary[0]))
    writer.writeheader()
    writer.writerows(summary)
print('Positive change = slower. Errors in paired-results.csv are stock Dumbbench uncertainties, not 95% confidence intervals.')
for row in summary:
    print('{case:16} blocks={blocks} installed={installed_us:10.3f}us current={current_us:10.3f}us change={median_change_percent:+6.2f}% range=[{min_change_percent:+6.2f}%,{max_change_percent:+6.2f}%] samples={samples}'.format(**row))
