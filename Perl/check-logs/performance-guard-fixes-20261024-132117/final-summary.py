import csv, json
from pathlib import Path
p=Path(Path('/tmp/sereal-cause/postfix-logdir').read_text())
rows=[]
for f in sorted(p.glob('prefix-*-encode-*.json')):
    pair={r['variant']:r for r in json.loads(f.read_text())}
    a,b=pair['prefix'],pair['current']
    rows.append(dict(case=a['case'],block=a['block'],before_us=a['mean_seconds']*1e6,
        after_us=b['mean_seconds']*1e6,change_percent=100*(b['mean_seconds']/a['mean_seconds']-1),
        before_error_percent=100*a['uncertainty_seconds']/a['mean_seconds'],
        after_error_percent=100*b['uncertainty_seconds']/b['mean_seconds'],
        before_samples=len(a['timings']),after_samples=len(b['timings']),bytes=b['metadata']['bytes']))
with (p/'paired-results.csv').open('w') as out:
    writer=csv.DictWriter(out,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
for r in rows: print('{case:20} block={block} {before_us:10.4f}us -> {after_us:10.4f}us {change_percent:+.2f}%'.format(**r))
