import csv
from pathlib import Path
from datetime import datetime
from collections import defaultdict

root = Path(__file__).resolve().parents[1]
source = root / 'outreach/Parsimonia/spotify_performance.csv'
target = root / 'outreach/Parsimonia/_spotify_stats.qmd'
with source.open(encoding='utf-8-sig', newline='') as f:
    rows = list(csv.DictReader(f))
if not rows:
    raise ValueError('CSV vacío')

def val(row, key):
    raw = (row.get(key) or '').strip()
    return int(float(raw)) if raw else 0

monthly = defaultdict(lambda: [0, 0, 0])
for row in rows:
    date = datetime.strptime(row['Fecha'], '%d/%m/%Y')
    m = date.strftime('%Y-%m')
    monthly[m][0] += val(row, 'Escuchas y descargas')
    monthly[m][1] += val(row, 'Escuchas (en Spotify)')
    monthly[m][2] += val(row, 'Descargas (en otros sitios)')

total = sum(val(r, 'Escuchas y descargas') for r in rows)
spotify = sum(val(r, 'Escuchas (en Spotify)') for r in rows)
others = sum(val(r, 'Descargas (en otros sitios)') for r in rows)
latest = max(datetime.strptime(r['Fecha'], '%d/%m/%Y') for r in rows)
fmt = lambda n: f'{n:,}'.replace(',', '.')
lines = [
    '## Spotify', '',
    '| Escuchas y descargas | Escuchas en Spotify | Descargas en otros sitios |',
    '|:---:|:---:|:---:|',
    f'| **{fmt(total)}** | **{fmt(spotify)}** | **{fmt(others)}** |', '',
    '### Evolución mensual', '',
    '| Mes | Total | Spotify | Otros sitios |',
    '|:---|---:|---:|---:|',
]
for month, nums in sorted(monthly.items()):
    lines.append(f'| {month} | {fmt(nums[0])} | {fmt(nums[1])} | {fmt(nums[2])} |')
lines += ['', f'*Fuente: Spotify for Creators. Datos hasta el {latest:%d/%m/%Y}. Actualizar el CSV para renovar estas cifras.*', '']
target.write_text('\n'.join(lines), encoding='utf-8')
print('Generado:', target)
