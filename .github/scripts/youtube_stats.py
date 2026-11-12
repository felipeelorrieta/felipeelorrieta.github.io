import json
import os
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

API_KEY = os.environ['YOUTUBE_API_KEY']
EPISODES = [
    ('EP06 · Abriendo la Ciencia', 'abfomyOt8ME'),
    ('EP05 · Tecnología y Emprendimiento', 'o2ifIkfmCKo'),
    ('EP04 · Perspectiva Crítica', 'U10z2XTchTc'),
    ('EP03 · Consultoría Estadística', 'mTe-_q4lyec'),
    ('EP02 · Educación Estadística', '75XhvwQA6RQ'),
    ('EP01 · La Bioestadística', 'UnpYBFj6IBg'),
]

def fetch(endpoint, params):
    query = urllib.parse.urlencode({**params, 'key': API_KEY})
    url = 'https://www.googleapis.com/youtube/v3/' + endpoint + '?' + query
    with urllib.request.urlopen(url, timeout=30) as response:
        return json.load(response)

video_ids = [video_id for _, video_id in EPISODES]
videos = fetch('videos', {'part': 'snippet,statistics', 'id': ','.join(video_ids)}).get('items', [])
video_by_id = {video['id']: video for video in videos}
missing = [video_id for video_id in video_ids if video_id not in video_by_id]
if missing:
    raise RuntimeError('YouTube no devolvió estos videos: ' + ', '.join(missing))

channel_ids = {video['snippet']['channelId'] for video in videos}
if len(channel_ids) != 1:
    raise RuntimeError('Los videos no pertenecen todos al mismo canal')
channels = fetch('channels', {'part': 'statistics', 'id': next(iter(channel_ids))}).get('items', [])
if not channels:
    raise RuntimeError('No se encontró el canal')
channel_stats = channels[0]['statistics']
subscribers = channel_stats.get('subscriberCount')

rows = []
for title, video_id in EPISODES:
    stats = video_by_id[video_id]['statistics']
    rows.append({
        'title': title,
        'video_id': video_id,
        'views': int(stats.get('viewCount', 0)),
        'likes': int(stats['likeCount']) if 'likeCount' in stats else None,
        'comments': int(stats['commentCount']) if 'commentCount' in stats else None,
    })

result = {
    'subscribers': int(subscribers) if subscribers is not None else None,
    'views': sum(row['views'] for row in rows),
    'episodes': len(rows),
    'episode_stats': rows,
    'updated_utc': datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M UTC'),
}

folder = Path('outreach/Parsimonia')
folder.mkdir(parents=True, exist_ok=True)
(folder / 'youtube_stats.json').write_text(
    json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8'
)

def fmt(value):
    return f'{value:,}'.replace(',', '.') if value is not None else 'No disponible'

table_rows = []
for row in rows:
    link = f"[{row['title']}](https://www.youtube.com/watch?v={row['video_id']})"
    table_rows.append(
        f"| {link} | {fmt(row['views'])} | {fmt(row['likes'])} | {fmt(row['comments'])} |"
    )

table = '\n'.join(table_rows)
markdown = f'''## Parsimonia del Caos en números

| Suscriptores en YouTube | Vistas de los 6 episodios | Episodios |
|:---:|:---:|:---:|
| **{fmt(result['subscribers'])}** | **{fmt(result['views'])}** | **{result['episodes']}** |

### Estadísticas por episodio

| Episodio | Visualizaciones | Likes | Comentarios |
|:---|---:|---:|---:|
{table}

*Actualizado: {result['updated_utc']}. Las cifras de suscriptores pueden estar redondeadas por YouTube.*
'''
(folder / '_youtube_stats.qmd').write_text(markdown, encoding='utf-8')
print('Estadísticas actualizadas:', result)
