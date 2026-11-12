import json
import os
import re
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

API_KEY = os.environ['YOUTUBE_API_KEY']
# Video existente: solo se usa para identificar el canal, no para listar episodios.
REFERENCE_VIDEO_ID = 'abfomyOt8ME'
FOLDER = Path('outreach/Parsimonia')
EPISODE_PATTERN = re.compile(r'^\s*EP\.?\s*0*(\d+)\b', re.IGNORECASE)


def fetch(endpoint, params):
    query = urllib.parse.urlencode({**params, 'key': API_KEY})
    url = 'https://www.googleapis.com/youtube/v3/' + endpoint + '?' + query
    with urllib.request.urlopen(url, timeout=30) as response:
        return json.load(response)


reference = fetch('videos', {'part': 'snippet', 'id': REFERENCE_VIDEO_ID}).get('items', [])
if not reference:
    raise RuntimeError('No se pudo identificar el canal a partir del video de referencia')
channel_id = reference[0]['snippet']['channelId']
channels = fetch('channels', {'part': 'statistics,contentDetails', 'id': channel_id}).get('items', [])
if not channels:
    raise RuntimeError('No se encontró el canal de YouTube')
channel = channels[0]
uploads_id = channel['contentDetails']['relatedPlaylists']['uploads']

# Recorrer todas las páginas de la playlist de subidas del canal.
uploads = []
page_token = None
while True:
    params = {'part': 'snippet', 'playlistId': uploads_id, 'maxResults': 50}
    if page_token:
        params['pageToken'] = page_token
    page = fetch('playlistItems', params)
    uploads.extend(page.get('items', []))
    page_token = page.get('nextPageToken')
    if not page_token:
        break

# Detectar episodios por prefijo EP01, EP.06, EP 07, etc.
selected = {}
for item in uploads:
    snippet = item.get('snippet', {})
    title = snippet.get('title', '')
    match = EPISODE_PATTERN.match(title)
    video_id = snippet.get('resourceId', {}).get('videoId')
    if match and video_id:
        selected[video_id] = {'title': title, 'number': int(match.group(1))}

if not selected:
    raise RuntimeError('No se encontraron episodios con títulos que comiencen por EP')

# Consultar estadísticas en lotes de hasta 50 videos.
video_by_id = {}
ids = list(selected)
for start in range(0, len(ids), 50):
    batch = ids[start:start + 50]
    items = fetch('videos', {'part': 'statistics', 'id': ','.join(batch)}).get('items', [])
    video_by_id.update({video['id']: video for video in items})

rows = []
for video_id, info in selected.items():
    if video_id not in video_by_id:
        continue
    stats = video_by_id[video_id]['statistics']
    rows.append({
        'title': info['title'],
        'number': info['number'],
        'video_id': video_id,
        'views': int(stats.get('viewCount', 0)),
        'likes': int(stats['likeCount']) if 'likeCount' in stats else None,
        'comments': int(stats['commentCount']) if 'commentCount' in stats else None,
    })
rows.sort(key=lambda row: row['number'], reverse=True)
if not rows:
    raise RuntimeError('No se pudieron obtener estadísticas de los episodios')

subscribers = channel['statistics'].get('subscriberCount')
result = {
    'subscribers': int(subscribers) if subscribers is not None else None,
    'views': sum(row['views'] for row in rows),
    'episodes': len(rows),
    'episode_stats': rows,
    'updated_utc': datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M UTC'),
}
FOLDER.mkdir(parents=True, exist_ok=True)
(FOLDER / 'youtube_stats.json').write_text(
    json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8'
)


def fmt(value):
    return f'{value:,}'.replace(',', '.') if value is not None else 'No disponible'


def clean_title(title):
    return title.replace('|', '–').replace('\n', ' ').replace('\r', ' ')


table_rows = []
for row in rows:
    link = f"[{clean_title(row['title'])}](https://www.youtube.com/watch?v={row['video_id']})"
    table_rows.append(
        f"| {link} | {fmt(row['views'])} | {fmt(row['likes'])} | {fmt(row['comments'])} |"
    )
table = '\n'.join(table_rows)
markdown = f'''## Parsimonia del Caos en números

| Suscriptores en YouTube | Vistas de los episodios | Episodios |
|:---:|:---:|:---:|
| **{fmt(result['subscribers'])}** | **{fmt(result['views'])}** | **{result['episodes']}** |

### Estadísticas por episodio

| Episodio | Visualizaciones | Likes | Comentarios |
|:---|---:|---:|---:|
{table}

*Actualizado: {result['updated_utc']}. Las cifras de suscriptores pueden estar redondeadas por YouTube.*
'''
(FOLDER / '_youtube_stats.qmd').write_text(markdown, encoding='utf-8')
print(f"Estadísticas actualizadas: {len(rows)} episodios, {fmt(result['views'])} vistas")
