import json
import os
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

API_KEY = os.environ['YOUTUBE_API_KEY']
VIDEO_IDS = ['abfomyOt8ME','o2ifIkfmCKo','U10z2XTchTc','mTe-_q4lyec','75XhvwQA6RQ','UnpYBFj6IBg']

def fetch(endpoint, params):
    query = urllib.parse.urlencode({**params, 'key': API_KEY})
    req = urllib.request.Request('https://www.googleapis.com/youtube/v3/' + endpoint + '?' + query)
    with urllib.request.urlopen(req, timeout=30) as response:
        return json.load(response)

videos = fetch('videos', {'part': 'snippet,statistics', 'id': ','.join(VIDEO_IDS)}).get('items', [])
if len(videos) != len(VIDEO_IDS):
    raise RuntimeError(f'Expected {len(VIDEO_IDS)} videos, found {len(videos)}; check visibility and IDs')
channel_ids = {v['snippet']['channelId'] for v in videos}
if len(channel_ids) != 1:
    raise RuntimeError('Videos do not belong to the same channel')
channel = fetch('channels', {'part': 'statistics', 'id': next(iter(channel_ids))}).get('items', [])
if not channel:
    raise RuntimeError('Channel not found')
stats = channel[0]['statistics']
subscribers = stats.get('subscriberCount')
episode_stats = []
for video_id in VIDEO_IDS:
    video = video_by_id[video_id]
    s = video['statistics']
    episode_stats.append({
        'id': video_id,
        'title': video['snippet']['title'],
        'views': int(s.get('viewCount', 0)),
        'likes': int(s['likeCount']) if 'likeCount' in s else None,
        'comments': int(s['commentCount']) if 'commentCount' in s else None,
    })

result = {
    'subscribers': int(subscribers) if subscribers is not None else None,
    'views': sum(ep['views'] for ep in episode_stats),
    'episodes': len(episode_stats),
    'episode_stats': episode_stats,
    'updated_utc': datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M UTC'),
}

folder = Path('outreach/Parsimonia')
folder.mkdir(parents=True, exist_ok=True)
(folder / 'youtube_stats.json').write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')

def fmt(n):
    return f'{n:,}'.replace(',', '.') if n is not None else 'No disponible'

tabla_episodios = '''### Estadísticas por episodio

| Episodio | Visualizaciones | Likes | Comentarios |
|:---|---:|---:|---:|
'''
for ep in episode_stats:
    title = ep['title'].replace('|', '-').replace('[', '').replace(']', '')
    tabla_episodios += (
        f"| [{title}](https://www.youtube.com/watch?v={ep['id']}) "
        f"| {fmt(ep['views'])} | {fmt(ep['likes'])} | {fmt(ep['comments'])} |\n"
    )

markdown = f'''## Parsimonia del Caos en números

| Suscriptores en YouTube | Vistas de los 6 episodios | Episodios |
|:---:|:---:|:---:|
| **{fmt(result['subscribers'])}** | **{fmt(result['views'])}** | **{result['episodes']}** |

{tabla_episodios}
*Actualizado: {result['updated_utc']}. Las cifras de suscriptores pueden estar redondeadas por YouTube.*
'''
(folder / '_youtube_stats.qmd').write_text(markdown, encoding='utf-8')
print('Stats updated:', result)