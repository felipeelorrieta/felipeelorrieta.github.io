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
result = {
    'subscribers': int(subscribers) if subscribers is not None else None,
    'views': sum(int(v['statistics'].get('viewCount', 0)) for v in videos),
    'episodes': len(videos),
    'updated_utc': datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M UTC'),
}
folder = Path('outreach/Parsimonia')
folder.mkdir(parents=True, exist_ok=True)
(folder / 'youtube_stats.json').write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
fmt = lambda n: f'{n:,}'.replace(',', '.') if n is not None else 'No disponible'
markdown = f'''## Parsimonia del Caos en números

| Suscriptores en YouTube | Vistas de los 6 episodios | Episodios |
|:---:|:---:|:---:|
| **{fmt(result['subscribers'])}** | **{fmt(result['views'])}** | **{result['episodes']}** |

*Actualizado: {result['updated_utc']}. Las cifras de suscriptores pueden estar redondeadas por YouTube.*
'''
(folder / '_youtube_stats.qmd').write_text(markdown, encoding='utf-8')
print('Stats updated:', result)
