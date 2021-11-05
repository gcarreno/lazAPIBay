# The Files Object

The Files object is a list of the files contained inside the torrent.

## HTTP Call

```console
$ culr 'https://apibay.org/f.php?id=52427665'
```

## Example Response

```json
[
  {
    "name": [
      "NCIS.S19E01.720p.HDTV.x265-MiNX.mkv"
    ],
    "size": [
      218480061
    ]
  },
  {
    "name": [
      "[TGx]Downloaded from torrentgalaxy.to .txt"
    ],
    "size": [
      718
    ]
  },
  {
    "name": [
      "source.txt"
    ],
    "size": [
      35
    ]
  }
]
```
