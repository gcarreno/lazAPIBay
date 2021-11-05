# The Query Object

The query object is a list of results from the query.

## HTTP Calls

### Torrent query

```console
$ curl 'https://apibay.org/q.php?q=NCIS%20S19E01&cat=200'
```

### User query

**Default Page 0**

```console
$ curl 'https://apibay.org/q.php?q=user:TvTeam'
```

**Page 0**

```console
$ curl 'https://apibay.org/q.php?q=user:TvTeam:0'
```

**Page 1**

```console
$ curl 'https://apibay.org/q.php?q=user:TvTeam:1'
```

### User Page Count

```console
$ curl 'https://apibay.org/q.php?q=pcnt:TvTeam'
```

This call returns a single integer number, it does **not** return a JSON string.

## Example Response

```json
[
  {
    "id": "52427669",
    "name": "NCIS S19E01 720p HDTV x264-SYNCOPY",
    "info_hash": "2B69D205EA979D41DA2CAE80107FDB45E9375D62",
    "leechers": "6",
    "seeders": "30",
    "num_files": "0",
    "size": "1000981135",
    "username": "jajaja",
    "added": "1632190813",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52427657",
    "name": "NCIS.S19E01.HDTV.x264-TORRENTGALAXY",
    "info_hash": "7FEFCD575EA5FAD3ADD2E4D53AC4F7892C53949A",
    "leechers": "0",
    "seeders": "13",
    "num_files": "3",
    "size": "296429369",
    "username": "Anonymous",
    "added": "1632190621",
    "status": "vip",
    "category": "205",
    "imdb": "tt0364845"
  },
  {
    "id": "52427665",
    "name": "NCIS.S19E01.720p.HDTV.x265-MiNX[TGx]",
    "info_hash": "F3A787AEDA1303AC98A72D8B377BB059C22587B4",
    "leechers": "2",
    "seeders": "12",
    "num_files": "3",
    "size": "218480814",
    "username": "Anonymous",
    "added": "1632190811",
    "status": "vip",
    "category": "208",
    "imdb": "tt0364845"
  },
  {
    "id": "52428576",
    "name": "NCIS S19E01 480p x264-mSD",
    "info_hash": "E77845A25CA312BEC7148EF1987EA53E4CC4A675",
    "leechers": "0",
    "seeders": "9",
    "num_files": "0",
    "size": "235877171",
    "username": "jajaja",
    "added": "1632192615",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52431991",
    "name": "NCIS S19E01 1080p WEB H264-PLZPROPER",
    "info_hash": "FF962DCACF042C49D15456222B8AE5C0B36DA6AD",
    "leechers": "1",
    "seeders": "5",
    "num_files": "0",
    "size": "1471026299",
    "username": "jajaja",
    "added": "1632201611",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52432879",
    "name": "NCIS S19E01 1080p HEVC x265-MeGusta",
    "info_hash": "4885FCC9B5EFB564D9F9C0F232E45BE60FA33A0E",
    "leechers": "1",
    "seeders": "3",
    "num_files": "0",
    "size": "338008474",
    "username": "jajaja",
    "added": "1632203413",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52431979",
    "name": "NCIS.S19E01.720p.WEB.H264-PLZPROPER[ettv]",
    "info_hash": "748E5C9AABE87565B026D6FCAEF81318DCEBEA63",
    "leechers": "0",
    "seeders": "2",
    "num_files": "5",
    "size": "1028744473",
    "username": "EtHD",
    "added": "1632201013",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52443282",
    "name": "NCIS.S19E01.Blood.in.the.Water.1080p.AMZN.WEBRip.DDP5.1.x264-NTb",
    "info_hash": "ADB0BDCE5BB7927D2EA6741986B46795FF120BF5",
    "leechers": "1",
    "seeders": "2",
    "num_files": "2",
    "size": "3091749636",
    "username": "Anonymous",
    "added": "1632230477",
    "status": "vip",
    "category": "208",
    "imdb": "tt0364845"
  },
  {
    "id": "52428582",
    "name": "NCIS S19E01 720p HEVC x265-MeGusta",
    "info_hash": "1445DBF4134D0E31AEBECEBFA5C397914CD51DDF",
    "leechers": "2",
    "seeders": "1",
    "num_files": "0",
    "size": "296799437",
    "username": "jajaja",
    "added": "1632192615",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52428584",
    "name": "NCIS S19E01 XviD-AFG",
    "info_hash": "39A70413F505483E62B50C58FEBCD5CE67D203D9",
    "leechers": "1",
    "seeders": "1",
    "num_files": "0",
    "size": "395543839",
    "username": "jajaja",
    "added": "1632192615",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52431973",
    "name": "NCIS.S19E01.720p.WEB.H264-PLZPROPER[TGx]",
    "info_hash": "43BC7CDCB6A0593F623DDBDE79A01DA683DFDB4E",
    "leechers": "0",
    "seeders": "1",
    "num_files": "3",
    "size": "1004508645",
    "username": "Anonymous",
    "added": "1632200800",
    "status": "vip",
    "category": "208",
    "imdb": "tt0364845"
  },
  {
    "id": "52434991",
    "name": "NCIS S19E01 INTERNAL 480p x264-mSD",
    "info_hash": "527688FBDCE64C6307D892799DD9FE4691183936",
    "leechers": "0",
    "seeders": "1",
    "num_files": "0",
    "size": "178132091",
    "username": "jajaja",
    "added": "1632208812",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52435616",
    "name": "NCIS S19E01 INTERNAL 1080p HEVC x265-MeGusta",
    "info_hash": "0D10318424A204878F930ABB31C3AA697FA13FEB",
    "leechers": "0",
    "seeders": "1",
    "num_files": "0",
    "size": "342999695",
    "username": "jajaja",
    "added": "1632210613",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52443284",
    "name": "NCIS.S19E01.Blood.in.the.Water.720p.AMZN.WEBRip.DDP5.1.x264-NTb[",
    "info_hash": "4E7593AA31256F849870C992BC0113A5C95509A1",
    "leechers": "0",
    "seeders": "1",
    "num_files": "2",
    "size": "1336930796",
    "username": "Anonymous",
    "added": "1632230580",
    "status": "vip",
    "category": "208",
    "imdb": "tt0364845"
  },
  {
    "id": "52471313",
    "name": "NCIS S19E01 Blood in the Water 1080p AMZN WEB-DL DDP5 1 H 264-NT",
    "info_hash": "FA173ED22F57FA363082C441EEBD24268B2D5DC7",
    "leechers": "0",
    "seeders": "1",
    "num_files": "0",
    "size": "3092376453",
    "username": "jajaja",
    "added": "1632304215",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52427654",
    "name": "NCIS.S19E01.720p.HDTV.x264-SYNCOPY[TGx]",
    "info_hash": "42FD1C4BC23B2169BD8A59315A10C4104990BE25",
    "leechers": "1",
    "seeders": "0",
    "num_files": "3",
    "size": "1000988288",
    "username": "Anonymous",
    "added": "1632190311",
    "status": "vip",
    "category": "208",
    "imdb": "tt0364845"
  },
  {
    "id": "52427667",
    "name": "NCIS.S19E01.720p.HDTV.x264-SYNCOPY[ettv]",
    "info_hash": "FF716005AAEF6A070D475974E2BBA3B173EB86B1",
    "leechers": "1",
    "seeders": "0",
    "num_files": "5",
    "size": "1037802260",
    "username": "EtHD",
    "added": "1632190813",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52428260",
    "name": "NCIS.S19E01.XviD-AFG[TGx]",
    "info_hash": "93319A1259F43058CB9C5DB184ED50BECF818C47",
    "leechers": "1",
    "seeders": "0",
    "num_files": "3",
    "size": "395543066",
    "username": "Anonymous",
    "added": "1632191009",
    "status": "vip",
    "category": "205",
    "imdb": "tt0364845"
  },
  {
    "id": "52428267",
    "name": "NCIS S19E01 AAC MP4-Mobile",
    "info_hash": "FD616C42F076D0444AC4A59F41704A5B85448040",
    "leechers": "1",
    "seeders": "0",
    "num_files": "6",
    "size": "171347496",
    "username": "TvTeam",
    "added": "1632191314",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52428268",
    "name": "NCIS S19E01 XviD-AFG",
    "info_hash": "36D7D526F79D6D4BC08C41E9E29CCE7297611171",
    "leechers": "1",
    "seeders": "0",
    "num_files": "6",
    "size": "396471316",
    "username": "TvTeam",
    "added": "1632191322",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52428544",
    "name": "NCIS S19E01 480p x264-mSD",
    "info_hash": "5C246942854B14E714E4C9C3EADBABF04B5B9458",
    "leechers": "0",
    "seeders": "0",
    "num_files": "6",
    "size": "236688169",
    "username": "TvTeam",
    "added": "1632191942",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52431977",
    "name": "NCIS.S19E01.1080p.WEB.H264-PLZPROPER[TGx]",
    "info_hash": "49308FEAAFDF29487A83C08476783875700D2A0E",
    "leechers": "1",
    "seeders": "0",
    "num_files": "3",
    "size": "1473988504",
    "username": "Anonymous",
    "added": "1632200958",
    "status": "vip",
    "category": "208",
    "imdb": "tt0364845"
  },
  {
    "id": "52431993",
    "name": "NCIS S19E01 720p WEB H264-PLZPROPER",
    "info_hash": "607BF8838E47A320641021F21596B92F33460452",
    "leechers": "1",
    "seeders": "0",
    "num_files": "0",
    "size": "1004504351",
    "username": "jajaja",
    "added": "1632201611",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52434982",
    "name": "NCIS S19E01 INTERNAL 480p x264-mSD",
    "info_hash": "0CBFB4C721FB0AE23B82E50D298138A8EE4C3D20",
    "leechers": "1",
    "seeders": "0",
    "num_files": "6",
    "size": "178775291",
    "username": "TvTeam",
    "added": "1632208719",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52434983",
    "name": "NCIS S19E01 INTERNAL AAC MP4-Mobile",
    "info_hash": "862E6E9C3F57E8F867DA76F79E7F6FACAF0B6A83",
    "leechers": "1",
    "seeders": "0",
    "num_files": "6",
    "size": "173011190",
    "username": "TvTeam",
    "added": "1632208730",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52434984",
    "name": "NCIS S19E01 INTERNAL XviD-AFG",
    "info_hash": "BED5FFD922EE17CF64B484A0A5C0CECF3B44C6CD",
    "leechers": "0",
    "seeders": "0",
    "num_files": "6",
    "size": "416360948",
    "username": "TvTeam",
    "added": "1632208739",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52434990",
    "name": "NCIS S19E01 INTERNAL XviD-AFG",
    "info_hash": "AC64D6AB384C91A142E3D1F1DC276A451795A47D",
    "leechers": "1",
    "seeders": "0",
    "num_files": "0",
    "size": "415634555",
    "username": "jajaja",
    "added": "1632208812",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52434993",
    "name": "NCIS S19E01 INTERNAL 1080p WEB h264-GOSSIP",
    "info_hash": "2503B23267995B32C44DCBEB701927D61FC634D0",
    "leechers": "1",
    "seeders": "0",
    "num_files": "0",
    "size": "2963527434",
    "username": "jajaja",
    "added": "1632208812",
    "status": "vip",
    "category": "208",
    "imdb": ""
  },
  {
    "id": "52465918",
    "name": "NCIS.S19E01.INTERNAL.720p.WEB.h264-GOSSIP[ettv]",
    "info_hash": "286E7B6750804CB9483D4CA4839AF121B227F1F0",
    "leechers": "1",
    "seeders": "0",
    "num_files": "5",
    "size": "1597721145",
    "username": "EtHD",
    "added": "1632288312",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52465925",
    "name": "NCIS.S19E01.INTERNAL.1080p.WEB.h264-GOSSIP[ettv]",
    "info_hash": "F3BE696B4EFE1B1528725473D053BB95884046ED",
    "leechers": "1",
    "seeders": "0",
    "num_files": "5",
    "size": "3032978509",
    "username": "EtHD",
    "added": "1632288617",
    "status": "vip",
    "category": "205",
    "imdb": ""
  },
  {
    "id": "52465955",
    "name": "NCIS S19E01 INTERNAL 720p WEB h264-GOSSIP",
    "info_hash": "DBF33114BB4B6CB60A4926606330ED36E5274645",
    "leechers": "1",
    "seeders": "0",
    "num_files": "0",
    "size": "1556925645",
    "username": "jajaja",
    "added": "1632289811",
    "status": "vip",
    "category": "208",
    "imdb": ""
  }
]
```
