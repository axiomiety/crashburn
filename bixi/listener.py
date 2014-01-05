# poll?
# compress periodically?

import  urllib.request
import  json
from    xml.etree       import ElementTree

PARSING_MAP = {
  'id'          : lambda e: e.find('id').text,
  'name'        : lambda e: e.find('name').text,
  'lat'         : lambda e: float(e.find('lat').text),
  'long'        : lambda e: float(e.find('long').text),
  'avail_bikes' : lambda e: int(e.find('nbBikes').text),
  'empty_docks' : lambda e: int(e.find('nbEmptyDocks').text),
  'updatets'    : lambda e: int(e.find('latestUpdateTime').text)
  }

def xml2json(root):
  stations = []
  for station in root.findall('station'):
    s = dict()
    for attrib in PARSING_MAP:
      s[attrib] = PARSING_MAP[attrib](station)
    stations.append(s)
  return json.dumps({root.get('lastUpdate'): stations}, sort_keys=True)

def fetch():
  req = urllib.request.urlopen('https://toronto.bixi.com/data/bikeStations.xml')
  raw_data = req.read()
  if raw_data:
    xml = ElementTree.fromstring(raw_data)
    return xml2json(xml)

#http://maps.googleapis.com/maps/api/distancematrix/json?origins=43.6633,-79.392891&destinations=43.652123,-79.28758&mode=bicycling&sensor=false
