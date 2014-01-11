# poll?
# compress periodically?

import  urllib.request
import  json
import  time
import  logging
import  os
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

POLLING_INTERVAL = 10 # a minute
UPDATES_DIR = 'updates'

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

def poll(delay=None):
  delay = delay or POLLING_INTERVAL
  if not os.path.exists(UPDATES_DIR):
    os.mkdir(UPDATES_DIR)
  baseline = fetch()
  running = True
  while(running):
    snap = fetch()
    if snap != baseline: # we have an update
      logging.info('update found!')
      o = json.loads(snap)
      ts = list(o.keys())[0]
      # save it out!
      with open(os.path.join(UPDATES_DIR, ts), 'a') as f:
        f.write(snap)
      baseline = snap

    time.sleep(delay)
    logging.info('slept %s seconds. yawn...' % POLLING_INTERVAL)

if __name__ == '__main__':
  poll()
