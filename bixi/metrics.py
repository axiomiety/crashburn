# documentation on Google's Distance Matrix API can be found here:
# https://developers.google.com/maps/documentation/distancematrix/

#TODO:
# it's a diagonal matrix - always order origin/dest?
# actually maybe there'll be differences as distance isn't necessarily the same
# in both directions (think one-way streets)

import  urllib.request
import  json
from    collections     import namedtuple, defaultdict

Location = namedtuple('Location', ['id', 'lat', 'long'])

class DistanceMatrix(object):

  def __init__(self):
    self.matrix = defaultdict(dict)
    self.locations = list()

  @staticmethod
  def fetch(url):
    req = urllib.request.urlopen(url)
    raw_data = req.read()
    if raw_data:
      return json.loads(raw_data.decode('utf8'))
  
  @staticmethod
  def gen_url(origin, dest):
    # sample url:
    # http://maps.googleapis.com/maps/api/distancematrix/json?origins=43.6633,-79.392891&destinations=43.652123,-79.28758&mode=bicycling&sensor=false
    return 'http://maps.googleapis.com/maps/api/distancematrix/json?origins={0},{1}&destinations={2},{3}&mode=bicycling&sensor=false'.format(origin.lat, origin.long, dest.lat, dest.long)

