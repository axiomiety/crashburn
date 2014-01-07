# documentation on Google's Distance Matrix API can be found here:
# https://developers.google.com/maps/documentation/distancematrix/

#TODO:
# it's a diagonal matrix - always order origin/dest?
# actually maybe there'll be differences as distance isn't necessarily the same
# in both directions (think one-way streets)

import  listener
import  urllib.request
import  json
import  pickle
import  os.path
import  logging
from    itertools       import  permutations
from    collections     import  namedtuple, defaultdict


Location = namedtuple('Location', ['id', 'name', 'lat', 'long'])

PFILE_LOCATIONS       = 'locations.pickle'
PFILE_DISTANCE_MATRIX = 'dmatrix.pickle'
PFILE_BATCHLOADER     = 'batchloader.pickle'

class DistanceMatrix(object):

  def __init__(self, loadDistanceMatrix=False):
    self.locations = self.getLocations()
    if loadDistanceMatrix:
      self.matrix = self.getDistanceMatrix()
    else:
      self.matrix = defaultdict(dict)

  def getLocations(self, cached=True):
    if cached and os.path.isfile(PFILE_LOCATIONS): # read from local pickle file
      with open(PFILE_LOCATIONS, 'rb') as f:
        locations = pickle.load(f)
    else:
      snapshot = json.loads(listener.fetch())
      locations = [ Location(**{ k: loc[k] for k in Location._fields }) for loc in snapshot ]
      # we went through the trouble of parsing, we might as well save the results
      with open(PFILE_LOCATIONS, 'wb') as f:
        pickle.dump(locations, f)
    
    return locations  
      
  def getDistanceMatrix(self, cached=True):
    if cached:
      with open(PFILE_DISTANCE_MATRIX, 'rb') as f:
        return picke.load(f)
    else:
      self.locationsMap = {loc.id: loc for loc in self.locations}
      # unfortunately dist(A,B) is not necessarily the same as dist(B,A)
      perms = list(permutations(self.locationsMap.keys(), 2))
      # the free Distance Matrix only allows a certain number of requests per day
      # we need to be able to batch those up so we can leave where we left off
      status = dict()
      ist = dict()
      if os.path.isfile(PFILE_BATCHLOADER):
        with open(PFILE_BATCHLOADER, 'wb') as f:
          ist = pickle.load(f)  
      
      status['remaining_location_pairs'] = ist.get('remaining_location_pairs', perms)
      
      dm[a.id][b.id] = fetch(gen_url(a, b))
    
  @staticmethod
  def fetch(url):
    '''
    data returned is of this format:
    [{'elements': [{'duration': {'text': '11 mins', 'value': 686}, 'distance': {'text': '2.8 km', 'value': 2771}, 'status': 'OK'}]}]
    '''
    logging.debug('fetching %s' % url)
    req = urllib.request.urlopen(url)
    raw_data = req.read()
    if raw_data:
      dr = json.loads(raw_data.decode('utf8'))
      if dr['status'] == 'OK':
        e = dr['rows'][0]['elements'][0]
        return e['duration']['value'] # time is what we're after

    #TODO: log this as an error!
    return None

  @staticmethod
  def gen_url(origin, dest):
    # sample url:
    # http://maps.googleapis.com/maps/api/distancematrix/json?origins=43.6633,-79.392891&destinations=43.652123,-79.28758&mode=bicycling&sensor=false
    return 'http://maps.googleapis.com/maps/api/distancematrix/json?origins={0},{1}&destinations={2},{3}&mode=bicycling&sensor=false'.format(origin.lat, origin.long, dest.lat, dest.long)
