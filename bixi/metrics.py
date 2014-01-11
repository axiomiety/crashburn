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
    self.locations = DistanceMatrix.getLocations()
    self.matrix = DistanceMatrix.getDistanceMatrix()

  def getDistance(self, locA, locB):
    if locA.id in self.matrix and locB.id in self.matrix:
      return self.matrix[locA.id][locB.id]

  @staticmethod
  def getLocations(cached=True):
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
      
  @staticmethod
  def getDistanceMatrix(cached=True):
    if cached and os.path.isfile(PFILE_DISTANCE_MATRIX):
      with open(PFILE_DISTANCE_MATRIX, 'rb') as f:
        return pickle.load(f)
    else:
      raise Exception('dynamic loading of the distance matrix is not supported')

  @staticmethod
  def _batchLoadDistanceMatrix(batchsize=100, dryrun=False):
    locations = DistanceMatrix.getLocations()
    locationsMap = {loc.id: loc for loc in locations}
    # unfortunately dist(A,B) is not necessarily the same as dist(B,A)
    perms = list(permutations(locationsMap.keys(), 2))
    # the free Distance Matrix only allows a certain number of requests per day
    # we need to be able to batch those up so we can leave where we left off
    status = dict()
    ist = defaultdict(dict)
    ist['distance_matrix'] = defaultdict(dict)
    if os.path.isfile(PFILE_BATCHLOADER): # we started processing
      with open(PFILE_BATCHLOADER, 'rb') as f:
        ist = pickle.load(f)  
    
    remaining_location_pairs = ist.get('remaining_location_pairs', perms)
    logging.info('remaining location pairs prior: %s' % len(remaining_location_pairs))
    while(batchsize and remaining_location_pairs):
      (aid, bid) = remaining_location_pairs.pop()
      a = locationsMap[aid]
      b = locationsMap[bid]
      o = DistanceMatrix.fetch(DistanceMatrix.gen_url(a, b))
      if o:
        ist['distance_matrix'][a.id][b.id] = o
        batchsize = batchsize-1
      else:
        logging.error('no data returned for %s and %s' % (a , b))
        # we abort and push the pair back
        remaining_location_pairs.append((aid, bid))
        batchsize=False
    logging.info('remaining location pairs after: %s' % len(remaining_location_pairs))

    if not dryrun:
      with open(PFILE_BATCHLOADER, 'wb') as f:
        # update status & save progress
        ist['remaining_location_pairs'] = remaining_location_pairs
        pickle.dump(ist, f)

    if not remaining_location_pairs: # we're done!
      logging.info('finished building location matrix!')
      with open(PFILE_DISTANCE_MATRIX, 'wb') as f:
        pickle.dump(ist['distance_matrix'], f)

    return ist

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

    logging.error('status not OK: %s' % dr['status'])
    return None

  @staticmethod
  def gen_url(origin, dest):
    # sample url:
    # http://maps.googleapis.com/maps/api/distancematrix/json?origins=43.6633,-79.392891&destinations=43.652123,-79.28758&mode=bicycling&sensor=false
    return 'http://maps.googleapis.com/maps/api/distancematrix/json?origins={0},{1}&destinations={2},{3}&mode=bicycling&sensor=false'.format(origin.lat, origin.long, dest.lat, dest.long)
