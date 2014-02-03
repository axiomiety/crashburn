# python3 only!

from    hashlib import sha256
import  unittest

# used to sum up the rows of the Merkel transaction tree
def pairs(r):
  it = iter(r)
  try:
    while True:
      a = next(it)
      b = next(it)
      yield a, b
  except StopIteration:
    # if there is an odd number of elements the last one is duplicated
    if len(r) % 2:
      yield a, a

def dhash(a):
  return sha256(sha256(a))

def merkel_root(tl, reduce_fn):
  while len(tl) != 1:
    # because tuple unpacking is gone from python3 : (
    print(list(pairs(tl)))
    tl = list(map(lambda tp: reduce_fn(tp[0]+tp[1]), pairs(tl)))
  return tl.pop()

class TestUtils(unittest.TestCase):
  
  def test_pairs(self):
    r = range(1, 5)
    self.assertEqual( list(pairs(r)), [(1,2), (3,4)] )
    r = range(1, 4)
    self.assertEqual( list(pairs(r)), [(1,2), (3,3)] )

  def test_merkel_root(self):
    reduce_fn = lambda x: x**2 # why not...
    self.assertEqual( merkel_root(range(3), reduce_fn), 289 )
    self.assertEqual( merkel_root(range(4), reduce_fn), 676 )

if __name__ == '__main__':
  unittest.main()
