# python3 only!

from    hashlib import sha256
import  unittest


###
# blockchain

def merkel_root(tl, reduce_fn):
  while len(tl) != 1:
    # because tuple unpacking is gone from python3 : (
    tl = list(map(lambda tp: reduce_fn(tp[0]+tp[1]), pairs(tl)))
  return tl.pop()

def pairs(r):
  '''used to sum up the rows of the Merkel transaction tree'''
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

###
# encryption-related

PRIMES = [2,3,5,7] # will cache primes - we could use a static list but hey...

def is_prime(n):
  from math import sqrt, ceil
  if n > 1:
    if n == 2:
      return True
    elif n % 2 == 0:
      return False
    else:
      for q in range(2, ceil(sqrt(n)+1)): #TODO: it's exclusive, is ceil sufficient?
        if n % q == 0:
          return False
      return True

def next_prime(p):
  # p is prime, and p > 2 => all p's will be odd
  candidate = p + 2
  while not is_prime(candidate):
    candidate += 2
  return candidate

def get_nth_prime(n):
  # for n > 1!
  if n > len(PRIMES): # we haven't cached this prime yet
    while len(PRIMES) != n:
      last_known_prime = PRIMES[-1]
      PRIMES.append(next_prime(last_known_prime))
  return PRIMES[n-1] # 0-based index
        

class RSA(object):
  '''an over-simplified RSA look-alike'''

  # sources:
  # http://content.hccfl.edu/pollock/AUnixSec/PublicKeyDemo.htm

  RANDINT_MIN = 10
  RANDINT_MAX = 100

  @staticmethod

  @staticmethod
  def gen_pair():
    from random import randint
    (n1, n2) = randint(RANDINT_MIN, RANDINT_MAX), randint(RANDINT_MIN, RANDINT_MAX)

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

  def test_nth_prime(self):
    self.assertEqual( len(PRIMES), 4 )
    p = get_nth_prime(12345)
    self.assertEqual( p, 132241 )
    self.assertEqual( len(PRIMES), 12345 )

if __name__ == '__main__':
  unittest.main()
