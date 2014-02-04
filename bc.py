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
  # assumes p is prime, and p > 2 => all p's will be odd
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

def get_rand_relative_prime(p):
  from fractions import gcd # why re-invent the wheel? says the guy re-implementing a version of RSA
  from random import randint
  while True:
    candidate = randint(3, p-1) # start from 3
    if gcd(p, candidate) == 1:
      return candidate

# https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
def egcd(a, b):
  x,y, u,v = 0,1, 1,0
  while a != 0:
      q, r = b//a, b%a
      m, n = x-u*q, y-v*q
      b,a, x,y, u,v = a,r, u,v, m,n
  return b, x, y

def modinv(a, m):
  g, x, y = egcd(a, m)
  if g != 1:
      return None  # modular inverse does not exist
  else:
      return x % m

class RSA(object):
  '''an over-simplified RSA look-alike'''

  # sources:
  # http://content.hccfl.edu/pollock/AUnixSec/PublicKeyDemo.htm

  RANDINT_MIN = 10
  RANDINT_MAX = 100

  @staticmethod
  def gen_pair(p=None, q=None):
    from random import randint
    n1, n2 = randint(RSA.RANDINT_MIN, RSA.RANDINT_MAX), randint(RSA.RANDINT_MIN, RSA.RANDINT_MAX)
    p, q = p or get_nth_prime(n1), q or get_nth_prime(n2)
    n = p*q
    phi = (p-1)*(q-1)
    # we now need a number e < phi such that gcd(phi, e) = 1
    e = get_rand_relative_prime(phi)
    d = modinv(e, phi)
    return [(e,n), (d,n)]

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

  def test_get_rand_relative_prime(self):
    p = 20
    prime_factors = [2, 5]
    num_rounds = 10
    while num_rounds:
      self.assertFalse( get_rand_relative_prime(p) in prime_factors )
      num_rounds -= 1

if __name__ == '__main__':
  unittest.main()
